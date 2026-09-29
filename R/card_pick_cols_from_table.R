# card_pick_cols_from_table.R v10 (DEBUG BUILD - has cat() diagnostics, remove once confirmed working)
#
# v10: choices is now passed alongside selected in restore()'s
# updateSelectInput() call for "table" - NOT selected alone. This was a
# targeted fix based on comparing restore() against the module's own
# steady-state observeEvent(tables_reactive(), ...) below, which always
# passes both together. Testing so far has NOT yet confirmed whether this
# alone resolves the underlying symptom (row1's Table dropdown showing a
# single empty <option> after a Load+re-upload cycle) - see the cat()
# diagnostics still present below for the current debugging session.
#
# v9: added restore() to pickColsServer()'s return value, for the
# Save/Load session feature. UNLIKE Tabs 1/2/7/8, this module's restore
# CANNOT run immediately on load - both its inputs (`table`, `column`)
# only have valid choices once tables_reactive() actually contains data,
# which (per this session's design decision) only happens after the user
# re-uploads matching CSVs post-load.
#
# restore() therefore does NOT eagerly apply the saved table/column here.
# Instead it returns immediately (FALSE) if the saved table isn't yet
# present in tables_reactive(), and the CALLING module (Tab 5/6, which
# owns the re-upload-matching observer) is responsible for calling
# restore() again once that table has appeared - restore() is safe to
# call repeatedly/speculatively and simply no-ops until its target table
# exists.
#
# Reusable sub-module: given a named list of data frames (as produced by
# tableMetadataServer()'s `data` reactive), let the user pick ONE table and
# then pick one or more columns from that table. Used by both the Geography
# module (lat/lon/site - single column each) and the Taxonomy module
# (scientific name column(s), possibly across multiple tables).
#
# This module does NOT read files itself - it is handed reactive data from
# Tab 3 so there is a single source of truth for "what tables/columns exist".
pickColsUI <- function(id, table_label = "Table", col_label = "Column",
                       multiple_cols = FALSE) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    shiny::selectInput(ns("table"), table_label, choices = NULL, width = "100%"),
    shiny::selectInput(ns("column"), col_label, choices = NULL, width = "100%",
                       multiple = multiple_cols),
    col_widths = c(6, 6)
  )
}
#' @param id module id
#' @param tables_reactive a reactive() returning a named list of data.frames
#'   (names = file names), e.g. the `data` reactive from tableMetadataServer()
#' @param filter_fn optional function(df) -> character vector of column names
#'   to restrict the column choices for a given table (e.g. only numeric
#'   columns, or only character columns). Defaults to all columns.
#' @return a reactive() list(table = <name>, column = <name(s)>, data = <df>),
#'   plus a restore(saved_table, saved_column) function - see file header
#'   note on why this restore is conditional/repeatable rather than
#'   immediate.
#' @noRd
pickColsServer <- function(id, tables_reactive, filter_fn = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Holds a saved column selection awaiting application by
    # observeEvent(input$table, ...) below, once IT rebuilds column's
    # choices in response to a table change (including one caused by
    # restore()'s own updateSelectInput() on `table`). See restore()'s
    # comment for the full explanation of why this hand-off exists.
    pending_column_restore <- shiny::reactiveVal(NULL)

    shiny::observeEvent(tables_reactive(), {
      tbls <- tables_reactive()
      if (length(tbls) == 0) {
        shiny::updateSelectInput(session, "table", choices = character(0))
        shiny::updateSelectInput(session, "column", choices = character(0))
        return(invisible(NULL))
      }
      shiny::updateSelectInput(session, "table", choices = names(tbls),
                               selected = input$table %||% names(tbls)[1])
    }, ignoreNULL = FALSE)
    shiny::observeEvent(input$table, {
      tbls <- tables_reactive()
      if (is.null(input$table) || is.null(tbls[[input$table]])) {
        shiny::updateSelectInput(session, "column", choices = character(0))
        return(invisible(NULL))
      }
      df <- tbls[[input$table]]
      col_choices <- if (!is.null(filter_fn)) filter_fn(df) else names(df)
      if (length(col_choices) == 0) {
        shiny::showNotification(
          paste0("No eligible columns found in '", input$table, "' for this field."),
          type = "warning"
        )
      }

      # Apply any pending column restoration as the LAST step here,
      # rather than in restore() itself - see restore()'s comment for why:
      # this observer already rebuilds `column`'s choices on every table
      # change (as it must, for normal user-driven table switches too,
      # not just restores), so it is the single authoritative place that
      # should also apply a saved selection, instead of a second observer
      # racing to set it independently and getting clobbered.
      pending <- pending_column_restore()
      if (!is.null(pending)) {
        pending_column_restore(NULL)
        saved_cols <- pending[!is.na(pending) & nzchar(pending)]
        matched_cols <- intersect(saved_cols, col_choices)
        if (length(matched_cols) > 0) {
          shiny::updateSelectInput(session, "column", choices = col_choices, selected = matched_cols)
          return(invisible(NULL))
        }
        # saved column(s) no longer exist in this version of the file
        # (e.g. renamed/removed) - fall through to the normal
        # choices-only update below, same as restore()'s prior behavior
        # for this case
      }

      shiny::updateSelectInput(session, "column", choices = col_choices)
    })
    result <- shiny::reactive({
      tbls <- tables_reactive()
      shiny::req(input$table, input$column, tbls[[input$table]])
      list(
        table = input$table,
        column = input$column,
        data = tbls[[input$table]]
      )
    })

    #' Attempt to restore a saved table/column selection. Safe to call
    #' before the target table exists in tables_reactive() - it simply
    #' does nothing in that case (returns FALSE) rather than erroring, so
    #' callers (Tab 5/6) can call this speculatively every time
    #' tables_reactive() changes, until it succeeds.
    #'
    #' @param saved_table character - the saved table name, or NA/NULL
    #' @param saved_column character - the saved column name(s), or NA/NULL
    #' @return logical - TRUE if the restore was actually applied (target
    #'   table was present), FALSE if skipped (target table not yet
    #'   available, or saved_table was NA/NULL to begin with)
    restore <- function(saved_table, saved_column) {
      if (is.null(saved_table) || is.na(saved_table) || !nzchar(saved_table)) {
        return(FALSE)
      }
      tbls <- tables_reactive()
      if (is.null(tbls[[saved_table]])) {
        return(FALSE)
      }

      # Stash the saved column for the observeEvent(input$table, ...)
      # below to apply, rather than setting `column`'s choices/selected
      # directly here. CONFIRMED root cause via testing: this module's
      # OWN steady-state observeEvent(input$table, ...) (below) ALSO
      # fires whenever `table`'s value changes - including when THIS
      # restore() call changes it via updateSelectInput() a few lines
      # down. That observer rebuilds `column`'s choices via
      # updateSelectInput(session, "column", choices = col_choices) with
      # NO `selected` argument - which resets/clears any selection
      # restore() had just set, since the two observers were racing to
      # manage the same input with no coordination between them. Table
      # selection appeared to work inconsistently and column reliably
      # ended up blank - this is why. Fix: let the steady-state observer
      # do its normal job of rebuilding choices when `table` changes (as
      # it must for ANY table change, not just a restore), and have IT
      # apply the pending saved column selection as its last step,
      # instead of two independent observers fighting over the same input.
      pending_column_restore(saved_column)

      shiny::updateSelectInput(session, "table", choices = names(tbls), selected = saved_table)

      TRUE
    }

    list(data = result, restore = restore)
  })
}
# small helper - avoids importing rlang just for %||%
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && a == "")) b else a
