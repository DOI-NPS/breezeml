# card_pick_cols_from_table.R v8
#
# Added @noRd to pickColsServer() - internal Shiny module server, not
# meant to have a public help page. Resolves roxygen2's "Skipping; no
# name and/or title" note.
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
#' @return a reactive() list(table = <name>, column = <name(s)>, data = <df>)
#' @noRd
pickColsServer <- function(id, tables_reactive, filter_fn = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

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

      shiny::updateSelectInput(session, "column", choices = col_choices)
    })

    shiny::reactive({
      tbls <- tables_reactive()
      shiny::req(input$table, input$column, tbls[[input$table]])
      list(
        table = input$table,
        column = input$column,
        data = tbls[[input$table]]
      )
    })
  })
}

# small helper - avoids importing rlang just for %||%
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && a == "")) b else a
