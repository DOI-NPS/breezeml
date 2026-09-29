# 06_taxonomy.R v12
#
# ROOT CAUSE FOUND (after v9-v11's failed timing-based attempts): row1's
# pickColsUI() was rendered via output$taxa_rows's renderUI(), a REACTIVE
# UI block that re-executes and re-creates its DOM contents from scratch
# every time row_ids() changes - including on the FIRST reveal of the
# conditionalPanel (has_taxa flipping true), since renderUI() blocks are
# lazy and don't actually render until their output becomes visible.
# Restoring via restore() checks the has_taxa checkbox AND tries to
# updateSelectInput() on row1's dropdown in close succession - but that
# updateSelectInput() call could land on a row1 <select> element that is
# either not yet created, or actively being destroyed/recreated by
# taxa_rows's renderUI() responding to the reveal. CONFIRMED via user
# observation: row1's dropdown visibly FLICKERS when its choices are
# restored, direct evidence of a destroy/recreate cycle happening around
# the same time restore() tries to update it. Geography (05_geography.R)
# never had this problem because none of its restorable inputs
# (lat_table/lon_table/etc.) live inside a renderUI() - they're static
# markup declared directly in geographyUI(), always present in the DOM
# from first paint, revealed via conditionalPanel's CSS show/hide only
# (no destroy/recreate), exactly like Tab 5's working behavior.
#
# FIX: row1 is no longer part of the dynamic row_ids()/renderUI() system
# at all. It is now static markup declared directly in taxonomyUI()
# (pickColsUI(ns("row1"), ...) called directly, same pattern as
# Geography's fields) - always present in the DOM, never destroyed/
# recreated, revealed only via conditionalPanel's CSS. The dynamic
# row_ids()/output$taxa_rows renderUI() system is now used ONLY for
# rows 2+ (added via "+Add another table/column"), which never exists
# before a user's own click creates them and therefore never has this
# restore-vs-render race in the first place (confirmed no issue in
# testing: manually-added rows always worked correctly, restored or not).
#
# row_ids() now starts EMPTY (character(0)), not c("row1") - row1 is
# handled entirely separately/statically. get_row_reactive("row1") is
# special-cased to return the STATIC row1 server instance rather than
# lazily creating one via row_ids()/renderUI(), so all the code that
# treats "all rows" uniformly (result(), attempt_restore()'s loop, etc.)
# can keep working with a single unified list of row ids
# (c("row1", row_ids())) without needing two different code paths.
#
# cat() diagnostics remain in place pending confirmation this fully
# resolves the issue; remove once confirmed.
#
# taxonomyServer()'s return value remains list(data = <reactive>,
# restore = <function>).
#
# Corresponds to skeleton.Rmd FUNCTION 5 - Taxonomic Coverage
# (EMLassemblyline::template_taxonomic_coverage)

TAXA_AUTHORITIES <- c("ITIS" = 3, "WORMS" = 9, "GBIF" = 11)

taxonomyUI <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    bslib::card(
      bslib::card_header("Taxonomic coverage (optional)"),
      shiny::helpText("If your data include scientific names, specify which ",
                      "table(s) and column(s) contain them. Skip this if your ",
                      "data package has no taxonomic component."),
      shiny::checkboxInput(ns("has_taxa"), "This data package includes taxonomic data", value = FALSE),
      shiny::conditionalPanel(
        condition = "input.has_taxa == true",
        ns = ns,
        # row1 is now static markup, declared directly here - NOT part of
        # the dynamic row_ids()/renderUI() system below. Always present
        # in the DOM from first paint (like Geography's lat_table/
        # lon_table), revealed only via this conditionalPanel's CSS
        # show/hide - never destroyed/recreated. This is the fix for the
        # restore-vs-renderUI race described in the file header note.
        pickColsUI(ns("row1"), table_label = "Table", col_label = "Scientific name column"),
        shiny::uiOutput(ns("extra_taxa_rows")),
        shiny::actionButton(ns("add_row"), "+ Add another table/column", class = "btn-sm btn-outline-secondary"),
        shiny::hr(),
        shiny::selectInput(ns("authorities"), "Taxonomic authorities to check (in order)",
                           choices = names(TAXA_AUTHORITIES),
                           selected = c("ITIS", "GBIF"),
                           multiple = TRUE),
        shiny::helpText("The app will try each authority in the order listed until ",
                        "a match is found. Checking many taxa against multiple ",
                        "authorities can take a while."),
        DT::DTOutput(ns("preview"))
      )
    ),
    col_widths = c(-2, 8, -2), fill = FALSE
  )
}

#' @param tables_reactive reactive() named list of data.frames (from Tab 3)
#' @return list(data = <reactive() list>, restore = <function>)
#' @noRd
taxonomyServer <- function(id, tables_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    character_cols <- function(df) names(df)[purrr::map_lgl(df, is.character)]

    # row1's server instance is created immediately/eagerly, matching its
    # now-static UI - NOT lazily via get_row_reactive(), since it's no
    # longer part of the dynamic system. This mirrors Geography's
    # site_pick <- pickColsServer("site_col", tables_reactive) pattern.
    row1_server <- pickColsServer("row1", tables_reactive, filter_fn = character_cols)

    # row_ids() now tracks ONLY the EXTRA (2+) rows, starting empty - row1
    # is handled separately/statically above. A row added via "+Add" is
    # still created lazily via output$extra_taxa_rows's renderUI(), which
    # is fine: those rows never exist before a user's own click, so there
    # is no restore-vs-render race for them (confirmed no issue in
    # testing for manually-added rows, restored or not).
    row_ids <- shiny::reactiveVal(character(0))
    row_servers <- list()

    shiny::observeEvent(input$add_row, {
      new_id <- paste0("row", length(row_ids()) + 2)  # +2: row1 is implicit, extra rows start at "row2"
      row_ids(c(row_ids(), new_id))
    })

    output$extra_taxa_rows <- shiny::renderUI({
      ids <- row_ids()
      shiny::req(length(ids) > 0)
      shiny::tagList(lapply(ids, function(rid) {
        pickColsUI(ns(rid), table_label = "Table", col_label = "Scientific name column")
      }))
    })

    #' Unified accessor for ANY row's server instance (row1 or an extra
    #' row) - lets the rest of this module (result(), attempt_restore())
    #' treat "all current rows" as one list without needing two separate
    #' code paths for the static vs. dynamic cases.
    get_row_reactive <- function(rid) {
      if (rid == "row1") return(row1_server)
      if (is.null(row_servers[[rid]])) {
        row_servers[[rid]] <<- pickColsServer(rid, tables_reactive, filter_fn = character_cols)
      }
      row_servers[[rid]]
    }

    all_row_ids <- shiny::reactive({
      c("row1", row_ids())
    })

    result <- shiny::reactive({
      if (!isTRUE(input$has_taxa)) {
        return(list(enabled = FALSE))
      }
      ids <- all_row_ids()
      pairs <- purrr::compact(lapply(ids, function(rid) {
        rs <- get_row_reactive(rid)
        tryCatch(rs$data(), error = function(e) NULL)
      }))

      shiny::req(length(pairs) > 0)
      shiny::req(length(input$authorities) > 0)

      list(
        enabled = TRUE,
        pairs = purrr::map(pairs, ~list(table = .x$table, column = .x$column)),
        authorities = unname(TAXA_AUTHORITIES[input$authorities])
      )
    })

    output$preview <- DT::renderDT({
      r <- result()
      shiny::req(isTRUE(r$enabled))
      tbls <- tables_reactive()
      preview_rows <- purrr::map_dfr(r$pairs, function(p) {
        df <- tbls[[p$table]]
        shiny::req(df, p$column %in% names(df))
        tibble::tibble(
          table = p$table,
          column = p$column,
          sample_values = paste(utils::head(unique(df[[p$column]]), 3), collapse = ", ")
        )
      })
      DT::datatable(preview_rows, options = list(dom = 't'), rownames = FALSE)
    })

    pending_restore <- shiny::reactiveVal(NULL)

    restore <- function(saved) {
      if (is.null(saved)) return(invisible(NULL))
      pending_restore(saved)
      attempt_restore()
      invisible(NULL)
    }

    attempt_restore <- function() {
      saved <- pending_restore()
      if (is.null(saved)) return(invisible(NULL))

      shiny::updateCheckboxInput(session, "has_taxa", value = isTRUE(saved$has_taxa))
      if (!isTRUE(saved$has_taxa)) {
        pending_restore(NULL)
        return(invisible(NULL))
      }

      if (!is.null(saved$authorities) && length(saved$authorities) > 0) {
        valid_auth <- intersect(saved$authorities, names(TAXA_AUTHORITIES))
        if (length(valid_auth) > 0) {
          shiny::updateSelectInput(session, "authorities", selected = valid_auth)
        }
      }

      saved_pairs <- saved$pairs
      n_saved <- length(saved_pairs$table %||% character(0))
      if (n_saved == 0) {
        pending_restore(NULL)
        return(invisible(NULL))
      }

      # Grow row_ids() (the EXTRA-rows list) only if MORE than 1 saved
      # pair needs a home - row1 already exists statically and never
      # needs to be "grown into existence". n_saved - 1 extra rows are
      # needed beyond row1.
      current_extra <- row_ids()
      n_extra_needed <- n_saved - 1
      if (n_extra_needed > length(current_extra)) {
        new_ids <- paste0("row", (length(current_extra) + 2):(n_extra_needed + 1))
        row_ids(c(current_extra, new_ids))
      }

      ids <- all_row_ids()
      all_matched <- TRUE
      for (i in seq_len(n_saved)) {
        rid <- ids[i]
        rs <- get_row_reactive(rid)
        saved_table <- saved_pairs$table[[i]]
        saved_column <- saved_pairs$column[[i]]
        applied <- rs$restore(saved_table, saved_column)
        if (!isTRUE(applied)) all_matched <- FALSE
      }

      if (all_matched) {
        pending_restore(NULL)
      }

      invisible(NULL)
    }

    shiny::observeEvent(tables_reactive(), {
      shiny::isolate(attempt_restore())
    }, ignoreInit = TRUE)

    list(data = result, restore = restore)
  })
}

#' Emit the R code chunk for taxonomic coverage. Pure function.
#' @noRd
emit_taxonomy_chunk <- function(state, working_folder_var = "working_folder") {
  if (is.null(state) || !isTRUE(state$enabled)) {
    return("# No taxonomic coverage specified.\n")
  }

  tables <- purrr::map_chr(state$pairs, "table")
  cols <- purrr::map_chr(state$pairs, "column")
  auth <- paste(state$authorities, collapse = ", ")

  tables_r <- paste0("c(", paste(sprintf('"%s"', tables), collapse = ", "), ")")
  cols_r <- paste0("c(", paste(sprintf('"%s"', cols), collapse = ", "), ")")

  glue::glue(
    'data_taxa_tables <- {tables_r}\n',
    'data_taxa_fields <- {cols_r}\n\n',
    'EMLassemblyline::template_taxonomic_coverage(\n',
    '  path = {working_folder_var},\n',
    '  data.path = {working_folder_var},\n',
    '  taxa.table = data_taxa_tables,\n',
    '  taxa.col = data_taxa_fields,\n',
    '  taxa.authority = c({auth}),\n',
    "  taxa.name.type = 'scientific',\n",
    '  write.file = TRUE\n',
    ')\n'
  )
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && a == "")) b else a
