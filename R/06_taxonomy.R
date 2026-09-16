# 06_taxonomy.R v7
#
# Added @noRd to taxonomyServer() - internal Shiny module server, not
# meant to have a public help page. Resolves roxygen2's "Skipping; no
# name and/or title" note.
#
# Corresponds to skeleton.Rmd FUNCTION 5 - Taxonomic Coverage
# (EMLassemblyline::template_taxonomic_coverage)
#
# EMLassemblyline supports multiple (table, column) pairs for scientific
# names, and multiple authorities to try in order (ITIS -> WORMS -> GBIF).
# We let the user add one or more table/column pairs via a repeatable row
# UI, plus authority selection.

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
        shiny::uiOutput(ns("taxa_rows")),
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
#' @return reactive() list:
#'   $enabled      logical
#'   $pairs        list of list(table=, column=) - one or more
#'   $authorities  integer vector of authority codes, in order
#' @noRd
taxonomyServer <- function(id, tables_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    character_cols <- function(df) names(df)[purrr::map_lgl(df, is.character)]

    # dynamic list of row ids for repeatable table/column pickers
    row_ids <- shiny::reactiveVal(c("row1"))
    row_servers <- list()

    shiny::observeEvent(input$add_row, {
      new_id <- paste0("row", length(row_ids()) + 1)
      row_ids(c(row_ids(), new_id))
    })

    output$taxa_rows <- shiny::renderUI({
      ids <- row_ids()
      shiny::tagList(lapply(ids, function(rid) {
        pickColsUI(ns(rid), table_label = "Table", col_label = "Scientific name column")
      }))
    })

    # lazily create a server for each row id as it appears; store reactive
    # accessors keyed by id so we can poll them all in the combined reactive
    get_row_reactive <- function(rid) {
      if (is.null(row_servers[[rid]])) {
        row_servers[[rid]] <<- pickColsServer(rid, tables_reactive, filter_fn = character_cols)
      }
      row_servers[[rid]]
    }

    result <- shiny::reactive({
      if (!isTRUE(input$has_taxa)) {
        return(list(enabled = FALSE))
      }
      ids <- row_ids()
      pairs <- purrr::compact(lapply(ids, function(rid) {
        fn <- get_row_reactive(rid)
        tryCatch(fn(), error = function(e) NULL)
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

    result
  })
}

#' Emit the R code chunk for taxonomic coverage. Pure function.
#'
#' @param state the list returned by taxonomyServer()'s reactive, evaluated
#'   (i.e. state <- taxonomy_reactive())
#' @param working_folder_var name of the R variable holding the working
#'   folder path in the generated script (default "working_folder")
#' @return character - the R code chunk for taxonomic coverage templating,
#'   or an explanatory comment if taxonomy coverage isn't enabled
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
