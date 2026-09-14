# card_pick_cols_from_table.R
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
  ns <- NS(id)
  layout_columns(
    selectInput(ns("table"), table_label, choices = NULL, width = "100%"),
    selectInput(ns("column"), col_label, choices = NULL, width = "100%",
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
pickColsServer <- function(id, tables_reactive, filter_fn = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observeEvent(tables_reactive(), {
      tbls <- tables_reactive()
      if (length(tbls) == 0) {
        updateSelectInput(session, "table", choices = character(0))
        updateSelectInput(session, "column", choices = character(0))
        return(invisible(NULL))
      }
      updateSelectInput(session, "table", choices = names(tbls),
                        selected = input$table %||% names(tbls)[1])
    }, ignoreNULL = FALSE)
    
    observeEvent(input$table, {
      tbls <- tables_reactive()
      req(input$table, tbls[[input$table]])
      df <- tbls[[input$table]]
      
      col_choices <- if (!is.null(filter_fn)) filter_fn(df) else names(df)
      
      if (length(col_choices) == 0) {
        showNotification(
          paste0("No eligible columns found in '", input$table, "' for this field."),
          type = "warning"
        )
      }
      
      updateSelectInput(session, "column", choices = col_choices)
    })
    
    reactive({
      tbls <- tables_reactive()
      req(input$table, input$column, tbls[[input$table]])
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