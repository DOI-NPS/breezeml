# 05_geography.R v7
#
# Added @noRd to geographyServer() - internal Shiny module server, not
# meant to have a public help page. Resolves roxygen2's "Skipping; no
# name and/or title" note.
#
# Corresponds to skeleton.Rmd FUNCTION 4 - Geographic Coverage
# (EMLassemblyline::template_geographic_coverage)
#
# Two independent, optional pieces:
#   1. Park unit content connections (handled elsewhere via
#      EMLeditor::set_content_units - park bounding boxes, not per-point data)
#   2. Point-level geographic coverage from lat/lon columns in a data table -
#      that's what this module builds.
#
# Users may have no geographic data at all (e.g. a species list), so
# everything here is skippable.

geographyUI <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    bslib::card(
      bslib::card_header("Site coordinates (optional)"),
      shiny::helpText("If your data include specific site coordinates (points, ",
                      "plots, or bounding boxes), specify the table and columns ",
                      "here. If your only geographic information is park unit ",
                      "boundaries, you can skip this - park bounding boxes are ",
                      "added separately."),
      shiny::checkboxInput(ns("has_coords"), "This data package includes site coordinates", value = FALSE),
      shiny::conditionalPanel(
        condition = "input.has_coords == true",
        ns = ns,
        pickColsUI(ns("site_col"), table_label = "Table", col_label = "Site name column"),
        bslib::layout_columns(
          shiny::selectInput(ns("lat_table"), "Latitude: table", choices = NULL),
          shiny::selectInput(ns("lat_col"), "Latitude column", choices = NULL),
          col_widths = c(6, 6)
        ),
        bslib::layout_columns(
          shiny::selectInput(ns("lon_table"), "Longitude: table", choices = NULL),
          shiny::selectInput(ns("lon_col"), "Longitude column", choices = NULL),
          col_widths = c(6, 6)
        ),
        shiny::helpText("Latitude/longitude must be in decimal degrees (WGS84). ",
                        "If your coordinates are in UTM, convert them first using ",
                        shiny::HTML("<code>QCkit::generate_ll_from_utm()</code>"),
                        " before uploading here."),
        DT::DTOutput(ns("preview"))
      )
    ),
    col_widths = c(-2, 8, -2), fill = FALSE
  )
}

#' @param tables_reactive reactive() named list of data.frames (from Tab 3)
#' @return reactive() list:
#'   $enabled          logical
#'   $table            character - table name used for coordinates
#'   $lat_col          character
#'   $lon_col          character
#'   $site_col         character
#' @noRd
geographyServer <- function(id, tables_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    numeric_cols <- function(df) names(df)[purrr::map_lgl(df, is.numeric)]

    site_pick <- pickColsServer("site_col", tables_reactive)

    shiny::observeEvent(tables_reactive(), {
      tbls <- tables_reactive()
      shiny::req(length(tbls) > 0)
      choices <- names(tbls)
      shiny::updateSelectInput(session, "lat_table", choices = choices)
      shiny::updateSelectInput(session, "lon_table", choices = choices)
    }, ignoreNULL = FALSE)

    shiny::observeEvent(input$lat_table, {
      tbls <- tables_reactive()
      shiny::req(input$lat_table, tbls[[input$lat_table]])
      shiny::updateSelectInput(session, "lat_col",
                               choices = numeric_cols(tbls[[input$lat_table]]))
    })

    shiny::observeEvent(input$lon_table, {
      tbls <- tables_reactive()
      shiny::req(input$lon_table, tbls[[input$lon_table]])
      shiny::updateSelectInput(session, "lon_col",
                               choices = numeric_cols(tbls[[input$lon_table]]))
    })

    result <- shiny::reactive({
      if (!isTRUE(input$has_coords)) {
        return(list(enabled = FALSE))
      }
      shiny::req(input$lat_table, input$lat_col, input$lon_col)

      sp <- tryCatch(site_pick(), error = function(e) NULL)

      if (!is.null(sp) && (sp$table != input$lat_table || sp$table != input$lon_table)) {
        shiny::showNotification(
          "Site name, latitude, and longitude should come from the same table.",
          type = "warning"
        )
      }

      list(
        enabled = TRUE,
        table = input$lat_table,
        lat_col = input$lat_col,
        lon_col = input$lon_col,
        site_col = if (!is.null(sp)) sp$column else NA_character_
      )
    })

    output$preview <- DT::renderDT({
      r <- result()
      shiny::req(isTRUE(r$enabled))
      tbls <- tables_reactive()
      df <- tbls[[r$table]]
      shiny::req(df)
      cols <- c(r$site_col, r$lat_col, r$lon_col)
      cols <- cols[!is.na(cols) & cols %in% names(df)]
      DT::datatable(df[, cols, drop = FALSE],
                    options = list(pageLength = 5, dom = 'tp'),
                    rownames = FALSE)
    })

    result
  })
}

#' Emit the R code chunk for geographic coverage.
#' Pure function - no Shiny dependencies - so it can be unit tested and
#' reused for both end-of-process script generation and (later) live preview.
#'
#' @param state the list returned by geographyServer()'s reactive, evaluated
#'   (i.e. state <- geo_reactive())
#' @param working_folder_var name of the R variable holding the working
#'   folder path in the generated script (default "working_folder")
emit_geo_chunk <- function(state, working_folder_var = "working_folder") {
  if (is.null(state) || !isTRUE(state$enabled)) {
    return(paste0(
      "# No site-level geographic coverage specified.\n",
      "# (Park unit boundaries, if any, are added later via EMLeditor::set_content_units())\n"
    ))
  }

  glue::glue(
    'data_coordinates_table <- "{state$table}"\n',
    'data_latitude <- "{state$lat_col}"\n',
    'data_longitude <- "{state$lon_col}"\n',
    'data_sitename <- "{state$site_col}"\n\n',
    'EMLassemblyline::template_geographic_coverage(\n',
    '  path = {working_folder_var},\n',
    '  data.path = {working_folder_var},\n',
    '  data.table = data_coordinates_table,\n',
    '  lat.col = data_latitude,\n',
    '  lon.col = data_longitude,\n',
    '  site.col = data_sitename,\n',
    '  write.file = TRUE\n',
    ')\n'
  )
}
