# 05_geography.R v8
#
# NEW: adds an interactive leaflet map alongside the existing coordinate
# preview table, so users can visually sanity-check their site coordinates
# (e.g. catching an accidental sign flip, a UTM value that slipped past
# the decimal-degrees warning, or a wildly out-of-range point) before
# generating. leaflet is a NEW dependency - add to DESCRIPTION's Imports.
#
# Rows with missing or invalid (non-numeric, or outside valid lat/lon
# range) coordinates are excluded from the map but NOT from the existing
# preview table (which is left as-is - it's a raw data preview, not a
# validation view). A warning notification reports how many rows were
# excluded and why, so the user isn't left wondering why fewer points
# appear on the map than rows exist in their table.
#
# Map points are labeled with the site name column on click, when a site
# column was selected - points render as plain unlabeled markers otherwise.
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
        leaflet::leafletOutput(ns("map"), height = 350),
        shiny::br(),
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

    # Build the subset of rows usable for mapping: numeric, non-NA, and
    # within valid lat/lon ranges. Returns NULL if the required columns
    # aren't resolvable at all (distinct from "resolvable but zero valid
    # rows", which returns a 0-row tibble instead) so the map/notification
    # logic can tell "nothing to check yet" apart from "checked, all bad".
    mappable_points <- shiny::reactive({
      r <- result()
      shiny::req(isTRUE(r$enabled))
      tbls <- tables_reactive()
      df <- tbls[[r$table]]
      if (is.null(df) || !all(c(r$lat_col, r$lon_col) %in% names(df))) return(NULL)

      lat <- suppressWarnings(as.numeric(df[[r$lat_col]]))
      lon <- suppressWarnings(as.numeric(df[[r$lon_col]]))
      site <- if (!is.na(r$site_col) && r$site_col %in% names(df)) {
        as.character(df[[r$site_col]])
      } else {
        NA_character_
      }

      valid <- !is.na(lat) & !is.na(lon) &
        lat >= -90 & lat <= 90 & lon >= -180 & lon <= 180

      list(
        valid_pts = tibble::tibble(lat = lat[valid], lon = lon[valid],
                                   site = if (length(site) == length(valid)) site[valid] else NA_character_),
        n_total = length(valid),
        n_invalid = sum(!valid)
      )
    })

    shiny::observeEvent(mappable_points(), {
      mp <- mappable_points()
      shiny::req(!is.null(mp))
      if (mp$n_invalid > 0) {
        shiny::showNotification(
          paste0(mp$n_invalid, " of ", mp$n_total, " row(s) have missing or invalid ",
                 "coordinates and are not shown on the map (they are still ",
                 "included in the preview table below)."),
          type = "warning", duration = 8
        )
      }
    })

    output$map <- leaflet::renderLeaflet({
      mp <- mappable_points()
      shiny::req(!is.null(mp))

      map <- leaflet::leaflet() |>
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap)

      if (nrow(mp$valid_pts) == 0) {
        # no valid points at all - still show a usable base map rather
        # than an empty/blank widget, just with a default world view
        return(map |> leaflet::setView(lng = 0, lat = 0, zoom = 1))
      }

      has_site_labels <- !all(is.na(mp$valid_pts$site))

      map |>
        leaflet::addCircleMarkers(
          data = mp$valid_pts,
          lng = ~lon, lat = ~lat,
          popup = if (has_site_labels) ~site else NULL,
          radius = 6, stroke = TRUE, weight = 1, fillOpacity = 0.7
        ) |>
        leaflet::fitBounds(
          lng1 = min(mp$valid_pts$lon), lat1 = min(mp$valid_pts$lat),
          lng2 = max(mp$valid_pts$lon), lat2 = max(mp$valid_pts$lat)
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
