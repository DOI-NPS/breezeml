# 05_geography.R v9
#
# v9: added restore() to geographyServer()'s return value, for the
# Save/Load session feature. UNLIKE Tabs 1/2/7/8, this tab's restore
# CANNOT complete immediately on load - lat_table/lat_col/lon_table/
# lon_col/site_col only have valid choices once tables_reactive()
# actually contains a table with that name (per this session's design
# decision, Tab 3's CSV contents are never saved - the user must
# re-upload, and restore only proceeds once names match).
#
# restore() is therefore SAFE TO CALL REPEATEDLY/SPECULATIVELY, same
# contract as pickColsServer()'s restore() (card_pick_cols_from_table.R
# v9) which it depends on for the site_col piece. app_server.R's
# orchestration is responsible for re-calling restore() every time
# tables_reactive() changes (i.e. every time the user uploads another
# file), until the saved table name(s) are found or the user gives up.
# Each call re-attempts whatever hasn't yet been successfully restored;
# already-restored pieces are harmlessly re-applied (idempotent) rather
# than tracked/skipped, since update*Input() calls are cheap and the
# saved values don't change between calls.
#
# has_coords (the checkbox gating this whole tab) IS restorable
# immediately, since it's a plain checkbox with no data dependency - but
# it's intentionally restored as part of THIS restore() call (not
# earlier/separately), so the checkbox and its dependent fields become
# available together rather than showing an empty conditionalPanel
# before any table data exists to populate it.
#
# geographyServer()'s return value changes from a bare reactive() to
# list(data = <reactive>, restore = <function>).
#
# site_pick <- pickColsServer(...) callers updated: site_pick() -> site_pick$data()
# (pickColsServer's return shape changed in card_pick_cols_from_table.R v9)
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
#' @return list(data = <reactive() list>, restore = <function>)
#'   data() returns:
#'     $enabled          logical
#'     $table            character - table name used for coordinates
#'     $lat_col          character
#'     $lon_col          character
#'     $site_col         character
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

      sp <- tryCatch(site_pick$data(), error = function(e) NULL)

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

    # Stash the last-seen saved state so restore() can be re-attempted
    # (by app_server.R's orchestration) every time tables_reactive()
    # changes, without the caller needing to re-pass the original saved
    # list each time.
    pending_restore <- shiny::reactiveVal(NULL)

    #' Push saved state into this module. UNLIKE Tabs 1/2/7/8, this
    #' cannot fully complete in one call - lat_table/lon_table/lat_col/
    #' lon_col only have valid choices once tables_reactive() contains a
    #' table with the saved name (see file header note). Safe to call
    #' repeatedly/speculatively; each call re-attempts whatever hasn't
    #' yet successfully stuck.
    #'
    #' @param saved list matching default_app_state()$geography's shape,
    #'   or NULL
    restore <- function(saved) {
      if (is.null(saved)) return(invisible(NULL))
      pending_restore(saved)
      attempt_restore()
      invisible(NULL)
    }

    attempt_restore <- function() {
      saved <- pending_restore()
      if (is.null(saved)) return(invisible(NULL))

      shiny::updateCheckboxInput(session, "has_coords", value = isTRUE(saved$has_coords))
      if (!isTRUE(saved$has_coords)) return(invisible(NULL))

      tbls <- tables_reactive()

      if (!is.null(saved$lat_table) && !is.na(saved$lat_table) && !is.null(tbls[[saved$lat_table]])) {
        shiny::updateSelectInput(session, "lat_table", selected = saved$lat_table)
        lat_choices <- numeric_cols(tbls[[saved$lat_table]])
        if (!is.null(saved$lat_col) && saved$lat_col %in% lat_choices) {
          shiny::updateSelectInput(session, "lat_col", choices = lat_choices, selected = saved$lat_col)
        }
      }

      if (!is.null(saved$lon_table) && !is.na(saved$lon_table) && !is.null(tbls[[saved$lon_table]])) {
        shiny::updateSelectInput(session, "lon_table", selected = saved$lon_table)
        lon_choices <- numeric_cols(tbls[[saved$lon_table]])
        if (!is.null(saved$lon_col) && saved$lon_col %in% lon_choices) {
          shiny::updateSelectInput(session, "lon_col", choices = lon_choices, selected = saved$lon_col)
        }
      }

      site_pick$restore(saved$site_table, saved$site_col)

      invisible(NULL)
    }

    # Re-attempt whatever hasn't yet stuck every time the table list
    # changes (i.e. every time the user re-uploads a file). Deferred via
    # session$onFlushed() for the same reason as 06_taxonomy.R's row1 fix
    # (v9->later) - lat_table/lon_table/site_pick's inputs are rendered
    # immediately at app startup (not lazily like Tab 6's later rows), so
    # the FIRST time this fires (e.g. right after a JSON Load, before any
    # CSV upload) their selectize.js widgets may not have finished
    # client-side initialization yet. Calling updateSelectInput()
    # pre-init can update the underlying <select>'s options without the
    # selectize wrapper ever re-reading them, leaving the visible
    # dropdown permanently empty - confirmed as the actual root cause of
    # an equivalent symptom on Tab 6's row1 in testing. app_server.R does
    # NOT need to re-call restore() itself; registering this once here is
    # sufficient, since pending_restore() persists the saved target
    # across re-uploads.
    #
    # shiny::isolate() is REQUIRED inside onFlushed()'s callback -
    # confirmed via testing that omitting it crashes the app ("Operation
    # not allowed without an active reactive context") the moment this
    # fires, since onFlushed()'s callback runs outside any reactive
    # context and attempt_restore() reads reactiveVals.
    #
    # once = TRUE is REQUIRED - once = FALSE means "call this callback on
    # EVERY future flush, forever," not "keep retrying until it
    # succeeds." CONFIRMED in testing: once = FALSE created an infinite
    # loop (attempt_restore() calls updateSelectInput(), which triggers a
    # flush, which re-fires the still-registered callback, forever - the
    # app had to be force-killed). A fresh once = TRUE callback is
    # registered every time this observeEvent fires (i.e. every time
    # tables_reactive() actually changes/a file is uploaded), which is
    # exactly the desired "one deferred attempt per upload event."
    shiny::observeEvent(tables_reactive(), {
      session$onFlushed(function() {
        shiny::isolate(attempt_restore())
      }, once = TRUE)
    }, ignoreInit = TRUE)

    list(data = result, restore = restore)
  })
}

#' Emit the R code chunk for geographic coverage.
#' Pure function - no Shiny dependencies - so it can be unit tested and
#' reused for both end-of-process script generation and (later) live preview.
#'
#' @param state the list returned by geographyServer()'s $data reactive,
#'   evaluated (i.e. state <- geography$data())
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
