# 05_geography.R v13
#
# v13: FIXED the long-standing lon_col restore bug, and removed all
# diagnostic instrumentation (v12's timestamped cat() tracing) now that
# the root cause is settled.
#
# ROOT CAUSE (confirmed via timestamped execution tracing, not guessed -
# several earlier timing-based theories were wrong): the bug was NOT a
# lat/lon asymmetry. BOTH lat_col and lon_col were being defaulted to the
# first available column choice on restore; lat_col merely LOOKED correct
# because its saved value (decimalLatitude) happened to BE the first
# choice, while lon_col's saved value (decimalLongitude) was the second
# choice and so visibly surfaced the bug. The code paths were symmetric
# and BOTH broken - which is why every symmetric-code read-through failed
# to find it.
#
# The actual mechanism: updateSelectInput(..., selected=) is ASYNCHRONOUS.
# It queues a client round-trip; the paired observeEvent(input$lat_table/
# lon_table, ...) fires on a LATER reactive flush, not inline. On a
# restore, the sequence was:
#   1. the tables-reset observer (fires on CSV re-upload) sets lat_table/
#      lon_table CHOICES, defaulting both to the first table alphabetically
#      (BICA_Herps.csv), NOT the saved table.
#   2. attempt_restore() (deferred via onFlushed) sets the pending column
#      vals and calls updateSelectInput(lat_table/lon_table,
#      selected=<saved table>), then RETURNS - the table observers have
#      not fired yet.
#   3. the table observers fire a FIRST time, for the STALE wrong table
#      (BICA_Herps.csv) with empty/irrelevant choices. The OLD code
#      DISCARDED the pending column value here ("not in choices"),
#      throwing it away before it could ever be used.
#   4. the table observers fire a SECOND time, for the CORRECT saved table
#      (with the right choices) - but pending was already discarded in
#      step 3, so the column defaulted to the first choice.
#
# THE FIX: the column observers now only CLEAR a pending restore value
# when they actually APPLY it (i.e. the pending value is a valid column
# for the table currently selected). A firing for an intermediate/stale
# table that doesn't contain the pending column just refreshes choices and
# LEAVES pending armed, so the later correct-table firing applies it. This
# does not rely on winning any race - whichever firing happens to be for
# the correct table applies the value, no matter how many wrong-table
# firings precede it.
#
# This is a refinement of the v10 approach (route lat_col/lon_col restore
# through pending_*_col_restore reactiveVals applied by the steady-state
# table observers, never written directly by attempt_restore()); v10's
# structure was correct and necessary but its discard-on-wrong-table
# branch was the defect. attempt_restore()'s end-of-function safety net
# (added v11) is retained as defense for the repeated-call case, but is no
# longer the primary path - the kept-armed pending value is.
#
# ---- Behavior / contract ----
#
# UNLIKE Tabs 1/2/7/8, this tab's restore() CANNOT complete immediately
# on load - lat_table/lat_col/lon_table/lon_col/site_col only have valid
# choices once tables_reactive() actually contains a table with that
# name (per this session's design decision, Tab 3's CSV contents are
# never saved - the user must re-upload, and restore only proceeds once
# names match).
#
# restore() is therefore SAFE TO CALL REPEATEDLY/SPECULATIVELY, same
# contract as pickColsServer()'s restore() (card_pick_cols_from_table.R
# v9) which it depends on for the site_col piece. The onFlushed
# registration at the bottom of this module re-attempts restore every time
# tables_reactive() changes (i.e. every CSV upload), until the saved table
# name(s) are found. Each call re-attempts whatever hasn't yet restored;
# already-restored pieces are harmlessly re-applied (idempotent).
#
# has_coords (the checkbox gating this whole tab) is restored as part of
# THIS restore() call (not earlier/separately), so the checkbox and its
# dependent fields become available together.
#
# geographyServer()'s return value is list(data = <reactive>,
# restore = <function>).
#
# site_pick <- pickColsServer(...) callers use site_pick$data() (not
# site_pick()); pickColsServer's return shape is list(data=, restore=)
# per card_pick_cols_from_table.R v9.
#
# Includes an interactive leaflet map alongside the coordinate preview
# table. leaflet is a dependency - must be in DESCRIPTION's Imports.
# Rows with missing/invalid coordinates are excluded from the map (with a
# warning notification) but kept in the preview table. Map points are
# labeled with the site name column on click, when a site column was
# selected.
#
# NOTE (v10 syntax fix, retained): popup = (if (has_site_labels) ~site
# else NULL) MUST be parenthesized - a one-sided formula (~site) cannot be
# directly followed by `else` or R's parser errors.
#
# Corresponds to skeleton.Rmd FUNCTION 4 - Geographic Coverage
# (EMLassemblyline::template_geographic_coverage). Point-level geographic
# coverage from lat/lon columns; park unit boundaries are handled
# separately via EMLeditor::set_content_units(). Everything here is
# skippable (e.g. a species list has no coordinates).

geographyUI <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    bslib::card(
      bslib::card_header("Site coordinates (optional)"),
      shiny::helpText("If your data include specific site coordinates (points,",
                      " plots, or bounding boxes), specify the table and ",
                      "columns here. If your only geographic information is ",
                      "park unit boundaries, you can skip this - park ",
                      "bounding boxes are added separately. For public data, ",
                      "GPS coordinates will be desplayed on the DataStore ",
                      "reference page. GPS coordinates will not be displayed ",
                      "for internal or restricted data."),
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
#'     $enabled   logical
#'     $table     character - table name used for coordinates
#'     $lat_col   character
#'     $lon_col   character
#'     $site_col  character
#' @noRd
geographyServer <- function(id, tables_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    numeric_cols <- function(df) names(df)[purrr::map_lgl(df, is.numeric)]

    site_pick <- pickColsServer("site_col", tables_reactive)

    # Holds a restore()-requested column selection until the steady-state
    # table observer below can apply it. Crucially (see v13 header note),
    # these are ONLY cleared when the value is actually applied - never
    # discarded just because an intermediate/stale table-change firing
    # doesn't contain the saved column. That discard-on-wrong-table
    # behavior was the root-caused bug.
    pending_lat_col_restore <- shiny::reactiveVal(NULL)
    pending_lon_col_restore <- shiny::reactiveVal(NULL)

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
      choices <- numeric_cols(tbls[[input$lat_table]])

      pending <- pending_lat_col_restore()
      if (!is.null(pending) && pending %in% choices) {
        # Pending restore value IS valid for the now-selected table -
        # apply and consume it.
        shiny::updateSelectInput(session, "lat_col", choices = choices, selected = pending)
        pending_lat_col_restore(NULL)
      } else {
        # Refresh choices, but do NOT discard a pending restore value just
        # because it isn't valid for THIS (possibly intermediate/stale)
        # table - leave it armed so a later correct-table firing applies
        # it. See v13 header note. Pending is only ever cleared when
        # applied (above) or by attempt_restore()'s safety net.
        shiny::updateSelectInput(session, "lat_col", choices = choices)
      }
    })

    shiny::observeEvent(input$lon_table, {
      tbls <- tables_reactive()
      shiny::req(input$lon_table, tbls[[input$lon_table]])
      choices <- numeric_cols(tbls[[input$lon_table]])

      pending <- pending_lon_col_restore()
      if (!is.null(pending) && pending %in% choices) {
        shiny::updateSelectInput(session, "lon_col", choices = choices, selected = pending)
        pending_lon_col_restore(NULL)
      } else {
        # See matching comment in the lat_table observer above.
        shiny::updateSelectInput(session, "lon_col", choices = choices)
      }
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
    # rows", which returns a 0-row tibble) so the map/notification logic
    # can tell "nothing to check yet" apart from "checked, all bad".
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
          popup = (if (has_site_labels) ~site else NULL),
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

    # Stash the last-seen saved state so restore() can be re-attempted (by
    # the onFlushed registration below) every time tables_reactive()
    # changes, without the caller re-passing the original saved list.
    pending_restore <- shiny::reactiveVal(NULL)

    #' Push saved state into this module. Cannot fully complete in one call
    #' - lat_table/lon_table/lat_col/lon_col only have valid choices once
    #' tables_reactive() contains a table with the saved name (see header).
    #' Safe to call repeatedly/speculatively.
    #'
    #' @param saved list matching default_app_state()$geography's shape, or NULL
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

      # Arm the pending column value, THEN change the table. The table
      # observer (above) applies the pending value when it fires for the
      # correct table - note that updateSelectInput(selected=) is async,
      # so that firing happens on a LATER flush, possibly after one or
      # more intermediate wrong-table firings; those no longer discard the
      # pending value (v13 fix), so the correct-table firing still applies
      # it. See v13 header note for the full mechanism.
      if (!is.null(saved$lat_table) && !is.na(saved$lat_table) && !is.null(tbls[[saved$lat_table]])) {
        if (!is.null(saved$lat_col) && !is.na(saved$lat_col)) {
          pending_lat_col_restore(saved$lat_col)
        }
        shiny::updateSelectInput(session, "lat_table", selected = saved$lat_table)
      }

      if (!is.null(saved$lon_table) && !is.na(saved$lon_table) && !is.null(tbls[[saved$lon_table]])) {
        if (!is.null(saved$lon_col) && !is.na(saved$lon_col)) {
          pending_lon_col_restore(saved$lon_col)
        }
        shiny::updateSelectInput(session, "lon_table", selected = saved$lon_table)
      }

      # Safety net for the repeated-call case: if input$lat_table/
      # input$lon_table is ALREADY on the correct saved value (e.g. a
      # later speculative attempt_restore() call) but input$lat_col/
      # input$lon_col hasn't been set to the saved value, correct it
      # directly. Not the primary restore path (the kept-armed pending
      # value above is) - retained as defense.
      if (shiny::isTruthy(input$lat_table) && identical(input$lat_table, saved$lat_table) &&
          !is.null(saved$lat_col) && !is.na(saved$lat_col) &&
          !identical(input$lat_col, saved$lat_col)) {
        tbl <- tbls[[saved$lat_table]]
        if (!is.null(tbl)) {
          lat_choices <- numeric_cols(tbl)
          if (saved$lat_col %in% lat_choices) {
            shiny::updateSelectInput(session, "lat_col", choices = lat_choices, selected = saved$lat_col)
            pending_lat_col_restore(NULL)
          }
        }
      }

      if (shiny::isTruthy(input$lon_table) && identical(input$lon_table, saved$lon_table) &&
          !is.null(saved$lon_col) && !is.na(saved$lon_col) &&
          !identical(input$lon_col, saved$lon_col)) {
        tbl <- tbls[[saved$lon_table]]
        if (!is.null(tbl)) {
          lon_choices <- numeric_cols(tbl)
          if (saved$lon_col %in% lon_choices) {
            shiny::updateSelectInput(session, "lon_col", choices = lon_choices, selected = saved$lon_col)
            pending_lon_col_restore(NULL)
          }
        }
      }

      site_pick$restore(saved$site_table, saved$site_col)

      invisible(NULL)
    }

    # Re-attempt restore every time the table list changes (i.e. every CSV
    # re-upload). Deferred via session$onFlushed() because lat_table/
    # lon_table/site_pick's selectize.js widgets may not have finished
    # client-side init the first time this fires (e.g. right after a JSON
    # Load, before any upload); calling updateSelectInput() pre-init can
    # update the underlying <select> without the selectize wrapper
    # re-reading it, leaving the dropdown permanently empty (confirmed as
    # the root cause of an equivalent symptom on Tab 6's row1).
    #
    # shiny::isolate() is REQUIRED - onFlushed()'s callback runs outside
    # any reactive context, and attempt_restore() reads reactiveVals;
    # omitting it crashes with "Operation not allowed without an active
    # reactive context".
    #
    # once = TRUE is REQUIRED - once = FALSE means "call on EVERY future
    # flush forever," which created an infinite loop in testing
    # (attempt_restore -> updateSelectInput -> flush -> re-fire). A fresh
    # once = TRUE callback is registered per tables_reactive() change,
    # giving exactly one deferred attempt per upload event.
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
