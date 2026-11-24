# 08_org_context.R v11
#
# v11: a map of the bounding boxes of the Content Unit Links, drawn under
# that card's table. Producing Units get no map: they may be
# administrative units with no geographic footprint at all, which is
# exactly why unit_category_server() takes require_geography = FALSE for
# them.
#
# WHY BOUNDING BOXES AND NOT OUTLINES. EMLeditor::set_content_units()
# documents what it writes into the EML: "Required child elements
# (bounding coordinates) are auto populated to produce a rectangle that
# encompasses the park unit in question." A rectangle per unit is
# therefore what the metadata will actually contain, so a rectangle per
# unit is what this map shows. Drawing the park outline would show the
# user something the data package does not record.
#
# The boxes come from NPSdatastore::get_unit_geography() with
# detail = "envelope" - the envelope IS the bounding box, so no hull has
# to be reduced to one here. (get_unit_geography()'s own documentation
# lists "envelope" as an acceptable alternative to the "convexHull"
# default.) This is a SECOND call to that API: the add-time call in
# unit_category_server() only asks whether geography exists and keeps
# nothing. Results are cached per unit code for the life of the session,
# so adding a sixth unit fetches one envelope, not six.
#
# THE GEOMETRY COLUMN IS FOUND BY CONTENT, NOT BY NAME. The add-time
# code in this file reads geo_result$Code, so "Code" is known to be the
# code column. The geometry column's name is NOT established anywhere
# this app can see: EMLeditor's own .get_unit_polygon() reads
# xml[[1]]$Geography from the same endpoint, which is a different casing
# convention from "Code", and the API's response shape is not documented
# in get_unit_geography()'s Rd beyond "A tibble of units and their
# geography". Rather than hardcode a guess that would silently yield an
# empty map, .geo_geometry_column() picks the column whose values look
# like geometry, and .geo_bbox_from_geometry() reads either WKT or
# GeoJSON. If neither is found the map says so instead of showing
# nothing.
#
# ---- v10 ----
#
# v10: added restore() to unit_category_server() and org_contextServer(),
# for the Save/Load session feature (app_state.R / app_server.R). No
# dependency on Tab 3's uploaded files - content/producing units, project
# ID, and cross-references are all independent of any CSV data - so
# restore() runs immediately on load.
#
# unit_category_server()'s return value changes from
#   list(data = codes, valid = shiny::reactive(TRUE))
# to
#   list(data = codes, valid = shiny::reactive(TRUE), restore = restore)
# restore() sets the `codes` reactiveVal directly from a saved character
# vector - no Shiny inputs back this table (same DT-rendered-from-
# reactiveVal pattern as Tabs 1/2), so setting the reactiveVal IS the
# restore. Deliberately does NOT re-validate restored codes against
# get_all_nps_units()/get_unit_geography() - those API calls already ran
# when the codes were first added and accepted; re-querying on every load
# would be slower and pointless (a code that was valid when saved hasn't
# stopped being an NPS unit code). If NPS unit codes are ever
# retired/renamed, that's an edge case a fresh Add would catch, not
# something restore() needs to defend against.
#
# Cross-references ($xrefs/$xref_titles) needed a NEW addition to saved
# state beyond what org_contextServer() already tracked internally:
# xref_titles() is normally populated by search_references_by_id_basic()
# at ADD time, not stored anywhere persistent. Saving titles directly
# (rather than re-querying DataStore for every restored reference ID on
# every load) avoids both an extra API round-trip and a blank Title
# column while that round-trip is in flight. Same non-revalidation
# reasoning as unit codes above applies here too - restore() does not
# re-check that cross-references are still Active.
#
# org_contextServer()'s return value changes from a bare reactive() to
# list(data = <reactive>, restore = <function>).

#' Fetch the live list of NPS units for validation/lookup. Falls back to
#' an empty tibble (with a warning) if the API call fails - unit entry
#' will then reject everything until the underlying issue is resolved,
#' rather than silently accepting unvalidated codes.
#'
#' @noRd
#' @return tibble with at least UnitCode, UnitName, FullName columns, or
#'   an empty tibble with those columns if the API call failed
#'
get_nps_units_table <- function() {
  units <- tryCatch({
    NPSdatastore::get_all_nps_units()
  }, error = function(e) {
    shiny::showNotification(
      paste0("Could not load NPS unit list from NPSdatastore::get_all_nps_units(): ",
             conditionMessage(e), ". Unit codes cannot be validated until this is resolved."),
      type = "warning", duration = 10
    )
    NULL
  })

  if (is.null(units) || nrow(units) == 0) {
    return(tibble::tibble(UnitCode = character(), UnitName = character(), FullName = character()))
  }

  units
}

# ---- Content Unit bounding boxes, for the map (v11) ----------------

# detail = "envelope" asks the units service for the bounding box itself
# rather than the "convexHull" default, so nothing has to be reduced to a
# rectangle here. get_unit_geography()'s documentation lists "envelope"
# as an acceptable alternative.
#
# This is the same geometry EMLeditor::set_content_units() turns into the
# N/E/S/W bounding coordinates it writes into each geographicCoverage
# element, which is why a rectangle is the honest thing to draw.
GEO_UNIT_BBOX_DETAIL <- "envelope"

#' Pull every coordinate pair out of a WKT geometry string and return its
#' bounding box. Pure function, base R only.
#'
#' Deliberately indifferent to the WKT type: POLYGON, MULTIPOLYGON and
#' POINT all reduce to "the numbers in this string, in x y order", and the
#' min/max of those pairs is the bounding box whichever it was. An
#' envelope is already a rectangle, so for the expected input this is the
#' rectangle's own corners.
#'
#' @noRd
#' @param wkt one WKT string
#' @return list(xmin=, ymin=, xmax=, ymax=) or NULL if no coordinate pairs
#'   could be read
.wkt_bbox <- function(wkt) {
  if (is.null(wkt) || length(wkt) != 1 || is.na(wkt)) return(NULL)

  # Drop the leading type name so its letters cannot be mistaken for
  # part of a number, then read every signed decimal / scientific-notation
  # number that remains.
  #
  # Only when there IS a "(": `sub("^[^(]*", "", x)` on a string with no
  # parenthesis matches the WHOLE string and erases it, which would make
  # .geometry_bbox()'s bare-number fallback always return NULL.
  body <- as.character(wkt)
  if (grepl("(", body, fixed = TRUE)) {
    body <- sub("^[^(]*", "", body)
  }
  nums <- regmatches(body, gregexpr("-?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?", body))[[1]]
  if (length(nums) < 2) return(NULL)

  vals <- suppressWarnings(as.numeric(nums))
  vals <- vals[!is.na(vals)]
  # An odd trailing value would mean a malformed pair; drop it rather than
  # letting it pair with nothing.
  if (length(vals) < 2) return(NULL)
  if (length(vals) %% 2 == 1) vals <- vals[-length(vals)]

  lon <- vals[seq(1, length(vals), by = 2)]
  lat <- vals[seq(2, length(vals), by = 2)]

  list(xmin = min(lon), ymin = min(lat), xmax = max(lon), ymax = max(lat))
}

#' Bounding box of a GeoJSON geometry string.
#'
#' EMLeditor's own .get_unit_polygon() reads this endpoint's geometry as
#' GeoJSON (sf::st_as_sfc(..., geoJSON = TRUE)), so GeoJSON is a shape
#' this app must be ready for even though breezeml asks for
#' dataformat = "wkt".
#'
#' Parsed with jsonlite rather than scraped for numbers: a GeoJSON object
#' can carry numbers that are not coordinates (a CRS code, for instance),
#' and only $coordinates may be read as such.
#'
#' @noRd
#' @param txt one GeoJSON geometry string
#' @return list(xmin=, ymin=, xmax=, ymax=) or NULL
.geojson_bbox <- function(txt) {
  if (is.null(txt) || length(txt) != 1 || is.na(txt)) return(NULL)

  parsed <- tryCatch(jsonlite::fromJSON(as.character(txt), simplifyVector = TRUE),
                     error = function(e) NULL)
  if (is.null(parsed)) return(NULL)

  coords <- parsed$coordinates
  if (is.null(coords)) {
    # A Feature or FeatureCollection wraps the geometry one or two levels
    # down. Reach for the documented paths only.
    coords <- parsed$geometry$coordinates
    if (is.null(coords) && !is.null(parsed$features)) {
      coords <- parsed$features$geometry$coordinates
    }
  }
  if (is.null(coords)) return(NULL)

  vals <- suppressWarnings(as.numeric(unlist(coords, use.names = FALSE)))
  vals <- vals[!is.na(vals)]
  if (length(vals) < 2) return(NULL)
  if (length(vals) %% 2 == 1) vals <- vals[-length(vals)]

  # GeoJSON position order is [longitude, latitude], and unlist() on a
  # nested coordinate array preserves that interleaving.
  lon <- vals[seq(1, length(vals), by = 2)]
  lat <- vals[seq(2, length(vals), by = 2)]

  list(xmin = min(lon), ymin = min(lat), xmax = max(lon), ymax = max(lat))
}

#' Bounding box of one geometry value, whichever of the two formats it is.
#'
#' @noRd
#' @param value one geometry string
#' @return list(xmin=, ymin=, xmax=, ymax=) or NULL
.geometry_bbox <- function(value) {
  if (is.null(value) || length(value) != 1 || is.na(value)) return(NULL)
  v <- trimws(as.character(value))
  if (!nzchar(v)) return(NULL)

  if (grepl("^(POLYGON|MULTIPOLYGON|POINT|MULTIPOINT|LINESTRING|MULTILINESTRING|GEOMETRYCOLLECTION)",
            v, ignore.case = TRUE)) {
    return(.wkt_bbox(v))
  }
  if (grepl("coordinates", v, fixed = TRUE)) {
    return(.geojson_bbox(v))
  }
  # Unrecognized: try WKT's number-scraping anyway rather than give up,
  # but only when the value contains no letters that would indicate some
  # other kind of payload entirely.
  if (!grepl("[A-Za-z]", v)) return(.wkt_bbox(v))
  NULL
}

#' Name of the column in a get_unit_geography() result that holds the
#' geometry, chosen by looking at the values rather than by name.
#'
#' The code column is known to be "Code" (the add-time path in this file
#' reads geo_result$Code), but the geometry column's name is not
#' established anywhere breezeml can see: get_unit_geography()'s Rd says
#' only "A tibble of units and their geography", and EMLeditor reads
#' $Geography from the same service while NPSdatastore returns $Code -
#' two different casing conventions in play. Guessing a name would fail
#' silently as an empty map, so pick the first column whose first
#' non-missing value parses as a geometry.
#'
#' @noRd
#' @param df a get_unit_geography() result
#' @return the column name, or NA_character_ if none qualifies
.geo_geometry_column <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NA_character_)

  for (nm in names(df)) {
    col <- df[[nm]]
    if (!is.character(col) && !is.factor(col)) next
    vals <- as.character(col)
    vals <- vals[!is.na(vals) & nzchar(trimws(vals))]
    if (length(vals) == 0) next
    if (!is.null(.geometry_bbox(vals[1]))) return(nm)
  }
  NA_character_
}

#' Fetch bounding boxes for unit codes, with a per-session cache.
#'
#' One API call for whichever codes are not cached yet, so adding a unit
#' to an existing list fetches only the new one.
#'
#' @noRd
#' @param codes character vector of unit codes (upper case)
#' @param cache an environment used as the store; keys are unit codes and
#'   values are either a bbox list or the string "none" for a code the
#'   service returned no usable geometry for (cached too, so a unit
#'   without geometry is not re-requested on every redraw)
#' @return list(boxes = <named list of bbox lists>, failed = <character>,
#'   error = <character or NA>) - `failed` names codes that were asked
#'   for but came back without usable geometry; `error` is set only when
#'   the call itself failed
.fetch_unit_bboxes <- function(codes, cache) {
  codes <- unique(toupper(trimws(codes[nzchar(trimws(codes))])))
  if (length(codes) == 0) {
    return(list(boxes = list(), failed = character(0), error = NA_character_))
  }

  missing_codes <- codes[!vapply(codes, function(cd) exists(cd, envir = cache, inherits = FALSE),
                                 logical(1))]

  err <- NA_character_
  if (length(missing_codes) > 0) {
    res <- tryCatch(
      NPSdatastore::get_unit_geography(missing_codes, detail = GEO_UNIT_BBOX_DETAIL),
      error = function(e) e
    )

    if (inherits(res, "condition")) {
      err <- conditionMessage(res)
    } else {
      # Same 0x0-tibble guard as the add-time path above: when the service
      # returns nothing usable, the result can have zero columns, not just
      # zero rows, so test both before indexing.
      geom_col <- .geo_geometry_column(res)
      if (nrow(res) == 0 || !"Code" %in% names(res) || is.na(geom_col)) {
        for (cd in missing_codes) assign(cd, "none", envir = cache)
      } else {
        res_codes <- toupper(as.character(res[["Code"]]))
        for (i in seq_len(nrow(res))) {
          bb <- .geometry_bbox(res[[geom_col]][i])
          if (!is.null(bb)) assign(res_codes[i], bb, envir = cache)
        }
        for (cd in missing_codes) {
          if (!exists(cd, envir = cache, inherits = FALSE)) {
            assign(cd, "none", envir = cache)
          }
        }
      }
    }
  }

  boxes <- list()
  failed <- character(0)
  for (cd in codes) {
    if (!exists(cd, envir = cache, inherits = FALSE)) {
      failed <- c(failed, cd)
      next
    }
    val <- get(cd, envir = cache, inherits = FALSE)
    if (identical(val, "none")) {
      failed <- c(failed, cd)
    } else {
      boxes[[cd]] <- val
    }
  }

  list(boxes = boxes, failed = failed, error = err)
}

#' Reusable UI for one unit-entry category (Content Units or Producing
#' Units) - matches person_category_ui()'s pattern in 02_people.R.
#'
#' @noRd
#' @keywords internal
#'
#' @param show_map if TRUE, a map of the units' bounding boxes is drawn
#'   under the table, with a line of status text beneath it. Used for
#'   Content Units only: Producing Units may be administrative units with
#'   no geographic footprint, which is the same reason their server is
#'   given require_geography = FALSE. The module's server must be given
#'   show_map = TRUE to match, or the outputs will never be rendered.
#'
unit_category_ui <- function(id, header, help_text, show_map = FALSE) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header(header),
    shiny::helpText(help_text),
    bslib::layout_columns(
      shiny::textInput(ns("new_code"), NULL,
                       placeholder = "Enter one or more unit codes, separated by commas", width = "100%"),
      shiny::actionButton(ns("add_code"), "Add", class = "btn-primary btn-sm"),
      col_widths = c(10, 2)
    ),
    DT::DTOutput(ns("units_table")),

    # The map is only built when asked for. Adding the output
    # unconditionally would reserve 300px of blank space in the Producing
    # Units card for a map that is never rendered there.
    if (isTRUE(show_map)) {
      shiny::tagList(
        shiny::br(),
        # Height matches Tab 5's map (leaflet::leafletOutput(height = 350))
        # less a little, since this one sits inside a card with a table.
        leaflet::leafletOutput(ns("unit_map"), height = 300),
        shiny::uiOutput(ns("unit_map_status"))
      )
    }
  )
}

#' Reusable server for one unit-entry category. Matches
#' person_category_server()'s pattern in 02_people.R: type/paste codes,
#' validate in a batch against the live unit table (and, if
#' require_geography is TRUE, additionally against
#' NPSdatastore::get_unit_geography()), add valid ones, report skipped
#' ones with a specific reason, allow per-row removal.
#'
#' @noRd
#' @param id module id
#' @param units_table the tibble from get_nps_units_table(), shared across
#'   both categories so get_all_nps_units() is only called once per session
#' @param require_geography if TRUE (Content Units), codes must ALSO have
#'   an entry in NPSdatastore::get_unit_geography() to be accepted - a
#'   recognized-but-geography-less code is rejected with a distinct reason
#'   from an unrecognized code entirely. If FALSE (Producing Units), only
#'   units_table membership is checked.
#' @param show_map if TRUE, render the bounding-box map and its status
#'   line. Must match the show_map passed to unit_category_ui() for the
#'   same id - the outputs only exist in the UI when that is TRUE.
#' @return list(data = reactive() character vector of UnitCode, valid = reactive() TRUE,
#'   restore = function)
#'

#'
unit_category_server <- function(id, units_table, require_geography = FALSE,
                                 show_map = FALSE) {
  shiny::moduleServer(id, function(input, output, session) {
    codes <- shiny::reactiveVal(character(0))

    # Per-session cache of unit code -> bounding box (or the string
    # "none"). An environment, not a reactiveVal: writing to it must NOT
    # invalidate the map that is reading it, or the renderer would loop.
    # One environment per module instance, created whether or not the map
    # is shown, so the object always exists where it is referenced.
    bbox_cache <- new.env(parent = emptyenv())

    shiny::observeEvent(input$add_code, {
      shiny::req(input$new_code)

      raw_entries <- strsplit(input$new_code, ",")[[1]]
      candidates <- toupper(trimws(raw_entries))
      candidates <- candidates[nzchar(candidates)]
      shiny::req(length(candidates) > 0)

      current <- codes()
      valid_lookup <- toupper(units_table$UnitCode)

      is_recognized <- candidates %in% valid_lookup
      already_present <- candidates %in% current
      dupe_within_batch <- duplicated(candidates)

      # Only check geography for codes that are otherwise eligible
      # (recognized, not already present, not a within-batch duplicate) -
      # no point spending an API call validating something that's going
      # to be rejected for a different reason anyway.
      has_geography <- rep(TRUE, length(candidates))
      if (require_geography) {
        needs_geo_check <- is_recognized & !already_present & !dupe_within_batch
        if (any(needs_geo_check)) {
          geo_result <- tryCatch(
            NPSdatastore::get_unit_geography(candidates[needs_geo_check]),
            error = function(e) {
              shiny::showNotification(
                paste0("Could not verify unit geography: ", conditionMessage(e),
                       ". Rejecting affected codes until this is resolved."),
                type = "warning"
              )
              tibble::tibble(Code = character())
            }
          )

          # CONFIRMED BUG SOURCE (v9): when none of the queried codes have
          # geography, get_unit_geography() returns a 0x0 tibble (zero
          # columns, not just zero rows). Extracting $Code from that shape
          # does not reliably behave like extracting $Code from a normal
          # 0-row tibble with the Code column intact, and previously let
          # geography-less codes (e.g. "IMD") pass through as if they had
          # geography. Guard explicitly on row count AND column presence
          # before attempting the $Code extraction, so a fully-empty
          # result is always treated as "nothing in this batch has
          # geography" rather than silently mismatching.
          if (nrow(geo_result) == 0 || !"Code" %in% names(geo_result)) {
            found_geo_codes <- character(0)
          } else {
            found_geo_codes <- toupper(geo_result$Code)
          }

          has_geography[needs_geo_check] <- candidates[needs_geo_check] %in% found_geo_codes
        }
      }

      skip_reason <- dplyr::case_when(
        !is_recognized ~ "not a recognized NPS unit code",
        already_present ~ "already added",
        dupe_within_batch ~ "duplicate in list",
        require_geography & !has_geography ~ "recognized unit but has no associated geography",
        TRUE ~ NA_character_
      )

      valid_new <- candidates[is.na(skip_reason)]
      skipped <- tibble::tibble(code = candidates[!is.na(skip_reason)],
                                reason = skip_reason[!is.na(skip_reason)])

      if (length(valid_new) > 0) {
        codes(c(current, valid_new))
      }
      shiny::updateTextInput(session, "new_code", value = "")

      msg <- paste0("Added ", length(valid_new), " unit(s).")
      if (nrow(skipped) > 0) {
        msg <- paste0(msg, " Skipped ", nrow(skipped), ": ",
                      paste0(skipped$code, " (", skipped$reason, ")", collapse = ", "))
      }
      shiny::showNotification(msg, type = if (nrow(skipped) > 0) "warning" else "message")
    })

    output$units_table <- DT::renderDT({
      current <- codes()
      display_df <- tibble::tibble(code = current)

      if (nrow(display_df) > 0) {
        match_idx <- match(display_df$code, toupper(units_table$UnitCode))
        display_df$name <- ifelse(
          !is.na(match_idx) & nzchar(units_table$FullName[match_idx]),
          units_table$FullName[match_idx],
          ifelse(!is.na(match_idx), units_table$UnitName[match_idx], NA_character_)
        )
        display_df$remove <- vapply(seq_len(nrow(display_df)), function(i) {
          as.character(
            shiny::actionButton(session$ns(paste0("remove_", i)), "Remove",
                                class = "btn-danger btn-sm",
                                onclick = sprintf(
                                  'Shiny.setInputValue(\"%s\", %d, {priority: \"event\"})',
                                  session$ns("remove_code"), i
                                ))
          )
        }, character(1))
      } else {
        display_df$name <- character(0)
        display_df$remove <- character(0)
      }

      DT::datatable(
        display_df,
        rownames = FALSE,
        selection = "none",
        colnames = c("Code", "Name", ""),
        escape = which(names(display_df) != "remove") - 1,
        options = list(dom = 't', pageLength = -1)
      )
    })

    shiny::observeEvent(input$remove_code, {
      idx <- input$remove_code
      current <- codes()
      shiny::req(idx >= 1, idx <= length(current))
      codes(current[-idx])
    })

    # ---- The bounding-box map (v11) ----
    #
    # One reactive does the fetching so the map and the status line below
    # it cannot disagree: both read this, and it runs once per change to
    # `codes`. NULL when no map was asked for, so neither output renders
    # anything in the Producing Units card.
    unit_boxes <- shiny::reactive({
      if (!isTRUE(show_map)) return(NULL)
      current <- codes()
      if (length(current) == 0) {
        return(list(boxes = list(), failed = character(0), error = NA_character_))
      }
      .fetch_unit_bboxes(current, bbox_cache)
    })

    output$unit_map <- leaflet::renderLeaflet({
      info <- unit_boxes()
      shiny::req(!is.null(info))

      map <- leaflet::leaflet() |>
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap)

      # No units yet, or none with a usable box: still show a base map
      # rather than a blank widget, matching 05_geography.R's behaviour
      # for the no-points case.
      if (length(info$boxes) == 0) {
        return(map |> leaflet::setView(lng = -98.6, lat = 39.8, zoom = 3))
      }

      codes_in_order <- names(info$boxes)
      # row.names = NULL on purpose: vapply() over a NAMED list returns a
      # named vector, and data.frame() would otherwise take the unit codes
      # as row names. Harmless to leaflet, but the codes already have
      # their own column and duplicated row names would error.
      bb <- data.frame(
        code = codes_in_order,
        xmin = unname(vapply(info$boxes, function(b) b$xmin, numeric(1))),
        ymin = unname(vapply(info$boxes, function(b) b$ymin, numeric(1))),
        xmax = unname(vapply(info$boxes, function(b) b$xmax, numeric(1))),
        ymax = unname(vapply(info$boxes, function(b) b$ymax, numeric(1))),
        stringsAsFactors = FALSE,
        row.names = NULL
      )

      # The unit's full name in the popup, from the table already loaded
      # for validation - no extra API call. Falls back to the code alone
      # when the name is not found.
      match_idx <- match(bb$code, toupper(units_table$UnitCode))
      unit_name <- ifelse(
        !is.na(match_idx) & nzchar(units_table$FullName[match_idx]),
        units_table$FullName[match_idx],
        ifelse(!is.na(match_idx), units_table$UnitName[match_idx], NA_character_)
      )
      bb$label <- ifelse(is.na(unit_name), bb$code,
                         paste0(bb$code, " - ", unit_name))

      # Rectangles, not polygons: this is what set_content_units() writes
      # into the metadata (see the file header).
      map <- map |>
        leaflet::addRectangles(
          data = bb,
          lng1 = ~xmin, lat1 = ~ymin, lng2 = ~xmax, lat2 = ~ymax,
          popup = ~label, label = ~code,
          weight = 2, opacity = 0.9, fillOpacity = 0.15
        )

      # fitBounds() on a single degenerate box (a point-like unit) would
      # zoom to an undefined extent, so fall back to setView() when the
      # overall extent has no width or height.
      lng1 <- min(bb$xmin); lng2 <- max(bb$xmax)
      lat1 <- min(bb$ymin); lat2 <- max(bb$ymax)

      if (lng1 == lng2 || lat1 == lat2) {
        map |> leaflet::setView(lng = (lng1 + lng2) / 2,
                                lat = (lat1 + lat2) / 2, zoom = 10)
      } else {
        map |> leaflet::fitBounds(lng1 = lng1, lat1 = lat1,
                                  lng2 = lng2, lat2 = lat2)
      }
    })

    output$unit_map_status <- shiny::renderUI({
      info <- unit_boxes()
      if (is.null(info)) return(NULL)

      if (!is.na(info$error)) {
        return(shiny::tags$div(
          class = "alert alert-warning mt-2 mb-0",
          paste0("Could not retrieve unit boundaries: ", info$error,
                 " The units listed above are still recorded; only this ",
                 "map is affected.")
        ))
      }

      if (length(info$boxes) == 0 && length(info$failed) == 0) {
        return(shiny::tags$div(
          class = "text-muted mt-2",
          "Add a content unit above to see its bounding box here."
        ))
      }

      shiny::tagList(
        shiny::tags$div(
          class = "text-muted mt-2",
          paste0(
            "Showing the bounding box for ", length(info$boxes), " of ",
            length(info$boxes) + length(info$failed), " content unit(s). ",
            "These rectangles are what EMLeditor::set_content_units() ",
            "records as each unit's N/E/S/W bounding coordinates - not the ",
            "park's actual boundary."
          )
        ),
        if (length(info$failed) > 0) {
          shiny::tags$div(
            class = "text-muted",
            paste0("No boundary returned for: ",
                   paste(info$failed, collapse = ", "), ".")
          )
        }
      )
    })

    # Push a saved character vector of unit codes directly into `codes`.
    # No Shiny inputs back this table, so setting the reactiveVal IS the
    # restore. Deliberately skips re-validation against
    # get_all_nps_units()/get_unit_geography() - see file header note.
    # The map is a separate matter: it reads `codes` reactively, so a
    # restore redraws it, and the boxes it needs are fetched then. That
    # is a geometry lookup, not a re-validation of the codes.
    #
    # @param saved character vector of UnitCode strings, or NULL/empty
    restore <- function(saved) {
      codes(if (is.null(saved)) character(0) else as.character(saved))
      invisible(NULL)
    }

    list(data = codes, valid = shiny::reactive(TRUE), restore = restore)
  })
}

org_context_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    unit_category_ui(
      ns("content_units"), "Content Unit Link(s) (Optional)",
      paste0("Park units where the DATA were collected. Must be units with ",
             "actual geographic boundaries (bounding box coordinates are ",
             "generated per unit) - administrative-only units without ",
             "geography will be rejected. If data span multiple units ",
             "within a network, list each unit individually rather than ",
             "the network. Enter 4-letter unit codes (e.g. ACAD, YELL). ",
             "The map below shows each unit's bounding box."),
      show_map = TRUE
    ),
    unit_category_ui(
      ns("producing_units"), "Producing Unit(s) (Required)",
      paste0("The unit(s) responsible for generating the data package - ",
             "may be a single park, a network, or an administrative unit ",
             "with no geographic footprint. May overlap with, or differ ",
             "entirely from, the content units above.")
    ),
    bslib::card(
      bslib::card_header("DataStore Project (optional)"),
      shiny::helpText("Input the DataStore reference id (typically a 7-digit ",
                      "number for the DataStore Project you would like to add.",
                      " DataStore only supports one project connection per ",
                      "data package. The project must already exist, be ",
                      "Active, and be of reference type \"Project\". Remove ",
                      "the current project before adding a different one."),
      bslib::layout_columns(
        shiny::numericInput(ns("new_project_id"), NULL, value = NA, width = "100%"),
        shiny::actionButton(ns("add_project"), "Add", class = "btn-primary btn-sm"),
        col_widths = c(10, 2)
      ),
      DT::DTOutput(ns("project_table"))
    ),
    bslib::card(
      bslib::card_header("Cross-references (optional)"),
      shiny::helpText("Reference IDs for related DataStore items (e.g. a sampling ",
                      "scheme diagram) not included in the data package itself. ",
                      "Must be valid, Active DataStore references (any reference ",
                      "type is allowed)."),
      bslib::layout_columns(
        shiny::textInput(ns("new_xref"), NULL,
                         placeholder = "Enter one or more reference IDs, separated by commas", width = "100%"),
        shiny::actionButton(ns("add_xref"), "Add", class = "btn-primary btn-sm"),
        col_widths = c(10, 2)
      ),
      DT::DTOutput(ns("xref_table"))
    ),
    col_widths = c(-2, 8, -2), fill = FALSE
  )
}

#' @noRd
#' @return list(data = <reactive() list>, restore = <function>)
#'   data() returns:
#'     $content_units, $producing_units - character vectors of UnitCode (may be empty)
#'     $project_id - integer or NA
#'     $cross_references - integer vector (may be empty)
#'     $cross_reference_titles - character vector, same length/order as
#'       $cross_references - saved alongside so restore() doesn't need to
#'       re-query DataStore for titles (see file header note)
#'     $valid, $errors
#'
org_contextServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    units_table <- get_nps_units_table()

    # show_map must match what unit_category_ui() was given for the same
    # id: TRUE for content units, absent (FALSE) for producing units,
    # which may have no geography at all.
    content_units <- unit_category_server("content_units", units_table,
                                          require_geography = TRUE,
                                          show_map = TRUE)
    producing_units <- unit_category_server("producing_units", units_table,
                                            require_geography = FALSE)

    # DataStore Project - single-entry only (EML schema allows exactly one
    # Project link per data package, unlike Cross-references, which allow
    # many). RESTORED (v13) after being accidentally dropped to a bare
    # numericInput in an earlier pass of this file during unrelated
    # Save/Load work - see 08_org_context.R v12 for the original,
    # unmodified version of this logic, ported back in unchanged here.
    #
    # Validated against NPSdatastore::search_references_by_id_basic():
    # the candidate ID must resolve (API errors outright if not found at
    # all), must have referenceType == "Project", and must have
    # lifecycle == "Active". No pre-API format check is performed;
    # malformed input is simply passed to the API and whatever it reports
    # (typically not-found) is surfaced as the skip reason. The project's
    # title (from the `title` column, NOT `publicationTitle`, which is
    # unreliable/often NA) is captured at add-time for display alongside
    # its ID. A second Add attempt while one is already present is
    # rejected outright (without even calling the API) with a message to
    # remove the existing one first.
    project_id <- shiny::reactiveVal(NA_integer_)
    project_title <- shiny::reactiveVal(NA_character_)

    shiny::observeEvent(input$add_project, {
      shiny::req(input$new_project_id, !is.na(input$new_project_id))
      candidate <- as.integer(input$new_project_id)

      if (!is.na(project_id())) {
        shiny::showNotification(
          "A project is already linked. Remove it before adding a new one.",
          type = "warning"
        )
        shiny::updateNumericInput(session, "new_project_id", value = NA)
        return()
      }

      ref_result <- tryCatch(
        NPSdatastore::search_references_by_id_basic(candidate),
        error = function(e) {
          NULL
        }
      )

      if (is.null(ref_result) || nrow(ref_result) == 0) {
        shiny::showNotification(
          paste0(candidate, ": reference ID not found on DataStore."),
          type = "warning"
        )
      } else if (ref_result$referenceType[1] != "Project") {
        shiny::showNotification(
          paste0(candidate, ": reference exists but is a \"",
                 ref_result$referenceType[1], "\", not a Project."),
          type = "warning"
        )
      } else if (ref_result$lifecycle[1] != "Active") {
        shiny::showNotification(
          paste0(candidate, ": reference is a Project but is not Active ",
                 "(status: \"", ref_result$lifecycle[1], "\")."),
          type = "warning"
        )
      } else {
        project_id(candidate)
        project_title(ref_result$title[1])
        shiny::showNotification(paste0("Added project ", candidate, "."), type = "message")
      }

      shiny::updateNumericInput(session, "new_project_id", value = NA)
    })

    output$project_table <- DT::renderDT({
      current <- project_id()
      display_df <- if (!is.na(current)) {
        tibble::tibble(reference_id = current, title = project_title())
      } else {
        tibble::tibble(reference_id = integer(0), title = character(0))
      }

      if (nrow(display_df) > 0) {
        display_df$remove <- vapply(seq_len(nrow(display_df)), function(i) {
          as.character(
            shiny::actionButton(session$ns(paste0("remove_project_", i)), "Remove",
                                class = "btn-danger btn-sm",
                                onclick = sprintf(
                                  'Shiny.setInputValue(\"%s\", %d, {priority: \"event\"})',
                                  session$ns("remove_project"), i
                                ))
          )
        }, character(1))
      } else {
        display_df$remove <- character(0)
      }

      DT::datatable(display_df, rownames = FALSE, selection = "none",
                    colnames = c("Reference ID", "Title", ""), escape = FALSE,
                    options = list(dom = 't', pageLength = -1))
    })

    shiny::observeEvent(input$remove_project, {
      project_id(NA_integer_)
      project_title(NA_character_)
    })

    # Cross-references - like Content/Producing Units, multiple may be
    # added at once via comma-separated batch entry, and (unlike the
    # Project field) any referenceType is acceptable; the only
    # requirements are that the reference exists and is Active.
    #
    # search_references_by_id_basic() behaves differently for batches than
    # for single IDs: a single not-found ID errors outright, but in a
    # batch of multiple IDs it instead returns rows for whichever WERE
    # found and emits a warning (not an error) naming the ones that
    # weren't - so "not found" for a batch is determined by diffing the
    # candidate IDs against the returned referenceId column, not by
    # catching an error. An error is only expected here if NONE of the
    # batch's eligible candidates were found, matching the single-ID case.
    # The API's own warning about missing IDs is suppressed (via
    # withCallingHandlers) since the same information is already being
    # derived and reported through the app's own skip-reason mechanism -
    # surfacing it twice would be redundant.
    xrefs <- shiny::reactiveVal(integer(0))
    xref_titles <- shiny::reactiveVal(character(0))

    shiny::observeEvent(input$add_xref, {
      shiny::req(input$new_xref)

      raw_entries <- strsplit(input$new_xref, ",")[[1]]
      candidate_strs <- trimws(raw_entries)
      candidate_strs <- candidate_strs[nzchar(candidate_strs)]
      shiny::req(length(candidate_strs) > 0)

      candidates <- suppressWarnings(as.integer(candidate_strs))
      not_numeric <- is.na(candidates)

      current <- xrefs()
      already_present <- candidates %in% current
      dupe_within_batch <- duplicated(candidates)

      needs_lookup <- !not_numeric & !already_present & !dupe_within_batch

      found_ids <- integer(0)
      active_ids <- integer(0)
      inactive_lookup <- character(0)
      titles_by_id <- character(0)

      if (any(needs_lookup, na.rm = TRUE)) {
        lookup_ids <- candidates[needs_lookup]
        ref_result <- withCallingHandlers(
          tryCatch(
            NPSdatastore::search_references_by_id_basic(lookup_ids),
            error = function(e) NULL
          ),
          warning = function(w) invokeRestart("muffleWarning")
        )

        if (!is.null(ref_result) && nrow(ref_result) > 0) {
          found_ids <- ref_result$referenceId
          active_mask <- ref_result$lifecycle == "Active"
          active_ids <- ref_result$referenceId[active_mask]
          inactive_lookup <- setNames(ref_result$lifecycle[!active_mask],
                                      ref_result$referenceId[!active_mask])
          titles_by_id <- setNames(ref_result$title, ref_result$referenceId)
        }
      }

      skip_reason <- rep(NA_character_, length(candidates))
      skip_reason[not_numeric] <- "not a valid reference ID"
      skip_reason[already_present & is.na(skip_reason)] <- "already added"
      skip_reason[dupe_within_batch & is.na(skip_reason)] <- "duplicate in list"
      skip_reason[needs_lookup & !(candidates %in% found_ids) & is.na(skip_reason)] <-
        "not found on DataStore"
      skip_reason[needs_lookup & (candidates %in% found_ids) & !(candidates %in% active_ids) & is.na(skip_reason)] <-
        vapply(candidates[needs_lookup & (candidates %in% found_ids) & !(candidates %in% active_ids)],
               function(id) paste0("not Active (status: \"", inactive_lookup[[as.character(id)]], "\")"),
               character(1))

      valid_new <- candidates[is.na(skip_reason)]
      valid_new_titles <- vapply(valid_new, function(id) {
        t <- titles_by_id[[as.character(id)]]
        if (is.null(t)) NA_character_ else t
      }, character(1))

      skipped <- tibble::tibble(code = candidate_strs[!is.na(skip_reason)],
                                reason = skip_reason[!is.na(skip_reason)])

      if (length(valid_new) > 0) {
        xrefs(c(current, valid_new))
        xref_titles(c(xref_titles(), valid_new_titles))
      }
      shiny::updateTextInput(session, "new_xref", value = "")

      msg <- paste0("Added ", length(valid_new), " reference(s).")
      if (nrow(skipped) > 0) {
        msg <- paste0(msg, " Skipped ", nrow(skipped), ": ",
                      paste0(skipped$code, " (", skipped$reason, ")", collapse = ", "))
      }
      shiny::showNotification(msg, type = if (nrow(skipped) > 0) "warning" else "message")
    })

    output$xref_table <- DT::renderDT({
      refs <- xrefs()
      titles <- xref_titles()
      display_df <- tibble::tibble(reference_id = refs, title = titles)
      if (nrow(display_df) > 0) {
        display_df$remove <- vapply(seq_len(nrow(display_df)), function(i) {
          as.character(
            shiny::actionButton(session$ns(paste0("remove_xref_", i)), "Remove",
                                class = "btn-danger btn-sm",
                                onclick = sprintf(
                                  'Shiny.setInputValue(\"%s\", %d, {priority: \"event\"})',
                                  session$ns("remove_xref"), i
                                ))
          )
        }, character(1))
      } else {
        display_df$remove <- character(0)
      }
      DT::datatable(display_df, rownames = FALSE, selection = "none",
                    colnames = c("Reference ID", "Title", ""), escape = FALSE,
                    options = list(dom = 't', pageLength = -1))
    })

    shiny::observeEvent(input$remove_xref, {
      idx <- input$remove_xref
      current <- xrefs()
      shiny::req(idx >= 1, idx <= length(current))
      xrefs(current[-idx])
      xref_titles(xref_titles()[-idx])
    })

    data <- shiny::reactive({
      list(
        content_units = content_units$data(),
        producing_units = producing_units$data(),
        project_id = project_id(),
        project_title = project_title(),
        cross_references = xrefs(),
        cross_reference_titles = xref_titles(),
        valid = TRUE,
        errors = character(0)
      )
    })

    # Push saved state into this module. Called once by app_server.R
    # right after a JSON load, with saved_state$org_context (see
    # app_state.R). No dependency on Tab 3's uploaded files - safe to
    # call immediately.
    #
    # Project and cross-reference IDs/titles are restored as matched
    # pairs directly into their respective reactiveVals - not
    # re-validated against search_references_by_id_basic() (same
    # non-revalidation reasoning as unit codes: a reference that was
    # valid when saved hasn't stopped existing; re-querying on every
    # load is slower and pointless. If a saved project/xref were ever
    # retired/made Inactive since saving, that's an edge case a fresh
    # Add or Generate-time check would catch, not something restore()
    # needs to defend against).
    #
    # @param saved list matching default_app_state()$org_context's shape
    restore <- function(saved) {
      if (is.null(saved)) return(invisible(NULL))

      content_units$restore(saved$content_units)
      producing_units$restore(saved$producing_units)

      if (!is.null(saved$project_id) && !is.na(saved$project_id)) {
        project_id(as.integer(saved$project_id))
        project_title(if (!is.null(saved$project_title)) as.character(saved$project_title) else NA_character_)
      } else {
        project_id(NA_integer_)
        project_title(NA_character_)
      }

      saved_refs <- if (is.null(saved$cross_references)) integer(0) else as.integer(saved$cross_references)
      saved_titles <- if (is.null(saved$cross_reference_titles)) character(0) else as.character(saved$cross_reference_titles)
      # defensive: if titles/refs somehow desynced in the save file (e.g.
      # hand-edited JSON), pad/truncate titles to match refs' length
      # rather than letting xref_table's display_df construction fail on
      # mismatched vector lengths
      if (length(saved_titles) != length(saved_refs)) {
        saved_titles <- rep(NA_character_, length(saved_refs))
      }
      xrefs(saved_refs)
      xref_titles(saved_titles)

      invisible(NULL)
    }

    list(data = data, restore = restore)
  })
}

#' Apply this tab's state to an in-memory EML object. Called from
#' run_generation() alongside apply_permissions_to_eml(), after make_eml()
#' and before write_eml(). Every piece here is optional, so each call is
#' skipped if its corresponding state is empty/NA.
#'
#' force = TRUE / NPS = TRUE on every call, matching apply_permissions_to_eml()
#' - see that function's docstring in 07_permissions.R for the rationale.
#'
#' @param my_metadata the in-memory EML object (post make_eml(), pre write_eml())
#' @param state the list returned by org_contextServer()'s $data reactive,
#'   evaluated (i.e. state <- org_context$data())
#' @return the EML object with content units, producing units, project,
#'   and cross-reference links applied
apply_org_context_to_eml <- function(my_metadata, state) {
  if (length(state$content_units) > 0) {
    my_metadata <- EMLeditor::set_content_units(my_metadata, state$content_units, force = TRUE, NPS = TRUE)
  }
  if (length(state$producing_units) > 0) {
    my_metadata <- EMLeditor::set_producing_units(my_metadata, state$producing_units, force = TRUE, NPS = TRUE)
  }
  if (!is.na(state$project_id)) {
    my_metadata <- EMLeditor::set_project(my_metadata, state$project_id, force = TRUE, NPS = TRUE)
  }
  for (ref in state$cross_references) {
    my_metadata <- EMLeditor::set_cross_reference(my_metadata, ref, force = TRUE, NPS = TRUE)
  }
  my_metadata
}

#' Emit the R code chunk that reproduces apply_org_context_to_eml()'s
#' calls as literal script text, for the "Preview script" modal and the
#' generation_script.R written to disk. Pure function - must be kept in
#' exact sync with apply_org_context_to_eml() above; if that function's
#' logic changes, update this to match.
#'
#' @param state the list returned by org_contextServer()'s $data reactive,
#'   evaluated (i.e. state <- org_context$data())
#' @return character - the R code chunk applying content/producing units,
#'   project link, and cross-references (only for whichever are non-empty)
emit_org_context_chunk <- function(state) {
  if (is.null(state)) {
    return("# No organizational context (units/project/cross-references) captured.\n")
  }

  lines <- c('# --- Organizational context: units, project, cross-references (Tab 8) ---')

  if (length(state$content_units) > 0) {
    units_r <- deparse(state$content_units)
    lines <- c(lines, glue::glue(
      'my_metadata <- EMLeditor::set_content_units(my_metadata, {units_r}, force = TRUE, NPS = TRUE)'
    ))
  }
  if (length(state$producing_units) > 0) {
    units_r <- deparse(state$producing_units)
    lines <- c(lines, glue::glue(
      'my_metadata <- EMLeditor::set_producing_units(my_metadata, {units_r}, force = TRUE, NPS = TRUE)'
    ))
  }
  if (!is.na(state$project_id)) {
    lines <- c(lines, glue::glue(
      'my_metadata <- EMLeditor::set_project(my_metadata, {state$project_id}, force = TRUE, NPS = TRUE)'
    ))
  }
  for (ref in state$cross_references) {
    lines <- c(lines, glue::glue(
      'my_metadata <- EMLeditor::set_cross_reference(my_metadata, {ref}, force = TRUE, NPS = TRUE)'
    ))
  }

  if (length(lines) == 1) {
    lines <- c(lines, "# (none of content units / producing units / project / cross-references were set)")
  }

  paste0(paste(lines, collapse = "\n"), "\n")
}
