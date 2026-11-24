# 05_geography.R v19.1
#
# v19.1: the "folder not known yet" message now asks for the metadata
# filename on tab 1 only. app_server.R v27 derives the path from that one
# field, defaulting the parent folder to getwd(), so sending the user to
# the Generate tab was both unnecessary and - because
# input$working_folder is NULL until that tab is rendered - the reason
# the button refused.
#
# v19: three changes, all requested after v18 was confirmed working.
#
# 1. NOTHING is shown until a coordinate source is chosen. The radio
#    group now starts with no button selected (selected = character(0);
#    radioButtons() only substitutes the first choice when `selected` is
#    NULL, and a zero-length vector is not NULL), and the map/preview
#    panel is gated on a source having been picked, not merely on
#    has_coords. So ticking the box shows the question and nothing else;
#    picking an answer reveals only that answer's fields.
#
#    This creates a FOURTH state that did not exist before: has_coords
#    TRUE with no source chosen. result() reports it as
#    source = NA_character_ with enabled = TRUE. It is not an error and
#    not "table" - run_generation() (09_generate.R v10) refuses to
#    generate with a clear message rather than silently templating from a
#    data table the user never selected. Every consumer of
#    result()$source must therefore test is.na() BEFORE comparing.
#
# 2. The template is SAVED INTO data_package_creation, not downloaded
#    through the browser, and the user is told the full path. The folder
#    is created if it does not exist yet, so the template can be fetched
#    before the first Generate. An existing geography_template.xlsx in
#    that folder is NEVER overwritten - this button is easy to press
#    twice and must not destroy a filled-out template. geographyServer()
#    takes a new optional argument, creation_dir_reactive, for the
#    folder's path; omitting it keeps the pre-v19 signature working and
#    the button reports that the folder is unknown.
#
# 3. The generated script reads the WORKBOOK when the template was used.
#    emit_geo_chunk() emits readxl::read_excel() against
#    geography_template.xlsx in the working folder, filters to the rows
#    with a site name and valid coordinates, writes them as a plain CSV,
#    and templates from that CSV. The conversion step is unavoidable:
#    EMLassemblyline's template_arguments() reads a data.table with
#        data.table::fread(file = f)
#    which parses delimited text only and cannot open .xlsx. The script
#    is self-contained because run_generation() copies the uploaded
#    workbook into the working folder beside it.
#
# ---- v18 ----
#
# v18: FIXED - the geography-template download button and upload file box
# added in v17 never appeared on the tab.
#
# ROOT CAUSE: v17 placed the two per-source panels INSIDE the
# has_coords conditionalPanel, i.e. a conditionalPanel nested in another
# conditionalPanel. Shiny evaluates these panels in
# ShinyApp.$updateConditionals(), which does
#
#   const conditionals = $(document).find("[data-display-if]");
#
# and then, per element, toggles the class "shiny-conditional--shown".
# The visibility of a panel is decided ONLY by its own condition and
# that class; an element inside a hidden parent is still found by the
# selector, but the parent's own hidden state governs what is painted.
# The nested arrangement therefore depends on the CSS interaction
# between the outer and inner panel's shown classes, and on this tab,
# inside bslib::card() within bslib::layout_columns(), the inner panels
# stayed hidden.
#
# This app does not nest conditionalPanel anywhere else. Every other
# instance - 07_permissions.R (access_level, language),
# start_screen.R (revise_source), app_ui.R (app_screen) - is a SINGLE
# level with `ns = ns`, and all of them work. The radio-button pattern
# in start_screen.R is the exact analogue of what this tab needed.
#
# THE FIX: all four panels on this tab are now siblings at one level,
# each with its own complete condition:
#   input.has_coords == true
#   input.has_coords == true && input.coord_source == 'table'
#   input.has_coords == true && input.coord_source == 'upload'
#   input.has_coords == true            (map and preview, both sources)
# Compound conditions are plain JavaScript, which is what
# scopeExprToFunc() compiles, so && is evaluated exactly as written.
#
# NOTHING ELSE CHANGED. The server, the validator, the download handler,
# the generation path and the saved-state shape are byte-identical to
# v17; this was a markup-structure defect only.
#
# ---- v17 (as written, behaviour retained) ----
#
# v17: coordinates may now come from an uploaded geography template, not
# only from a column of an imported data table.
#
# WHY. Users sometimes hold GPS points that are not in the data at all,
# or that describe a subset of the rows in the data (one point per site
# rather than one per observation). Tab 5 could only name a lat column
# and a lon column inside a table imported on Tab 3, so those users had
# nowhere to put their points.
#
# WHAT WAS ADDED. A "Where do the coordinates come from?" choice. The
# existing behaviour is the default and is untouched; the second option
# offers a download of a three-column template
# (SiteID / decimalLatitude / decimalLongitude) and a file box to upload
# the filled-out copy. The template ships with the package at
# inst/extdata/geography_template.xlsx and is located the same way
# app_ui.R locates its browser assets, so it works both from an
# installed package and under devtools::load_all().
#
# The map and the preview table below the controls serve BOTH sources.
# Everything downstream of result() was made source-aware rather than
# duplicated.
#
# THE UPLOAD IS VALIDATED BEFORE IT IS ACCEPTED, and a rejected file
# leaves the previous state alone rather than half-replacing it. The
# checks are: readable as .xlsx; all three template columns present
# (compared after trimws(), so a stray space in a header is tolerated,
# but a renamed or missing column is not); latitude and longitude
# coercible to numbers; latitude within -90..90 and longitude within
# -180..180; SiteID non-blank. Rows failing the per-row checks are
# dropped and counted in the status message; a file with no usable row
# at all is rejected outright.
#
# WHY A SITE ID IS REQUIRED HERE AND NOT FOR A TABLE.
# EMLassemblyline::template_geographic_coverage() takes site.col as a
# required argument and uses it to collapse rows:
#
#   site_name <- unique(unlist(df_table[site.col[j]]))
#   ...
#   latitude_out[i] <- suppressWarnings(as.numeric(latitude[useI][1]))
#
# that is, ONE coordinate pair per distinct site name, taken from that
# site's first row, after stats::complete.cases() has dropped any row
# with a missing value in any of the three columns. A template row
# without a SiteID would therefore be silently discarded by the library
# rather than by us, which is worse, so it is rejected here with a count
# the user can see.
#
# HOW IT REACHES THE METADATA. The library function reads its
# coordinates from a FILE on disk - data.table is validated by
# EMLassemblyline's own validate_file_names() against data.path - so an
# in-memory tibble cannot be handed to it. run_generation()
# (09_generate.R) therefore writes the accepted points to
# GEO_POINTS_CSV_NAME inside the EMLassemblyline working directory and
# names that file as data.table. The file is written ONLY into
# data_package_creation, never into data_package, so it is not part of
# the deliverable, is not described as a data table by make_eml(), and
# is not seen by the congruence checks (which run against the
# data_package directory). It is scratch input for the templating step,
# in the one place that step expects to find its input.
#
# NOT PERSISTED IN A SAVED SESSION, deliberately. Saved state records
# WHICH source was chosen, not the points themselves, matching this
# app's standing decision that imported data content is never written
# into the session JSON - and these are GPS coordinates, which this
# tab's own help text flags as withheld for internal or restricted
# data. On load the user re-uploads the template, exactly as they
# re-upload their CSVs; a notice says so when a saved session expected
# an upload and none is present.
#
# v16: comment only, no code change. Recorded that QCkit is NAMED in the
# UTM help text as advice to the user and is never called by breezeml, so
# it belongs in Suggests rather than Imports in DESCRIPTION.
#
# v15: devtools::document() hygiene. Every roxygen comment (#') that
# sat INSIDE a function body is now a plain comment (#). The text is
# unchanged and still documents the inner closures for a reader; it is
# simply no longer addressed to roxygen2.
#
# Why it had to change: a #' block is only meaningful directly above a
# TOP-LEVEL object. Inside a body there is nothing for roxygen2 to attach
# it to, so it appends the text to the last tag of the preceding
# top-level block - which produced errors such as "@keywords must be only
# 1 line long, not 3" against a perfectly correct @keywords several
# hundred lines earlier. See 02_people.R's header for the full account.
#
# v14: wording only. The UTM help text says "before importing your data"
# instead of "before uploading here". No logic changed.
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

# The three columns of the uploaded geography template, in the order the
# template presents them. ONE definition, used by the upload validator,
# by the preview, and by emit_geo_chunk()/run_generation() when they name
# the columns for EMLassemblyline. Changing the template means changing
# this vector and the shipped .xlsx together.
GEO_TEMPLATE_COLUMNS <- c("SiteID", "decimalLatitude", "decimalLongitude")

# File name run_generation() writes the accepted template points to,
# inside the EMLassemblyline working directory (data_package_creation),
# so that template_geographic_coverage() has a file to read. Defined
# here, beside the columns it holds, and used from 09_generate.R - see
# the "HOW IT REACHES THE METADATA" note in this file's header for why a
# file on disk is unavoidable.
GEO_POINTS_CSV_NAME <- "breezeml_geography_points.csv"

# The template's file name, used in FOUR places that must agree: the
# packaged copy under inst/extdata, the copy written into
# data_package_creation by the "Save template" button, the copy
# run_generation() places there so the generated script can read it, and
# the name the generated script itself opens. ONE definition.
GEO_TEMPLATE_FILE_NAME <- "geography_template.xlsx"

# The valid values of input$coord_source. The radio group starts with
# NOTHING selected (v19), so a fourth state exists - "not chosen yet" -
# represented as NA_character_ by result()$source. Consumers must test
# is.na() before branching; see the v19 header note.
GEO_COORD_SOURCES <- c("table", "upload")

# Locate the packaged geography template in both installed-package and
# devtools::load_all() contexts. Same two-step lookup app_ui.R uses for
# inst/app/www. Returns "" when it cannot be found, which the download
# handler reports rather than writing an empty file.
geo_template_path <- function() {
  packaged <- system.file("extdata", GEO_TEMPLATE_FILE_NAME,
                          package = "breezeml")
  if (nzchar(packaged) && file.exists(packaged)) return(packaged)

  development <- file.path("inst", "extdata", GEO_TEMPLATE_FILE_NAME)
  if (file.exists(development)) {
    return(normalizePath(development, winslash = "/", mustWork = TRUE))
  }

  ""
}

#' Validate an uploaded geography template.
#'
#' Pure function - no Shiny dependencies - so the rules above can be unit
#' tested without a running app.
#'
#' @param df a data.frame read from the uploaded workbook
#' @return list(ok = <logical>, points = <tibble or NULL>,
#'   message = <character>, n_dropped = <integer>)
#'   On ok = TRUE, points has exactly GEO_TEMPLATE_COLUMNS, with
#'   decimalLatitude/decimalLongitude numeric and every row usable.
#' @noRd
validate_geo_template <- function(df) {
  fail <- function(msg) {
    list(ok = FALSE, points = NULL, message = msg, n_dropped = 0L)
  }

  if (is.null(df) || !is.data.frame(df)) {
    return(fail("The file could not be read as a spreadsheet."))
  }

  # Tolerate stray whitespace in the header row, but not a renamed,
  # missing or reordered-away column.
  names(df) <- trimws(as.character(names(df)))
  missing_cols <- setdiff(GEO_TEMPLATE_COLUMNS, names(df))
  if (length(missing_cols) > 0) {
    return(fail(paste0(
      "The template is missing the column(s): ",
      paste(missing_cols, collapse = ", "),
      ". Expected exactly ", paste(GEO_TEMPLATE_COLUMNS, collapse = ", "),
      ". Download a fresh template and fill that out rather than ",
      "renaming columns."
    )))
  }

  df <- df[, GEO_TEMPLATE_COLUMNS, drop = FALSE]
  if (nrow(df) == 0) {
    return(fail("The template contains no rows."))
  }

  site <- trimws(as.character(df[["SiteID"]]))
  lat <- suppressWarnings(as.numeric(df[["decimalLatitude"]]))
  lon <- suppressWarnings(as.numeric(df[["decimalLongitude"]]))

  # A SiteID is required per row - see the header note on why
  # template_geographic_coverage() would otherwise drop the row itself.
  usable <- !is.na(site) & nzchar(site) &
    !is.na(lat) & !is.na(lon) &
    lat >= -90 & lat <= 90 &
    lon >= -180 & lon <= 180

  n_dropped <- sum(!usable)
  if (!any(usable)) {
    return(fail(paste0(
      "None of the ", nrow(df), " row(s) are usable. Every row needs a ",
      "SiteID, a decimalLatitude between -90 and 90, and a ",
      "decimalLongitude between -180 and 180, all in decimal degrees."
    )))
  }

  points <- tibble::tibble(
    SiteID = site[usable],
    decimalLatitude = lat[usable],
    decimalLongitude = lon[usable]
  )

  message <- paste0(
    nrow(points), " coordinate row(s) accepted",
    if (n_dropped > 0) {
      paste0("; ", n_dropped, " row(s) ignored for a missing SiteID or a ",
             "missing/out-of-range coordinate")
    } else {
      ""
    },
    ". ", length(unique(points$SiteID)), " distinct site name(s)."
  )

  list(ok = TRUE, points = points, message = message,
       n_dropped = as.integer(n_dropped))
}

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

      # --- Source choice -----------------------------------------------
      # EVERY conditionalPanel on this tab is a SIBLING at this one level,
      # with a compound condition. See the v18 header note: v17 nested the
      # per-source panels INSIDE the has_coords panel, and the inner
      # panels never appeared. Nesting is not used anywhere else in this
      # app; single-level panels with `ns = ns` are the proven pattern
      # (07_permissions.R, start_screen.R), so this tab now follows it.
      shiny::conditionalPanel(
        condition = "input.has_coords == true",
        ns = ns,
        # selected = character(0) leaves the group with NO button checked
        # (v19). radioButtons() substitutes the first choice only when
        # `selected` is NULL:
        #   selected <- if (is.null(selected)) args$choiceValues[[1]] ...
        # A zero-length character vector is not NULL, so it survives that
        # line, and generateOptions() marks a button checked only when
        # `value %in% selected` - true for nothing here. input$coord_source
        # is therefore character(0) until the user clicks, which `%||%`
        # (length-0 aware) maps to the no-choice state.
        shiny::radioButtons(
          ns("coord_source"),
          "Where do the coordinates come from?",
          choices = c(
            "Columns of a data table I imported" = "table",
            "A geography template I fill out and upload" = "upload"
          ),
          selected = character(0)
        ),
        shiny::helpText(class = "text-muted",
                        "Use the template if your GPS points are not in your ",
                        "data files, or if they describe a subset of your ",
                        "rows - one point per site rather than one per ",
                        "observation.")
      ),

      # --- Option 1: columns of an imported table (behaviour through v16) ---
      shiny::conditionalPanel(
        condition = "input.has_coords == true && input.coord_source == 'table'",
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
        )
      ),

      # --- Option 2: the uploaded geography template -------------------
      shiny::conditionalPanel(
        condition = "input.has_coords == true && input.coord_source == 'upload'",
        ns = ns,
        shiny::helpText("Save the template into this package's ",
                        shiny::HTML("<code>data_package_creation</code>"),
                        " folder, enter one row per site, then upload the ",
                        "filled-out file. Keep the three column names exactly ",
                        "as they are."),
        shiny::actionButton(
          ns("save_template"),
          "Save template to data_package_creation",
          class = "btn-outline-primary btn-sm",
          icon = shiny::icon("download")
        ),
        shiny::uiOutput(ns("template_location")),
        shiny::tags$div(style = "height: 12px;"),
        shiny::fileInput(
          ns("points_file"),
          "Filled-out geography template",
          accept = c(".xlsx",
                     "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"),
          width = "100%",
          buttonLabel = "Browse...",
          placeholder = "*.xlsx"
        ),
        shiny::uiOutput(ns("upload_status"))
      ),

      # --- Shared by both sources --------------------------------------
      # QCkit is NAMED here as advice to the user; breezeml does not call
      # it. It therefore belongs in Suggests, not Imports - putting it in
      # Imports earns the mirror-image warning, "Namespace in Imports
      # field not imported from: 'QCkit'", which is exactly what lubridate
      # was doing.
      # Shared by both sources, but NOT shown until a source is chosen
      # (v19): with nothing selected there is no table and no template, so
      # the map and preview would render empty. The condition therefore
      # names both valid values rather than only has_coords.
      shiny::conditionalPanel(
        condition = paste0("input.has_coords == true && ",
                           "(input.coord_source == 'table' || ",
                           "input.coord_source == 'upload')"),
        ns = ns,
        shiny::helpText("Latitude/longitude must be in decimal degrees (WGS84). ",
                        "If your coordinates are in UTM, convert them first using ",
                        shiny::HTML("<code>QCkit::generate_ll_from_utm()</code>"),
                        " before importing your data."),
        leaflet::leafletOutput(ns("map"), height = 350),
        shiny::br(),
        DT::DTOutput(ns("preview"))
      )
    ),
    col_widths = c(-2, 8, -2), fill = FALSE
  )
}

#' @param tables_reactive reactive() named list of data.frames (from Tab 3)
#' @noRd
#' @return list(data = <reactive() list>, restore = <function>)
#'   data() returns:
#'     $enabled   logical
#'     $source    character - "table" (columns of an imported table) or
#'                "upload" (an uploaded geography template)
#'     $table     character - table name used for coordinates; NA when
#'                $source is "upload"
#'     $lat_col   character
#'     $lon_col   character
#'     $site_col  character
#'     $points    tibble of GEO_TEMPLATE_COLUMNS when $source is
#'                "upload"; NULL otherwise
#'
#'
#' @param creation_dir_reactive reactive() returning the absolute path of
#'   THIS package's data_package_creation folder, or NA_character_/"" when
#'   the metadata filename on Tab 1 is still empty - that one field is all
#'   the path needs (app_server.R v27). Optional: defaults to a reactive
#'   returning NA_character_, so a caller that does not pass it keeps the
#'   pre-v19 behaviour and the "Save template" button reports that it does
#'   not know where to write.
geographyServer <- function(id, tables_reactive,
                            creation_dir_reactive = shiny::reactive(NA_character_)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    numeric_cols <- function(df) names(df)[purrr::map_lgl(df, is.numeric)]

    site_pick <- pickColsServer("site_col", tables_reactive)

    # Accepted template points, and the message describing the last
    # upload attempt. A REJECTED upload sets only the message - points
    # are left exactly as they were, so a bad second file never discards
    # a good first one. See the header note on validation.
    uploaded_points <- shiny::reactiveVal(NULL)
    upload_message <- shiny::reactiveVal(NULL)
    upload_ok <- shiny::reactiveVal(FALSE)

    # Path of the .xlsx the user uploaded, as Shiny staged it. v19:
    # run_generation() copies THIS FILE into data_package_creation so the
    # generated script can read the workbook itself, making the script
    # re-runnable from the folder it is written to. Shiny stages uploads
    # in a per-session temp directory that is removed when the session
    # ends, so it is only valid during this session - which is exactly
    # when Generate runs. Never saved to a session file.
    uploaded_file_path <- shiny::reactiveVal(NULL)

    # Set by restore() when a saved session chose the upload source. The
    # points themselves are never saved (see header), so this only drives
    # the "re-upload needed" notice, and is cleared by a successful upload.
    awaiting_upload <- shiny::reactiveVal(FALSE)

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

    # --- The geography template: download, upload, status (v17) ---

    # Where the template was last written, and whether that succeeded.
    # Drives output$template_location, which tells the user the full path -
    # the point of writing to a known folder instead of the browser's
    # download directory is that the user can be told exactly where it is.
    template_written_path <- shiny::reactiveVal(NULL)
    template_write_error <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$save_template, {
      template_written_path(NULL)
      template_write_error(NULL)

      src <- geo_template_path()
      if (!nzchar(src)) {
        # Fail loudly rather than writing a zero-byte file Excel refuses
        # to open.
        template_write_error(paste0(
          "The packaged geography template could not be found. Expected ",
          "inst/extdata/", GEO_TEMPLATE_FILE_NAME, " in the source tree, or ",
          "extdata/", GEO_TEMPLATE_FILE_NAME, " in the installed package."
        ))
        shiny::showNotification(template_write_error(), type = "error", duration = 12)
        return(invisible(NULL))
      }

      dest_dir <- creation_dir_reactive()
      if (is.null(dest_dir) || length(dest_dir) != 1 || is.na(dest_dir) ||
          !nzchar(dest_dir)) {
        # v19.1: the folder is named after the metadata filename, and
        # nothing else is required, so that is the only field to ask for.
        # Name the tab by its visible label.
        template_write_error(paste0(
          "Enter the metadata filename on tab 1 (High-level info) first - the ",
          "folder is named after it. Then save the template again."
        ))
        shiny::showNotification(template_write_error(), type = "warning", duration = 12)
        return(invisible(NULL))
      }

      # The folder normally exists only after a Generate. Create it now so
      # the template can be saved before the first generate - this is the
      # same path setup_package_dirs() builds, so a later Generate simply
      # finds it already there (dir.create is showWarnings = FALSE there).
      made <- tryCatch({
        if (!dir.exists(dest_dir)) {
          dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
        }
        dir.exists(dest_dir)
      }, error = function(e) e)

      if (inherits(made, "condition") || !isTRUE(made)) {
        template_write_error(paste0(
          "Could not create the folder ", dest_dir,
          if (inherits(made, "condition")) paste0(": ", conditionMessage(made)) else "."
        ))
        shiny::showNotification(template_write_error(), type = "error", duration = 12)
        return(invisible(NULL))
      }

      dest <- file.path(dest_dir, GEO_TEMPLATE_FILE_NAME)

      # Do NOT overwrite a template the user has already filled out. This
      # button is easy to press twice, and the second press must not
      # destroy an afternoon of typing.
      if (file.exists(dest)) {
        template_written_path(dest)
        shiny::showNotification(
          paste0("A geography template is already saved at ", dest,
                 " - it was left unchanged."),
          type = "warning", duration = 12
        )
        return(invisible(NULL))
      }

      copied <- tryCatch(file.copy(src, dest, overwrite = FALSE),
                         error = function(e) e)

      if (inherits(copied, "condition") || !isTRUE(copied)) {
        template_write_error(paste0(
          "Could not write the template to ", dest,
          if (inherits(copied, "condition")) paste0(": ", conditionMessage(copied)) else "."
        ))
        shiny::showNotification(template_write_error(), type = "error", duration = 12)
        return(invisible(NULL))
      }

      template_written_path(dest)
      shiny::showNotification(
        paste0("Geography template saved to ", dest),
        type = "message", duration = 15
      )
    })

    output$template_location <- shiny::renderUI({
      err <- template_write_error()
      if (!is.null(err)) {
        return(shiny::tags$div(class = "alert alert-danger mt-2", err))
      }

      path <- template_written_path()
      if (is.null(path)) {
        dest_dir <- creation_dir_reactive()
        if (is.null(dest_dir) || length(dest_dir) != 1 || is.na(dest_dir) ||
            !nzchar(dest_dir)) {
          return(shiny::tags$div(
            class = "text-muted mt-2",
            "The template will be saved into this package's ",
            shiny::tags$code("data_package_creation"),
            " folder, which is named after the metadata filename on tab 1 ",
            "(High-level info). Enter that filename to see the exact path here."
          ))
        }
        return(shiny::tags$div(
          class = "text-muted mt-2",
          "The template will be saved to: ",
          shiny::tags$code(file.path(dest_dir, GEO_TEMPLATE_FILE_NAME))
        ))
      }

      shiny::tags$div(
        class = "alert alert-success mt-2",
        shiny::tags$div(shiny::tags$strong("Template saved to:")),
        shiny::tags$div(shiny::tags$code(path)),
        shiny::tags$div(
          class = "mt-1",
          "Open it, enter one row per site, save it as .xlsx, then upload it below."
        )
      )
    })

    shiny::observeEvent(input$points_file, {
      shiny::req(input$points_file)
      path <- input$points_file$datapath

      df <- tryCatch(
        as.data.frame(
          readxl::read_excel(path, sheet = 1, .name_repair = "minimal"),
          stringsAsFactors = FALSE
        ),
        error = function(e) e
      )

      if (inherits(df, "condition")) {
        upload_ok(FALSE)
        upload_message(paste0("Could not read the file: ", conditionMessage(df)))
        shiny::showNotification(
          "The geography template could not be read. The previous coordinates, if any, are unchanged.",
          type = "error", duration = 10
        )
        return(invisible(NULL))
      }

      check <- validate_geo_template(df)

      if (!isTRUE(check$ok)) {
        upload_ok(FALSE)
        upload_message(check$message)
        shiny::showNotification(
          paste0("Geography template rejected: ", check$message),
          type = "error", duration = 12
        )
        return(invisible(NULL))
      }

      uploaded_points(check$points)
      uploaded_file_path(path)
      upload_ok(TRUE)
      upload_message(check$message)
      awaiting_upload(FALSE)
      shiny::showNotification(check$message, type = "message", duration = 8)
    })

    output$upload_status <- shiny::renderUI({
      if (isTRUE(awaiting_upload()) && is.null(uploaded_points())) {
        return(shiny::tags$div(
          class = "alert alert-warning mt-2",
          shiny::tags$strong("Re-upload needed to finish restoring your saved session: "),
          "this package used an uploaded geography template. Coordinates are ",
          "not stored in a session file, so please upload the template again."
        ))
      }

      msg <- upload_message()
      if (is.null(msg)) {
        return(shiny::tags$div(
          class = "text-muted",
          "No geography template uploaded yet."
        ))
      }

      shiny::tags$div(
        class = if (isTRUE(upload_ok())) "text-success" else "text-danger",
        msg
      )
    })

    result <- shiny::reactive({
      if (!isTRUE(input$has_coords)) {
        return(list(enabled = FALSE))
      }

      # v19: the radio group starts unselected, so "no source chosen yet"
      # is a real state. It is NOT an error and NOT "table": enabled stays
      # TRUE (the user did tick the has_coords box) but source is NA, and
      # run_generation() refuses to proceed with a clear message rather
      # than silently templating from a data table the user never picked.
      # With no button checked, the radio binding's getValue() returns
      # null ("If none are checked, the input will return null"), so R
      # sees NULL here. length-0 and NA are handled too rather than
      # relying on which %||% is in scope - this module's local one tests
      # is.null() only.
      source <- input$coord_source
      if (is.null(source) || length(source) != 1 || is.na(source) ||
          !source %in% GEO_COORD_SOURCES) {
        source <- NA_character_
      }

      if (is.na(source)) {
        return(list(
          enabled = TRUE,
          source = NA_character_,
          table = NA_character_,
          lat_col = NA_character_,
          lon_col = NA_character_,
          site_col = NA_character_,
          points = NULL,
          upload_path = NULL
        ))
      }

      if (identical(source, "upload")) {
        # Deliberately NOT shiny::req(pts) here. This reactive is read by
        # do_generate() in app_server.R; a req() failure raises a silent
        # exception that would abort the whole Generate with no message at
        # all. $points stays NULL instead, and run_generation()
        # (09_generate.R v9) reports "no accepted coordinate rows" in the
        # usual failure banner. The map and preview below req() the points
        # themselves, so they stay blank until a file is accepted.
        return(list(
          enabled = TRUE,
          source = "upload",
          table = NA_character_,
          lat_col = "decimalLatitude",
          lon_col = "decimalLongitude",
          site_col = "SiteID",
          points = uploaded_points(),
          # The uploaded workbook itself, for run_generation() to copy
          # into data_package_creation (v19). NULL if nothing uploaded.
          upload_path = uploaded_file_path()
        ))
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
        source = "table",
        table = input$lat_table,
        lat_col = input$lat_col,
        lon_col = input$lon_col,
        site_col = if (!is.null(sp)) sp$column else NA_character_,
        points = NULL,
        upload_path = NULL
      )
    })

    # Build the subset of rows usable for mapping: numeric, non-NA, and
    # within valid lat/lon ranges. Returns NULL if the required columns
    # aren't resolvable at all (distinct from "resolvable but zero valid
    # rows", which returns a 0-row tibble) so the map/notification logic
    # can tell "nothing to check yet" apart from "checked, all bad".
    #
    # v17: serves both sources. Uploaded points have already passed
    # validate_geo_template(), so every row is valid by construction and
    # n_invalid is 0 - the range filtering below is a no-op for them, and
    # is kept rather than branched around so there is one definition of
    # "mappable".
    mappable_points <- shiny::reactive({
      r <- result()
      shiny::req(isTRUE(r$enabled))
      # v19: nothing to map until a source is chosen.
      shiny::req(!is.na(r$source))

      if (identical(r$source, "upload")) {
        df <- r$points
        shiny::req(!is.null(df))
        lat <- df$decimalLatitude
        lon <- df$decimalLongitude
        site <- df$SiteID
      } else {
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
      # v19: nothing to preview until a source is chosen.
      shiny::req(!is.na(r$source))

      if (identical(r$source, "upload")) {
        shiny::req(!is.null(r$points))
        return(DT::datatable(r$points,
                             options = list(pageLength = 5, dom = 'tp'),
                             rownames = FALSE))
      }

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

    # Push saved state into this module. Cannot fully complete in one call
    # - lat_table/lon_table/lat_col/lon_col only have valid choices once
    # tables_reactive() contains a table with the saved name (see header).
    # Safe to call repeatedly/speculatively.
    #
    # @param saved list matching default_app_state()$geography's shape, or NULL
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

      # v17/v19. A session saved before v17 has no coord_source at all and
      # must restore as "table" - the only source that existed then. A
      # v19+ session can legitimately have saved NA (has_coords ticked but
      # no source chosen yet); that restores to nothing selected, which is
      # exactly what the user left.
      saved_source <- saved$coord_source
      if (is.null(saved_source) || length(saved_source) != 1 ||
          is.na(saved_source) || !saved_source %in% GEO_COORD_SOURCES) {
        # Either a pre-v17 session (no coord_source field at all) or a
        # v19+ session saved with no source chosen. These need opposite
        # treatment, and the two are told apart by whether the FIELD is
        # absent rather than by its value: a pre-v17 session must restore
        # as "table", the only source that existed when it was saved.
        saved_source <- if (is.null(saved$coord_source)) "table" else NA_character_
      }

      if (is.na(saved_source)) {
        shiny::updateRadioButtons(session, "coord_source", selected = character(0))
        return(invisible(NULL))
      }

      shiny::updateRadioButtons(session, "coord_source", selected = saved_source)

      # The points are not in the session file (see header), so all that
      # can be restored is the fact that an upload is expected. Cleared by
      # a successful upload, and never raised if one has already happened
      # in this session.
      if (identical(saved_source, "upload")) {
        if (is.null(uploaded_points())) awaiting_upload(TRUE)
        # Nothing below applies to an uploaded template - its columns are
        # fixed and it has no table to select - so stop here rather than
        # arming table/column restores that would never resolve.
        return(invisible(NULL))
      }

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

    `%||%` <- function(a, b) if (is.null(a)) b else a

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

  # v19. An uploaded geography template is not one of the imported data
  # tables, so the script must read the WORKBOOK, not a Tab 3 table.
  #
  # The script reads geography_template.xlsx directly. It cannot hand the
  # workbook to EMLassemblyline, because template_arguments() reads a
  # data.table with
  #     data.table::fread(file = f)
  # which parses delimited text only and has no .xlsx path at all. So the
  # script does what the app does: read the sheet with readxl, drop rows
  # without a site and coordinates (template_geographic_coverage() would
  # otherwise silently drop them inside complete.cases()), write a plain
  # CSV beside it, and template from that CSV.
  #
  # run_generation() copies the uploaded workbook into the working folder
  # first, so the file this script opens sits next to the script itself
  # and the whole folder can be moved or re-run as a unit.
  #
  # THREE cases, and NULL must not collapse into NA:
  #   $source ABSENT (NULL)  -> a pre-v17 state list; "table" is what it
  #                             was, so keep emitting the table chunk
  #   $source NA             -> v19+, user has not chosen yet
  #   "table" / "upload"     -> as named
  #
  # Written out rather than with %||% on purpose. emit_geo_chunk() is a
  # top-level function, so it sees the LAST top-level `%||%` in the
  # package (alphabetical collation, no Collate field): import_eml.R's,
  # which treats NA as missing. `state$source %||% "table"` would
  # therefore turn an unchosen source into "table" and emit a chunk
  # naming table "NA" in the Preview Script modal.
  geo_source <- if (is.null(state$source)) {
    "table"
  } else if (length(state$source) != 1 || is.na(state$source)) {
    "unset"
  } else {
    state$source
  }

  if (identical(geo_source, "unset")) {
    return(paste0(
      "# Site coordinates are marked as included, but no coordinate source\n",
      "# has been chosen on the Geography tab yet, so there is nothing to\n",
      "# template from. Choose either a data table's columns or an uploaded\n",
      "# geography template.\n"
    ))
  }

  if (identical(geo_source, "upload")) {
    return(glue::glue(
      '# Site coordinates came from an uploaded geography template, NOT from\n',
      '# a column of an imported data table.\n',
      '#\n',
      '# breezeml copied the workbook you filled out to\n',
      '#   {GEO_TEMPLATE_FILE_NAME}\n',
      '# in the folder below, so this script is self-contained.\n',
      '#\n',
      '# EMLassemblyline reads data tables with data.table::fread(), which\n',
      '# cannot open .xlsx, so the sheet is converted to a plain CSV\n',
      '# ("{GEO_POINTS_CSV_NAME}") first. That CSV is input to the templating\n',
      '# step only: it is not part of the published data package and is not\n',
      '# described as a data table.\n',
      '#\n',
      '# readxl is installed alongside tidyverse but is NOT attached by\n',
      '# library(tidyverse), so it is called with its namespace below.\n',
      'geography_template_file <- file.path({working_folder_var}, "{GEO_TEMPLATE_FILE_NAME}")\n',
      'data_coordinates_table <- "{GEO_POINTS_CSV_NAME}"\n',
      'data_latitude <- "decimalLatitude"\n',
      'data_longitude <- "decimalLongitude"\n',
      'data_sitename <- "SiteID"\n\n',
      'geography_points <- readxl::read_excel(\n',
      '  geography_template_file, sheet = 1, .name_repair = "minimal"\n',
      ')\n',
      'names(geography_points) <- trimws(names(geography_points))\n',
      'geography_points <- geography_points[, c(data_sitename, data_latitude, data_longitude)]\n\n',
      '# Keep only rows with a site name and usable coordinates - the same\n',
      '# rows breezeml accepted when the template was uploaded.\n',
      'geography_points[[data_sitename]] <- trimws(as.character(geography_points[[data_sitename]]))\n',
      'geography_points[[data_latitude]] <- suppressWarnings(as.numeric(geography_points[[data_latitude]]))\n',
      'geography_points[[data_longitude]] <- suppressWarnings(as.numeric(geography_points[[data_longitude]]))\n',
      'geography_points <- geography_points[\n',
      '  !is.na(geography_points[[data_sitename]]) &\n',
      '    nzchar(geography_points[[data_sitename]]) &\n',
      '    !is.na(geography_points[[data_latitude]]) &\n',
      '    !is.na(geography_points[[data_longitude]]) &\n',
      '    geography_points[[data_latitude]] >= -90 &\n',
      '    geography_points[[data_latitude]] <= 90 &\n',
      '    geography_points[[data_longitude]] >= -180 &\n',
      '    geography_points[[data_longitude]] <= 180,\n',
      '  , drop = FALSE\n',
      ']\n',
      'stopifnot(nrow(geography_points) > 0)\n\n',
      'readr::write_csv(\n',
      '  geography_points,\n',
      '  file.path({working_folder_var}, data_coordinates_table),\n',
      '  na = ""\n',
      ')\n\n',
      'EMLassemblyline::template_geographic_coverage(\n',
      '  path = {working_folder_var},\n',
      '  data.path = {working_folder_var},\n',
      '  data.table = data_coordinates_table,\n',
      '  lat.col = data_latitude,\n',
      '  lon.col = data_longitude,\n',
      '  site.col = data_sitename,\n',
      '  write.file = TRUE\n',
      ')\n'
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
