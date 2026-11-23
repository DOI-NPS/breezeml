# run_breezeml.R v3
#
# v3: RAISED THE FILE UPLOAD SIZE LIMIT.
#
# Users reported a file-size limitation when importing files. The cause is
# Shiny's default web request size limit, which breezeml never changed.
# Shiny's own documentation for shiny.maxRequestSize states:
#
#   shiny.maxRequestSize (defaults to 5MB)
#       This is a number which specifies the maximum web request size,
#       which serves as a size limit for file uploads.
#
# and Shiny applies that default in two places when the option is unset
# (both from Shiny's source, shiny.R and middleware.R):
#
#   maxSize <- getOption('shiny.maxRequestSize', 5 * 1024 * 1024)
#   ...
#   if (maxSize > 0 && any(sizes > maxSize)) stop("Maximum upload size exceeded")
#
#   maxSize <- getOption('shiny.maxRequestSize') %||% (5 * 1024 * 1024)
#   ...
#   return(list(status = 413L, ..., body = 'Maximum upload size exceeded'))
#
# The limit is PER FILE, not per upload batch: the check is
# any(sizes > maxSize) over the files in one selection, so ten 4 MB .csv
# files are fine and one 6 MB .csv is refused.
#
# Because breezeml set no option at all, the effective limit was 5 MB for
# every file chosen through a file box: the .csv file box on Tab 3
# (03_data_tables.R), and the session .json and metadata .xml boxes on the
# start screens (app_server.R). It is set here, once, at the application
# entry point, which is the only place that governs all three.
#
# WHY 2 GB, AND WHY NOT UNLIMITED. Shiny treats maxSize <= 0 as "no limit"
# (`if (maxSize <= 0) return(NULL)` in middleware.R, and `maxSize > 0 &&`
# in shiny.R), so a limit could be removed entirely. A finite, very large
# limit is used instead, deliberately:
#
#   - It is a reporting boundary, not a restriction. A .csv larger than
#     this is far more likely to be a mistaken selection than a real data
#     table, and a clear refusal is better for the user than an upload
#     that appears to hang while R reads it.
#   - It stays inside R's own integer limits when the value reaches
#     httpuv as a request size, and does not depend on 0 being treated as
#     a sentinel by every Shiny version.
#
# 2 GB is roughly 400 times the previous limit. Anyone who needs it
# changed again edits ONE constant below.
#
# WHAT THIS DOES NOT CHANGE, and what users should know:
#
#   - IMPORTING A FOLDER IS NOT, AND NEVER WAS, LIMITED. The "Revise an
#     existing data package" and congruence-check paths read .csv files
#     straight from a selected folder (tables$add_files() and
#     tables$replace_files() in 03_data_tables.R, called from
#     app_server.R). Those files never travel through a web request, so
#     shiny.maxRequestSize never applied to them. A user blocked by the
#     5 MB limit today can already work around it by putting the .csv
#     files in a folder and using that route.
#   - EXPORTS ARE NOT LIMITED BY THIS OPTION. shiny.maxRequestSize is a
#     limit on requests INTO the app. The generated *_metadata.xml, the
#     generation_script.R, the abstract/methods/additional_info .txt
#     files (writeLines() in 09_generate.R) and the .csv copies
#     (file.copy()) are written directly to the working folder on disk by
#     R, with no web request involved. The session .json is produced by
#     shiny::downloadHandler() (app_server.R), which is a response, not a
#     request, and is likewise unaffected.
#   - UPLOADING TO DATASTORE IS A SEPARATE SYSTEM. Tab 11 calls
#     EMLeditor::upload_data_package(); any size limit there belongs to
#     DataStore and its API, is outside breezeml, and is not changed or
#     known from this code.
#
#   - R MUST STILL HOLD THE PARSED DATA. Every imported .csv is read with
#     readr::read_csv() and kept in memory for the session
#     (parsed_data()), so a very large table is limited by the computer's
#     RAM well before it is limited by this option. Raising the limit
#     removes an artificial barrier; it does not make a 2 GB .csv
#     practical.
#
# v2: corrected the EMLeditor install instruction in check_dependencies().
# It previously told users to install the cui_2026 branch of EMLeditor,
# while DESCRIPTION (Remotes:) and README.md pin DOI-NPS/EMLeditor@breezeml_dev.
# The message now matches DESCRIPTION. The branch name lives in ONE place
# (EMLEDITOR_INSTALL_REF below) so the planned move to EMLeditor main is a
# small, obvious edit.
#
# Known limitation (unchanged from v1, now documented): EMLeditor main and
# breezeml_dev both currently report Version 1.3.0, and set_permissions()
# exists on both. check_dependencies() therefore confirms only that
# set_permissions() is present; it cannot tell which branch is installed.
#
# PLANNED CHANGE when EMLeditor v1.3.1 is released on main (after code
# review):
#   1. DESCRIPTION Imports:  EMLeditor (>= 1.3.0)  ->  EMLeditor (>= 1.3.1)
#   2. DESCRIPTION Remotes:  DOI-NPS/EMLeditor@breezeml_dev  ->  DOI-NPS/EMLeditor
#   3. README.md: replace the EMLeditor@breezeml_dev install line with the
#      main-branch install.
#   4. Here: set EMLEDITOR_INSTALL_REF <- "main" and replace the
#      set_permissions() existence test with
#      utils::packageVersion("EMLeditor") >= EMLEDITOR_MIN_VERSION, with
#      EMLEDITOR_MIN_VERSION <- "1.3.1". A version test becomes reliable
#      at that point because 1.3.1 will be distinguishable from 1.3.0.
#
# v1: package entry point. Converted from the top-level script version of
# app.R as part of packaging breezeml as a proper R package - see prior
# conversation history for the full development history of the app itself.
#
# Everything that existed only to work around non-package script sourcing
# (explicit source() calls in dependency order, options(shiny.autoload.r
# = FALSE) to disable Shiny's alphabetical R/ auto-sourcing) is GONE as of
# this conversion - R's normal package loading mechanism (library(breezeml)
# or devtools::load_all()) resolves all function definitions correctly,
# once, regardless of which file they're defined in, which structurally
# eliminates the whole class of stale-file/load-order bugs that came up
# repeatedly while this was a loose collection of sourced scripts.

# The EMLeditor GitHub branch breezeml currently requires. Must match the
# EMLeditor entry in DESCRIPTION's Remotes: field.
EMLEDITOR_INSTALL_REF <- "breezeml_dev"

# The largest single file, in bytes, that may be chosen in any of
# breezeml's file boxes: the .csv box on Tab 3 and the session .json and
# metadata .xml boxes on the start screens. 2 GB, against Shiny's own
# default of 5 MB. See this file's header for why the limit is finite.
#
# To change it, edit this ONE number. Setting it to 0 or a negative value
# makes Shiny apply no limit at all, which is supported but not
# recommended - see the header.
BREEZEML_MAX_UPLOAD_BYTES <- 2 * 1024^3

#' Launch the bReezEML Shiny application
#'
#' bReezEML provides a graphical interface for creating NPS-compliant EML
#' (Ecological Metadata Language) metadata for DataStore data packages,
#' built on the NPSdataverse R packages (EMLassemblyline, EMLeditor,
#' DPchecker, NPSdatastore).
#'
#' Raises Shiny's per-file upload limit from its 5 MB default to
#' \code{BREEZEML_MAX_UPLOAD_BYTES} for the life of the app, and restores
#' the session's previous setting when the app stops.
#'
#' @param ... additional arguments passed to \code{shiny::shinyApp()}
#'   (e.g. \code{options = list(port = 1234)})
#'
#' @export
#'
#' @examples
#' \dontrun{
#' run_breezeml()
#' }
run_breezeml <- function(...) {
  check_dependencies()
  shiny::shinyApp(
    ui = app_ui(),
    server = app_server,
    onStart = set_max_upload_size,
    ...
  )
}

#' Raise Shiny's per-file upload size limit, and arrange to put the
#' session's previous setting back when the app stops.
#'
#' Called through \code{shinyApp(onStart = )}, which Shiny invokes once
#' before the app starts serving. The option is set here rather than in
#' \code{run_breezeml()} so that it is in place for the app's lifetime and
#' is cleaned up with it, instead of being left changed in the user's R
#' session afterwards.
#'
#' \code{shiny::onStop()} registers the restore callback; Shiny invokes
#' registered callbacks when the app stops, including when it stops
#' because of an error during startup.
#'
#' @param bytes largest permitted size of a single uploaded file, in bytes
#'
#' @keywords internal
set_max_upload_size <- function(bytes = BREEZEML_MAX_UPLOAD_BYTES) {
  previous <- getOption("shiny.maxRequestSize")
  options(shiny.maxRequestSize = bytes)

  # Put the previous value back when the app stops. A session that had no
  # setting of its own gets NULL, which is how an unset option is cleared,
  # so Shiny's own 5 MB default applies again afterwards.
  shiny::onStop(function() {
    options(shiny.maxRequestSize = previous)
  })

  invisible(bytes)
}

#' Fail fast and loud if a required dependency isn't installed correctly,
#' rather than deep inside a Generate click. Currently checks that
#' EMLeditor is installed and provides EMLeditor::set_permissions().
#'
#' breezeml currently requires the breezeml_dev branch of EMLeditor (see
#' EMLEDITOR_INSTALL_REF and DESCRIPTION's Remotes: field). This check
#' cannot distinguish breezeml_dev from main, because both report version
#' 1.3.0 and both define set_permissions(). When EMLeditor v1.3.1 is
#' released on main, replace this test with a minimum-version test - see
#' the "PLANNED CHANGE" note in this file's header.
#'
#' @keywords internal
check_dependencies <- function() {
  if (!requireNamespace("EMLeditor", quietly = TRUE) ||
      !exists("set_permissions", where = asNamespace("EMLeditor"), inherits = FALSE)) {
    stop(
      "EMLeditor::set_permissions() was not found. breezeml requires the ",
      EMLEDITOR_INSTALL_REF, " branch of EMLeditor. Install it with:\n\n",
      '  remotes::install_github("DOI-NPS/EMLeditor", ref = "',
      EMLEDITOR_INSTALL_REF, '")\n\n',
      "Then restart your R session before launching breezeml.",
      call. = FALSE
    )
  }
}
