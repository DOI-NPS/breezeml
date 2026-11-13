# run_breezeml.R v2
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

#' Launch the bReezEML Shiny application
#'
#' bReezEML provides a graphical interface for creating NPS-compliant EML
#' (Ecological Metadata Language) metadata for DataStore data packages,
#' built on the NPSdataverse R packages (EMLassemblyline, EMLeditor,
#' DPchecker, NPSdatastore).
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
  shiny::shinyApp(ui = app_ui(), server = app_server, ...)
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
