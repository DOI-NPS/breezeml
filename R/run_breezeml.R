# run_breezeml.R v1
#
# Package entry point. Converted from the top-level script version of
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
#' rather than deep inside a Generate click. Currently checks specifically
#' for EMLeditor::set_permissions(), which as of this writing lives only
#' on EMLeditor's cui_2026 development branch, not yet merged to main.
#'
#' Once cui_2026 merges to EMLeditor main, this check (and the install
#' instruction below) can be removed - a normal EMLeditor/NPSdataverse
#' install will be sufficient, and the Remotes: entry in DESCRIPTION
#' pinning EMLeditor to cui_2026 should be removed at the same time.
#'
#' @keywords internal
check_dependencies <- function() {
  if (!requireNamespace("EMLeditor", quietly = TRUE) ||
      !exists("set_permissions", where = asNamespace("EMLeditor"), inherits = FALSE)) {
    stop(
      "EMLeditor::set_permissions() was not found. breezeml requires the ",
      "cui_2026 development branch of EMLeditor, which has not yet been ",
      "merged to main. Install it with:\n\n",
      '  remotes::install_github("DOI-NPS/EMLeditor", ref = "cui_2026")\n\n',
      "Once cui_2026 merges to EMLeditor main, this check (and this ",
      "install instruction) can be removed.",
      call. = FALSE
    )
  }
}
