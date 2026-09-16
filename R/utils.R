# utils.R v4
#
# Shared, cross-module helpers. Pure base R - no shiny/bslib dependency,
# so no package::function qualification needed here.

# R CMD check's static analysis cannot distinguish dplyr's non-standard
# evaluation (NSE) column references - e.g. dplyr::select(file_name = name)
# inside tableMetadataServer() (03_data_tables.R), where `name`, `size`,
# and `datapath` are column names in the data frame being operated on,
# not undefined global variables - from genuine missing/undefined
# globals. This is a well-known, standard tidyverse false positive, and
# utils::globalVariables() is the conventional fix: it tells R CMD check
# "these symbols are known and intentional NSE references, not bugs."
utils::globalVariables(c(
  "name", "size", "datapath", "file_name", "table_name",
  "description", "size_mb", "file_loc"
))

# Single source of truth for whether DataStore-touching calls
# (EMLeditor::set_datastore_doi(), NPSdatastore::delete_inactive_ref(),
# and any other NPSdataverse function taking a `dev` argument) hit the
# dev/staging environment or production. Controlled by the
# BREEZEML_DATASTORE_DEV environment variable so behavior can differ per
# deployment without a code change - same pattern as other credentials.
#
# DEFAULTS TO TRUE (dev/staging) if the variable is unset or unrecognized.
# This is intentional while the app is still under active development -
# it prevents a forgotten/misconfigured environment variable from
# silently hitting production DataStore. BEFORE SHIPPING/PRODUCTION USE,
# change the default below to FALSE (or, better, require the variable to
# be explicitly set with no silent default at all) so a forgotten
# variable fails loudly rather than defaulting to either environment.
is_datastore_dev <- function() {
  val <- Sys.getenv("BREEZEML_DATASTORE_DEV", unset = "TRUE")
  toupper(trimws(val)) == "TRUE"
}
