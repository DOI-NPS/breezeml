# utils.R v5
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
# (EMLeditor::set_datastore_doi(), EMLeditor::set_doi(),
# NPSdatastore::delete_inactive_ref(), and any other NPSdataverse function
# taking a `dev` argument) hit the dev/staging environment or production.
# Controlled by the BREEZEML_DATASTORE_DEV environment variable so
# behavior can differ per deployment without a code change - same pattern
# as other credentials.
#
# DEFAULTS TO FALSE (production) as of the first beta release - beta
# testers are expected to create real, reviewable draft references on
# production DataStore, not throwaway dev-environment entries. This is a
# deliberate change from earlier development, when the default was TRUE
# (dev) specifically to prevent a forgotten/misconfigured environment
# variable from silently hitting production while the app was still
# under active, exploratory development.
#
# To use the dev/staging environment instead (e.g. for future development
# work on this app itself, not for beta testing), explicitly set
# BREEZEML_DATASTORE_DEV=TRUE.
is_datastore_dev <- function() {
  val <- Sys.getenv("BREEZEML_DATASTORE_DEV", unset = "FALSE")
  toupper(trimws(val)) == "TRUE"
}
