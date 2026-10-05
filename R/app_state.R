# app_state.R v3 -- INTEGRITY MARKER: 9d41be-CHECK-THIS-STRING-APPEARS
#
# If the file you have saved on disk does NOT contain the string
# "9d41be-CHECK-THIS-STRING-APPEARS" anywhere, then whatever you are
# running is NOT this version - stop testing and re-copy this entire
# artifact into R/app_state.R before doing anything else. A quick way to
# confirm from the R console after devtools::load_all():
#   any(grepl("9d41be", readLines("R/app_state.R")))
# should return TRUE.
#
# v3: FIX for a real, CONFIRMED bug - a saved character vector containing
# a MIX of NA and a real string value (e.g. missingValueCode = c(NA, "NA")
# - one attribute with no missing-value code, another whose actual,
# legitimate missing-value code happens to be the literal string "NA")
# round-trips through jsonlite::write_json()/read_json() as a JSON array
# mixing `null` and a string. jsonlite's simplifyVector = TRUE cannot
# represent that mixed-type array as a character vector on the way back
# in - it silently falls back to a LOGICAL vector instead, where EVERY
# element (including the real string "NA") becomes NA. This destroys the
# data irrecoverably before any of this app's own restore()/coercion code
# ever runs - as.character() on an already-collapsed logical NA cannot
# resurrect the original string, because jsonlite discarded it at
# read_json() time, before any R code this app controls gets to see it.
# CONFIRMED via live diagnostic testing (04_fields.R's
# restore_attributes_by_name(), temporary cat() instrumentation): the
# incoming saved_attrs$missingValueCode was ALREADY `c(NA, NA)` of class
# "logical" the moment it arrived - the string "NA" was gone before any
# of this app's code ran.
#
# FIX: na_character_to_empty_string() recursively converts every
# NA_character_ (scalar or within a vector) to "" immediately before
# jsonlite::write_json() is called in save_app_state(). This guarantees
# every element of every character-valued field is consistently
# string-typed in the saved JSON - there is never a `null` mixed with a
# string in the same array, so jsonlite has no ambiguity to resolve
# incorrectly on load. Restore-side code already treats "" and NA as
# equivalent "unset" in the places that matter (e.g. 04_fields.R's
# is_blank()/normalize_missing_value_pair()), so this round-trip change
# is semantically transparent to the rest of the app. Only CHARACTER
# values are affected - logical/integer/numeric NAs (e.g.
# project_id = NA_integer_) are untouched, since same-type NA/value
# arrays don't have this collapse problem.
#
# IMPORTANT: this fix only affects files saved AFTER this version is
# actually running. Any .json file saved before this fix still has the
# lossy null/string mix baked in and will NOT be fixed by loading it with
# this version - you must re-save a fresh .json file to get one that
# benefits from this fix.
#
# v2 (superseded): shared internal state shape used by
# save_app_state()/load_app_state(). Replaced earlier guessed tab shapes
# with shapes confirmed from each tab's real server code (01-08). Tab 3's
# parsed CSV contents ($data, a named list of data.frames) are
# deliberately NOT included here - save/load only persists the $metadata
# tibble (file_name/table_name/description/size_mb/file_loc); actual file
# contents are never serialized into the JSON. On load, the user must
# re-upload the same-named CSVs; restore() logic (03_data_tables.R) will
# re-attach saved table_name/description to matching re-uploaded files by
# file_name, and flag any saved file_names that haven't been re-uploaded
# yet.
#
# schema_version bumps whenever a tab's state shape changes in a way that
# would break loading an older saved file (e.g. Tab 4's fieldsServer()
# return-shape change earlier this project). Old save files can be
# migrated in load_app_state() by checking schema_version and upgrading
# old shapes before merging.

#' Canonical empty/default state - a brand-new session before any tab has
#' been touched. Also the structural reference every loaded file is
#' merged onto (see load_app_state()), so a save file missing a key
#' (older app version, partial save) doesn't crash - it just falls back
#' to this default for whatever's absent.
#' @noRd
default_app_state <- function() {
  list(
    schema_version = 1L,
    saved_at = NA_character_,   # ISO8601 timestamp, set on save

    # Tab 1 - High-level info (01_high_level_info.R: highLevelServer())
    high_level_info = list(
      metadata_id = "",
      package_title = "",
      data_status = "complete",
      abstract = "",
      methods = "",
      additional_notes = "",
      keywords = list(keyword = character(0), keywordThesaurus = character(0)),  # tibble on restore
      start_date = NA_character_,  # ISO8601 date string; parsed to Date on restore
      end_date = NA_character_
    ),

    # Tab 2 - People (02_people.R: peopleServer(), 4x person_category_server())
    # Each category is its own tibble matching empty_person_tbl(with_role)'s
    # columns; contributors additionally has $role. Editors require
    # verified Active Directory matches at ADD time (see 02_people.R) but
    # restored rows are trusted as-is, not re-verified.
    people = list(
      authors = list(),       # list of row-lists; tibble on restore
      contacts = list(),
      contributors = list(),  # includes $role per row
      editors = list()
    ),

    # Tab 3 - Data tables (03_data_tables.R: tableMetadataServer())
    # ONLY $metadata is saved - see v2 note above. $data (parsed CSV
    # contents) is never part of saved state; user re-uploads.
    data_tables = list(
      metadata = list()  # list of row-lists (file_name/table_name/description/size_mb/file_loc); tibble on restore
    ),

    # Tab 4 - Fields (04_fields.R: fieldsServer())
    # list(tables=, valid=, errors=) - tables is a named list keyed by
    # file_name -> list(attributes=, catvars=). Restored by matching
    # attributeName (attributes) / (attributeName, code) (catvars) onto
    # freshly-built default state once a matching table is re-uploaded.
    fields = list(
      tables = list()
    ),

    # Tab 5 - Geography (05_geography.R: geographyServer())
    geography = list(
      has_coords = FALSE,
      lat_table = NA_character_,
      lat_col = NA_character_,
      lon_table = NA_character_,
      lon_col = NA_character_,
      site_table = NA_character_,
      site_col = NA_character_
    ),

    # Tab 6 - Taxonomy (06_taxonomy.R: taxonomyServer())
    # row1 is static/always-present; pairs here represent ALL rows
    # (row1 + any extra rows added via "+Add"), restored in saved order.
    taxonomy = list(
      has_taxa = FALSE,
      pairs = list(table = character(0), column = character(0)),
      authorities = character(0)  # names from TAXA_AUTHORITIES, e.g. c("ITIS","GBIF")
    ),

    # Tab 7 - Permissions (07_permissions.R: permissionsServer())
    permissions = list(
      access_level = "PUBLIC",
      legal_authority_id = NA_integer_,
      contact_email = "",
      authority_designator = "",
      int_rights = "CC0",
      language = "English",
      language_other = ""
    ),

    # Tab 8 - Org context (08_org_context.R: org_contextServer())
    org_context = list(
      content_units = character(0),
      producing_units = character(0),
      project_id = NA_integer_,
      project_title = NA_character_,
      cross_references = integer(0),
      cross_reference_titles = character(0)
    ),

    # Tab 9 - Generate (app_server.R's doi_state reactiveVal) - not tab
    # input, but needed to resume "existing draft" vs "no draft" correctly
    doi_state = NULL  # NULL, or list(reference_id=, doi=)
  )
}

#' Recursively walk a state list/vector and convert every NA_character_
#' (and NA within a character vector) to "" - see v3 note above for full
#' rationale. Only affects character values; logical/integer/numeric NAs
#' pass through unchanged.
#'
#' @param x any R value - typically a list (recurses into every element),
#'   a vector (converts in place), or a scalar
#' @return x with every NA_character_ (including within vectors/lists)
#'   replaced by ""
#' @noRd
na_character_to_empty_string <- function(x) {
  if (is.list(x)) {
    return(lapply(x, na_character_to_empty_string))
  }
  if (is.character(x)) {
    x[is.na(x)] <- ""
    return(x)
  }
  x
}

#' Serialize the live app state (collected from every tab module) to a
#' JSON file and hand it to the browser as a download. Call from a
#' downloadHandler() in app_server.R.
#'
#' @param state a list matching default_app_state()'s shape
#' @param file the file path/connection from downloadHandler()
#' @noRd
save_app_state <- function(state, file) {
  state$saved_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  state <- na_character_to_empty_string(state)
  # auto_unbox: scalars serialize as bare values, not 1-element arrays -
  # matters for clean round-tripping and for readability if a user opens
  # the file directly. na = "null" is now largely moot for character
  # fields (na_character_to_empty_string() already converted those to ""
  # above) but is kept as a safety net for any remaining non-character NA
  # (e.g. NA_integer_ fields like project_id) that genuinely should
  # serialize as JSON null.
  jsonlite::write_json(state, file, auto_unbox = TRUE, na = "null", pretty = TRUE)
}

#' Deserialize a previously-saved JSON file back into the app state shape.
#' Does NOT push values into tab modules itself - app_server.R calls each
#' tab module's restore(saved_state$<tab>) after this.
#'
#' @param file path to a .json file produced by save_app_state()
#' @return list matching default_app_state()'s shape
#' @noRd
load_app_state <- function(file) {
  raw <- jsonlite::read_json(file, simplifyVector = TRUE)

  if (is.null(raw$schema_version) || raw$schema_version < 1L) {
    stop("Unrecognized or missing schema_version - cannot load this file.")
  }

  # Future: if raw$schema_version < CURRENT_SCHEMA_VERSION, apply
  # migration steps here before merging into default_app_state().

  utils::modifyList(default_app_state(), raw, keep.null = TRUE)
}
