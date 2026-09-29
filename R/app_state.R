# app_state.R (new file) v2
#
# Shared internal state shape used by save_app_state()/load_app_state().
# v2 replaces earlier guessed tab shapes with the ACTUAL shapes confirmed
# from each tab's real server code (01-08). Tab 3's parsed CSV contents
# ($data, a named list of data.frames) are deliberately NOT included here -
# per explicit decision, save/load only persists the $metadata tibble
# (file_name/table_name/description/size_mb/file_loc); actual file
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
    # columns; contributors additionally has $role.
    people = list(
      authors = list(),       # list of row-lists; tibble on restore
      contacts = list(),
      contributors = list(),  # includes $role per row
      editors = list()
    ),

    # Tab 3 - Data tables (03_data_tables.R: tableMetadataServer())
    # ONLY $metadata is saved - see file header note above. $data (parsed
    # CSV contents) is never part of saved state; user re-uploads.
    data_tables = list(
      metadata = list()  # list of row-lists (file_name/table_name/description/size_mb/file_loc); tibble on restore
    ),

    # Tab 4 - Fields (04_fields.R: fieldsServer())
    # CURRENT shape: list(tables=, valid=, errors=) - tables is a named
    # list keyed by file_name -> list(attributes=, catvars=). Exact
    # per-table internal shape not yet confirmed against 04_fields.R
    # source - restore() there should be checked carefully once written.
    fields = list(
      tables = list()
    ),

    # Tab 5 - Geography (05_geography.R: geographyServer())
    # Plain Shiny inputs - restorable via update*Input() calls.
    geography = list(
      has_coords = FALSE,
      lat_table = NA_character_,
      lat_col = NA_character_,
      lon_table = NA_character_,
      lon_col = NA_character_,
      site_table = NA_character_,  # from pickColsServer("site_col", ...)
      site_col = NA_character_
    ),

    # Tab 6 - Taxonomy (06_taxonomy.R: taxonomyServer())
    # has_taxa/authorities are plain inputs. pairs is a DYNAMIC list (one
    # row per row_ids() entry, via pickColsServer per row) - restore()
    # must first recreate the correct number of rows (row_ids()) before
    # the per-row table/column pickers can be seeded.
    taxonomy = list(
      has_taxa = FALSE,
      pairs = list(),  # list of list(table=, column=)
      authorities = character(0)  # names from TAXA_AUTHORITIES, e.g. c("ITIS","GBIF")
    ),

    # Tab 7 - Permissions (07_permissions.R: permissionsServer())
    # All plain Shiny inputs - restorable via update*Input() calls.
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
      cross_references = integer(0),
      cross_reference_titles = character(0)  # xref_titles(), needed to redisplay without re-querying on load
    ),

    # Tab 9 - Generate (app_server.R's doi_state reactiveVal) - not tab
    # input, but needed to resume "existing draft" vs "no draft" correctly
    doi_state = NULL  # NULL, or list(reference_id=, doi=)
  )
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
  # auto_unbox: scalars serialize as bare values, not 1-element arrays -
  # matters for clean round-tripping and for readability if a user opens
  # the file directly.
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
