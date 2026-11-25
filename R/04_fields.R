# Check-to-revise update: restore(rebuild = TRUE) rebuilds field defaults
# even for same-name tables, then overlays the checked package metadata.
# 04_fields.R v28
#
# v28: CUSTOM UNITS. A collapsed "Custom units (optional)" panel at the top
# of this tab lets the user define units that are not in the EML standard
# unit dictionary, one at a time, with the five fields
# EMLassemblyline's custom_units.txt template uses:
#
#   id, unitType, parentSI, multiplierToSI, description
#
# What a custom unit does:
#   - its `id` becomes an accepted value in the Unit column of every
#     numeric attribute (the cell-edit check below used to accept only
#     EML dictionary ids);
#   - the units are returned from fieldsServer()'s data() as $custom_units,
#     and 09_generate.R writes them to custom_units.txt in
#     data_package_creation, where make_eml() finds them and writes the
#     <additionalMetadata><unitList> block;
#   - emit_custom_units_chunk() puts the same table in generation_script.R;
#   - they are saved in, and restored from, the session .json file.
#
# Only Unit id, Unit type and Description are required. Parent SI unit and
# Multiplier to SI may be left blank. All of unitType and parentSI are free
# text: they are deliberately NOT checked against the EML unit dictionary,
# because a custom unit exists when the dictionary has no equivalent for it.
# A multiplier that IS given must be a plain decimal number (the EML schema
# type is xsd:decimal, which has no exponent notation).
#
# A custom unit that is still used by an attribute cannot be removed; the
# user is told which table and column to change first. Nothing about the
# attributes grid, its redraw tokens or its keyboard navigation changed.
#
# v27: fixed the page jumping back to the top after every cell edit, and
# added keyboard navigation between cells.
#
# THE SCROLL JUMP. The attributes grid's render read the state directly:
#
#   output[[attrs_id]] <- DT::renderDT({
#     shiny::req(state[[table_name]])
#     DT::datatable(state[[table_name]]$attributes, ...
#
# and the cell-edit observer wrote the same element:
#
#   state[[table_name]]$attributes <- updated
#
# In R that assignment reads and rewrites the WHOLE list element, so it
# invalidated every dependent of state[[table_name]] - which included
# BOTH grids for that table, because the categorical-codes render reads
# state[[table_name]]$catvars. One edit in the attributes grid therefore
# rebuilt the attributes grid AND the codes grid underneath it. With
# pageLength = -1 (every row, no paging) on a wide table that is a large
# DOM rebuild per cell: the page briefly got shorter, the browser
# clamped its scroll position, and the user was returned to the top with
# no open editor - the reported symptom, and worse here than on Tab 3.
#
# Each render now takes its reactive dependency on a per-grid token in
# `shape` and reads the state with shiny::isolate(). A token is bumped
# only when that grid's CONTENT has to be redrawn from R:
#
#   attrs_<id>    defaults built or rebuilt; saved state restored onto
#                 them; a cell edit REVERTED by validation (an invalid
#                 class, or a unit that is not in the EML dictionary),
#                 where the browser is showing a value R has rejected
#                 and must be shown the corrected one
#   catvars_<id>  defaults built or rebuilt; saved definitions restored;
#                 codes rebuilt because a column's class changed
#
# An ACCEPTED edit bumps nothing and so rebuilds nothing.
#
# This is why both renders now pass server = FALSE (matching Tab 3,
# which already did). It is required, not cosmetic. In server mode DT
# serves rows from data registered at render time, so with the render no
# longer firing the grid would show stale values; DT's own answer,
# replaceData()/reloadData(), is documented as server-mode only and
# would arrive a round trip later, destroying the editor the user had
# just moved into. In client mode the browser holds the rows, DT's blur
# handler has already written the committed value into the cell, and its
# table.draw(false) re-runs the rowCallback locally - so the amber
# required-cell highlighting still updates live as cells are filled, and
# still re-highlights if one is cleared (the v23 behaviour, unchanged).
# These grids hold one row per COLUMN of a .csv, never the .csv data, so
# client mode carries no size concern.
#
# `state` remains the single source of truth for every other consumer
# (data(), validate_table_fields(), the generation pipeline). Only the
# two renders stopped depending on it.
#
# KEYBOARD NAVIGATION. Enter moves down a column, Tab moves across a
# row, Shift reverses either, and read-only columns are skipped - see
# dt_keyboard_nav.R, which explains why DT does not do this on its own
# and how the keys are wired. The attributes grid already had a callback
# of its own (the <datalist> on the class column), so the navigation is
# appended to it with dt_keyboard_nav_body(); the codes grid had none
# and uses dt_keyboard_nav_callback(). Both are passed the SAME column
# indices as editable$disable, from the two constants
# ATTRS_DISABLED_COLUMNS and CATVARS_DISABLED_COLUMNS below, so
# navigation and editability cannot disagree.
#
# Nothing else changed: no UI layout beyond the help text describing the
# keys, no validation rules, no restore logic, and no change to the
# module's return value.
#
# v26: devtools::document() hygiene. Every roxygen comment (#') that
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
# v25: wording only. "Re-upload needed" -> "Re-load needed" and
# "re-uploaded file" -> "re-imported file" in two notices. No logic
# changed.
#
# v24: RESOLVED - the missingValueCode == "NA" string bug tracked since
# v22 is CLOSED. app_state.R v3's na_character_to_empty_string() fix
# (converting NA_character_ to "" before JSON serialization, eliminating
# the mixed null/string array that caused jsonlite::read_json() to
# silently collapse the column to all-NA logical) resolved it. Confirmed
# via live re-test: a "habitat" attribute with missingValueCode = "NA"
# now saves and restores correctly, matching
# missingValueCodeExplanation's behavior on the same row. The v22
# diagnostic cat() calls in restore_attributes_by_name() have been
# removed - they are no longer needed and were never where the bug
# actually was (trace output confirmed the string "NA" was intact at
# every stage of that function; the loss happened upstream, in
# app_state.R's save-side JSON serialization, before this function ever
# ran).
#
# v23: added dynamic cell highlighting (amber background) to flag cells
# that still require user input, in BOTH the attributes and categorical-
# codes DT tables. Implemented with a DT rowCallback (JS) in each table's
# `options` - purely VALUE-DRIVEN, so a cell highlights whenever it is
# blank-and-required and clears the instant it's filled, and RE-highlights
# if the user later clears it again (per explicit request). No new
# reactive/observer is needed: these tables already re-render whenever
# `state` changes (each renderDT reads the reactiveValues `state`), so the
# rowCallback re-runs on every draw and always reflects current values.
#
# Highlight rules - deliberately identical to validate_table_fields(), so
# the colour and the Generate-gate can never disagree:
#   attributes table:
#     - attributeDefinition : highlight while blank (ALWAYS required)
#     - unit                : highlight while blank ONLY when class == "numeric"
#     - dateTimeFormatString: highlight while blank ONLY when class == "Date"
#   categorical codes table:
#     - definition          : highlight while blank (ALWAYS required)
#
# "Blank" = JS null / undefined / empty-or-whitespace-only. The literal
# string "NA" is NOT treated as blank (so a real, if unusual, definition
# of "NA" is not wrongly flagged). Note NA_character_ values (e.g. an
# unset unit or dateTimeFormatString) serialize to JSON null on the way to
# DT, which IS treated as blank - exactly what's wanted for an unset cell.
#
# Column indices used by the rowCallbacks are 0-based and assume
# rownames = FALSE (which both tables use). Attributes order:
#   0 attributeName, 1 attributeDefinition, 2 class, 3 unit,
#   4 dateTimeFormatString, 5 missingValueCode, 6 missingValueCodeExplanation
# Catvars order: 0 attributeName, 1 code, 2 definition
# If column order ever changes, update the rowCallback indices to match.
#
# v21 (prior fix, confirmed working for its own bug): unname() added to
# every column assignment in restore_attributes_by_name()/
# restore_catvars_by_key() - CONFIRMED FIXED a different bug where
# saved_row$class (pulled from a single-row tibble slice) carried an
# implicit name that silently turned the WHOLE class column into a named
# character vector after restore, which corrupted readr::write_tsv()'s
# output (a malformed/short last row) for any table that had been
# restored. That fix is CONFIRMED correct and unrelated to the
# (now-resolved) missingValueCode "NA" string issue noted above.
#
# FIXED (still true, kept from v20): fieldsServer()'s reactive never
# computed or returned $valid/$errors at all - every other tab's module
# follows the pattern list(..., valid = length(errors) == 0, errors =
# errors), but this one simply returned reactiveValuesToList(state) with
# nothing else. Combined with app_server.R's all_errors() never even
# including fields() in its aggregation, this meant Tab 4 could NEVER
# block Generate, regardless of content - confirmed via live testing: a
# numeric column with no unit (required by EMLassemblyline/EML schema)
# was silently accepted, the Generate tab reported "All required
# information is complete," and the resulting EML was schema-invalid AND
# silently dropped that entire data table from <dataTable> in the output.
#
# NEW validation, computed per table and aggregated into a single
# $errors vector, checked before Generate is allowed to run:
#   - attributeDefinition non-blank for every attribute
#   - unit non-blank for every attribute with class == "numeric"
#   - dateTimeFormatString non-blank for every attribute with class == "Date"
#   - definition non-blank for every row in catvars (categorical codes)
#
# Errors are specific (table + column named) rather than a generic
# "Tab 4 incomplete" message, so the user can find and fix the exact cell.
#
# Corresponds to skeleton.Rmd FUNCTION 2 (template_table_attributes) and
# FUNCTION 3 (template_categorical_variables). This is the largest module:
# for every uploaded data table, for every column, the user must specify
# attributeDefinition, class, unit (if numeric), dateTimeFormatString (if
# Date), and missing value codes - plus, for categorical columns, a
# definition for every unique code.
#
# UI shape: one sub-tab per data table (via nav_panel, built dynamically),
# each containing an editable attributes table plus, for columns flagged
# categorical, an editable code/definition table underneath.
#
# Categorical auto-suggestion: character columns with <= CATEGORICAL_MAX_LEVELS
# unique values are pre-flagged as "categorical" but the user can override.
#
# Unit choices are sourced from EML::get_unitList()$units$id - NOT
# EMLassemblyline::view_unit_dictionary(), which is only a wrapper that
# opens an RStudio View() pane for interactive browsing and returns
# nothing programmatically usable.
#
# restore() (added for Save/Load): matches saved per-table attributes/
# catvars onto freshly-built default state by key (attributeName for
# attributes; (attributeName, code) for catvars) once a matching table is
# re-uploaded - see restore_attributes_by_name()/restore_catvars_by_key()
# and fieldsServer()'s observeEvent(tables_reactive(), ...) block for the
# full matching/gating logic. New columns not in saved state keep fresh
# defaults; saved columns no longer in the re-uploaded file are reported
# (not silently dropped) via restore_notice; new/removed categorical
# codes are handled the same way at the (attributeName, code) level.

CATEGORICAL_MAX_LEVELS <- 20

# Valid values for the "class" column - now a fixed factor level set, not
# free text. Defined at top level (not inside fieldsServer()) since
# build_attributes_tibble() needs it too, and is called before any server
# function runs (from fieldsServer()'s own observeEvent, but conceptually
# a pure/standalone builder).
CLASS_CHOICES <- c("numeric", "character", "categorical", "Date")

# Background colour used to highlight a blank-but-required cell (see v23
# note). Light amber, matching Bootstrap's alert-warning background used
# elsewhere in the app so the visual language stays consistent.
REQUIRED_CELL_HIGHLIGHT <- "#fff3cd"

# Read-only columns of the two grids, 0-based, with rownames = FALSE.
# ONE definition each, used both by editable$disable and by the keyboard
# navigation, so a cell can never be navigable but not editable (or the
# reverse). Column orders are set by build_attributes_tibble() and
# build_catvars_tibble():
#   attributes: 0 attributeName, 1 attributeDefinition, 2 class, 3 unit,
#               4 dateTimeFormatString, 5 missingValueCode,
#               6 missingValueCodeExplanation
#   catvars:    0 attributeName, 1 code, 2 definition
# If either column order changes, update these AND the rowCallback
# indices documented in the v23 note above.
ATTRS_DISABLED_COLUMNS <- 0
CATVARS_DISABLED_COLUMNS <- c(0, 1)

# EMLassemblyline strips the file extension before naming its attribute/
# categorical template files - e.g. "BICA_Herps.csv" -> "attributes_BICA_Herps.txt",
# NOT "attributes_BICA_Herps.csv.txt". Getting this wrong means make_eml()
# silently can't find the attributes file at all and drops that table's
# attribute metadata entirely - it looks like malformed content but is
# actually a missing-file problem.
strip_extension <- function(file_name) {
  sub("\\.[^.]*$", "", file_name)
}

# The canonical, programmatically-usable EML unit dictionary lives in the
# EML package, not EMLassemblyline. EMLassemblyline::view_unit_dictionary()
# is just a wrapper that opens an RStudio View() pane on this same data for
# interactive browsing - it does not return a usable value, which is why an
# earlier version of this function silently produced an empty unit list.
#
# Returns the FULL units tibble (id, name, unitType, description, etc.) -
# not just the id vector - so both validation (which only needs id) and
# the unit lookup/reference modal (which needs name/unitType/description
# for a human-readable search) can share one fetch instead of hitting the
# API twice.
get_unit_table <- function() {
  units <- tryCatch({
    EML::get_unitList()$units
  }, error = function(e) NULL)

  if (is.null(units) || nrow(units) == 0) {
    shiny::showNotification(
      paste0("Could not load the full EML unit dictionary from EML::get_unitList() - ",
             "falling back to a short list of common units. Some valid units may be rejected, ",
             "and the unit lookup reference will be limited."),
      type = "warning", duration = 10
    )
    # small, genuinely-valid fallback list, matching EML::get_unitList()'s
    # actual `id` spellings (not invented names)
    return(tibble::tibble(
      id = c("meter", "centimeter", "kilometer", "gram", "kilogram",
             "hectare", "squareMeter", "celsius", "percent",
             "number", "dimensionless"),
      name = NA_character_, unitType = NA_character_, description = NA_character_
    ))
  }

  tibble::as_tibble(units) |> dplyr::arrange(id)
}

# Just the valid `id` strings, for validation - kept as a thin wrapper
# around get_unit_table() so existing callers don't need to change.
get_unit_choices <- function() {
  sort(unique(get_unit_table()$id))
}

# ---- Custom units (v28) ---------------------------------------------------
# The five columns of EMLassemblyline's custom_units.txt template, in the
# order it uses. EML::set_unitList() fills in the optional name,
# abbreviation and constantToSI columns itself.
empty_custom_units_tbl <- function() {
  tibble::tibble(
    id = character(),
    unitType = character(),
    parentSI = character(),
    multiplierToSI = character(),
    description = character()
  )
}

# One-row tibble from the raw form inputs. Everything is trimmed, a NULL or
# NA input becomes "", and runs of whitespace in the description (including
# tabs and newlines, which would break the tab-separated .txt file) are
# collapsed to single spaces.
build_custom_unit_row <- function(id, unitType, parentSI, multiplierToSI, description) {
  clean <- function(x) {
    if (is.null(x) || length(x) == 0 || is.na(x[[1]])) return("")
    trimws(as.character(x[[1]]))
  }
  tibble::tibble(
    id = clean(id),
    unitType = clean(unitType),
    parentSI = clean(parentSI),
    multiplierToSI = clean(multiplierToSI),
    description = gsub("\\s+", " ", clean(description))
  )
}

#' Validate one new custom unit. Pure function - no side effects.
#'
#' @param new_unit one-row tibble from build_custom_unit_row()
#' @param existing_ids ids of the custom units already defined
#' @param standard_ids ids in the EML standard unit dictionary
#' @return character vector of error messages, empty if the unit is valid
#' @noRd
validate_custom_unit <- function(new_unit, existing_ids, standard_ids) {
  errors <- character(0)

  if (is_blank(new_unit$id)) {
    errors <- c(errors, "Unit id is required.")
  } else {
    if (!grepl("^[A-Za-z][A-Za-z0-9_-]*$", new_unit$id)) {
      errors <- c(errors, paste0(
        "Unit id '", new_unit$id, "' must start with a letter and contain ",
        "only letters, numbers, underscores or hyphens (no spaces)."
      ))
    }
    if (new_unit$id %in% standard_ids) {
      errors <- c(errors, paste0(
        "'", new_unit$id, "' is already a standard EML unit - use it ",
        "directly instead of defining it again."
      ))
    }
    if (new_unit$id %in% existing_ids) {
      errors <- c(errors, paste0(
        "A custom unit with the id '", new_unit$id, "' is already defined."
      ))
    }
  }

  if (is_blank(new_unit$unitType)) {
    errors <- c(errors, "Unit type is required.")
  }

  # Multiplier to SI is optional. When it is given it must be xsd:decimal in
  # the EML schema: digits with an optional sign and decimal point, and NO
  # exponent - 1e-3 would make the EML schema-invalid.
  number_pattern <- "^[-+]?([0-9]+\\.?[0-9]*|\\.[0-9]+)$"
  multiplier <- new_unit$multiplierToSI
  if (!is_blank(multiplier) &&
      (!grepl(number_pattern, multiplier) || isTRUE(as.numeric(multiplier) == 0))) {
    errors <- c(errors, paste0(
      "Multiplier to SI, if given, must be a decimal number other than zero, ",
      "such as 0.001 or 1000 (no exponent notation); '", multiplier,
      "' is not."
    ))
  }

  if (is_blank(new_unit$description)) {
    errors <- c(errors, "Description is required.")
  }

  errors
}

#' Where is a unit in use? Pure function.
#'
#' @param tables_state named list of list(attributes = tibble, catvars = tibble)
#' @param unit_id the unit id to look for
#' @return character vector like "file.csv: columnName", empty if unused
#' @noRd
find_unit_usage <- function(tables_state, unit_id) {
  usage <- purrr::imap(tables_state, function(tbl_state, file_name) {
    attrs <- tbl_state$attributes
    used <- !is.na(attrs$unit) & attrs$unit == unit_id
    sprintf("%s: %s", file_name, attrs$attributeName[used])
  })
  unlist(usage, use.names = FALSE)
}

#' Rebuild the custom units tibble from saved session state (list of
#' columns, the same shape the attributes use). NULL or empty gives zero
#' rows. unname() and the NA-to-"" step follow restore_attributes_by_name()
#' - a named vector in a tibble column corrupts readr::write_tsv() output.
#'
#' @param saved list(id =, unitType =, parentSI =, multiplierToSI =,
#'   description =) of character vectors, or NULL
#' @return tibble from empty_custom_units_tbl()'s shape
#' @noRd
restore_custom_units_tbl <- function(saved) {
  n_units <- length(saved$id)
  if (is.null(saved) || n_units == 0) return(empty_custom_units_tbl())

  column_values <- function(column) {
    values <- unname(as.character(saved[[column]]))
    values[is.na(values)] <- ""
    if (length(values) == n_units) values else rep_len("", n_units)
  }

  tibble::tibble(
    id = column_values("id"),
    unitType = column_values("unitType"),
    parentSI = column_values("parentSI"),
    multiplierToSI = column_values("multiplierToSI"),
    description = column_values("description")
  )
}

infer_class <- function(col) {
  if (inherits(col, "Date")) return("Date")
  if (is.numeric(col)) return("numeric")
  if (is.character(col) || is.factor(col)) {
    n_unique <- length(unique(stats::na.omit(col)))
    if (n_unique > 0 && n_unique <= CATEGORICAL_MAX_LEVELS) return("categorical")
    return("character")
  }
  "character"
}

# EMLassemblyline requires missingValueCode and missingValueCodeExplanation
# to be a matched pair per row: both blank (no missing-value code declared
# for that attribute) or both populated. DT's cell-edit round-trip can
# silently turn an untouched NA into "" when ANY cell in that row is
# edited (JSON has no native NA, and DT's editor coerces blank cells to
# empty strings) - this desyncs the pair without the user ever touching
# either missing-value column directly. Call this after every edit to
# keep the pair consistent: treat NA and "" as equivalent "unset", and if
# only one side of a row is unset while the other has content, that's a
# genuine partial-entry the user needs to finish (handled by validation,
# not silently fixed) - but if BOTH sides are just empty-vs-NA, normalize
# them to the same NA representation so a "no missing value code" row
# doesn't get flagged as an incomplete pair.
normalize_missing_value_pair <- function(df) {
  is_unset <- function(x) is.na(x) | !nzchar(trimws(ifelse(is.na(x), "", x)))
  code_unset <- is_unset(df$missingValueCode)
  expl_unset <- is_unset(df$missingValueCodeExplanation)

  both_unset <- code_unset & expl_unset
  df$missingValueCode[both_unset] <- NA_character_
  df$missingValueCodeExplanation[both_unset] <- NA_character_

  df
}

# Blank/NA check used throughout validation - treats NA and whitespace-only
# strings both as "not filled in".
is_blank <- function(x) {
  is.na(x) | !nzchar(trimws(ifelse(is.na(x), "", x)))
}

#' Validate one table's attributes + catvars state, per skeleton.Rmd/EML
#' schema requirements. Pure function - no side effects.
#'
#' @param file_name the table's file name, used to prefix error messages
#'   so the user knows which table/tab to go fix
#' @param tbl_state list(attributes = tibble, catvars = tibble) for one table
#' @return character vector of error messages, empty if this table is valid
validate_table_fields <- function(file_name, tbl_state) {
  errors <- character(0)
  attrs <- tbl_state$attributes
  catvars <- tbl_state$catvars

  blank_def <- is_blank(attrs$attributeDefinition)
  if (any(blank_def)) {
    errors <- c(errors, sprintf(
      "%s: column '%s' needs a definition (attributeDefinition).",
      file_name, attrs$attributeName[blank_def]
    ))
  }

  needs_unit <- attrs$class == "numeric" & is_blank(attrs$unit)
  if (any(needs_unit)) {
    errors <- c(errors, sprintf(
      "%s: column '%s' is numeric and needs a unit.",
      file_name, attrs$attributeName[needs_unit]
    ))
  }

  needs_datetime_fmt <- attrs$class == "Date" & is_blank(attrs$dateTimeFormatString)
  if (any(needs_datetime_fmt)) {
    errors <- c(errors, sprintf(
      "%s: column '%s' is a Date and needs a date/time format string.",
      file_name, attrs$attributeName[needs_datetime_fmt]
    ))
  }

  if (nrow(catvars) > 0) {
    blank_catvar_def <- is_blank(catvars$definition)
    if (any(blank_catvar_def)) {
      # report distinct (attributeName, code) pairs, not just attributeName,
      # since a categorical column with 5 codes could have some defined and
      # some not - the user needs to know exactly which code is missing.
      bad_rows <- catvars[blank_catvar_def, ]
      errors <- c(errors, sprintf(
        "%s: column '%s', code '%s' needs a definition.",
        file_name, bad_rows$attributeName, bad_rows$code
      ))
    }
  }

  errors
}

# Build the initial attributes tibble for one data.frame. `class` is
# plain character, restricted to CLASS_CHOICES via the cell-edit
# validate-and-revert logic in fieldsServer() (not a factor - an earlier
# attempt used factor() hoping DT would auto-render it as a dropdown, but
# DT's cell editor always generates a plain text <input> regardless of
# column type; that approach was reverted once confirmed. A small JS
# callback now attaches an HTML5 <datalist> autocomplete hint to the
# class column's editor input instead - see the callback = DT::JS(...)
# in fieldsServer()'s attrs_id renderDT.)
build_attributes_tibble <- function(df) {
  tibble::tibble(
    attributeName = names(df),
    attributeDefinition = "",
    class = purrr::map_chr(df, infer_class),
    unit = NA_character_,
    dateTimeFormatString = NA_character_,
    missingValueCode = NA_character_,
    missingValueCodeExplanation = NA_character_
  )
}

# Build the initial categorical-codes tibble for one data.frame, given its
# current attributes tibble (so we only include columns currently marked
# categorical).
build_catvars_tibble <- function(df, attributes_tbl) {
  cat_cols <- attributes_tbl$attributeName[attributes_tbl$class == "categorical"]
  if (length(cat_cols) == 0) {
    return(tibble::tibble(attributeName = character(), code = character(), definition = character()))
  }
  purrr::map_dfr(cat_cols, function(col) {
    codes <- sort(unique(as.character(stats::na.omit(df[[col]]))))
    tibble::tibble(attributeName = col, code = codes, definition = "")
  })
}

#' Overwrite specific fields of a freshly-auto-built attributes tibble
#' with saved values, matched by attributeName. Never replaces the
#' tibble wholesale - see file header note for the full contract:
#' new columns keep their fresh defaults, columns present in saved data
#' but missing from the current file are simply not restored (reported
#' separately via the returned $missing_columns), and existing matches
#' get their editable fields overwritten in place.
#'
#' @param fresh_attrs the just-built (build_attributes_tibble()) tibble
#'   for the CURRENT re-uploaded file
#' @param saved_attrs list-of-columns shape (jsonlite simplifyVector) for
#'   this table's saved attributes, or NULL
#' @return list(attrs = tibble, new_columns = character vector,
#'   missing_columns = character vector)
restore_attributes_by_name <- function(fresh_attrs, saved_attrs) {
  if (is.null(saved_attrs) || length(saved_attrs$attributeName %||% character(0)) == 0) {
    return(list(attrs = fresh_attrs, new_columns = character(0), missing_columns = character(0)))
  }

  saved_tbl <- tibble::tibble(
    attributeName = as.character(saved_attrs$attributeName),
    attributeDefinition = as.character(saved_attrs$attributeDefinition %||% ""),
    class = as.character(saved_attrs$class %||% "character"),
    unit = as.character(saved_attrs$unit %||% NA_character_),
    dateTimeFormatString = as.character(saved_attrs$dateTimeFormatString %||% NA_character_),
    missingValueCode = as.character(saved_attrs$missingValueCode %||% NA_character_),
    missingValueCodeExplanation = as.character(saved_attrs$missingValueCodeExplanation %||% NA_character_)
  )

  matched_names <- intersect(fresh_attrs$attributeName, saved_tbl$attributeName)
  new_columns <- setdiff(fresh_attrs$attributeName, saved_tbl$attributeName)
  missing_columns <- setdiff(saved_tbl$attributeName, fresh_attrs$attributeName)

  updated <- fresh_attrs
  for (nm in matched_names) {
    saved_row <- saved_tbl[saved_tbl$attributeName == nm, ][1, ]
    idx <- which(updated$attributeName == nm)

    # unname() is REQUIRED here - CONFIRMED BUG (found via live testing):
    # saved_row$class (and in principle any column pulled from a
    # single-row tibble slice like this) can carry an implicit name
    # (e.g. inherited from row-extraction) that gets attached to the
    # TARGET vector element on assignment, silently turning updated$class
    # from a plain character vector into a NAMED character vector after
    # just one restored row. A named vector column inside a tibble is NOT
    # equivalent to a plain one for readr::write_tsv()'s purposes - it
    # produced a malformed/truncated last row in the written
    # attributes_*.txt file (fewer tab-separated fields than columns),
    # which downstream caused EMLassemblyline to report "invalid column
    # names" / dropped dataTable entries. unname() strips any such name
    # before assignment, guaranteeing every column stays a plain,
    # unnamed vector regardless of restore activity.
    updated$attributeDefinition[idx] <- unname(saved_row$attributeDefinition)
    updated$class[idx] <- unname(saved_row$class)
    updated$unit[idx] <- unname(saved_row$unit)
    updated$dateTimeFormatString[idx] <- unname(saved_row$dateTimeFormatString)
    updated$missingValueCode[idx] <- unname(saved_row$missingValueCode)
    updated$missingValueCodeExplanation[idx] <- unname(saved_row$missingValueCodeExplanation)
  }

  list(attrs = updated, new_columns = new_columns, missing_columns = missing_columns)
}

#' Overwrite catvars definitions matched by (attributeName, code) pair -
#' same non-destructive contract as restore_attributes_by_name(). Called
#' AFTER attributes have been restored and catvars rebuilt from the
#' (possibly restored) class column, so this only needs to fill in
#' `definition` for codes that still exist; new codes keep their fresh
#' blank definition, and saved codes no longer present in the data are
#' silently dropped (least consequential case per the agreed contract).
#'
#' @param fresh_catvars the just-rebuilt (build_catvars_tibble()) tibble
#' @param saved_catvars list-of-columns shape for this table's saved
#'   catvars, or NULL
#' @return tibble - fresh_catvars with definitions overwritten where a
#'   saved (attributeName, code) match exists
restore_catvars_by_key <- function(fresh_catvars, saved_catvars) {
  if (is.null(saved_catvars) || length(saved_catvars$attributeName %||% character(0)) == 0) {
    return(fresh_catvars)
  }
  if (nrow(fresh_catvars) == 0) return(fresh_catvars)

  saved_tbl <- tibble::tibble(
    attributeName = as.character(saved_catvars$attributeName),
    code = as.character(saved_catvars$code),
    definition = as.character(saved_catvars$definition %||% "")
  )

  updated <- fresh_catvars
  saved_key <- paste(saved_tbl$attributeName, saved_tbl$code, sep = "\u0001")
  fresh_key <- paste(updated$attributeName, updated$code, sep = "\u0001")

  match_idx <- match(fresh_key, saved_key)
  has_match <- !is.na(match_idx)
  # unname() here too, for the same reason as restore_attributes_by_name()
  # above - guards against saved_tbl$definition[match_idx[has_match]]
  # carrying an implicit name that would otherwise taint `definition`
  # into a named vector, even though this particular extraction (a full
  # vector slice, not a single-row tibble pull) was not confirmed to
  # actually trigger the bug - applied defensively since the failure mode
  # (a malformed written .txt row) is severe and silent.
  updated$definition[has_match] <- unname(saved_tbl$definition[match_idx[has_match]])

  updated
}

# JS rowCallback for the ATTRIBUTES table: highlight blank-but-required
# cells (see v23 note). attributeDefinition is always required; unit only
# when class == "numeric"; dateTimeFormatString only when class == "Date".
# Non-required / filled cells have their inline background cleared so the
# default DT row striping shows through. Runs on every table draw, so it
# re-evaluates live as cells are edited (and re-highlights if cleared).
attrs_row_callback_js <- function() {
  DT::JS(
    "function(row, data) {",
    sprintf("  var hl = '%s';", REQUIRED_CELL_HIGHLIGHT),
    "  function blank(v){ return v === null || v === undefined || String(v).trim() === ''; }",
    "  var cells = $('td', row);",
    "  // attributeDefinition (col 1): always required",
    "  cells.eq(1).css('background-color', blank(data[1]) ? hl : '');",
    "  // unit (col 3): required only when class (col 2) is numeric",
    "  if (data[2] === 'numeric') {",
    "    cells.eq(3).css('background-color', blank(data[3]) ? hl : '');",
    "  } else {",
    "    cells.eq(3).css('background-color', '');",
    "  }",
    "  // dateTimeFormatString (col 4): required only when class is Date",
    "  if (data[2] === 'Date') {",
    "    cells.eq(4).css('background-color', blank(data[4]) ? hl : '');",
    "  } else {",
    "    cells.eq(4).css('background-color', '');",
    "  }",
    "}"
  )
}

# JS rowCallback for the CATEGORICAL CODES table: highlight the definition
# cell (col 2) whenever it is blank (always required). Same live behaviour
# as the attributes callback above.
catvars_row_callback_js <- function() {
  DT::JS(
    "function(row, data) {",
    sprintf("  var hl = '%s';", REQUIRED_CELL_HIGHLIGHT),
    "  function blank(v){ return v === null || v === undefined || String(v).trim() === ''; }",
    "  $('td', row).eq(2).css('background-color', blank(data[2]) ? hl : '');",
    "}"
  )
}

fieldsUI <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    bslib::card(
      bslib::card_header("Field (attribute) metadata"),
      shiny::helpText("For each data table, describe every column: what it means, ",
                      "its data class, and (for numeric columns) its unit of ",
                      "measurement. Columns auto-flagged as 'categorical' need a ",
                      "definition for each unique code below - review the ",
                      "auto-detected class column and adjust if needed. ",
                      "Cells highlighted in amber still need your input."),
      shiny::helpText(class = "text-muted",
                      "Double-click a cell to edit it. Press Enter to save and ",
                      "move down the column, or Tab to save and move across the ",
                      "row (hold Shift to go back). Press Esc to cancel an edit."),
      shiny::actionLink(ns("open_unit_lookup"), "Look up valid units",
                        icon = shiny::icon("magnifying-glass")),
      bslib::accordion(
        open = FALSE,
        bslib::accordion_panel(
          "Custom units (optional)",
          shiny::helpText("Define a unit that is not in the EML standard unit ",
                          "dictionary (for example speciesPerSquareMeter). Once ",
                          "added, type its Unit id into the Unit column of any ",
                          "numeric attribute below. Unit id, Unit type and ",
                          "Description are required. Parent SI unit and ",
                          "Multiplier to SI are optional; the multiplier, if ",
                          "given, converts one of your unit into the parent ",
                          "SI unit."),
          shiny::uiOutput(ns("custom_unit_inputs")),
          shiny::actionButton(ns("add_custom_unit"), "Add custom unit",
                              icon = shiny::icon("plus"),
                              class = "btn-primary btn-sm"),
          shiny::hr(),
          DT::DTOutput(ns("custom_units_table")),
          shiny::actionButton(ns("remove_custom_unit"), "Remove selected unit",
                              icon = shiny::icon("trash"),
                              class = "btn-outline-danger btn-sm mt-2")
        )
      ),
      shiny::uiOutput(ns("restore_notice")),
      shiny::uiOutput(ns("table_tabs"))
    ),
    col_widths = c(-1, 10, -1), fill = FALSE
  )
}


#' @param tables_reactive reactive() named list of data.frames (from Tab 3)
#' @noRd
#' @return list(data = <reactive() list>, restore = <function>,
#'   unmatched_saved_files = <reactive() character vector>)
#'   data() returns:
#'     $tables - named list, one entry per table:
#'       list(<file_name> = list(attributes = tibble, catvars = tibble))
#'     $custom_units - tibble(id, unitType, parentSI, multiplierToSI,
#'       description), zero rows when none are defined
#'     $valid - logical
#'     $errors - character vector, empty if valid
#'

#'
fieldsServer <- function(id, tables_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    unit_table <- get_unit_table()
    unit_choices <- sort(unique(unit_table$id))

    shiny::observeEvent(input$open_unit_lookup, {
      shiny::showModal(shiny::modalDialog(
        title = "Look up valid EML units",
        shiny::helpText("Search by name, type, or description to find the exact spelling ",
                        "to enter in the Unit column (e.g. search 'temp' to find celsius, ",
                        "fahrenheit, kelvin)."),
        DT::DTOutput(ns("unit_lookup_table")),
        size = "l", easyClose = TRUE, footer = shiny::modalButton("Close")
      ))
    })

    output$unit_lookup_table <- DT::renderDT({
      DT::datatable(
        unit_table[, c("id", "name", "unitType", "description")],
        rownames = FALSE,
        selection = "none",
        colnames = c("Unit (enter exactly as shown)", "Name", "Type", "Description"),
        options = list(dom = 'ftp', pageLength = 10)
      )
    })

    # store per-table state: reactiveValues keyed by table name, each holding
    # $attributes and $catvars tibbles
    state <- shiny::reactiveValues()

    # Per-grid redraw tokens, keyed "attrs_<safe_id>" / "catvars_<safe_id>"
    # (v27). Each renderDT depends on its own token and reads `state` with
    # isolate(), so only a change R has to PUSH to the browser rebuilds a
    # grid - see the v27 note at the top of this file for which changes
    # those are. An accepted cell edit bumps nothing.
    shape <- shiny::reactiveValues()

    bump_shape <- function(key) {
      current <- shiny::isolate(shape[[key]])
      shape[[key]] <- if (is.null(current)) 1L else current + 1L
    }

    # Both grids of one table, by its display name.
    bump_table_shape <- function(table_name) {
      safe_id <- gsub("[^A-Za-z0-9_]", "_", table_name)
      bump_shape(paste0("attrs_", safe_id))
      bump_shape(paste0("catvars_", safe_id))
    }

    # ---- Custom units (v28) ----------------------------------------------
    # The user's custom units. Kept apart from `state` because they belong
    # to the whole package, not to one table, and because nothing here may
    # touch the attributes grids' redraw tokens (v27).
    custom_units <- shiny::reactiveVal(empty_custom_units_tbl())

    # Bumped after a successful Add to rebuild the form with blank boxes.
    custom_unit_form_epoch <- shiny::reactiveVal(0L)

    output$custom_unit_inputs <- shiny::renderUI({
      custom_unit_form_epoch()
      shiny::tagList(
        bslib::layout_columns(
          col_widths = c(3, 3, 3, 3),
          shiny::textInput(ns("custom_unit_id"), "Unit id",
                           placeholder = "e.g. speciesPerSquareMeter"),
          shiny::textInput(ns("custom_unit_type"), "Unit type",
                           placeholder = "e.g. velocity"),
          shiny::textInput(ns("custom_unit_parent_si"), "Parent SI unit (optional)",
                           placeholder = "e.g. metersPerSecond"),
          shiny::textInput(ns("custom_unit_multiplier"), "Multiplier to SI (optional)",
                           placeholder = "e.g. 0.001")
        ),
        shiny::textInput(ns("custom_unit_description"), "Description",
                         width = "100%")
      )
    })

    output$custom_units_table <- DT::renderDT({
      DT::datatable(
        custom_units(),
        rownames = FALSE,
        selection = "single",
        colnames = c("Unit id", "Unit type", "Parent SI unit",
                     "Multiplier to SI", "Description"),
        options = list(dom = "t", pageLength = -1, scrollX = TRUE)
      )
    })

    shiny::observeEvent(input$add_custom_unit, {
      new_unit <- build_custom_unit_row(
        id = input$custom_unit_id,
        unitType = input$custom_unit_type,
        parentSI = input$custom_unit_parent_si,
        multiplierToSI = input$custom_unit_multiplier,
        description = input$custom_unit_description
      )
      current <- custom_units()

      errors <- validate_custom_unit(
        new_unit,
        existing_ids = current$id,
        standard_ids = unit_choices
      )

      if (length(errors) > 0) {
        shiny::showNotification(paste(errors, collapse = " "),
                                type = "error", duration = 10)
      } else {
        custom_units(dplyr::bind_rows(current, new_unit))
        custom_unit_form_epoch(shiny::isolate(custom_unit_form_epoch()) + 1L)
      }
    })

    shiny::observeEvent(input$remove_custom_unit, {
      selected <- input$custom_units_table_rows_selected
      if (length(selected) == 0) {
        shiny::showNotification("Select a custom unit in the table first.",
                                type = "warning")
      } else {
        current <- custom_units()
        unit_id <- current$id[selected]
        usage <- find_unit_usage(shiny::reactiveValuesToList(state), unit_id)

        if (length(usage) > 0) {
          shiny::showNotification(
            paste0("'", unit_id, "' is still used by: ",
                   paste(usage, collapse = "; "),
                   ". Change the Unit of those columns first."),
            type = "error", duration = 10
          )
        } else {
          custom_units(current[-selected, ])
        }
      }
    })

    # Saved per-table attributes/catvars awaiting a matching re-uploaded
    # table - see file header note. NULL until restore() is called; a
    # table's entry is removed once that table's default state has been
    # built AND the saved values have been matched onto it.
    pending_restore_fields <- shiny::reactiveVal(NULL)
    restore_epoch <- shiny::reactiveVal(0L)

    # Tables whose defaults were just (re)built this round, and for which
    # a saved-state match was attempted - used to build restore_notice's
    # new/missing-column summary. Cleared each time it's consumed.
    last_restore_summary <- shiny::reactiveVal(NULL)

    shiny::observeEvent(list(tables_reactive(), restore_epoch()), {
      tbls <- tables_reactive()
      newly_built <- character(0)

      for (nm in names(tbls)) {
        if (is.null(state[[nm]])) {
          attrs <- build_attributes_tibble(tbls[[nm]])
          state[[nm]] <- list(
            attributes = attrs,
            catvars = build_catvars_tibble(tbls[[nm]], attrs)
          )
          newly_built <- c(newly_built, nm)
          # fresh defaults have to be drawn (v27)
          bump_table_shape(nm)
        }
      }
      # drop state for tables that were removed
      removed <- setdiff(names(shiny::reactiveValuesToList(state)), names(tbls))
      for (nm in removed) state[[nm]] <- NULL

      # also forget that removed tables were "registered" - if a file with
      # the same name is uploaded again later, its observers/outputs need
      # to be wired up fresh (their old renderUI-generated DOM elements are
      # gone once the table list shrinks and table_tabs re-renders)
      registered_tables <<- setdiff(registered_tables, removed)

      # Attempt to match pending saved state onto any table whose default
      # state was JUST built this round. Tables built in earlier rounds
      # were either already matched (removed from pending) or had no
      # saved entry to match - no need to re-attempt those.
      pending <- pending_restore_fields()
      if (!is.null(pending) && length(newly_built) > 0) {
        summary_new <- list()
        summary_missing <- list()

        for (nm in newly_built) {
          saved_entry <- pending[[nm]]
          if (is.null(saved_entry)) next

          attrs_result <- restore_attributes_by_name(state[[nm]]$attributes, saved_entry$attributes)
          state[[nm]]$attributes <- attrs_result$attrs

          # rebuild catvars from the (possibly restored) class column,
          # THEN overlay saved catvars definitions by key - mirrors the
          # existing "class changed -> rebuild catvars" logic in the
          # cell-edit observer below
          rebuilt_catvars <- build_catvars_tibble(tbls[[nm]], state[[nm]]$attributes)
          state[[nm]]$catvars <- restore_catvars_by_key(rebuilt_catvars, saved_entry$catvars)

          # restored values replace what was just drawn (v27)
          bump_table_shape(nm)

          if (length(attrs_result$new_columns) > 0) summary_new[[nm]] <- attrs_result$new_columns
          if (length(attrs_result$missing_columns) > 0) summary_missing[[nm]] <- attrs_result$missing_columns

          pending[[nm]] <- NULL
        }

        pending_restore_fields(if (length(pending) == 0) NULL else pending)
        if (length(summary_new) > 0 || length(summary_missing) > 0) {
          last_restore_summary(list(new_columns = summary_new, missing_columns = summary_missing))
        }
      }
    })

    output$restore_notice <- shiny::renderUI({
      pending <- pending_restore_fields()
      summary <- last_restore_summary()

      notices <- list()

      if (!is.null(pending) && length(pending) > 0) {
        notices <- c(notices, list(shiny::tags$div(
          class = "alert alert-warning mt-2",
          shiny::tags$strong("Re-load needed to finish restoring saved field metadata for: "),
          paste(names(pending), collapse = ", ")
        )))
      }

      if (!is.null(summary)) {
        if (length(summary$new_columns) > 0) {
          msgs <- purrr::imap_chr(summary$new_columns, function(cols, tbl) {
            paste0(tbl, ": ", paste(cols, collapse = ", "))
          })
          notices <- c(notices, list(shiny::tags$div(
            class = "alert alert-info mt-2",
            shiny::tags$strong("New column(s) found (not in your saved session) - these need definitions: "),
            shiny::tags$ul(lapply(msgs, shiny::tags$li))
          )))
        }
        if (length(summary$missing_columns) > 0) {
          msgs <- purrr::imap_chr(summary$missing_columns, function(cols, tbl) {
            paste0(tbl, ": ", paste(cols, collapse = ", "))
          })
          notices <- c(notices, list(shiny::tags$div(
            class = "alert alert-warning mt-2",
            shiny::tags$strong("Column(s) from your saved session were not found in the re-imported file - their saved definitions were not restored: "),
            shiny::tags$ul(lapply(msgs, shiny::tags$li))
          )))
        }
      }

      if (length(notices) == 0) return(NULL)
      shiny::tagList(notices)
    })

    output$table_tabs <- shiny::renderUI({
      tbls <- tables_reactive()
      shiny::req(length(tbls) > 0)

      tabs <- lapply(names(tbls), function(nm) {
        safe_id <- gsub("[^A-Za-z0-9_]", "_", nm)
        bslib::nav_panel(
          title = nm,
          shiny::br(),
          shiny::h5("Attributes"),
          DT::DTOutput(ns(paste0("attrs_", safe_id))),
          shiny::hr(),
          shiny::h5("Categorical codes"),
          shiny::helpText("Only columns currently marked 'categorical' above appear here. ",
                          "Codes are pulled from the data; add definitions for each."),
          DT::DTOutput(ns(paste0("catvars_", safe_id)))
        )
      })

      do.call(bslib::navset_tab, tabs)
    })

    # dynamically wire up DT render + edit handling for each table. Each
    # table's observers/outputs must be created EXACTLY ONCE - this observer
    # can fire many times over the app's life (any change to the table
    # list), so we track which tables already have their observers
    # registered and skip re-registering them. Without this guard, editing
    # a cell would fire its handler once per prior invocation of this
    # block, producing duplicate notifications that multiply over time.
    registered_tables <- character(0)

    shiny::observeEvent(tables_reactive(), {
      tbls <- tables_reactive()

      for (nm in names(tbls)) {
        if (nm %in% registered_tables) next
        registered_tables <<- c(registered_tables, nm)

        local({
          table_name <- nm
          safe_id <- gsub("[^A-Za-z0-9_]", "_", table_name)
          attrs_id <- paste0("attrs_", safe_id)
          catvars_id <- paste0("catvars_", safe_id)

          # Rebuilt only when shape[[attrs_id]] changes - see the v27 note
          # at the top of this file. The tibble is read with isolate(), so
          # an accepted cell edit leaves this render untouched and the
          # browser keeps its scroll position, its focus and its open
          # editor.
          #
          # server = FALSE is REQUIRED here, not cosmetic (v27): in server
          # mode DT would keep serving the rows registered at render time
          # and the grid would show stale values once this render stopped
          # firing on every edit. In client mode the browser holds the
          # rows, DT's own blur handler has already written the committed
          # value into the cell, and its table.draw(false) re-runs the
          # rowCallback locally - so the amber required-cell highlighting
          # still updates live. One row per column of a .csv; the .csv
          # data itself never reaches this grid.
          output[[attrs_id]] <- DT::renderDT({
            shape[[attrs_id]]
            shiny::req(shiny::isolate(state[[table_name]]))
            DT::datatable(
              shiny::isolate(state[[table_name]]$attributes),
              rownames = FALSE,
              selection = "none",
              # rowCallback highlights blank-but-required cells live on
              # every draw (see v23 note / attrs_row_callback_js()).
              options = list(
                dom = 't', pageLength = -1, scrollX = TRUE,
                rowCallback = attrs_row_callback_js()
              ),
              # attributeName (col 0) stays locked/read-only - see
              # ATTRS_DISABLED_COLUMNS.
              editable = list(
                target = "cell",
                disable = list(columns = ATTRS_DISABLED_COLUMNS)
              ),
              # TWO things are wired up in this one callback.
              #
              # 1. "class" (col index 2, 0-based) gets an HTML5 <datalist>
              # autocomplete attached to its cell-edit <input> via a small
              # JS callback - DT's cell editor always generates a plain
              # text <input> with no native dropdown/list support, so this
              # is the least-JS way to give a suggestion list while typing.
              # This is a UX hint only, NOT enforcement - the browser does
              # not prevent typing something outside the list, so the
              # existing R-side validate-and-revert in the cell_edit
              # observer below remains the actual enforcement mechanism.
              #
              # DT's cell editor <input> is created fresh each time a cell
              # is double-clicked (it does not exist at table-init time),
              # so a delegated event listener on the table body (fires on
              # any focusin, checks if the target is the class column's
              # editor) is used rather than trying to attach the datalist
              # once up front.
              #
              # 2. Keyboard navigation (v27): Enter moves down a column,
              # Tab across a row, Shift reverses either, read-only and
              # hidden columns are skipped. Appended from
              # dt_keyboard_nav_body() rather than given its own callback,
              # because datatable() accepts only one. It is passed the
              # same ATTRS_DISABLED_COLUMNS as editable$disable above, so
              # the two agree by construction.
              callback = DT::JS(
                sprintf(
                  "
                var datalistId = 'class-options-%s';
                if (!document.getElementById(datalistId)) {
                  var dl = document.createElement('datalist');
                  dl.id = datalistId;
                  %s.forEach(function(opt) {
                    var o = document.createElement('option');
                    o.value = opt;
                    dl.appendChild(o);
                  });
                  document.body.appendChild(dl);
                }
                table.on('focus', 'input', function() {
                  var cellIdx = table.cell(this.closest('td')).index();
                  if (cellIdx && cellIdx.column === 2) {
                    this.setAttribute('list', datalistId);
                  }
                });
                ",
                  safe_id,
                  jsonlite::toJSON(CLASS_CHOICES)
                ),
                dt_keyboard_nav_body(
                  disabled_columns = ATTRS_DISABLED_COLUMNS
                )
              )
            )
          }, server = FALSE)

          shiny::observeEvent(input[[paste0(attrs_id, "_cell_edit")]], {
            edit <- input[[paste0(attrs_id, "_cell_edit")]]
            current <- state[[table_name]]$attributes
            updated <- DT::editData(current, edit, rownames = FALSE)
            updated <- normalize_missing_value_pair(updated)

            col_edited <- names(updated)[edit$col + 1]

            # TRUE if anything below overrides what the user typed. The
            # browser is then showing a value R has rejected, so that
            # grid - and only then - has to be redrawn (v27).
            reverted <- FALSE

            if (col_edited == "class") {
              bad <- !updated$class %in% CLASS_CHOICES
              if (any(bad)) {
                shiny::showNotification(
                  paste0("Class must be one of: ", paste(CLASS_CHOICES, collapse = ", ")),
                  type = "error"
                )
                updated$class[bad] <- current$class[bad]
                reverted <- TRUE
              }
            }

            if (col_edited == "unit") {
              edited_row <- edit$row
              new_class <- updated$class[edited_row]
              new_unit <- updated$unit[edited_row]

              if (new_class != "numeric" && !is.na(new_unit) && nzchar(new_unit)) {
                shiny::showNotification(
                  "Unit only applies to numeric columns. Set class to 'numeric' first.",
                  type = "error"
                )
                updated$unit[edited_row] <- current$unit[edited_row]
                reverted <- TRUE
              } else if (new_class == "numeric" && (is.na(new_unit) || !nzchar(new_unit))) {
                # blank is allowed transiently while typing/clearing - only
                # reject non-blank values that aren't in the dictionary
              } else if (!is.na(new_unit) && nzchar(new_unit) &&
                         !(new_unit %in% c(unit_choices, custom_units()$id))) {
                shiny::showNotification(
                  paste0("'", new_unit, "' is not a recognized EML unit or a ",
                         "custom unit you defined above. ",
                         "See EML::get_unitList() for valid options. ",
                         "Value reverted."),
                  type = "error"
                )
                updated$unit[edited_row] <- current$unit[edited_row]
                reverted <- TRUE
              }
            }

            state[[table_name]]$attributes <- updated

            # warn about GENUINE mismatches (one side has real content, the
            # other doesn't) - these are not auto-fixable and need the user
            # to either fill in both sides or clear both
            code_set <- !is.na(updated$missingValueCode) & nzchar(trimws(updated$missingValueCode))
            expl_set <- !is.na(updated$missingValueCodeExplanation) & nzchar(trimws(updated$missingValueCodeExplanation))
            mismatched <- xor(code_set, expl_set)
            if (any(mismatched)) {
              shiny::showNotification(
                paste0("In '", table_name, "': these attributes have a missing value CODE ",
                       "but no explanation, or vice versa - both are required together: ",
                       paste(updated$attributeName[mismatched], collapse = ", ")),
                type = "warning", duration = 8
              )
            }

            # Show the corrected value. Only on a revert: an accepted edit
            # is already on screen, and redrawing it is what sent the page
            # back to the top before v27.
            if (reverted) bump_shape(attrs_id)

            # if class changed to/from categorical, rebuild catvars for this table
            if (col_edited == "class") {
              df <- tables_reactive()[[table_name]]
              state[[table_name]]$catvars <- build_catvars_tibble(df, updated)
              # the codes grid's ROWS changed, so it must be redrawn (v27)
              bump_shape(catvars_id)
            }
          })

          # Same contract as the attributes grid above: redrawn only when
          # shape[[catvars_id]] changes (defaults built, saved definitions
          # restored, or codes rebuilt after a class change), and
          # server = FALSE for the same reason (v27).
          output[[catvars_id]] <- DT::renderDT({
            shape[[catvars_id]]
            shiny::req(shiny::isolate(state[[table_name]]))
            DT::datatable(
              shiny::isolate(state[[table_name]]$catvars),
              rownames = FALSE,
              selection = "none",
              # rowCallback highlights blank definition cells live on every
              # draw (see v23 note / catvars_row_callback_js()).
              options = list(
                dom = 't', pageLength = -1, scrollX = TRUE,
                rowCallback = catvars_row_callback_js()
              ),
              # attributeName (col 0) and code (col 1) stay read-only -
              # see CATVARS_DISABLED_COLUMNS.
              editable = list(
                target = "cell",
                disable = list(columns = CATVARS_DISABLED_COLUMNS)
              ),
              # Only `definition` is editable here, so Enter down the
              # column is the whole workflow (v27).
              callback = dt_keyboard_nav_callback(
                disabled_columns = CATVARS_DISABLED_COLUMNS
              )
            )
          }, server = FALSE)

          shiny::observeEvent(input[[paste0(catvars_id, "_cell_edit")]], {
            edit <- input[[paste0(catvars_id, "_cell_edit")]]
            updated <- DT::editData(state[[table_name]]$catvars, edit, rownames = FALSE)
            # No bump here: definition is free text, nothing rejects it,
            # and the browser already shows it (v27).
            state[[table_name]]$catvars <- updated
          })
        })
      }
    })

    data <- shiny::reactive({
      tbls <- shiny::reactiveValuesToList(state)

      # NOTE: purrr::imap(.x, .f) calls .f(value, name) POSITIONALLY -
      # validate_table_fields()'s own signature is (file_name, tbl_state),
      # the OPPOSITE order. Calling imap(tbls, validate_table_fields)
      # directly silently swapped the two arguments (tbl_state's tibble
      # landing in the file_name parameter and vice versa), which didn't
      # error but produced garbage sprintf() output that rendered as
      # "[object Object]" once it reached the UI. Using an explicit
      # wrapper with named arguments avoids relying on positional order
      # matching between the two functions.
      errors <- unlist(
        purrr::imap(tbls, function(tbl_state, file_name) {
          validate_table_fields(file_name = file_name, tbl_state = tbl_state)
        }),
        use.names = FALSE
      )
      if (is.null(errors)) errors <- character(0)

      list(
        tables = tbls,
        custom_units = custom_units(),
        valid = length(errors) == 0,
        errors = errors
      )
    })

    # Record saved Tab 4 field metadata to watch for on future
    # table-matches. Does NOT touch `state` directly - actual
    # restoration only happens inside the observeEvent(tables_reactive())
    # block above, once a table's default state has just been built AND
    # a pending saved entry exists for it (same gating pattern as Tab 3's
    # restore()/pending_restore_metadata).
    #
    # @param saved named list keyed by file_name, matching
    #   default_app_state()$fields$tables's shape:
    #   list(<file_name> = list(attributes = <list-of-columns>,
    #                            catvars = <list-of-columns>))
    #
    # @param saved_custom_units saved custom units (list of columns), or
    #   NULL when the saved state has none. Custom units do not depend on
    #   any uploaded .csv, so they are restored at once - and a NULL clears
    #   whatever was defined before, like the rest of a load/import.
    restore <- function(saved, rebuild = FALSE, saved_custom_units = NULL) {
      custom_units(restore_custom_units_tbl(saved_custom_units))
      if (isTRUE(rebuild)) {
        for (nm in names(shiny::reactiveValuesToList(state))) state[[nm]] <- NULL
        last_restore_summary(NULL)
        restore_epoch(shiny::isolate(restore_epoch()) + 1L)
      }
      if (is.null(saved) || length(saved) == 0) {
        pending_restore_fields(NULL)
        return(invisible(NULL))
      }
      pending_restore_fields(saved)
      invisible(NULL)
    }

    # @return character vector of file_names with saved field metadata
    #   that hasn't yet been matched to a re-uploaded table. Empty if
    #   nothing pending.
    unmatched_saved_files <- shiny::reactive({
      pending <- pending_restore_fields()
      if (is.null(pending)) character(0) else names(pending)
    })

    list(data = data, restore = restore, unmatched_saved_files = unmatched_saved_files)
  })
}

#' Emit the R code chunks for attributes + categorical variables. Pure
#' function - writes the .txt template files' *content* is actually produced
#' by EMLassemblyline itself (write.file = TRUE writes blank templates), so
#' what we emit here is:
#'   1. the template_table_attributes()/template_categorical_variables() calls
#'      (matching skeleton.Rmd), AND
#'   2. a post-processing chunk that programmatically overwrites those blank
#'      templates with the user's actual entries from the app - this is what
#'      lets the app's captured metadata flow into the files EMLassemblyline
#'      expects, without requiring the user to re-type everything in Excel.
#'
#' @param state the $tables element of fieldsServer()'s $data reactive
#'   result: list(<file_name> = list(attributes = tibble, catvars = tibble))
#' @param working_folder_var name of the working folder variable in-script
emit_fields_chunk <- function(state, working_folder_var = "working_folder") {
  if (is.null(state) || length(state) == 0) {
    return("# No field/attribute metadata captured.\n")
  }

  header <- glue::glue(
    'EMLassemblyline::template_table_attributes(\n',
    '  path = {working_folder_var},\n',
    '  data.table = data_files,\n',
    '  write.file = TRUE\n',
    ')\n\n',
    'EMLassemblyline::template_categorical_variables(\n',
    '  path = {working_folder_var},\n',
    '  data.path = {working_folder_var},\n',
    '  write.file = TRUE\n',
    ')\n\n',
    '# The two calls above generate blank templates. The chunk below\n',
    '# overwrites them with the values captured in the app UI.\n'
  )

  write_chunks <- purrr::imap_chr(state, function(tbl_state, file_name) {
    attrs_tsv <- tibble_to_r_tribble(normalize_missing_value_pair(tbl_state$attributes))
    catvars_tsv <- tibble_to_r_tribble(tbl_state$catvars)
    base_name <- strip_extension(file_name)

    glue::glue(
      '\n# --- {file_name} ---\n',
      'attributes_df <- {attrs_tsv}\n',
      'readr::write_tsv(attributes_df, file.path({working_folder_var}, "attributes_{base_name}.txt"), na = "")\n\n',
      'catvars_df <- {catvars_tsv}\n',
      'if (nrow(catvars_df) > 0) {{\n',
      '  readr::write_tsv(catvars_df, file.path({working_folder_var}, "catvars_{base_name}.txt"), na = "")\n',
      '}}\n'
    )
  })

  paste0(header, paste(write_chunks, collapse = "\n"))
}

#' Emit the R code chunk that recreates custom_units.txt in the script.
#' Pure function. Returns "" when there are no custom units, so no custom
#' unit code appears in scripts for packages that do not use the feature.
#'
#' @param custom_units tibble(id, unitType, parentSI, multiplierToSI,
#'   description), or NULL
#' @param working_folder_var name of the working folder variable in-script
#' @return character - the R code chunk, or "" if there is nothing to write
#' @noRd
emit_custom_units_chunk <- function(custom_units, working_folder_var = "working_folder") {
  if (is.null(custom_units) || nrow(custom_units) == 0) return("")

  units_tsv <- tibble_to_r_tribble(custom_units)

  glue::glue(
    '# --- Custom units ---\n',
    '# make_eml() reads custom_units.txt from the working folder and writes\n',
    '# these definitions into <additionalMetadata><unitList>. A numeric\n',
    '# attribute may use any `id` below as its unit.\n',
    'custom_units_df <- {units_tsv}\n',
    'readr::write_tsv(custom_units_df, file.path({working_folder_var}, "custom_units.txt"), na = "")\n'
  )
}

#' Write custom_units.txt into the working folder, first deleting any copy
#' left by an earlier Generate - make_eml() reads whatever file is there, so
#' a stale one would keep units the user has since removed. Nothing is
#' written when there are no custom units.
#'
#' @param custom_units tibble(id, unitType, parentSI, multiplierToSI,
#'   description), or NULL
#' @param working_folder the data_package_creation directory
#' @return invisible logical - TRUE if a file was written
#' @noRd
write_custom_units_file <- function(custom_units, working_folder) {
  units_file <- file.path(working_folder, "custom_units.txt")
  if (file.exists(units_file)) file.remove(units_file)

  if (is.null(custom_units) || nrow(custom_units) == 0) return(invisible(FALSE))

  readr::write_tsv(custom_units, units_file, na = "")
  invisible(TRUE)
}

# Helper: render a tibble as an R tibble::tribble(...) literal so the
# generated script is self-contained and reproducible without needing the
# app's internal state. Uses deparse() for every value so embedded quotes,
# backslashes, and newlines in free-text fields (e.g. attributeDefinition)
# are handled correctly rather than hand-escaped.
tibble_to_r_tribble <- function(df) {
  col_headers <- paste(sprintf("~%s", names(df)), collapse = ", ")

  if (nrow(df) == 0) {
    return(glue::glue("tibble::tribble({col_headers})"))
  }

  rows <- apply(df, 1, function(row) {
    vals <- purrr::map_chr(row, function(v) {
      if (is.na(v)) "NA_character_" else deparse(as.character(v))
    })
    paste(vals, collapse = ", ")
  })

  glue::glue(
    "tibble::tribble(\n",
    "  {col_headers},\n",
    "  {paste(rows, collapse = ',\\n  ')}\n",
    ")"
  )
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && a == "")) b else a
