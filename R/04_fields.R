# 04_fields.R v21
#
# v21: added restore() to fieldsServer()'s return value, for the
# Save/Load session feature. Hardest of the four dependent tabs, per
# explicit design decision worked through with the user:
#   - new columns in a re-uploaded CSV not in saved state -> left as
#     fresh auto-built defaults (normal unconfigured-column treatment),
#     with a notification listing them
#   - columns in saved state but missing from the re-uploaded CSV ->
#     NOT restored (nothing to attach to); notification lists them so
#     the user knows their saved definitions weren't applied
#   - new categorical levels/codes not in saved catvars -> fresh
#     blank-definition rows, same as first upload for that value
#   - categorical codes in saved state no longer present in the data ->
#     silently dropped (no notification - least consequential case)
#
# Mechanically: restore() does NOT run at load time - fields' state
# (state[[table_name]]) only gets built once a table exists in
# tables_reactive() (see the existing observeEvent(tables_reactive())
# block below, unchanged). restore() therefore hooks into that SAME
# observer's aftermath: it stashes saved per-table attributes/catvars in
# pending_restore_fields, and a second observeEvent(tables_reactive())
# (registered by restore()'s setup, effectively) checks, for every table
# that just got its default state built AND has a pending saved entry,
# whether to overwrite specific fields by attributeName/[attributeName,code]
# match rather than replacing the tibble wholesale.
#
# Overwritable per-attribute fields: attributeDefinition, class, unit,
# dateTimeFormatString, missingValueCode, missingValueCodeExplanation.
# Restoring `class` from saved state (rather than trusting the freshly
# auto-inferred class) matters because the user may have manually
# corrected the auto-inferred class before saving (e.g. a numeric-looking
# ID column the user marked "character") - re-inferring on every reload
# would silently discard that correction. After restoring class,
# catvars is rebuilt from the (now-restored) class column via the same
# build_catvars_tibble() used elsewhere, THEN saved catvars definitions
# are matched onto it by (attributeName, code) - mirroring the existing
# "class changed -> rebuild catvars" logic already in the cell-edit
# observer below.
#
# fieldsServer()'s return value changes from a bare reactive() to
# list(data = <reactive>, restore = <function>,
#      unmatched_saved_files = <reactive>) - matching the shape
# established for 03_data_tables.R's tableMetadataServer(), since Tab 4
# has the exact same "gated on re-upload" structure.
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

CATEGORICAL_MAX_LEVELS <- 20

# Valid values for the "class" column - now a fixed factor level set, not
# free text. Defined at top level (not inside fieldsServer()) since
# build_attributes_tibble() needs it too, and is called before any server
# function runs (from fieldsServer()'s own observeEvent, but conceptually
# a pure/standalone builder).
CLASS_CHOICES <- c("numeric", "character", "categorical", "Date")

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
    updated$attributeDefinition[idx] <- saved_row$attributeDefinition
    updated$class[idx] <- saved_row$class
    updated$unit[idx] <- saved_row$unit
    updated$dateTimeFormatString[idx] <- saved_row$dateTimeFormatString
    updated$missingValueCode[idx] <- saved_row$missingValueCode
    updated$missingValueCodeExplanation[idx] <- saved_row$missingValueCodeExplanation
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
  updated$definition[has_match] <- saved_tbl$definition[match_idx[has_match]]

  updated
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && a == "")) b else a

fieldsUI <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    bslib::card(
      bslib::card_header("Field (attribute) metadata"),
      shiny::helpText("For each data table, describe every column: what it means, ",
                      "its data class, and (for numeric columns) its unit of ",
                      "measurement. Columns auto-flagged as 'categorical' need a ",
                      "definition for each unique code below - review the ",
                      "auto-detected class column and adjust if needed."),
      shiny::actionLink(ns("open_unit_lookup"), "Look up valid units",
                        icon = shiny::icon("magnifying-glass")),
      shiny::uiOutput(ns("restore_notice")),
      shiny::uiOutput(ns("table_tabs"))
    ),
    col_widths = c(-1, 10, -1), fill = FALSE
  )
}

#' @param tables_reactive reactive() named list of data.frames (from Tab 3)
#' @return list(data = <reactive() list>, restore = <function>,
#'   unmatched_saved_files = <reactive() character vector>)
#'   data() returns:
#'     $tables - named list, one entry per table:
#'       list(<file_name> = list(attributes = tibble, catvars = tibble))
#'     $valid - logical
#'     $errors - character vector, empty if valid
#' @noRd
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

    # Saved per-table attributes/catvars awaiting a matching re-uploaded
    # table - see file header note. NULL until restore() is called; a
    # table's entry is removed once that table's default state has been
    # built AND the saved values have been matched onto it.
    pending_restore_fields <- shiny::reactiveVal(NULL)

    # Tables whose defaults were just (re)built this round, and for which
    # a saved-state match was attempted - used to build restore_notice's
    # new/missing-column summary. Cleared each time it's consumed.
    last_restore_summary <- shiny::reactiveVal(NULL)

    shiny::observeEvent(tables_reactive(), {
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
          shiny::tags$strong("Re-upload needed to finish restoring saved field metadata for: "),
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
            shiny::tags$strong("Column(s) from your saved session were not found in the re-uploaded file - their saved definitions were not restored: "),
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

          output[[attrs_id]] <- DT::renderDT({
            shiny::req(state[[table_name]])
            DT::datatable(
              state[[table_name]]$attributes,
              rownames = FALSE,
              selection = "none",
              options = list(dom = 't', pageLength = -1, scrollX = TRUE),
              # attributeName (col 0) stays locked/read-only.
              editable = list(target = "cell", disable = list(columns = 0)),
              # "class" (col index 2, 0-based) gets an HTML5 <datalist>
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
              callback = DT::JS(sprintf(
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
              ))
            )
          })

          shiny::observeEvent(input[[paste0(attrs_id, "_cell_edit")]], {
            edit <- input[[paste0(attrs_id, "_cell_edit")]]
            current <- state[[table_name]]$attributes
            updated <- DT::editData(current, edit, rownames = FALSE)
            updated <- normalize_missing_value_pair(updated)

            col_edited <- names(updated)[edit$col + 1]

            if (col_edited == "class") {
              bad <- !updated$class %in% CLASS_CHOICES
              if (any(bad)) {
                shiny::showNotification(
                  paste0("Class must be one of: ", paste(CLASS_CHOICES, collapse = ", ")),
                  type = "error"
                )
                updated$class[bad] <- current$class[bad]
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
              } else if (new_class == "numeric" && (is.na(new_unit) || !nzchar(new_unit))) {
                # blank is allowed transiently while typing/clearing - only
                # reject non-blank values that aren't in the dictionary
              } else if (!is.na(new_unit) && nzchar(new_unit) && !(new_unit %in% unit_choices)) {
                shiny::showNotification(
                  paste0("'", new_unit, "' is not a recognized EML unit. ",
                         "See EML::get_unitList() for valid options. ",
                         "Value reverted."),
                  type = "error"
                )
                updated$unit[edited_row] <- current$unit[edited_row]
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

            # if class changed to/from categorical, rebuild catvars for this table
            if (col_edited == "class") {
              df <- tables_reactive()[[table_name]]
              state[[table_name]]$catvars <- build_catvars_tibble(df, updated)
            }
          })

          output[[catvars_id]] <- DT::renderDT({
            shiny::req(state[[table_name]])
            DT::datatable(
              state[[table_name]]$catvars,
              rownames = FALSE,
              selection = "none",
              options = list(dom = 't', pageLength = -1, scrollX = TRUE),
              editable = list(target = "cell", disable = list(columns = c(0, 1)))
            )
          })

          shiny::observeEvent(input[[paste0(catvars_id, "_cell_edit")]], {
            edit <- input[[paste0(catvars_id, "_cell_edit")]]
            updated <- DT::editData(state[[table_name]]$catvars, edit, rownames = FALSE)
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
        valid = length(errors) == 0,
        errors = errors
      )
    })

    #' Record saved Tab 4 field metadata to watch for on future
    #' table-matches. Does NOT touch `state` directly - actual
    #' restoration only happens inside the observeEvent(tables_reactive())
    #' block above, once a table's default state has just been built AND
    #' a pending saved entry exists for it (same gating pattern as Tab 3's
    #' restore()/pending_restore_metadata).
    #'
    #' @param saved named list keyed by file_name, matching
    #'   default_app_state()$fields$tables's shape:
    #'   list(<file_name> = list(attributes = <list-of-columns>,
    #'                            catvars = <list-of-columns>))
    restore <- function(saved) {
      if (is.null(saved) || length(saved) == 0) {
        pending_restore_fields(NULL)
        return(invisible(NULL))
      }
      pending_restore_fields(saved)
      invisible(NULL)
    }

    #' @return character vector of file_names with saved field metadata
    #'   that hasn't yet been matched to a re-uploaded table. Empty if
    #'   nothing pending.
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
