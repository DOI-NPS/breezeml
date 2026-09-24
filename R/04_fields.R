# 04_fields.R v20
#
# FIXED: fieldsServer()'s reactive never computed or returned $valid/
# $errors at all - every other tab's module follows the pattern
# list(..., valid = length(errors) == 0, errors = errors), but this one
# simply returned reactiveValuesToList(state) with nothing else. Combined
# with app_server.R's all_errors() never even including fields() in its
# aggregation, this meant Tab 4 could NEVER block Generate, regardless of
# content - confirmed via live testing: a numeric column with no unit
# (required by EMLassemblyline/EML schema) was silently accepted, the
# Generate tab reported "All required information is complete," and the
# resulting EML was schema-invalid AND silently dropped that entire data
# table from <dataTable> in the output.
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
# NOTE: app_server.R's all_errors() must ALSO be updated to include
# fields() in its aggregation - this file alone is not sufficient to
# restore the Generate gate. See accompanying app_server.R update.
#
# Added @noRd to fieldsServer() - internal Shiny module server, not meant
# to have a public help page. Resolves roxygen2's "Skipping; no name
# and/or title" note.
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
get_unit_choices <- function() {
  units <- tryCatch({
    ul <- EML::get_unitList()
    ul$units$id
  }, error = function(e) NULL)

  if (is.null(units) || length(units) == 0) {
    shiny::showNotification(
      paste0("Could not load the full EML unit dictionary from EML::get_unitList() - ",
             "falling back to a short list of common units. Some valid units may be rejected."),
      type = "warning", duration = 10
    )
    # small, genuinely-valid fallback list, matching EML::get_unitList()'s
    # actual `id` spellings (not invented names)
    return(c("meter", "centimeter", "kilometer", "gram", "kilogram",
             "hectare", "squareMeter", "celsius", "percent",
             "number", "dimensionless"))
  }

  sort(unique(units))
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

# Build the initial attributes tibble for one data.frame
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
      shiny::uiOutput(ns("table_tabs"))
    ),
    col_widths = c(-1, 10, -1), fill = FALSE
  )
}

#' @param tables_reactive reactive() named list of data.frames (from Tab 3)
#' @return reactive() list:
#'   $tables - named list, one entry per table:
#'     list(<file_name> = list(attributes = tibble, catvars = tibble))
#'   $valid - logical
#'   $errors - character vector, empty if valid
#' @noRd
fieldsServer <- function(id, tables_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    unit_choices <- get_unit_choices()
    class_choices <- c("numeric", "character", "categorical", "Date")

    # store per-table state: reactiveValues keyed by table name, each holding
    # $attributes and $catvars tibbles
    state <- shiny::reactiveValues()

    shiny::observeEvent(tables_reactive(), {
      tbls <- tables_reactive()
      for (nm in names(tbls)) {
        if (is.null(state[[nm]])) {
          attrs <- build_attributes_tibble(tbls[[nm]])
          state[[nm]] <- list(
            attributes = attrs,
            catvars = build_catvars_tibble(tbls[[nm]], attrs)
          )
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
              editable = list(target = "cell", disable = list(columns = 0))
            )
          })

          shiny::observeEvent(input[[paste0(attrs_id, "_cell_edit")]], {
            edit <- input[[paste0(attrs_id, "_cell_edit")]]
            current <- state[[table_name]]$attributes
            updated <- DT::editData(current, edit, rownames = FALSE)
            updated <- normalize_missing_value_pair(updated)

            col_edited <- names(updated)[edit$col + 1]

            if (col_edited == "class") {
              bad <- !updated$class %in% class_choices
              if (any(bad)) {
                shiny::showNotification(
                  paste0("Class must be one of: ", paste(class_choices, collapse = ", ")),
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

    shiny::reactive({
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
#' @param state list as returned by fieldsServer()'s reactive - specifically
#'   its $tables element: list(<file_name> = list(attributes = tibble, catvars = tibble))
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
