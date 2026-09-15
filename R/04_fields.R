# 04_fields.R
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

# EMLassemblyline unit dictionary is large; in the running app this should be
# loaded once via EMLassemblyline::view_unit_dictionary() and cached. We
# expose a loader function so the UI/server can call it without hard-coding
# the dependency at source-time (keeps this file loadable/testable without
# the package installed, e.g. in CI).
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
    showNotification(
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
  ns <- NS(id)
  layout_columns(
    card(
      card_header("Field (attribute) metadata"),
      helpText("For each data table, describe every column: what it means, ",
               "its data class, and (for numeric columns) its unit of ",
               "measurement. Columns auto-flagged as 'categorical' need a ",
               "definition for each unique code below - review the ",
               "auto-detected class column and adjust if needed."),
      uiOutput(ns("table_tabs"))
    ),
    col_widths = c(-1, 10, -1), fill = FALSE
  )
}

#' @param tables_reactive reactive() named list of data.frames (from Tab 3)
#' @return reactive() named list, one entry per table:
#'   list(<file_name> = list(attributes = tibble, catvars = tibble))
fieldsServer <- function(id, tables_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    unit_choices <- get_unit_choices()
    class_choices <- c("numeric", "character", "categorical", "Date")
    
    # store per-table state: reactiveValues keyed by table name, each holding
    # $attributes and $catvars tibbles
    state <- reactiveValues()
    
    observeEvent(tables_reactive(), {
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
      removed <- setdiff(names(reactiveValuesToList(state)), names(tbls))
      for (nm in removed) state[[nm]] <- NULL
      
      # also forget that removed tables were "registered" - if a file with
      # the same name is uploaded again later, its observers/outputs need
      # to be wired up fresh (their old renderUI-generated DOM elements are
      # gone once the table list shrinks and table_tabs re-renders)
      registered_tables <<- setdiff(registered_tables, removed)
    })
    
    output$table_tabs <- renderUI({
      tbls <- tables_reactive()
      req(length(tbls) > 0)
      
      tabs <- lapply(names(tbls), function(nm) {
        safe_id <- gsub("[^A-Za-z0-9_]", "_", nm)
        nav_panel(
          title = nm,
          br(),
          h5("Attributes"),
          DT::DTOutput(ns(paste0("attrs_", safe_id))),
          hr(),
          h5("Categorical codes"),
          helpText("Only columns currently marked 'categorical' above appear here. ",
                   "Codes are pulled from the data; add definitions for each."),
          DT::DTOutput(ns(paste0("catvars_", safe_id)))
        )
      })
      
      do.call(navset_tab, tabs)
    })
    
    # dynamically wire up DT render + edit handling for each table. Each
    # table's observers/outputs must be created EXACTLY ONCE - this observer
    # can fire many times over the app's life (any change to the table
    # list), so we track which tables already have their observers
    # registered and skip re-registering them. Without this guard, editing
    # a cell would fire its handler once per prior invocation of this
    # block, producing duplicate notifications that multiply over time.
    registered_tables <- character(0)
    
    observeEvent(tables_reactive(), {
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
            req(state[[table_name]])
            DT::datatable(
              state[[table_name]]$attributes,
              rownames = FALSE,
              selection = "none",
              options = list(dom = 't', pageLength = -1, scrollX = TRUE),
              editable = list(target = "cell", disable = list(columns = 0))
            )
          })
          
          observeEvent(input[[paste0(attrs_id, "_cell_edit")]], {
            edit <- input[[paste0(attrs_id, "_cell_edit")]]
            current <- state[[table_name]]$attributes
            updated <- DT::editData(current, edit, rownames = FALSE)
            updated <- normalize_missing_value_pair(updated)
            
            col_edited <- names(updated)[edit$col + 1]
            
            if (col_edited == "class") {
              bad <- !updated$class %in% class_choices
              if (any(bad)) {
                showNotification(
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
                showNotification(
                  "Unit only applies to numeric columns. Set class to 'numeric' first.",
                  type = "error"
                )
                updated$unit[edited_row] <- current$unit[edited_row]
              } else if (new_class == "numeric" && (is.na(new_unit) || !nzchar(new_unit))) {
                # blank is allowed transiently while typing/clearing - only
                # reject non-blank values that aren't in the dictionary
              } else if (!is.na(new_unit) && nzchar(new_unit) && !(new_unit %in% unit_choices)) {
                showNotification(
                  paste0("'", new_unit, "' is not a recognized EML unit. ",
                         "See EMLassemblyline::view_unit_dictionary() for valid options. ",
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
              showNotification(
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
            req(state[[table_name]])
            DT::datatable(
              state[[table_name]]$catvars,
              rownames = FALSE,
              selection = "none",
              options = list(dom = 't', pageLength = -1, scrollX = TRUE),
              editable = list(target = "cell", disable = list(columns = c(0, 1)))
            )
          })
          
          observeEvent(input[[paste0(catvars_id, "_cell_edit")]], {
            edit <- input[[paste0(catvars_id, "_cell_edit")]]
            updated <- DT::editData(state[[table_name]]$catvars, edit, rownames = FALSE)
            state[[table_name]]$catvars <- updated
          })
        })
      }
    })
    
    reactive({
      out <- reactiveValuesToList(state)
      out
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
#' @param state named list as returned by fieldsServer(), i.e.
#'   list(<file_name> = list(attributes = tibble, catvars = tibble))
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