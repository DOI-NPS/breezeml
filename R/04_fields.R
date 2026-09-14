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

# EMLassemblyline unit dictionary is large; in the running app this should be
# loaded once via EMLassemblyline::view_unit_dictionary() and cached. We
# expose a loader function so the UI/server can call it without hard-coding
# the dependency at source-time (keeps this file loadable/testable without
# the package installed, e.g. in CI).
get_unit_choices <- function() {
  tryCatch({
    dict <- EMLassemblyline::view_unit_dictionary()
    sort(unique(dict$id))
  }, error = function(e) {
    # fallback so the UI doesn't break if the dictionary can't be loaded
    c("meter", "centimeter", "kilometer", "gram", "kilogram",
      "hectare", "squareMeter", "degreeCelsius", "percent",
      "number", "dimensionless", "date")
  })
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
    
    # dynamically wire up DT render + edit handling for each table. This runs
    # inside an observer keyed off the table list so it re-registers cleanly
    # if tables are added/removed.
    observeEvent(tables_reactive(), {
      tbls <- tables_reactive()
      
      for (nm in names(tbls)) {
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
            
            # validate class column edits
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
            
            state[[table_name]]$attributes <- updated
            
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
    attrs_tsv <- tibble_to_r_tribble(tbl_state$attributes)
    catvars_tsv <- tibble_to_r_tribble(tbl_state$catvars)
    
    glue::glue(
      '\n# --- {file_name} ---\n',
      'attributes_df <- {attrs_tsv}\n',
      'readr::write_tsv(attributes_df, file.path({working_folder_var}, "attributes_{file_name}.txt"))\n\n',
      'catvars_df <- {catvars_tsv}\n',
      'if (nrow(catvars_df) > 0) {{\n',
      '  readr::write_tsv(catvars_df, file.path({working_folder_var}, "catvars_{file_name}.txt"))\n',
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