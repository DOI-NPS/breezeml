tableMetadataUI <- function(id) {
  layout_columns(
    card(
      card_header("Upload data tables"),
      fileInput(NS(id, "upload"),
                NULL,
                buttonLabel = "Add your .csv files",
                multiple = TRUE,
                accept = (".csv"), 
                width = "100%"),
      actionButton(NS(id, "remove_all"), "Remove all files",
                   class = "btn-outline-danger btn-sm"),
      helpText("Removing files clears all uploaded tables so you can start ",
               "over with a new set. This also clears any field, geography, ",
               "or taxonomy selections that depended on those tables.")
    ),
    card(
      card_header("Fill in table metadata"),
      helpText("Double-click inside the table to add table names and descriptions. Use ctrl+enter to save your edits."),
      helpText(class = "text-muted",
               "Both Name and Description are required for every file before ",
               "a script can be generated."),
      DT::DTOutput(NS(id, "file_table")),
      uiOutput(NS(id, "table_meta_status"))
    ),
    col_widths = c(-2, 8, -2), fill = FALSE
  )
}

#' @return a reactive() list with:
#'   $metadata - tibble of file_name, table_name, description, size_mb, file_loc
#'   $data     - named list of data.frames (names = original file names),
#'               the actual parsed CSV contents for use by downstream modules
#'               (geography, taxonomy, fields/attributes)
tableMetadataServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    uploaded_files <- reactiveVal(tibble::tibble(
      file_name = character(),
      table_name = character(),
      description = character(),
      size_mb = numeric(),
      file_loc = character()
    ))
    
    # named list of parsed data frames, keyed by file_name. Built up
    # incrementally so we don't re-read files that are already loaded.
    parsed_data <- reactiveVal(list())
    
    observeEvent(input$upload, {
      current <- uploaded_files()
      
      new_files <- input$upload %>%
        dplyr::select(file_name = name,
                      size,
                      file_loc = datapath) %>%
        dplyr::mutate(size_mb = round(size / 1024),
                      table_name = NA_character_,
                      description = NA_character_) %>%
        dplyr::select(file_name, table_name, description, size_mb, file_loc)
      
      dups <- intersect(new_files$file_name, current$file_name)
      
      if (length(dups) > 0) {
        showModal(
          modalDialog(
            title = "WARNING: Duplicate Files",
            easyClose = FALSE,
            p(glue::glue(
              "You cannot upload multiple files with the same name. ",
              "Ignoring the following duplicates: {toString(dups)}"
            )),
            footer = modalButton("Dismiss")
          )
        )
        new_files <- dplyr::filter(new_files, !(file_name %in% dups))
      }
      
      if (nrow(new_files) == 0) return(invisible(NULL))
      
      # parse the genuinely new files and merge into parsed_data
      newly_parsed <- purrr::map(new_files$file_loc, function(path) {
        tryCatch(
          readr::read_csv(path, show_col_types = FALSE),
          error = function(e) {
            showNotification(
              paste0("Failed to parse file: ", conditionMessage(e)),
              type = "error"
            )
            NULL
          }
        )
      }) %>% purrr::set_names(new_files$file_name)
      
      # drop any that failed to parse, and their metadata rows too
      failed <- names(newly_parsed)[purrr::map_lgl(newly_parsed, is.null)]
      if (length(failed) > 0) {
        newly_parsed[failed] <- NULL
        new_files <- dplyr::filter(new_files, !(file_name %in% failed))
      }
      
      parsed_data(c(parsed_data(), newly_parsed))
      uploaded_files(rbind(current, new_files))
    })
    
    observeEvent(input$remove_all, {
      showModal(
        modalDialog(
          title = "Remove all files?",
          "This will remove all uploaded data tables and any field, ",
          "geography, or taxonomy selections that reference them. This ",
          "cannot be undone.",
          easyClose = FALSE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton(session$ns("confirm_remove_all"), "Remove all",
                         class = "btn-danger")
          )
        )
      )
    })
    
    observeEvent(input$confirm_remove_all, {
      uploaded_files(tibble::tibble(
        file_name = character(),
        table_name = character(),
        description = character(),
        size_mb = numeric(),
        file_loc = character()
      ))
      parsed_data(list())
      removeModal()
      showNotification("All uploaded files removed.", type = "message")
    })
    
    output$file_table <- DT::renderDT(
      uploaded_files(),
      selection = "none",
      server = FALSE,
      rownames = FALSE,
      editable = list(
        target = "all",
        disable = list(columns = 0)
      ),
      options = list(dom = 't',
                     columnDefs = list(
                       list(
                         targets = c("file_loc", "size_mb"),
                         visible = FALSE
                       )
                     )
      ),
      colnames = c("File name",
                   "Name",
                   "Description",
                   "Size (MB)",
                   "Path")
    )
    
    observeEvent(input$file_table_cell_edit, {
      updated <- DT::editData(uploaded_files(), input$file_table_cell_edit,
                              rownames = FALSE)
      uploaded_files(updated)
    })
    
    validity <- reactive({
      df <- uploaded_files()
      if (nrow(df) == 0) {
        return(list(valid = FALSE, errors = "At least one data table must be uploaded."))
      }
      missing_name <- df$file_name[is.na(df$table_name) | !nzchar(trimws(df$table_name %||% ""))]
      missing_desc <- df$file_name[is.na(df$description) | !nzchar(trimws(df$description %||% ""))]
      
      errors <- character(0)
      if (length(missing_name) > 0) {
        errors <- c(errors, paste0("Missing table Name for: ", paste(missing_name, collapse = ", ")))
      }
      if (length(missing_desc) > 0) {
        errors <- c(errors, paste0("Missing Description for: ", paste(missing_desc, collapse = ", ")))
      }
      list(valid = length(errors) == 0, errors = errors)
    })
    
    output$table_meta_status <- renderUI({
      v <- validity()
      if (isTRUE(v$valid)) {
        return(tags$div(class = "text-success", "All tables have name and description."))
      }
      tagList(
        tags$div(class = "text-danger", "Resolve before generating:"),
        tags$ul(lapply(v$errors, tags$li))
      )
    })
    
    `%||%` <- function(a, b) if (is.null(a)) b else a
    
    reactive({
      list(
        metadata = uploaded_files(),
        data = parsed_data(),
        valid = validity()$valid,
        errors = validity()$errors
      )
    })
  })
}