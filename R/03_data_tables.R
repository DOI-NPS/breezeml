tableMetadataUI <- function(id) {
  layout_columns(
    card(
      card_header("Upload data tables"),
      fileInput(NS(id, "upload"),
                NULL,
                buttonLabel = "Add your .csv files",
                multiple = TRUE,
                accept = (".csv"), 
                width = "100%")
    ),
    card(
      card_header("Fill in table metadata"),
      helpText("Double-click inside the table to add table names and descriptions. Use ctrl+enter to save your edits."),
      DT::DTOutput(NS(id, "file_table"))
    ),
    col_widths = c(-2, 8, -2), fill = FALSE
  )
}

tableMetadataServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    uploaded_files <- tibble::tibble()
    
    table_metadata <- reactive({
      req(input$upload)
      
      # wrangle table metadata from file upload
      new_files <- input$upload %>%
        dplyr::select(file_name = name,
                      size,
                      file_loc = datapath) %>%
        dplyr::mutate(size_mb = round(size/1024),
                      table_name = NA,
                      description = NA) %>%
        dplyr::select(file_name, table_name, description, size_mb, file_loc)
      
      if (nrow(uploaded_files) > 0 && any(new_files$file_name %in% uploaded_files$file_name)) {
        dups <- new_files$file_name[new_files$file_name %in% uploaded_files$file_name]
        
        if (length(dups) == nrow(new_files)) {
          new_files <- NULL
        } else {
          new_files <- dplyr::filter(new_files, !(file_name %in% dups))
        }
        
        showModal(
          modalDialog(
            title = "WARNING: Duplicate Files",
            easyClose = FALSE, 
            p(glue::glue("You cannot upload multiple files with the same name. Ignoring the following duplicates: {toString(dups)}")),
            footer = modalButton("Dismiss")
          )
        )
      }
      uploaded_files <<- rbind(uploaded_files, new_files)
    })
    output$file_table <- DT::renderDT(
      table_metadata(),
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
                   "Description"
      )
    )
    data <- reactive({
      req(input$upload)
      purrr::map(input$upload$datapath, readr::read_csv) %>%
        purrr::set_names(input$upload$name)
    })
    
    reactive({
      DT::editData(uploaded_files, input$file_table_cell_edit, rownames = FALSE)
      DT::editData(table_metadata(), input$file_table_cell_edit, rownames = FALSE)
    })
  })
}



