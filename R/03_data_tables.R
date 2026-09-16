# 03_data_tables.R v14
#
# Added @noRd to tableMetadataServer() - internal Shiny module server, not
# meant to have a public help page. Resolves roxygen2's "Skipping; no
# name and/or title" note.

tableMetadataUI <- function(id) {
  bslib::layout_columns(
    bslib::card(
      bslib::card_header("Upload data tables"),
      shiny::fileInput(shiny::NS(id, "upload"),
                       NULL,
                       buttonLabel = "Add your .csv files",
                       multiple = TRUE,
                       accept = (".csv"),
                       width = "100%"),
      shiny::actionButton(shiny::NS(id, "remove_all"), "Remove all files",
                          class = "btn-outline-danger btn-sm"),
      shiny::helpText("Removing files clears all uploaded tables so you can start ",
                      "over with a new set. This also clears any field, geography, ",
                      "or taxonomy selections that depended on those tables.")
    ),
    bslib::card(
      bslib::card_header("Fill in table metadata"),
      shiny::helpText("Double-click a Name or Description cell to edit it, then ",
                      "press Enter or click elsewhere to save."),
      shiny::helpText(class = "text-muted",
                      "Both Name and Description are required for every file before ",
                      "a script can be generated."),
      DT::DTOutput(shiny::NS(id, "file_table")),
      shiny::uiOutput(shiny::NS(id, "table_meta_status"))
    ),
    col_widths = c(-2, 8, -2), fill = FALSE
  )
}

#' @return a reactive() list with:
#'   $metadata - tibble of file_name, table_name, description, size_mb, file_loc
#'   $data     - named list of data.frames (names = original file names),
#'               the actual parsed CSV contents for use by downstream modules
#'               (geography, taxonomy, fields/attributes)
#'   $valid, $errors - every table must have a non-blank Name and Description
#' @noRd
tableMetadataServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    uploaded_files <- shiny::reactiveVal(tibble::tibble(
      file_name = character(),
      table_name = character(),
      description = character(),
      size_mb = numeric(),
      file_loc = character()
    ))

    # named list of parsed data frames, keyed by file_name. Built up
    # incrementally so we don't re-read files that are already loaded.
    parsed_data <- shiny::reactiveVal(list())

    shiny::observeEvent(input$upload, {
      current <- uploaded_files()

      new_files <- input$upload |>
        dplyr::select(file_name = name,
                      size,
                      file_loc = datapath) |>
        dplyr::mutate(size_mb = round(size / 1024),
                      table_name = NA_character_,
                      description = NA_character_) |>
        dplyr::select(file_name, table_name, description, size_mb, file_loc)

      dups <- intersect(new_files$file_name, current$file_name)

      if (length(dups) > 0) {
        shiny::showModal(
          shiny::modalDialog(
            title = "WARNING: Duplicate Files",
            easyClose = FALSE,
            shiny::p(glue::glue(
              "You cannot upload multiple files with the same name. ",
              "Ignoring the following duplicates: {toString(dups)}"
            )),
            footer = shiny::modalButton("Dismiss")
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
            shiny::showNotification(
              paste0("Failed to parse file: ", conditionMessage(e)),
              type = "error"
            )
            NULL
          }
        )
      }) |> purrr::set_names(new_files$file_name)

      # drop any that failed to parse, and their metadata rows too
      failed <- names(newly_parsed)[purrr::map_lgl(newly_parsed, is.null)]
      if (length(failed) > 0) {
        newly_parsed[failed] <- NULL
        new_files <- dplyr::filter(new_files, !(file_name %in% failed))
      }

      parsed_data(c(parsed_data(), newly_parsed))
      uploaded_files(rbind(current, new_files))
    })

    shiny::observeEvent(input$remove_all, {
      shiny::showModal(
        shiny::modalDialog(
          title = "Remove all files?",
          "This will remove all uploaded data tables and any field, ",
          "geography, or taxonomy selections that reference them. This ",
          "cannot be undone.",
          easyClose = FALSE,
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton(session$ns("confirm_remove_all"), "Remove all",
                                class = "btn-danger")
          )
        )
      )
    })

    shiny::observeEvent(input$confirm_remove_all, {
      uploaded_files(tibble::tibble(
        file_name = character(),
        table_name = character(),
        description = character(),
        size_mb = numeric(),
        file_loc = character()
      ))
      parsed_data(list())
      shiny::removeModal()
      shiny::showNotification("All uploaded files removed.", type = "message")
    })

    output$file_table <- DT::renderDT(
      uploaded_files(),
      selection = "none",
      server = FALSE,
      rownames = FALSE,
      editable = list(
        target = "cell",
        disable = list(columns = c(0, 3, 4))  # file_name, size_mb, file_loc locked
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

    shiny::observeEvent(input$file_table_cell_edit, {
      updated <- DT::editData(uploaded_files(), input$file_table_cell_edit,
                              rownames = FALSE)
      uploaded_files(updated)
    })

    validity <- shiny::reactive({
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

    output$table_meta_status <- shiny::renderUI({
      v <- validity()
      if (isTRUE(v$valid)) {
        return(shiny::tags$div(class = "text-success", "All tables have name and description."))
      }
      shiny::tagList(
        shiny::tags$div(class = "text-danger", "Resolve before generating:"),
        shiny::tags$ul(lapply(v$errors, shiny::tags$li))
      )
    })

    `%||%` <- function(a, b) if (is.null(a)) b else a

    shiny::reactive({
      list(
        metadata = uploaded_files(),
        data = parsed_data(),
        valid = validity()$valid,
        errors = validity()$errors
      )
    })
  })
}
