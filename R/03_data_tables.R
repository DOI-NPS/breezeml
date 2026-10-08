# 03_data_tables.R v16
#
# v16: added restore() to tableMetadataServer()'s return value, for the
# Save/Load session feature. Per explicit design decision, ONLY
# $metadata (file_name/table_name/description) is ever saved or restored
# here - $data (actual parsed CSV contents) is NEVER part of saved state.
# The user must re-upload the same-named CSV files after a Load; this
# module's job on restore is to remember what WAS saved, and re-attach
# the saved table_name/description to each file as it gets re-uploaded,
# matched by file_name.
#
# This required a real behavior change to the upload handler
# (observeEvent(input$upload, ...)): it now checks a pending "expected
# restore" list on every upload and, for any newly-uploaded file whose
# name matches a pending saved entry, fills in that file's table_name/
# description from the saved data instead of leaving them NA (blank) as
# for a genuinely new upload. This is the mechanism the file header note
# in app_state.R referred to as "restore() logic will re-attach saved
# table_name/description to matching re-uploaded files by file_name."
#
# pending_restore_metadata (reactiveVal) holds whatever saved rows have
# NOT yet been matched to an uploaded file - this is exposed via
# unmatched_saved_files() (a reactive() returning the character vector of
# still-pending file_names) so app_server.R's Load banner can display
# "still need to re-upload: X, Y, Z" and clear entries off the list as
# they resolve. This is the cross-tab piece Tab 4 also needs (fields
# restore is gated on the SAME re-upload event), so
# unmatched_saved_files() is deliberately public/returned, not private -
# app_server.R's orchestration polls it from all of Tabs 3/4/5/6 to build
# one combined "still pending" banner rather than four separate ones.
#
# tableMetadataServer()'s return value changes from a bare reactive() to
# list(data = <reactive>, restore = <function>, unmatched_saved_files = <reactive>).
#
# Added "(Required)" to both card headers, matching the labeling pattern
# applied across Tabs 1 and 2.
#
# Added @noRd to tableMetadataServer() - internal Shiny module server, not
# meant to have a public help page. Resolves roxygen2's "Skipping; no
# name and/or title" note.

tableMetadataUI <- function(id) {
  bslib::layout_columns(
    bslib::card(
      bslib::card_header("Load data tables (Required)"),
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
                      "or taxonomy selections that depended on those tables."),
      shiny::uiOutput(shiny::NS(id, "pending_restore_notice"))
    ),
    bslib::card(
      bslib::card_header("Fill in table metadata (Required)"),
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

#' @return list(data = <reactive() list>, restore = <function>,
#'   unmatched_saved_files = <reactive() character vector>)
#'   data() returns:
#'     $metadata - tibble of file_name, table_name, description, size_mb, file_loc
#'     $data     - named list of data.frames (names = original file names),
#'                 the actual parsed CSV contents for use by downstream modules
#'                 (geography, taxonomy, fields/attributes)
#'     $valid, $errors - every table must have a non-blank Name and Description
#'   unmatched_saved_files() - character vector of file_names from the last
#'     restore() call that have NOT yet been matched by a re-upload. Empty
#'     once everything's matched (or if restore() was never called).
#'     Consumed by app_server.R to build the "please re-upload" banner.
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

    # Saved (file_name, table_name, description) rows awaiting a matching
    # re-upload - see file header note. NULL until restore() is called;
    # rows are removed from this as their file_name shows up in a fresh
    # upload.
    pending_restore_metadata <- shiny::reactiveVal(NULL)

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

      # Re-attach saved table_name/description for any newly-uploaded
      # file whose name matches a pending restore entry - this is the
      # ONLY place saved Tab 3 metadata actually gets applied, since it's
      # gated entirely on the matching upload event, not on restore()
      # itself (restore() only RECORDS what to watch for).
      pending <- pending_restore_metadata()
      if (!is.null(pending) && nrow(pending) > 0) {
        matched <- intersect(new_files$file_name, pending$file_name)
        if (length(matched) > 0) {
          for (fn in matched) {
            saved_row <- pending[pending$file_name == fn, ][1, ]
            idx <- which(new_files$file_name == fn)
            new_files$table_name[idx] <- saved_row$table_name
            new_files$description[idx] <- saved_row$description
          }
          # remove matched rows from the pending list so the "please
          # re-upload" banner shrinks as files are matched
          pending_restore_metadata(pending[!pending$file_name %in% matched, , drop = FALSE])

          shiny::showNotification(
            paste0("Restored saved Name/Description for: ", paste(matched, collapse = ", ")),
            type = "message"
          )
        }
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

    output$pending_restore_notice <- shiny::renderUI({
      pending <- pending_restore_metadata()
      shiny::req(!is.null(pending), nrow(pending) > 0)
      shiny::tags$div(
        class = "alert alert-warning mt-2",
        shiny::tags$strong("Re-load needed to finish restoring your saved session: "),
        paste(pending$file_name, collapse = ", ")
      )
    })

    `%||%` <- function(a, b) if (is.null(a)) b else a

    data <- shiny::reactive({
      list(
        metadata = uploaded_files(),
        data = parsed_data(),
        valid = validity()$valid,
        errors = validity()$errors
      )
    })

    #' Record saved Tab 3 metadata to watch for on future uploads. Does
    #' NOT touch uploaded_files()/parsed_data() directly - actual
    #' restoration only happens inside observeEvent(input$upload, ...)
    #' above, once a file_name match is found. Safe to call even if some
    #' or all saved files are already uploaded (e.g. re-loading a JSON
    #' file mid-session) - matching against ALREADY-uploaded files is
    #' intentionally NOT done here, since this app's upload flow has no
    #' "re-scan already-uploaded files" trigger; if this is a concern,
    #' the user can remove-all and re-upload after loading.
    #'
    #' @param saved list matching default_app_state()$data_tables$metadata's
    #'   shape (list-of-columns for file_name/table_name/description), or NULL
    restore <- function(saved) {
      if (is.null(saved) || length(saved$file_name %||% character(0)) == 0) {
        pending_restore_metadata(NULL)
        return(invisible(NULL))
      }

      pending_restore_metadata(tibble::tibble(
        file_name = as.character(saved$file_name),
        table_name = as.character(saved$table_name %||% NA_character_),
        description = as.character(saved$description %||% NA_character_)
      ))
      invisible(NULL)
    }

    #' @return character vector of file_names from the most recent
    #'   restore() call that have not yet been matched by a re-upload.
    #'   Empty character(0) if nothing is pending.
    unmatched_saved_files <- shiny::reactive({
      pending <- pending_restore_metadata()
      if (is.null(pending)) character(0) else pending$file_name
    })

    list(data = data, restore = restore, unmatched_saved_files = unmatched_saved_files)
  })
}
