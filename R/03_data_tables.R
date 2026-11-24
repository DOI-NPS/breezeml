# 03_data_tables.R v22
#
# v22: the file table's size column was labelled "Size (MB)" but held
# kibibytes, and was never shown to the user anyway.
#
# THE WRONG UNIT. Both places that built the column divided a byte count
# by 1024 and called the result MB:
#
#   size_mb = round(size / 1024)                              # file box
#   size_mb = round(as.numeric(file.size(paths)) / 1024)      # package folder
#
# file.size() and Shiny's input$upload$size are both in BYTES, so
# dividing by 1024 gives kibibytes. A 7 MiB CSV was recorded as 7168
# under a header reading "Size (MB)" - a factor of 1024 out. Dividing by
# 1024^2 would have been the arithmetic fix.
#
# It is not what this version does, for a reason worth stating. The
# column is HIDDEN: the render passes
#
#   columnDefs = list(list(targets = c("file_loc", "size_mb"),
#                          visible = FALSE))
#
# so neither the header nor the value has ever reached the screen, and
# nothing in the package reads the column either - searching every file
# for size_mb found only this module's own declarations, two comments in
# app_state.R, and the NSE false-positive list in utils.R. Rounding to
# whole megabytes would therefore have thrown away precision in a value
# no one sees, to make a unit label correct in a header no one reads.
#
# So the column now stores what its sources actually give: exact bytes,
# unrounded, named size_bytes, with the DT header "Size (bytes)" for the
# day it is unhidden. A byte count is the honest unit for a file size
# and the right thing to hold in state; formatting for a human is a
# presentation concern, and 11_upload_publish.R already does it properly
# with a B/KB/MB/GB ladder if this column is ever displayed.
#
# RENAMED, not just recomputed, deliberately: a column called size_mb
# that holds bytes is the same trap one factor further along, and the
# rename makes every reader of the saved session JSON see the change.
# Saved sessions are unaffected either way - app_server.R persists only
# file_name, table_name and description (see its state$data_tables
# block), so no size value has ever been written to a session file or
# read back from one.
#
# v21: fixed the page jumping back to the top after every cell edit, and
# added keyboard navigation between cells.
#
# THE SCROLL JUMP. output$file_table read uploaded_files() directly:
#
#   output$file_table <- DT::renderDT(
#     uploaded_files(), ...
#
# and the cell-edit observer wrote that same reactiveVal:
#
#   uploaded_files(updated)
#
# So every committed edit invalidated the render and Shiny rebuilt the
# whole DataTable widget. While the old DOM was gone the page was
# shorter, the browser clamped its scroll position, and the user was
# returned to the top of the page with no open editor - the reported
# symptom.
#
# The render now reads the data under shiny::isolate() and takes its
# reactive dependency on table_shape() instead, a counter bumped only
# when the SET OF ROWS changes: files added, files replaced, all files
# removed, or saved metadata re-attached during a restore. An accepted
# edit bumps nothing, so it rebuilds nothing. Nothing is lost by this:
# server = FALSE (already in use here) means the browser holds the data
# and DT has written the new value into the cell itself before R is
# told, so the grid on screen is already correct.
#
# uploaded_files() remains the single source of truth for every other
# consumer (validity(), data(), the generation pipeline). Only this one
# render stopped depending on it.
#
# KEYBOARD NAVIGATION. Enter moves down a column, Tab moves across a
# row, Shift reverses either, and read-only columns are skipped - see
# dt_keyboard_nav.R, which explains why DT does not do this on its own
# and how the keys are wired. The disabled columns are passed from the
# single DISABLED_COLUMNS constant below, which is also what
# editable$disable gets, so navigation and editability cannot disagree.
#
# The help text under the card header now describes the keys.
#
# Nothing else changed: no UI layout, no validation, no restore logic,
# and no change to the module's return value.
#
# v20: replace_files(paths, saved) stages and parses a complete package
# before replacing the loaded tables. Unlike add_files(), it intentionally
# replaces same-name files and drops tables from other packages. A read
# failure leaves the current tables unchanged. Used by check-to-revise.
#
# v19: devtools::document() hygiene. Every roxygen comment (#') that
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
# v18: the module can now take .csv files it was handed, rather than only
# ones chosen in the file box. The body of observeEvent(input$upload, ...)
# moved unchanged into ingest_csv_files(file_name, file_loc), which the
# observer now calls, and the module returns a new add_files(paths)
# entry alongside data/restore/unmatched_saved_files. app_server.R calls
# add_files() for the "Revise an existing data package" screen, where the
# .csv files come from a package folder the user selected or downloaded
# from DataStore, so they do not have to be picked by hand a second time.
# Duplicate names, parse failures, and matching against pending saved
# metadata behave exactly as they do for the file box, because it is the
# same code. Nothing else changed.
#
# v17: wording only. User-facing text says "imported" / "import" instead of
# "uploaded" / "upload" for the .csv files (they are read from the user's
# own computer). The sentence about sending the finished data package to
# DataStore keeps "uploaded" because that is a real upload. No logic
# changed; the internal ids (input$upload, uploaded_files) are unchanged.
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

# Read-only columns of the file table, 0-based: file_name (0),
# size_bytes (3) and file_loc (4). ONE definition, used both by editable$disable
# and by the keyboard navigation, so a cell can never be navigable but
# not editable (or the reverse). Column order is set by the
# dplyr::select() in ingest_csv_files(): file_name, table_name,
# description, size_bytes, file_loc.
FILE_TABLE_DISABLED_COLUMNS <- c(0, 3, 4)

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
      shiny::helpText("All files are handled locally. No data is transmited to remote servers ",
                      "until the data package is uploaded to fedramp certified DataStore ",
                      "server for review and publication. ",
                      "breezeml is suitable for working with sensitive or data. ",
                      "Removing files clears all imported tables so you can start ",
                      "over with a new set. This also clears any field, geography, ",
                      "or taxonomy selections that depended on those tables."),
      shiny::uiOutput(shiny::NS(id, "pending_restore_notice"))
    ),
    bslib::card(
      bslib::card_header("Fill in table metadata (Required)"),
      shiny::helpText("Double-click a Name or Description cell to edit it. ",
                      "Press Enter to save and move down the column, or Tab to ",
                      "save and move across the row (hold Shift to go back). ",
                      "Press Esc to cancel an edit."),
      shiny::helpText(class = "text-muted",
                      "Both Name and Description are required for every file before ",
                      "a script can be generated."),
      DT::DTOutput(shiny::NS(id, "file_table")),
      shiny::uiOutput(shiny::NS(id, "table_meta_status"))
    ),
    col_widths = c(-2, 8, -2), fill = FALSE
  )
}

#' @noRd
#' @return list(data = <reactive() list>, restore = <function>,
#'   unmatched_saved_files = <reactive() character vector>)
#'   data() returns:
#'     $metadata - tibble of file_name, table_name, description, size_bytes, file_loc
#'     $data     - named list of data.frames (names = original file names),
#'                 the actual parsed CSV contents for use by downstream modules
#'                 (geography, taxonomy, fields/attributes)
#'     $valid, $errors - every table must have a non-blank Name and Description
#'   add_files(paths) - add .csv files already on disk (from a selected or
#'     downloaded data package folder) as data tables, as if they had been
#'     chosen in the file box. See v18 note above.
#'   replace_files(paths, saved) - atomically replace tables from a package,
#'     applying its saved metadata; throws on unreadable CSVs.
#'   unmatched_saved_files() - character vector of file_names from the last
#'     restore() call that have NOT yet been matched by a re-upload. Empty
#'     once everything's matched (or if restore() was never called).
#'     Consumed by app_server.R to build the "please re-upload" banner.
#'
#'
tableMetadataServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    uploaded_files <- shiny::reactiveVal(tibble::tibble(
      file_name = character(),
      table_name = character(),
      description = character(),
      size_bytes = numeric(),
      file_loc = character()
    ))

    # Bumped ONLY when the set of rows in the file table changes - files
    # added, files replaced, all files removed, or saved metadata
    # re-attached by a restore. output$file_table takes its reactive
    # dependency on this rather than on uploaded_files(), so an accepted
    # cell edit does not rebuild the widget and the page keeps its scroll
    # position and its open editor (v21).
    table_shape <- shiny::reactiveVal(0L)

    bump_table_shape <- function() {
      table_shape(shiny::isolate(table_shape()) + 1L)
    }

    # named list of parsed data frames, keyed by file_name. Built up
    # incrementally so we don't re-read files that are already loaded.
    parsed_data <- shiny::reactiveVal(list())

    # Saved (file_name, table_name, description) rows awaiting a matching
    # re-upload - see file header note. NULL until restore() is called;
    # rows are removed from this as their file_name shows up in a fresh
    # upload.
    pending_restore_metadata <- shiny::reactiveVal(NULL)

    # Take a set of .csv files and add them as data tables. Shared by the
    # file box and by add_files() below, so both behave identically.
    #
    # @param file_name character vector of names to show in the table
    # @param file_loc character vector of full paths to read from
    # @param size optional file sizes in bytes; read from disk if absent
    ingest_csv_files <- function(file_name, file_loc, size = NULL) {
      if (length(file_name) == 0) return(invisible(NULL))
      if (is.null(size)) {
        size <- vapply(file_loc, function(p) {
          s <- suppressWarnings(file.size(p))
          if (is.na(s)) 0 else s
        }, numeric(1), USE.NAMES = FALSE)
      }

      current <- uploaded_files()

      new_files <- tibble::tibble(
        file_name = as.character(file_name),
        size = as.numeric(size),
        file_loc = as.character(file_loc)
      ) |>
        dplyr::mutate(size_bytes = size,
                      table_name = NA_character_,
                      description = NA_character_) |>
        dplyr::select(file_name, table_name, description, size_bytes, file_loc)

      dups <- intersect(new_files$file_name, current$file_name)

      if (length(dups) > 0) {
        shiny::showModal(
          shiny::modalDialog(
            title = "WARNING: Duplicate Files",
            easyClose = FALSE,
            shiny::p(glue::glue(
              "You cannot import multiple files with the same name. ",
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
      # rows were added, so the grid genuinely has to be rebuilt (v21)
      bump_table_shape()
      invisible(nrow(new_files))
    }

    shiny::observeEvent(input$upload, {
      ingest_csv_files(
        file_name = input$upload$name,
        file_loc = input$upload$datapath,
        size = input$upload$size
      )
    })

    # Add .csv files the app already has on disk (a data package folder
    # the user selected, or one downloaded from DataStore) as data
    # tables, exactly as if they had been chosen in the file box above.
    # Files are read from where they are; nothing is copied or moved.
    #
    # @param paths character vector of full paths to .csv files
    # @return invisibly, the number of files added
    add_files <- function(paths) {
      paths <- paths[!is.na(paths) & nzchar(paths)]
      paths <- paths[file.exists(paths)]
      if (length(paths) == 0) return(invisible(0L))
      ingest_csv_files(file_name = basename(paths), file_loc = paths)
    }

    # Stage every read before publishing reactive state; never append or skip
    # same-name files from a previous package. No disk writes or network calls.
    replace_files <- function(paths, saved = NULL) {
      if (anyNA(paths) || any(!file.exists(paths)) || anyDuplicated(basename(paths))) {
        stop("Package CSV paths must exist and have unique file names.")
      }
      staged_data <- lapply(paths, function(p) readr::read_csv(p, show_col_types = FALSE))
      names(staged_data) <- basename(paths)
      staged <- tibble::tibble(
        file_name = basename(paths), table_name = rep(NA_character_, length(paths)),
        description = rep(NA_character_, length(paths)),
        size_bytes = as.numeric(file.size(paths)), file_loc = paths
      )
      pending <- NULL
      if (!is.null(saved) && length(saved$file_name %||% character(0)) > 0) {
        pending <- tibble::tibble(
          file_name = as.character(saved$file_name),
          table_name = as.character(saved$table_name %||% NA_character_),
          description = as.character(saved$description %||% NA_character_)
        )
        idx <- match(staged$file_name, pending$file_name)
        matched <- !is.na(idx)
        staged$table_name[matched] <- pending$table_name[idx[matched]]
        staged$description[matched] <- pending$description[idx[matched]]
        pending <- pending[!pending$file_name %in% staged$file_name, , drop = FALSE]
      }
      pending_restore_metadata(pending)
      parsed_data(staged_data)
      uploaded_files(staged)
      # the whole row set was replaced (v21)
      bump_table_shape()
      invisible(nrow(staged))
    }

    shiny::observeEvent(input$remove_all, {
      shiny::showModal(
        shiny::modalDialog(
          title = "Remove all files?",
          "This will remove all imported data tables and any field, ",
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
        size_bytes = numeric(),
        file_loc = character()
      ))
      parsed_data(list())
      # every row is gone (v21)
      bump_table_shape()
      shiny::removeModal()
      shiny::showNotification("All imported files removed.", type = "message")
    })

    # Rebuilt only when table_shape() changes - see the v21 note at the
    # top of this file. The data itself is read with isolate(), so an
    # accepted cell edit leaves this render untouched and the browser
    # keeps its scroll position, its focus and its open editor.
    #
    # server = FALSE (unchanged from v20) is what makes that safe: the
    # browser holds the rows, and DT's own editor has already written the
    # committed value into the cell before input$file_table_cell_edit
    # reaches R.
    output$file_table <- DT::renderDT(
      {
        table_shape()
        shiny::isolate(uploaded_files())
      },
      selection = "none",
      server = FALSE,
      rownames = FALSE,
      editable = list(
        target = "cell",
        # file_name, size_bytes, file_loc locked - see
        # FILE_TABLE_DISABLED_COLUMNS
        disable = list(columns = FILE_TABLE_DISABLED_COLUMNS)
      ),
      # Enter moves down a column, Tab across a row, Shift reverses
      # either, read-only and hidden columns are skipped (v21). The same
      # constant feeds editable$disable above, so the two agree by
      # construction. See dt_keyboard_nav.R.
      callback = dt_keyboard_nav_callback(
        disabled_columns = FILE_TABLE_DISABLED_COLUMNS
      ),
      options = list(dom = 't',
                     columnDefs = list(
                       list(
                         targets = c("file_loc", "size_bytes"),
                         visible = FALSE
                       )
                     )
      ),
      colnames = c("File name",
                   "Name",
                   "Description",
                   "Size (bytes)",
                   "Path")
    )

    shiny::observeEvent(input$file_table_cell_edit, {
      updated <- DT::editData(uploaded_files(), input$file_table_cell_edit,
                              rownames = FALSE)
      # No bump_table_shape() here, deliberately: this is the edit path,
      # and rebuilding the widget is exactly what sent the page back to
      # the top before v21. Row membership has not changed, and the
      # browser already shows the new value.
      uploaded_files(updated)
    })

    validity <- shiny::reactive({
      df <- uploaded_files()
      if (nrow(df) == 0) {
        return(list(valid = FALSE, errors = "At least one data table must be imported."))
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

    # Record saved Tab 3 metadata to watch for on future uploads. Does
    # NOT touch uploaded_files()/parsed_data() directly - actual
    # restoration only happens inside observeEvent(input$upload, ...)
    # above, once a file_name match is found. Safe to call even if some
    # or all saved files are already uploaded (e.g. re-loading a JSON
    # file mid-session) - matching against ALREADY-uploaded files is
    # intentionally NOT done here, since this app's upload flow has no
    # "re-scan already-uploaded files" trigger; if this is a concern,
    # the user can remove-all and re-upload after loading.
    #
    # @param saved list matching default_app_state()$data_tables$metadata's
    #   shape (list-of-columns for file_name/table_name/description), or NULL
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

    # @return character vector of file_names from the most recent
    #   restore() call that have not yet been matched by a re-upload.
    #   Empty character(0) if nothing is pending.
    unmatched_saved_files <- shiny::reactive({
      pending <- pending_restore_metadata()
      if (is.null(pending)) character(0) else pending$file_name
    })

    list(data = data, restore = restore, add_files = add_files,
         replace_files = replace_files,
         unmatched_saved_files = unmatched_saved_files)
  })
}
