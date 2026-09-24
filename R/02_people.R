# 02_people.R v18
#
# Tightened vertical spacing to reduce scrolling on this tab, same
# reasoning as 01_high_level_info.R v17: 4 full-width stacked cards
# (layout_columns() here recycles a single col_widths = c(-2, 8, -2) spec,
# producing ONE centered column with every card stacked, not a grid) add
# up in height mostly from card chrome, not from empty space inside any
# one short table. Changes made:
#   - each card gets class = "mb-2" instead of bslib's larger default
#     spacing between stacked layout_columns() rows
#   - person_category_ui()'s with_role helpText moved to sit directly
#     under the header help_text (both are now one helpText() call per
#     card instead of two separate paragraphs), removing one paragraph's
#     worth of margin from the Contributors card
#
# Layout stays single-column/stacked - explicit user choice over a grid
# rework.
#
# Corresponds to skeleton.Rmd's personnel.txt content (part of FUNCTION 1 -
# template_core_metadata). EMLassemblyline requires one row per person with:
#   givenName, surName, organizationName, electronicMailAddress, userId, role
# (projectTitle, fundingAgency, fundingNumber are optional and left blank -
# not used in the NPS context per current guidance).
#
# The only role REQUIRED by EMLassemblyline is "creator" (== Authors here).
# Contact is also expected but not strictly required. Contributors get a
# free-text custom role per person. Editors are NOT an EML personnel role -
# they're an NPS DataStore reference-permission concept - but per current
# guidance we capture the same full record for Editors too, for
# completeness and future DataStore use, even though emit_people_chunk()
# does not include Editors in personnel.txt.
#
# UI pattern per category: type an email (or comma-separated list) +
# press "Add" -> appends row(s) to that category's table (deduped by email
# within that category, NOT across categories - the same person can
# legitimately be both a creator and a contact). Adding an email triggers
# a batched NPSdatastore::active_directory_lookup() to auto-fill
# givenName/surName/userId (ORCID)/organizationName where possible; a
# failed/not-found lookup is non-fatal and just leaves those fields blank
# for manual entry. The table is then editable inline for any remaining
# fields. Contributors additionally get an editable "role" column,
# defaulted to "contributor".

PERSON_COLS <- c("email", "givenName", "surName", "organizationName", "userId")
PERSON_COL_LABELS <- c("Email", "Given name", "Surname", "Organization", "ORCID")

empty_person_tbl <- function(with_role = FALSE) {
  tbl <- tibble::tibble(
    email = character(),
    givenName = character(),
    surName = character(),
    organizationName = character(),
    userId = character()
  )
  if (with_role) tbl$role <- character()
  tbl
}

person_category_ui <- function(id, header, help_text, with_role = FALSE) {
  ns <- shiny::NS(id)
  bslib::card(
    class = "mb-2",
    bslib::card_header(header),
    shiny::helpText(
      help_text,
      if (with_role) {
        paste0(" Role is a free-text custom role for each contributor ",
               "(e.g. 'Field Technician', 'Laboratory Assistant').")
      }
    ),
    bslib::layout_columns(
      shiny::textInput(ns("new_email"), NULL,
                       placeholder = "Enter one or more emails, separated by commas", width = "100%"),
      shiny::actionButton(ns("add_email"), "Add", class = "btn-primary btn-sm"),
      col_widths = c(10, 2)
    ),
    DT::DTOutput(ns("people_table"))
  )
}

#' Reusable server for one personnel category (Authors, Contacts,
#' Contributors, Editors). Returns a reactive() tibble of that category's
#' people, plus a reactive() logical "is this category valid" (all required
#' fields present on every row - email is always required since it's the
#' add-key; other fields are required once a row exists at all).
#'
#' @param id module id
#' @param with_role whether this category tracks a per-person custom role
#'   (Contributors only)
#' @param require_nonempty whether at least one person is required in this
#'   category (Authors requires >= 1 creator; Contacts/Contributors/Editors
#'   do not)
person_category_server <- function(id, with_role = FALSE, require_nonempty = FALSE) {
  shiny::moduleServer(id, function(input, output, session) {
    people <- shiny::reactiveVal(empty_person_tbl(with_role))

    shiny::observeEvent(input$add_email, {
      shiny::req(input$new_email)

      raw_entries <- strsplit(input$new_email, ",")[[1]]
      candidates <- trimws(raw_entries)
      candidates <- candidates[nzchar(candidates)]
      shiny::req(length(candidates) > 0)

      email_pattern <- "^[^@\\s]+@[^@\\s]+\\.[^@\\s]+$"
      is_valid_format <- grepl(email_pattern, candidates, perl = TRUE)

      current <- people()
      already_present <- candidates %in% current$email
      # dedupe within the pasted batch itself too, keeping first occurrence
      dupe_within_batch <- duplicated(candidates)

      skip_reason <- dplyr::case_when(
        !is_valid_format ~ "invalid format",
        already_present ~ "already added",
        dupe_within_batch ~ "duplicate in list",
        TRUE ~ NA_character_
      )

      valid_emails <- candidates[is.na(skip_reason)]
      skipped <- tibble::tibble(email = candidates[!is.na(skip_reason)],
                                reason = skip_reason[!is.na(skip_reason)])

      if (length(valid_emails) == 0) {
        shiny::showNotification("No valid new email addresses to add.", type = "warning")
        return(invisible(NULL))
      }

      # Single batched Active Directory lookup for all valid new emails.
      # organizationName is not part of the AD response and always requires
      # manual entry; a failed lookup (e.g. network issue) is non-fatal -
      # rows just fall back to blank/manual for the fields AD would fill.
      ad_result <- tryCatch(
        as.data.frame(NPSdatastore::active_directory_lookup(emails = valid_emails)),
        error = function(e) {
          shiny::showNotification(
            paste0("Active Directory lookup failed (you can still enter details manually): ",
                   conditionMessage(e)),
            type = "warning"
          )
          NULL
        }
      )

      new_rows <- purrr::map_dfr(valid_emails, function(email) {
        row <- tibble::tibble(
          email = email,
          givenName = "",
          surName = "",
          organizationName = "",
          userId = ""
        )
        if (with_role) row$role <- "contributor"

        if (!is.null(ad_result)) {
          # match by searchTerm rather than position, in case the API
          # ever reorders or drops rows relative to the input vector
          ad_row <- ad_result[ad_result$searchTerm == email, ][1, ]
          if (!is.na(ad_row$found) && isTRUE(ad_row$found)) {
            row$givenName <- ifelse(is.na(ad_row$givenName), "", ad_row$givenName)
            row$surName <- ifelse(is.na(ad_row$sn), "", ad_row$sn)
            row$userId <- ifelse(is.na(ad_row$orcid), "", ad_row$orcid)
            row$organizationName <- "National Park Service"
          }
        }
        row
      })

      people(rbind(current, new_rows))
      shiny::updateTextInput(session, "new_email", value = "")

      msg <- paste0("Added ", nrow(new_rows), " email(s).")
      if (nrow(skipped) > 0) {
        msg <- paste0(msg, " Skipped ", nrow(skipped), ": ",
                      paste0(skipped$email, " (", skipped$reason, ")", collapse = ", "))
      }
      shiny::showNotification(msg, type = if (nrow(skipped) > 0) "warning" else "message")
    })

    output$people_table <- DT::renderDT({
      df <- people()
      col_labels <- if (with_role) c(PERSON_COL_LABELS, "Role") else PERSON_COL_LABELS

      display_df <- df
      if (nrow(display_df) > 0) {
        display_df$remove <- vapply(seq_len(nrow(display_df)), function(i) {
          as.character(
            shiny::actionButton(session$ns(paste0("remove_", i)), "Remove",
                                class = "btn-danger btn-sm",
                                onclick = sprintf(
                                  'Shiny.setInputValue(\"%s\", %d, {priority: \"event\"})',
                                  session$ns("remove_row"), i
                                ))
          )
        }, character(1))
      } else {
        display_df$remove <- character(0)
      }

      DT::datatable(
        display_df,
        rownames = FALSE,
        selection = "none",
        colnames = c(col_labels, ""),
        escape = which(names(display_df) != "remove") - 1,
        options = list(dom = 't', pageLength = -1, scrollX = TRUE),
        editable = list(target = "cell", disable = list(columns = c(0, ncol(display_df) - 1)))  # email + remove button locked
      )
    })

    shiny::observeEvent(input$remove_row, {
      idx <- input$remove_row
      current <- people()
      shiny::req(idx >= 1, idx <= nrow(current))
      removed_email <- current$email[idx]
      people(current[-idx, , drop = FALSE])
      shiny::showNotification(paste0("Removed ", removed_email, "."), type = "message")
    })

    shiny::observeEvent(input$people_table_cell_edit, {
      edit <- input$people_table_cell_edit
      # The displayed table has an extra trailing "remove" button column not
      # present in the underlying data - edits should never target it since
      # it's excluded from `editable` columns, but guard defensively anyway.
      current <- people()
      if (edit$col >= ncol(current)) return(invisible(NULL))
      updated <- DT::editData(current, edit, rownames = FALSE)
      people(updated)
    })

    is_valid <- shiny::reactive({
      df <- people()
      if (nrow(df) == 0) return(!require_nonempty)

      required_cols <- c("givenName", "surName", "organizationName")
      # userId (ORCID) is recommended but not required by EMLassemblyline
      all(purrr::map_lgl(required_cols, function(col) {
        all(nzchar(trimws(df[[col]])))
      }))
    })

    list(data = people, valid = is_valid)
  })
}

peopleInput <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    person_category_ui(
      ns("authors"), "Authors (Creators)",
      paste0("Authors must be individuals (not organizations) and are ",
             "listed as 'creator' in the metadata - they will appear in ",
             "the data package citation. At least one author is required. ",
             "ORCIDs are strongly recommended."),
    ),
    person_category_ui(
      ns("contacts"), "Contacts",
      paste0("Contacts must be NPS employees or partners familiar with all ",
             "aspects of the data package - almost always one or more of ",
             "the authors. Think 'corresponding author.'")
    ),
    person_category_ui(
      ns("contributors"), "Contributors",
      paste0("Contributors did not rise to the level of authorship but ",
             "should still be acknowledged. Each needs a custom role ",
             "(e.g. 'Field Assistant')."),
      with_role = TRUE
    ),
    person_category_ui(
      ns("editors"), "Editors",
      paste0("Editors can make reasonable updates to the DataStore ",
             "reference (e.g. fixing typos, updating permissions) and are ",
             "the only ones who can access a draft reference - include ",
             "potential reviewers here. Editors must be NPS employees or ",
             "partners. (Not part of the EML personnel record - used by ",
             "DataStore.)")
    ),
    col_widths = c(-2, 8, -2), fill = FALSE
  )
}

#' @return reactive() list:
#'   $authors, $contacts, $contributors, $editors - each a tibble
#'   $valid - logical, TRUE only if every category's required-field check
#'            passes AND at least one author exists
#'   $errors - character vector of human-readable problems, empty if valid
#' @noRd
peopleServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    authors <- person_category_server("authors", require_nonempty = TRUE)
    contacts <- person_category_server("contacts")
    contributors <- person_category_server("contributors", with_role = TRUE)
    editors <- person_category_server("editors")

    shiny::reactive({
      errors <- character(0)

      if (nrow(authors$data()) == 0) {
        errors <- c(errors, "At least one Author (Creator) is required.")
      }
      if (!authors$valid()) {
        errors <- c(errors, "Every Author needs given name, surname, and organization filled in.")
      }
      if (!contacts$valid()) {
        errors <- c(errors, "Every Contact needs given name, surname, and organization filled in.")
      }
      if (!contributors$valid()) {
        errors <- c(errors, "Every Contributor needs given name, surname, and organization filled in.")
      }
      if (!editors$valid()) {
        errors <- c(errors, "Every Editor needs given name, surname, and organization filled in.")
      }

      list(
        authors = authors$data(),
        contacts = contacts$data(),
        contributors = contributors$data(),
        editors = editors$data(),
        valid = length(errors) == 0,
        errors = errors
      )
    })
  })
}

#' Emit the personnel.txt-writing chunk. Pure function - EMLassemblyline's
#' template_core_metadata() writes a BLANK personnel.txt; this chunk
#' overwrites it with the app's captured data, same pattern as
#' emit_fields_chunk(). Editors are intentionally excluded - not an EML role.
#'
#' @param state the list returned by peopleServer()'s reactive, evaluated
#'   (i.e. state <- people_reactive())
#' @param working_folder_var name of the R variable holding the working
#'   folder path in the generated script (default "working_folder")
#' @return character - the R code chunk to write personnel.txt, or an
#'   explanatory comment if state is incomplete/invalid
emit_people_chunk <- function(state, working_folder_var = "working_folder") {
  if (is.null(state) || !isTRUE(state$valid)) {
    return(paste0(
      "# Personnel information is incomplete - resolve the following before\n",
      "# generating a final script:\n",
      paste0("#   - ", state$errors, collapse = "\n"), "\n"
    ))
  }

  to_personnel_rows <- function(df, role) {
    if (nrow(df) == 0) return(NULL)
    tibble::tibble(
      givenName = df$givenName,
      middleInitial = "",
      surName = df$surName,
      organizationName = df$organizationName,
      electronicMailAddress = df$email,
      userId = df$userId,
      role = if ("role" %in% names(df)) df$role else role,
      projectTitle = "",
      fundingAgency = "",
      fundingNumber = ""
    )
  }

  personnel <- dplyr::bind_rows(
    to_personnel_rows(state$authors, "creator"),
    to_personnel_rows(state$contacts, "contact"),
    to_personnel_rows(state$contributors, NA_character_)  # role col already present
  )

  tribble_str <- tibble_to_r_tribble(personnel)

  glue::glue(
    'personnel_df <- {tribble_str}\n',
    'readr::write_tsv(personnel_df, file.path({working_folder_var}, "personnel.txt"), na = "")\n'
  )
}
