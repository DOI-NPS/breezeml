# 02_people.R v25
#
# v25: replaced the pure ORCID *format* check with a live *resolution*
# check against the ORCID public API, per request. An author's ORCID is
# now verified to actually exist, not just to be well-formed.
#
# NEW DEPENDENCY: httr2 - add to DESCRIPTION's Imports. (httr2, not httr:
# hitting the ORCID PUBLIC API endpoint https://pub.orcid.org/v3.0/<id>
# with Accept: application/json returns a clean 404 for a nonexistent ID
# and 200 for a real one. This is why the earlier httr2::req_perform()
# attempt "always returned 200" - that was hitting the www.orcid.org PAGE
# url, which serves a page regardless; the pub API does not. No auth is
# required for the public API.)
#
# CRITICAL DESIGN POINT - the network call does NOT live in errors()/
# is_valid(). Those reactives recompute on EVERY cell edit; a blocking
# HTTP request in that path would fire one network call per author row on
# every keystroke-completing edit, freezing the Shiny session. Instead the
# resolution check runs ONCE, at the moment an ORCID is actually set:
#   - when Active Directory auto-fills an ORCID (in the add_email flow), and
#   - when the user finishes editing the ORCID (userId) cell,
# and its verdict is CACHED in a new hidden per-row `orcid_status` column
# (same pattern as v23's ad_found). errors()/warnings() just READ that
# cached column - cheap, no network. This bounds network calls to "once
# per ORCID actually entered/changed."
#
# orcid_resolves(id) returns one of three states (NOT a bare TRUE/FALSE),
# because a network check has a failure mode a regex doesn't:
#   "ok"        - pub API returned 200: the ORCID exists.
#   "not_found" - pub API returned a clean 404: the ORCID does not exist.
#   "unchecked" - could not determine (timeout / offline / DNS / unexpected
#                 status / blank or malformed input). Format-gated: a
#                 malformed id is never sent to the network.
# Per request, severity is split:
#   - "not_found"  -> HARD error (blocks Generate; the ORCID really doesn't
#                     exist). Also a malformed ORCID is a hard error (format).
#   - "unchecked"  -> SOFT warning (shown inline, does NOT block Generate -
#                     a network hiccup must not stop everyone from working).
#   - "ok"         -> no message.
# This required splitting the per-category output into errors() (hard,
# blocking - feeds is_valid and the Generate gate) and warnings() (soft,
# advisory - shown inline only). The inline panel (v24) now renders both.
#
# The ORCID *format* check (is_valid_orcid / ORCID_PATTERN) is RETAINED as
# a cheap pre-gate: a malformed string is reported as a format error and is
# never sent to the network (you cannot meaningfully resolve-check garbage).
#
# orcid_status rides through Save/Load automatically (app_server.R
# serializes every people column). restore() defaults it for older save
# files that predate the column: "unchecked" for any row that has a
# non-blank ORCID (honest - we have not verified it this session; shows a
# soft, non-blocking warning prompting re-verification), "" for blank. New
# save files carry the real cached status.
#
# The network check (both add-time and edit-time) is gated on
# require_orcid_if_ad_found, so only Authors incur any ORCID network
# traffic; Contacts/Contributors/Editors never do.
#
# ---- (prior version notes retained) ----
#
# v24: per-category INLINE validity display - each person_category_server()
# exposes an errors() reactive of specific, row-naming messages rendered
# under that category's table (new uiOutput in person_category_ui()).
# Updates live as cells are edited.
#
# v23: require an ORCID for any author whose email resolved to a VERIFIED
# AD match (found == TRUE) at add time. Hidden per-row ad_found column
# records the verified-match status; ORCID format validation added.
# require_orcid_if_ad_found (authors only) gates the rule. A manually-added
# author ("Add blank row") is never AD-verified, so the "must have an
# ORCID" rule does not fire for manual entries (the resolve check in v25
# DOES still apply to any ORCID a user types, see below).
#
# v22: loosened Authors via three call-site params (authors only):
# allow_manual_add (Add blank row), require_org = FALSE, unlock_email.
#
# v21: Editors INCLUDED in personnel.txt (role = "editor").
# v20: Editors restricted to VERIFIED AD matches (require_ad_verification).
#
# Corresponds to skeleton.Rmd's personnel.txt content (part of FUNCTION 1 -
# template_core_metadata). EMLassemblyline requires one row per person with:
#   givenName, surName, organizationName, electronicMailAddress, userId, role

PERSON_COLS <- c("email", "givenName", "surName", "organizationName", "userId")
PERSON_COL_LABELS <- c("Email", "Given name", "Surname", "Organization", "ORCID")

# ORCID is 16 characters shown as four hyphen-separated groups of four.
# Per the ORCID spec, the first 15 characters are digits and ONLY the
# final check character may be the letter X (uppercase by convention;
# lowercase accepted here for user convenience).
ORCID_PATTERN <- "^[0-9]{4}-[0-9]{4}-[0-9]{4}-[0-9]{3}[0-9Xx]$"

#' TRUE iff every element of x is a well-formed ORCID. Vectorized.
#' @noRd
is_valid_orcid <- function(x) {
  if (is.null(x)) return(logical(0))
  x <- trimws(x)
  x[is.na(x)] <- ""
  grepl(ORCID_PATTERN, x, perl = TRUE)
}

#' Pull the bare 16-char ORCID out of a value that may arrive from Active
#' Directory as a full URL (e.g. "https://orcid.org/0000-0002-1825-0097").
#' Returns the input trimmed if no ORCID-shaped substring is found.
#' @noRd
normalize_orcid <- function(x) {
  if (is.null(x)) return("")
  x <- trimws(x)
  if (is.na(x) || !nzchar(x)) return("")
  m <- regmatches(x, regexpr("[0-9]{4}-[0-9]{4}-[0-9]{4}-[0-9]{3}[0-9Xx]", x, perl = TRUE))
  if (length(m) == 1 && nzchar(m)) m else x
}

#' Check whether a (single) ORCID actually resolves, via the ORCID public
#' API. Returns one of "ok" / "not_found" / "unchecked" - never throws.
#'
#' "" / malformed input returns "unchecked" WITHOUT a network call (format-
#' gated - callers surface malformed ORCIDs as a format error separately).
#' Timeout / offline / DNS / unexpected HTTP status all return "unchecked"
#' (a network problem must not be mistaken for "this ORCID is invalid").
#' A clean 404 returns "not_found" (the ORCID genuinely does not exist).
#'
#' @param id character scalar - an ORCID identifier (bare, e.g.
#'   "0000-0002-1825-0097")
#' @return character scalar: "ok", "not_found", or "unchecked"
#' @noRd
orcid_resolves <- function(id) {
  id <- trimws(id %||% "")
  # Format gate: never send a blank or malformed value to the network.
  if (!nzchar(id) || !isTRUE(is_valid_orcid(id))) return("unchecked")

  url <- paste0("https://pub.orcid.org/v3.0/", id)
  tryCatch({
    resp <- httr2::request(url) |>
      httr2::req_headers(Accept = "application/json") |>
      httr2::req_timeout(5) |>
      # don't throw on non-2xx - we want to inspect the status ourselves
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform()
    status <- httr2::resp_status(resp)
    if (status == 200) {
      "ok"
    } else if (status == 404) {
      "not_found"
    } else {
      # unexpected status (rate limit, 5xx, etc.) - don't block the user
      "unchecked"
    }
  }, error = function(e) {
    # timeout, DNS failure, offline, etc. - don't block the user
    "unchecked"
  })
}

empty_person_tbl <- function(with_role = FALSE) {
  tbl <- tibble::tibble(
    email = character(),
    givenName = character(),
    surName = character(),
    organizationName = character(),
    userId = character()
  )
  if (with_role) tbl$role <- character()
  # hidden per-row flags (see v23 / v25). Kept as the LAST data columns so
  # the "remove" button column is always appended after them.
  tbl$ad_found <- logical()       # TRUE iff a verified AD match at add time
  tbl$orcid_status <- character() # "", "ok", "not_found", or "unchecked"
  tbl
}

person_category_ui <- function(id, header, help_text, with_role = FALSE,
                               allow_manual_add = FALSE) {
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
    if (allow_manual_add) {
      shiny::actionButton(ns("add_blank"), "Add blank row",
                          class = "btn-outline-secondary btn-sm mb-2")
    },
    DT::DTOutput(ns("people_table")),
    # per-category inline validity messages (errors + soft warnings)
    shiny::uiOutput(ns("validity"))
  )
}

#' Reusable server for one personnel category. Returns the category's
#' reactive() tibble, an errors() reactive (HARD, blocking, row-naming
#' messages), a warnings() reactive (SOFT, non-blocking advisories), a
#' derived valid() logical (== no hard errors), and restore().
#'
#' @param id module id
#' @param with_role whether this category tracks a per-person custom role
#' @param require_nonempty whether at least one person is required
#' @param require_ad_verification Editors-only: reject emails that don't
#'   resolve to a verified AD match (found == TRUE); fail safe on API error
#' @param allow_manual_add Authors-only: show an "Add blank row" button
#' @param require_org whether organizationName is required for validity
#' @param unlock_email Authors-only: make the email column editable inline
#' @param require_orcid_if_ad_found Authors-only: enforce ORCID rules -
#'   every AD-verified row must have an ORCID, every non-blank ORCID must be
#'   well-formed AND must resolve via the ORCID public API (a clean 404 is a
#'   hard error; an unreachable API is a soft warning). Gating the ORCID
#'   network check here means only Authors incur ORCID network traffic.
#' @param category_label singular lowercase noun used in messages
person_category_server <- function(id, with_role = FALSE, require_nonempty = FALSE,
                                   require_ad_verification = FALSE,
                                   allow_manual_add = FALSE,
                                   require_org = TRUE,
                                   unlock_email = FALSE,
                                   require_orcid_if_ad_found = FALSE,
                                   category_label = "entry") {
  shiny::moduleServer(id, function(input, output, session) {
    people <- shiny::reactiveVal(empty_person_tbl(with_role))

    safe_chr <- function(x) {
      if (is.null(x)) return(character(0))
      x <- as.character(x); x[is.na(x)] <- ""; trimws(x)
    }
    and_list <- function(x) {
      if (length(x) == 0) return("")
      if (length(x) == 1) return(x)
      if (length(x) == 2) return(paste(x, collapse = " and "))
      paste0(paste(x[-length(x)], collapse = ", "), ", and ", x[length(x)])
    }

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
      dupe_within_batch <- duplicated(candidates)

      skip_reason <- dplyr::case_when(
        !is_valid_format ~ "invalid format",
        already_present ~ "already added",
        dupe_within_batch ~ "duplicate in list",
        TRUE ~ NA_character_
      )

      eligible <- is.na(skip_reason)

      ad_result <- NULL
      ad_call_failed <- FALSE
      if (any(eligible)) {
        ad_result <- tryCatch(
          as.data.frame(NPSdatastore::active_directory_lookup(emails = candidates[eligible])),
          error = function(e) {
            ad_call_failed <<- TRUE
            if (require_ad_verification) {
              shiny::showNotification(
                paste0("Active Directory lookup failed - cannot verify Editor email(s): ",
                       conditionMessage(e), ". No unverified Editors can be added."),
                type = "error"
              )
            } else {
              shiny::showNotification(
                paste0("Active Directory lookup failed (you can still enter details manually): ",
                       conditionMessage(e)),
                type = "warning"
              )
            }
            NULL
          }
        )
      }

      is_ad_verified <- rep(TRUE, length(candidates))
      if (require_ad_verification) {
        is_ad_verified <- rep(FALSE, length(candidates))
        if (!is.null(ad_result)) {
          for (i in which(eligible)) {
            ad_row <- ad_result[ad_result$searchTerm == candidates[i], ][1, ]
            is_ad_verified[i] <- !is.na(ad_row$found) && isTRUE(ad_row$found)
          }
        }
        skip_reason[eligible & !is_ad_verified] <- "not a verified Active Directory match"
      }

      valid_emails <- candidates[is.na(skip_reason)]
      skipped <- tibble::tibble(email = candidates[!is.na(skip_reason)],
                                reason = skip_reason[!is.na(skip_reason)])

      if (length(valid_emails) == 0) {
        shiny::showNotification("No valid new email addresses to add.", type = "warning")
        return(invisible(NULL))
      }

      new_rows <- purrr::map_dfr(valid_emails, function(email) {
        row <- tibble::tibble(
          email = email,
          givenName = "",
          surName = "",
          organizationName = "",
          userId = ""
        )
        if (with_role) row$role <- "contributor"
        row$ad_found <- FALSE
        row$orcid_status <- ""

        if (!is.null(ad_result)) {
          ad_row <- ad_result[ad_result$searchTerm == email, ][1, ]
          if (nrow(ad_row) > 0 && !is.na(ad_row$found) && isTRUE(ad_row$found)) {
            row$givenName <- ifelse(is.na(ad_row$givenName), "", ad_row$givenName)
            row$surName <- ifelse(is.na(ad_row$sn), "", ad_row$sn)
            row$userId <- normalize_orcid(ifelse(is.na(ad_row$orcid), "", ad_row$orcid))
            row$organizationName <- "National Park Service"
            row$ad_found <- TRUE
            # Resolve-check the AD-supplied ORCID once, now, and cache it.
            # Gated to categories that enforce ORCID rules (authors) so no
            # other category incurs ORCID network traffic.
            if (require_orcid_if_ad_found && nzchar(row$userId)) {
              row$orcid_status <- orcid_resolves(row$userId)
            }
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

    shiny::observeEvent(input$add_blank, {
      current <- people()
      new_row <- tibble::tibble(
        email = "",
        givenName = "",
        surName = "",
        organizationName = "",
        userId = ""
      )
      if (with_role) new_row$role <- "contributor"
      new_row$ad_found <- FALSE
      new_row$orcid_status <- ""
      people(rbind(current, new_row))
      shiny::showNotification("Added a blank row - fill in the author's details in the table.",
                              type = "message")
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

      # 0-based column indices (rownames = FALSE), resolved by NAME.
      cn <- names(display_df)
      ad_found_idx <- which(cn == "ad_found") - 1
      orcid_status_idx <- which(cn == "orcid_status") - 1
      remove_idx   <- which(cn == "remove") - 1
      email_idx    <- which(cn == "email") - 1

      # hidden internal columns + remove button are always locked;
      # email is locked unless unlock_email.
      locked_cols <- c(ad_found_idx, orcid_status_idx, remove_idx)
      if (!unlock_email) locked_cols <- c(email_idx, locked_cols)

      # colnames must match display_df's column count:
      # [person cols incl userId/(role)], ad_found, orcid_status, remove.
      disp_colnames <- c(col_labels, "AD verified", "ORCID status", "")

      DT::datatable(
        display_df,
        rownames = FALSE,
        selection = "none",
        colnames = disp_colnames,
        escape = which(names(display_df) != "remove") - 1,
        options = list(
          dom = 't', pageLength = -1, scrollX = TRUE,
          columnDefs = list(list(targets = c(ad_found_idx, orcid_status_idx), visible = FALSE))
        ),
        editable = list(target = "cell", disable = list(columns = locked_cols))
      )
    })

    shiny::observeEvent(input$remove_row, {
      idx <- input$remove_row
      current <- people()
      shiny::req(idx >= 1, idx <= nrow(current))
      removed_email <- current$email[idx]
      people(current[-idx, , drop = FALSE])
      label <- if (nzchar(trimws(removed_email %||% ""))) removed_email else "author"
      shiny::showNotification(paste0("Removed ", label, "."), type = "message")
    })

    shiny::observeEvent(input$people_table_cell_edit, {
      edit <- input$people_table_cell_edit
      current <- people()
      if (edit$col >= ncol(current)) return(invisible(NULL))
      edited_col <- names(current)[edit$col + 1]
      updated <- DT::editData(current, edit, rownames = FALSE)

      # If the ORCID (userId) was the edited cell, re-run the resolution
      # check for JUST that row and cache the verdict. This is the only
      # place (besides AD auto-fill) a network call happens - never inside
      # errors()/warnings(). Gated to authors via require_orcid_if_ad_found.
      if (identical(edited_col, "userId") && require_orcid_if_ad_found) {
        i <- edit$row
        if (!is.null(i) && i >= 1 && i <= nrow(updated)) {
          val <- trimws(updated$userId[i] %||% "")
          updated$orcid_status[i] <- if (nzchar(val)) orcid_resolves(val) else ""
        }
      }
      people(updated)
    })

    #' HARD, blocking, row-naming validity problems. Empty == valid.
    #' Reads the cached orcid_status column - does NOT hit the network.
    errors <- shiny::reactive({
      df <- people()
      msgs <- character(0)

      if (nrow(df) == 0) {
        if (require_nonempty) {
          msgs <- c(msgs, paste0("At least one ", category_label, " is required."))
        }
        return(msgs)
      }

      gn  <- safe_chr(df$givenName)
      sn  <- safe_chr(df$surName)
      org <- safe_chr(df$organizationName)
      orc <- safe_chr(df$userId)
      em  <- safe_chr(df$email)
      status <- safe_chr(df$orcid_status)

      af <- df$ad_found
      if (is.null(af)) af <- rep(FALSE, nrow(df))
      af <- as.logical(af); af[is.na(af)] <- FALSE

      for (i in seq_len(nrow(df))) {
        name <- trimws(paste(gn[i], sn[i]))
        label <- if (nzchar(name)) name else if (nzchar(em[i])) em[i] else paste0("Row ", i)

        missing <- character(0)
        if (!nzchar(gn[i])) missing <- c(missing, "a given name")
        if (!nzchar(sn[i])) missing <- c(missing, "a surname")
        if (require_org && !nzchar(org[i])) missing <- c(missing, "an organization")
        if (length(missing) > 0) {
          msgs <- c(msgs, paste0(label, " is missing ", and_list(missing), "."))
        }

        if (require_orcid_if_ad_found) {
          if (nzchar(orc[i]) && !isTRUE(is_valid_orcid(orc[i]))) {
            # malformed - hard error (format), never network-checked
            msgs <- c(msgs, paste0(
              label, " has an ORCID that isn't in the form 0000-0000-0000-0000 ",
              "(the final character may be X)."
            ))
          } else if (nzchar(orc[i]) && identical(status[i], "not_found")) {
            # well-formed but the ORCID public API says it doesn't exist
            msgs <- c(msgs, paste0(
              label, " has an ORCID (", orc[i], ") that does not resolve on orcid.org ",
              "- check it is correct."
            ))
          } else if (af[i] && !nzchar(orc[i])) {
            msgs <- c(msgs, paste0(
              label, " was found in Active Directory and must have an ORCID."
            ))
          }
        }
      }

      msgs
    })

    #' SOFT, non-blocking advisories (e.g. ORCID could not be verified due
    #' to a network issue). Shown inline but do NOT affect validity/Generate.
    warnings <- shiny::reactive({
      df <- people()
      if (nrow(df) == 0 || !require_orcid_if_ad_found) return(character(0))

      gn  <- safe_chr(df$givenName)
      sn  <- safe_chr(df$surName)
      orc <- safe_chr(df$userId)
      em  <- safe_chr(df$email)
      status <- safe_chr(df$orcid_status)

      msgs <- character(0)
      for (i in seq_len(nrow(df))) {
        # only warn about well-formed ORCIDs we couldn't verify; malformed
        # ones are already a hard error above, blanks are irrelevant here
        if (nzchar(orc[i]) && isTRUE(is_valid_orcid(orc[i])) &&
            identical(status[i], "unchecked")) {
          name <- trimws(paste(gn[i], sn[i]))
          label <- if (nzchar(name)) name else if (nzchar(em[i])) em[i] else paste0("Row ", i)
          msgs <- c(msgs, paste0(
            label, "'s ORCID could not be verified against orcid.org (network ",
            "issue) - it looks valid and you can proceed, but double-check it."
          ))
        }
      }
      msgs
    })

    is_valid <- shiny::reactive(length(errors()) == 0)

    output$validity <- shiny::renderUI({
      errs <- errors()
      warns <- warnings()
      if (length(errs) == 0 && length(warns) == 0) return(NULL)
      shiny::tagList(
        if (length(errs) > 0) {
          shiny::tags$div(
            class = "alert alert-warning mt-2 mb-0",
            style = "font-size: 0.9em;",
            shiny::tags$ul(class = "mb-0", lapply(errs, shiny::tags$li))
          )
        },
        if (length(warns) > 0) {
          shiny::tags$div(
            class = "alert alert-info mt-2 mb-0",
            style = "font-size: 0.9em;",
            shiny::tags$ul(class = "mb-0", lapply(warns, shiny::tags$li))
          )
        }
      )
    })

    #' Restore a saved category tibble. Does NOT re-run AD lookup or the
    #' ORCID resolution check - saved flags/status are trusted as-is.
    #' Older save files predating orcid_status default to "unchecked" for
    #' any non-blank ORCID (honest soft warning, non-blocking) and "" for
    #' blank; predating ad_found default to FALSE.
    #'
    #' @param saved list-of-columns shape for one category, or NULL/empty
    restore <- function(saved) {
      if (is.null(saved) || length(saved$email %||% character(0)) == 0) {
        people(empty_person_tbl(with_role))
        return(invisible(NULL))
      }

      restored <- tibble::tibble(
        email = as.character(saved$email),
        givenName = as.character(saved$givenName %||% ""),
        surName = as.character(saved$surName %||% ""),
        organizationName = as.character(saved$organizationName %||% ""),
        userId = as.character(saved$userId %||% "")
      )
      if (with_role) {
        restored$role <- as.character(saved$role %||% "contributor")
      }

      af <- saved$ad_found
      if (is.null(af) || length(af) != nrow(restored)) {
        af <- rep(FALSE, nrow(restored))
      }
      af <- as.logical(af); af[is.na(af)] <- FALSE
      restored$ad_found <- af

      st <- saved$orcid_status
      if (is.null(st) || length(st) != nrow(restored)) {
        # older save file: default unchecked where an ORCID exists
        uid <- trimws(restored$userId)
        st <- ifelse(nzchar(uid), "unchecked", "")
      }
      st <- as.character(st); st[is.na(st)] <- ""
      restored$orcid_status <- st

      people(restored)
      invisible(NULL)
    }

    list(data = people, valid = is_valid, errors = errors,
         warnings = warnings, restore = restore)
  })
}

peopleInput <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    person_category_ui(
      ns("authors"), "Authors (Creators) (Required)",
      paste0("At least one author is required. Authors must be individuals, ",
             "not organizations. Authors must have a given name and surname. ",
             "Authors are listed as 'creator' in the metadata and appear in ",
             "the data package citation. Entering an NPS email checks Active ",
             "Directory and auto-fills name, ORCID, and organization where ",
             "possible. You can also use 'Add blank row' to enter an author ",
             "by hand (e.g. an external collaborator with no email)."),
      allow_manual_add = TRUE
    ),
    person_category_ui(
      ns("contacts"), "Contacts (Required)",
      paste0("Contacts must be NPS employees or partners familiar with all ",
             "aspects of the data package - almost always one or more of ",
             "the authors. Think 'corresponding author.'")
    ),
    person_category_ui(
      ns("contributors"), "Contributors (Optional)",
      paste0("Contributors did not rise to the level of authorship but ",
             "should still be acknowledged. Each can be given a custom role ",
             "(e.g. 'Field Assistant')."),
      with_role = TRUE
    ),
    person_category_ui(
      ns("editors"), "Editors (Optional)",
      paste0("Editors can make reasonable updates to the DataStore ",
             "reference (e.g. fixing typos, updating permissions) and are ",
             "the only ones who can access a draft reference - include ",
             "potential reviewers here. Editors must be NPS employees or ",
             "partners with a VERIFIED Active Directory account - an ",
             "email that cannot be matched in Active Directory will be ",
             "rejected.")
    ),
    col_widths = c(-2, 8, -2), fill = FALSE
  )
}

#' @return list(data = <reactive() list>, restore = <function>)
#'   data() returns $authors/$contacts/$contributors/$editors tibbles,
#'   $valid (logical), and $errors (the concatenation of every category's
#'   HARD errors() - soft warnings are inline-only and do NOT block).
#' @noRd
peopleServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    authors <- person_category_server("authors", require_nonempty = TRUE,
                                      allow_manual_add = TRUE, require_org = FALSE,
                                      unlock_email = TRUE,
                                      require_orcid_if_ad_found = TRUE,
                                      category_label = "author")
    contacts <- person_category_server("contacts", category_label = "contact")
    contributors <- person_category_server("contributors", with_role = TRUE,
                                           category_label = "contributor")
    editors <- person_category_server("editors", require_ad_verification = TRUE,
                                      category_label = "editor")

    data <- shiny::reactive({
      # Only HARD errors gate Generate; soft warnings are inline-only.
      errors <- c(authors$errors(), contacts$errors(),
                  contributors$errors(), editors$errors())

      list(
        authors = authors$data(),
        contacts = contacts$data(),
        contributors = contributors$data(),
        editors = editors$data(),
        valid = length(errors) == 0,
        errors = errors
      )
    })

    restore <- function(saved) {
      if (is.null(saved)) return(invisible(NULL))
      authors$restore(saved$authors)
      contacts$restore(saved$contacts)
      contributors$restore(saved$contributors)
      editors$restore(saved$editors)
      invisible(NULL)
    }

    list(data = data, restore = restore)
  })
}

#' Emit the personnel.txt-writing chunk. Pure function. Editors ARE
#' included (role = "editor"). The internal ad_found / orcid_status columns
#' are NOT written - only the EMLassemblyline-expected columns are emitted.
#'
#' @param state the list returned by peopleServer()'s $data reactive
#' @param working_folder_var name of the R variable holding the working
#'   folder path in the generated script (default "working_folder")
#' @return character - the R code chunk, or an explanatory comment if invalid
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
    to_personnel_rows(state$contributors, NA_character_),  # role col already present
    to_personnel_rows(state$editors, "editor")
  )

  tribble_str <- tibble_to_r_tribble(personnel)

  glue::glue(
    'personnel_df <- {tribble_str}\n',
    'readr::write_tsv(personnel_df, file.path({working_folder_var}, "personnel.txt"), na = "")\n'
  )
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && a == "")) b else a
