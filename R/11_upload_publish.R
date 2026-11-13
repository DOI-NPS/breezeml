# 11_upload_publish.R v10
#
# v10 changes (bug fix):
#   - The Step 4 "Activate reference" button could be clicked before the
#     "I certify..." checkbox was checked. Cause: v6 passed
#     actionButton(disabled = NA). Current Shiny versions test
#     isTRUE(disabled), and isTRUE(NA) is FALSE, so the button was never
#     disabled. The disabled attribute is now added with
#     htmltools::tagAppendAttributes(), which works on every Shiny version.
#   - The unavailable button now uses the new btn-unavailable class
#     (breezeml.css), which is plainly gray. The theme's btn-secondary
#     is dark green (#4f633c), so the old "gray" state looked enabled.
#   - Hovering the unavailable button explains how to enable it.
#   - The server-side check in build_activation_confirmation() (blocker
#     "not_certified") is unchanged and still refuses activation if the
#     box is unchecked.
#
# Tab 11: "Upload and Publish" - the step AFTER Generate (Tab 9) and
# Congruence checks (Tab 10). Four actions against a DataStore draft
# reference:
#
#   1. UPLOAD the data package (all .csv + the _metadata.xml) to the draft
#      reference. The reference id is DERIVED (read-only) from the DOI inside
#      the directory's *_metadata.xml - the same derivation
#      upload_data_package() does internally - so users cannot mis-set it.
#      The upload DIRECTORY is editable (prefilled from the last Generate),
#      so the tab works standalone on any data package directory, including
#      one built in an earlier session.
#   2. EXTRACT metadata (STUB) - DataStore endpoint not built yet.
#   3. REQUEST REVIEW - two steps:
#        a. "Verify & add reviewer": the reviewer's email (or UPN) is looked
#           up in Active Directory (NPSdatastore::active_directory_lookup);
#           if found and not disabled, the reviewer is ADDED as an editor on
#           the DataStore reference (NPSdatastore::add_reference_editors).
#        b. "Generate Draft Email": a mailto: link opens a prefilled draft
#           (addressed to the verified reviewer) in the user's own email
#           program (e.g. Outlook). The user reviews it and clicks Send
#           there. breezeml never sends mail. The body is PLAIN TEXT (a
#           mailto: body cannot carry HTML), so the links are URLs, each on
#           its own line; whether they are clickable depends on the mail
#           client.
#   4. ACTIVATE / PUBLISH - self-certified. The Activate button is disabled
#      and gray until the certification checkbox is checked, then becomes
#      enabled and green (v6). Activation API call still TODO.
#
# v9 changes (Step 3B):
#   - REVERTED the v8 .eml download (most users would not know how to use
#     it). Back to a mailto: link that opens a draft in the email program.
#     Restored build_reviewer_mailto() and the MAILTO_MAX_CHARS backstop;
#     removed build_reviewer_eml() / .encode_mime_header() / the
#     downloadHandler.
#   - KEPT from v8: link to the Data Package Review best practices page
#     (REVIEW_BEST_PRACTICES_URL); the user's custom message appears near
#     the START of the email (right after the greeting and opening request,
#     above the links); no on-screen email preview; button renamed
#     "Generate Draft Email".
#   - build_reviewer_invite_email() returns list(subject, body) (plain
#     text).
#
# v8 (superseded): .eml/HTML draft download with live hyperlinks - dropped
#   in v9 for usability.
#
# v7 changes (Step 3 rework):
#   - Reviewer identifier is REQUIRED. An identifier with an underscore before
#     the "@" (first_last@nps.gov) is treated as an EMAIL and passed as
#     emails =; one without an underscore (flast@nps.gov) is treated as a
#     UPN and passed as upns =. The other argument is left unspecified.
#   - AD check -> add_reference_editors(dev = is_datastore_dev(),
#     interactive = FALSE). Verified state is tied to BOTH the identifier
#     text and the reference id: editing either invalidates it (the email
#     button re-disables) without any explicit clearing.
#   - Custom message box, capped at REVIEW_CUSTOM_TEXT_MAX characters
#     (client-side maxlength + server-side truncation) so the mailto: URL
#     stays under common mail-client limits.
#   - New pure helpers: parse_reviewer_identifier(), lookup_reviewer(),
#     add_reviewer_to_reference().
#
# v6 changes:
#   - Activate button (Step 4) is now rendered reactively: disabled + gray
#     (btn-secondary) until the "I certify..." checkbox is checked, then
#     enabled + green (btn-success).
#   - Removed the temporary DEBUG cat() from confirm_upload (post-upload
#     banner behavior confirmed working).
#
# v5 changes:
#   - Post-upload suppression of the "already has files" warning keys off a
#     just_uploaded_ref reactiveVal compared with == (NA-safe) rather than
#     identical() (type-strict). Shows ONLY the green success banner after an
#     upload.
#   - API error messages ANSI-stripped (strip_ansi()).
#   - Derived-reference line links to the reference PROFILE URL (works
#     pre-activation) instead of the DOI (doesn't resolve until activated).
#   - Added the "Have DataStore extract metadata" stub card.
#
# ============================= STUB STATUS =============================
# STILL stubbed / `# TODO`:
#   - ACTIVATE/PUBLISH API call (function name unknown - NOT guessed).
#   - EXTRACT metadata API call (endpoint not built yet).
#   - datastore_reference_url(): PROD format confirmed
#     (https://irma.nps.gov/DataStore/Reference/Profile/<id>); DEV format
#     (irmadev...) still unconfirmed - verify before relying on dev links.
#
# FIRST-RUN CHECKS (not yet exercised against live systems):
#   - add_reference_editors() is called with ONLY emails= or ONLY upns=
#     (the other left unspecified), per the user's instruction. If it errors
#     with something like 'argument "upns" is missing', that is where to
#     look.
#   - Whether adding the SAME reviewer twice to a reference is harmless is
#     unknown; the Verify button disables itself after a successful add for
#     the same identifier + reference to avoid it.
#   - Confirm the generated mailto: draft opens correctly in Outlook with
#     the full body (best-practices link added in v9 makes the URL longer;
#     the MAILTO_MAX_CHARS backstop will disable the button and explain if
#     it ever gets too long).
#
# FUTURE-PROOFING: reference ids are passed to EMLeditor/NPSdatastore API
# functions as NUMERIC (as.numeric()) - NPSdatastore's .validate_ref_id()
# rejects character ids.
#
# Module contract (matches 07_permissions.R etc.):
#   uploadPublishServer(id, package_dir_reactive, doi_state_reactive,
#                       package_title_reactive)
#     returns list(data = <reactive>, restore = <function>)
#   data() returns $upload_dir, $reference_id (derived), $certified_review.
#   restore() re-applies the persisted upload_dir.
#   (Reviewer verification state is deliberately module-local and NOT part
#   of data()/save-load: it must be re-done per session.)

# ---- Constants (Step 3) ------------------------------------------------

# Data package review checklist. The ?Inv=TRUE suffix is important: if the
# reference is versioned it makes the link resolve to the most recent
# version. Always the PROD URL (the checklist lives on prod).
REVIEW_CHECKLIST_URL <- "https://irma.nps.gov/DataStore/Reference/Profile/2306391?Inv=TRUE"

# Data Package Review best practices (SharePoint).
REVIEW_BEST_PRACTICES_URL <- "https://doimspp.sharepoint.com/sites/nps-nrss-imdiv/data-publication/SitePages/Data-Package-Reviews.aspx"

# Max characters the user may type into the custom-message box. Chosen so
# the fully URL-encoded mailto: link (boilerplate + three URLs + this text)
# stays comfortably under the ~2000 character limit some mail clients/OS
# handlers impose.
REVIEW_CUSTOM_TEXT_MAX <- 400L

# Backstop: refuse to offer a mailto: link longer than this (encoded).
MAILTO_MAX_CHARS <- 1900L

#' Summarize the files that upload_data_package() would upload from a
#' directory. Pure function (filesystem read only).
#'
#' @param directory the data_package/ directory
#' @return list(files = tibble(name, size_bytes, size_human),
#'   has_metadata = logical, metadata_count = integer)
summarize_package_files <- function(directory) {
  if (is.null(directory) || !nzchar(directory) || !dir.exists(directory)) {
    return(list(
      files = tibble::tibble(name = character(0), size_bytes = numeric(0),
                             size_human = character(0)),
      has_metadata = FALSE, metadata_count = 0L
    ))
  }

  csvs <- list.files(directory, pattern = "\\.csv$", ignore.case = TRUE, full.names = FALSE)
  xmls <- list.files(directory, pattern = "metadata\\.xml$", ignore.case = TRUE, full.names = FALSE)
  all_names <- c(csvs, xmls)

  sizes <- vapply(all_names, function(n) file.size(file.path(directory, n)), numeric(1))

  human <- function(b) {
    if (is.na(b)) return(NA_character_)
    units <- c("B", "KB", "MB", "GB")
    i <- if (b <= 0) 1 else min(length(units), floor(log(b, 1024)) + 1)
    sprintf("%.1f %s", b / (1024^(i - 1)), units[i])
  }

  files <- tibble::tibble(
    name = all_names,
    size_bytes = sizes,
    size_human = vapply(sizes, human, character(1))
  )

  list(files = files, has_metadata = length(xmls) >= 1, metadata_count = length(xmls))
}

#' Strip ANSI/cli color escape sequences from a string so it renders as
#' clean text in HTML. Uses cli::ansi_strip() when available, with a plain
#' regex fallback. Also collapses the whitespace runs that cli's multi-bullet
#' formatting leaves behind once the codes are gone.
#'
#' @param x character (possibly length > 1; collapsed to one string)
#' @return character scalar, escape-free
strip_ansi <- function(x) {
  if (length(x) == 0) return("")
  x <- paste(x, collapse = " ")
  stripped <- tryCatch(cli::ansi_strip(x), error = function(e) NULL)
  if (is.null(stripped)) {
    stripped <- gsub("\033\\[[0-9;]*[A-Za-z]", "", x)  # CSI: ESC [ ... letter
  }
  stripped <- gsub("[ \t\r\n]+", " ", stripped)
  trimws(stripped)
}

#' TRUE if x is a single, non-NA, non-empty string. NA-safe (unlike the
#' module's %||% helper, which cannot take an NA_character_).
#' @noRd
.nz <- function(x) {
  !is.null(x) && length(x) == 1 && !is.na(x) && nzchar(x)
}

#' Derive the DataStore reference id from the *_metadata.xml inside a
#' directory, mirroring upload_data_package()'s internal derivation: read the
#' single metadata file (EML::read_eml), get its DOI (EMLeditor::get_doi),
#' take the last 7 digits. Filesystem read only, no network. Returns a
#' structured result so the UI can distinguish the failure modes.
#'
#' @param directory the data_package/ directory
#' @return list(ok = logical, reference_id = integer or NA, doi = chr or NA,
#'   metadata_file = chr or NA, message = chr or NULL)
reference_from_directory <- function(directory) {
  out <- list(ok = FALSE, reference_id = NA_integer_, doi = NA_character_,
              metadata_file = NA_character_, message = NULL)

  if (is.null(directory) || !nzchar(directory) || !dir.exists(directory)) {
    out$message <- "Directory does not exist."
    return(out)
  }

  xmls <- list.files(directory, pattern = "metadata\\.xml$", ignore.case = TRUE,
                     full.names = TRUE)
  if (length(xmls) < 1) {
    out$message <- "No *_metadata.xml found in this directory."
    return(out)
  }
  if (length(xmls) > 1) {
    out$message <- "More than one *_metadata.xml found - a data package may contain only one."
    return(out)
  }
  out$metadata_file <- xmls[1]

  doi <- tryCatch({
    md <- EML::read_eml(xmls[1], from = "xml")
    EMLeditor::get_doi(md)
  }, error = function(e) e)

  if (inherits(doi, "condition")) {
    out$message <- paste0("Could not read a DOI from the metadata file: ",
                          strip_ansi(conditionMessage(doi)))
    return(out)
  }
  if (is.null(doi) || is.na(doi) || !nzchar(doi)) {
    out$message <- paste0("The metadata file has no DOI. Create a DataStore ",
                          "draft reference (Generate tab / set_datastore_doi) first.")
    return(out)
  }

  digits <- gsub("[^0-9]", "", doi)
  if (nchar(digits) < 7) {
    out$message <- paste0("The DOI (", doi, ") does not contain a 7-digit reference id.")
    out$doi <- doi
    return(out)
  }

  out$ok <- TRUE
  out$doi <- doi
  out$reference_id <- as.integer(substr(digits, nchar(digits) - 6, nchar(digits)))
  out
}

#' Build the DataStore reference landing-page (Profile) URL. Pure function.
#' PROD format confirmed; DEV format (irmadev) still TODO - verify.
#'
#' @param reference_id integer
#' @param dev logical - is_datastore_dev()
#' @return character URL, or NA_character_ if no id
datastore_reference_url <- function(reference_id, dev = FALSE) {
  if (is.null(reference_id) || is.na(reference_id)) return(NA_character_)
  base <- if (isTRUE(dev)) "https://irmadev.nps.gov/DataStore/Reference/Profile/"
  else             "https://irma.nps.gov/DataStore/Reference/Profile/"
  paste0(base, reference_id)
}

#' Query a DataStore reference's current file info via
#' NPSdatastore::get_file_info(). VPN-gated. nps_internal = TRUE (draft
#' references aren't on the public endpoint yet). nrow() = file count.
#' Error messages are ANSI-stripped for clean HTML display.
#'
#' @param reference_id integer
#' @return list(ok, count, files, message)
reference_file_status <- function(reference_id) {
  if (is.null(reference_id) || is.na(reference_id)) {
    return(list(ok = FALSE, count = NA_integer_, files = character(0),
                message = "No reference ID available to check."))
  }
  res <- tryCatch(
    NPSdatastore::get_file_info(reference_id, nps_internal = TRUE),
    error = function(e) e
  )
  if (inherits(res, "condition")) {
    return(list(ok = FALSE, count = NA_integer_, files = character(0),
                message = strip_ansi(conditionMessage(res))))
  }
  n <- nrow(res)
  fnames <- if (n > 0 && "fileName" %in% names(res)) as.character(res$fileName) else character(0)
  list(ok = TRUE, count = n, files = fnames, message = NULL)
}

# ---- Step 3 helpers: reviewer verification / email ---------------------

#' Classify and sanity-check what the user typed as the reviewer identifier.
#' Pure function (no network).
#'
#' An identifier whose part before the "@" contains an underscore
#' (first_last@nps.gov) is an EMAIL; one without (flast@nps.gov) is a UPN.
#'
#' @param x the raw text from the input box
#' @return list(ok, kind = "email"|"upn"|NA, value, input, message)
#'   - input is the trimmed text (used to detect later edits)
parse_reviewer_identifier <- function(x) {
  x <- trimws(if (is.null(x) || length(x) == 0 || is.na(x[1])) "" else x[1])
  out <- list(ok = FALSE, kind = NA_character_, value = NA_character_,
              input = x, message = NULL)

  if (!nzchar(x)) {
    out$message <- "Enter the reviewer's email address."
    return(out)
  }
  if (!grepl("^[^@[:space:]]+@[^@[:space:]]+$", x)) {
    out$message <- paste0("That doesn't look like an email address. Use the ",
                          "form first_last@nps.gov.")
    return(out)
  }

  local_part <- sub("@.*$", "", x)
  out$ok <- TRUE
  out$value <- x
  out$kind <- if (grepl("_", local_part, fixed = TRUE)) "email" else "upn"
  out
}

#' Safely read one field from the first row of an AD-lookup tibble; NA if
#' the column is absent (never uses $ on a maybe-absent column).
#' @noRd
.ad_field <- function(res, name) {
  if (is.data.frame(res) && nrow(res) >= 1 && name %in% names(res)) {
    as.character(res[[name]][1])
  } else {
    NA_character_
  }
}

#' Look the reviewer up in Active Directory via
#' NPSdatastore::active_directory_lookup(). VPN-gated. Passes the
#' identifier as emails= OR upns= (never both), per parse_reviewer_identifier.
#' Accepts only found == TRUE and disabled != TRUE.
#'
#' @param parsed result of parse_reviewer_identifier() (ok = TRUE)
#' @return on success list(ok = TRUE, mail, upn, display_name, given_name);
#'   on failure list(ok = FALSE, reason = "not_found"|"disabled"|"error",
#'   stage = "lookup", message)
lookup_reviewer <- function(parsed) {
  res <- tryCatch(
    if (identical(parsed$kind, "email")) {
      NPSdatastore::active_directory_lookup(emails = parsed$value)
    } else {
      NPSdatastore::active_directory_lookup(upns = parsed$value)
    },
    error = function(e) e
  )

  if (inherits(res, "condition")) {
    return(list(ok = FALSE, reason = "error", stage = "lookup",
                message = strip_ansi(conditionMessage(res))))
  }

  found <- is.data.frame(res) && nrow(res) >= 1 && "found" %in% names(res) &&
    isTRUE(res[["found"]][1])
  if (!found) {
    return(list(ok = FALSE, reason = "not_found", stage = "lookup",
                message = paste0("No Active Directory account was found for ",
                                 parsed$value, ".")))
  }

  is_disabled <- "disabled" %in% names(res) && isTRUE(res[["disabled"]][1])
  if (is_disabled) {
    return(list(ok = FALSE, reason = "disabled", stage = "lookup",
                message = paste0("The Active Directory account for ", parsed$value,
                                 " exists but is deactivated.")))
  }

  mail <- .ad_field(res, "mail")
  if (!.nz(mail)) mail <- parsed$value   # fall back to what the user typed

  list(ok = TRUE,
       mail = mail,
       upn = .ad_field(res, "userPrincipalName"),
       display_name = .ad_field(res, "displayName"),
       given_name = .ad_field(res, "givenName"))
}

#' Add the (already AD-verified) reviewer as an editor on a DataStore
#' reference via NPSdatastore::add_reference_editors(). Additive - existing
#' editors are kept. dev is passed through (is_datastore_dev()); interactive
#' = FALSE because console prompts are meaningless in the GUI. Only ONE of
#' emails=/upns= is supplied (the other is left unspecified).
#'
#' Errors are caught (cli aborts THROW); warnings are collected and
#' returned rather than printed.
#'
#' @param reference_id integer/numeric reference id
#' @param parsed result of parse_reviewer_identifier()
#' @param dev logical - is_datastore_dev()
#' @return list(ok, n_editors, warnings, message)
add_reviewer_to_reference <- function(reference_id, parsed, dev = FALSE) {
  warnings_seen <- character(0)

  res <- withCallingHandlers(
    tryCatch({
      editors <- if (identical(parsed$kind, "email")) {
        NPSdatastore::add_reference_editors(
          reference_id = as.numeric(reference_id),
          emails = parsed$value,
          dev = dev, interactive = FALSE)
      } else {
        NPSdatastore::add_reference_editors(
          reference_id = as.numeric(reference_id),
          upns = parsed$value,
          dev = dev, interactive = FALSE)
      }
      list(ok = TRUE,
           n_editors = if (is.data.frame(editors)) nrow(editors) else NA_integer_,
           message = NULL)
    }, error = function(e) {
      list(ok = FALSE, n_editors = NA_integer_,
           message = strip_ansi(conditionMessage(e)))
    }),
    warning = function(w) {
      warnings_seen <<- c(warnings_seen, strip_ansi(conditionMessage(w)))
      invokeRestart("muffleWarning")
    }
  )

  res$warnings <- warnings_seen
  res
}

#' Draft the review-request email (PLAIN TEXT, for a mailto: body). Pure
#' function.
#'
#' Order: greeting -> opening request -> the user's CUSTOM message (near the
#' start) -> reference link -> review checklist link -> review best
#' practices link -> closing. Each URL is on its own line so mail clients
#' that auto-link URLs can do so.
#'
#' @param reference_id integer
#' @param dev logical - is_datastore_dev()
#' @param package_title chr or NULL
#' @param custom_text chr or NULL - user text (truncated to
#'   REVIEW_CUSTOM_TEXT_MAX here as a safety net)
#' @param reviewer_given_name chr or NULL - used for the greeting
#' @return list(subject, body)
build_reviewer_invite_email <- function(reference_id, dev = FALSE,
                                        package_title = NULL,
                                        custom_text = NULL,
                                        reviewer_given_name = NULL) {
  has_title <- .nz(package_title)
  has_ref <- !is.null(reference_id) && length(reference_id) == 1 && !is.na(reference_id)
  url <- datastore_reference_url(if (has_ref) reference_id else NA, dev)

  subject <- if (has_title) {
    paste0("Data package review request: ", package_title,
           if (has_ref) paste0(" (DataStore reference ", reference_id, ")") else "")
  } else {
    paste0("Data package review request",
           if (has_ref) paste0(": DataStore reference ", reference_id) else "")
  }

  greeting <- if (.nz(reviewer_given_name)) paste0("Hello ", reviewer_given_name, ",") else "Hello,"

  custom <- ""
  if (.nz(custom_text)) {
    custom <- trimws(substr(custom_text, 1, REVIEW_CUSTOM_TEXT_MAX))
  }

  body <- paste0(
    greeting, "\n\n",
    "I have given you access to a draft NPS data package on DataStore and ",
    "would like to request your review before it is activated (made public).\n\n",
    if (nzchar(custom)) paste0(custom, "\n\n") else "",
    if (has_title) paste0("Data package: ", package_title, "\n") else "",
    "Draft reference", if (isTRUE(dev)) " (DEV)" else "", ":\n",
    if (.nz(url)) url else "[reference URL]", "\n\n",
    "Data package review checklist:\n",
    REVIEW_CHECKLIST_URL, "\n\n",
    "Data package review best practices:\n",
    REVIEW_BEST_PRACTICES_URL, "\n\n",
    "Once your review is complete, please let me know and I will activate the ",
    "reference.\n\n",
    "Thank you,\n"
  )

  list(subject = subject, body = body)
}

#' Build a mailto: URL. Pure function. Newlines become CRLF before encoding
#' (%0D%0A), which mail clients expect. The recipient is not encoded.
#'
#' @return character mailto: URL
build_reviewer_mailto <- function(to, subject, body) {
  enc <- function(x) {
    utils::URLencode(gsub("\n", "\r\n", x, fixed = TRUE), reserved = TRUE)
  }
  paste0("mailto:", to, "?subject=", enc(subject), "&body=", enc(body))
}

#' Confirmation spec for ACTIVATING a reference. Pure function.
build_activation_confirmation <- function(reference_id, dev, certified) {
  if (is.null(reference_id) || is.na(reference_id)) {
    return(list(ok_to_proceed = FALSE, title = "Cannot activate",
                message = "No DataStore reference ID is available to activate.",
                blocker = "no_reference"))
  }
  if (!isTRUE(certified)) {
    return(list(ok_to_proceed = FALSE, title = "Confirm review first",
                message = "Check the box certifying that review is complete before activating the reference.",
                blocker = "not_certified"))
  }
  list(
    ok_to_proceed = TRUE,
    title = "Activate DataStore reference?",
    message = paste0(
      "This will ACTIVATE (publish) DataStore reference ", reference_id,
      if (isTRUE(dev)) " (DEV)" else "", ", making it publicly visible. ",
      "This cannot be undone through this app. Continue?"
    ),
    blocker = NULL
  )
}

uploadPublishUI <- function(id) {
  ns <- shiny::NS(id)

  # Custom-message box with a client-side maxlength (server also truncates).
  custom_text_input <- htmltools::tagQuery(
    shiny::textAreaInput(ns("invite_custom_text"),
                         "Custom message (optional)",
                         value = "", rows = 4, width = "100%",
                         placeholder = "Anything you want the reviewer to know...",
                         resize = "vertical")
  )$find("textarea")$addAttrs(maxlength = REVIEW_CUSTOM_TEXT_MAX)$allTags()

  bslib::layout_columns(

    # --- 1. Upload ---------------------------------------------------
    bslib::card(
      bslib::card_header("1. Upload data package to DataStore"),
      shiny::helpText("Uploads every data file and the metadata .xml in the ",
                      "directory below to the draft DataStore reference named ",
                      "in that package's metadata. Requires an active VPN ",
                      "connection."),

      shiny::textInput(ns("upload_dir"), "Data package directory",
                       placeholder = "path to a folder containing the .csv files and *_metadata.xml",
                       width = "100%"),
      shiny::helpText("Prefilled from the last package you generated this ",
                      "session. You can point this at any data package ",
                      "directory - including one created in an earlier session."),

      shiny::tableOutput(ns("file_list")),

      shiny::uiOutput(ns("derived_reference")),

      shiny::uiOutput(ns("reference_status")),
      shiny::uiOutput(ns("reference_actions")),

      shiny::uiOutput(ns("upload_button")),
      shiny::uiOutput(ns("upload_result")),

      shiny::uiOutput(ns("extract_reminder"))
    ),

    # --- 2. Extract metadata (stub) ---------------------------------
    bslib::card(
      bslib::card_header("2. Have DataStore extract metadata"),
      shiny::helpText("After the files are uploaded, DataStore extracts the ",
                      "metadata from the .xml to populate the reference's ",
                      "landing page (title, abstract, authors, spatial/temporal ",
                      "coverage, etc.)."),
      shiny::tags$div(class = "alert alert-info",
                      shiny::tags$strong("Coming soon. "),
                      "The DataStore API endpoint that triggers metadata extraction does ",
                      "not exist yet - it is planned for the next DataStore release. For ",
                      "now, open the reference and extract the metadata manually (see the ",
                      "link shown after a successful upload above)."),
      shiny::actionButton(ns("extract_metadata"), "Extract metadata",
                          class = "btn-primary", disabled = NA),
      shiny::uiOutput(ns("extract_result"))
    ),

    # --- 3. Request review ------------------------------------------
    bslib::card(
      bslib::card_header("3. Request a review"),
      shiny::helpText("Reviewers must have an account that can be verified in ",
                      "Active Directory. Step A verifies the reviewer and gives ",
                      "them access to the draft reference. Step B opens a draft ",
                      "review request email, with links to the reference, the ",
                      "data package review checklist, and review best ",
                      "practices, in your email program so you can send it. ",
                      "Requires an active VPN connection."),

      shiny::tags$h6("A. Verify the reviewer and give them access"),
      shiny::textInput(ns("reviewer_email"), "Reviewer email",
                       placeholder = "first_last@nps.gov", width = "100%"),
      shiny::helpText("Enter the reviewer's NPS email address (e.g. ",
                      "first_last@nps.gov). A UPN (e.g. flast@nps.gov) also works."),
      shiny::uiOutput(ns("verify_button")),
      shiny::uiOutput(ns("reviewer_result")),

      shiny::tags$hr(),

      shiny::tags$h6("B. Generate the review request email"),
      custom_text_input,
      shiny::uiOutput(ns("invite_char_count")),
      shiny::uiOutput(ns("send_button"))
    ),

    # --- 4. Activate / publish --------------------------------------
    bslib::card(
      bslib::card_header("4. Activate (publish) the reference"),
      shiny::helpText("Once review is complete, activate the reference to make ",
                      "it public. This app does not verify that a review ",
                      "actually occurred - activation relies on your ",
                      "certification below. ",
                      shiny::HTML("<b>","CAUTION:","</b>")," once a reference is ",
                      "activated it cannot be de-activated and most edits ",
                      "are locked. Only publish if you are ",
                      shiny::HTML("<b>", "SURE", "</b>"), " you are ready ",
                      "to publish!"),
      shiny::checkboxInput(ns("certified_review"),
                           label = paste0("I certify that review of this data ",
                                          "package is complete and I have ",
                                          "addressed or responded to all ",
                                          " reviewer concerns."),
                           value = FALSE,
                           width = '100%'),
      # Rendered reactively: gray + disabled until certified, then green +
      # enabled. See output$activate_button in the server.
      shiny::uiOutput(ns("activate_button")),
      shiny::uiOutput(ns("activate_result"))
    ),

    col_widths = c(12, 12, 12, 12), fill = FALSE
  )
}

#' @keywords internal
#' @param id module id
#' @param package_dir_reactive reactive() returning the data_package/ dir from
#'   the last successful run_generation() (used only to PREFILL the editable
#'   directory box).
#' @param doi_state_reactive reactive() returning list(reference_id=, doi=) or
#'   NULL. v4+: not used for the reference id (the .xml is the source of
#'   truth); kept in the signature for contract compatibility.
#' @param package_title_reactive reactive() returning the package title or NULL.
#' @return list(data = <reactive>, restore = <function>)
uploadPublishServer <- function(id,
                                package_dir_reactive,
                                doi_state_reactive = shiny::reactive(NULL),
                                package_title_reactive = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {

    ref_status <- shiny::reactiveVal(NULL)                # reference_file_status() result
    upload_outcome <- shiny::reactiveVal(NULL)            # last upload result
    last_checked_ref <- shiny::reactiveVal(NA_integer_)   # auto-check dedupe
    # Reference id we most recently uploaded to SUCCESSFULLY. Drives the
    # post-upload success view and suppresses the (now-irrelevant) "already
    # has files" warning about the files we just put there. Survives the
    # post-upload re-check; cleared only when the target reference changes.
    just_uploaded_ref <- shiny::reactiveVal(NA_integer_)
    # Result of the last "Verify & add reviewer" click. Carries the
    # identifier text and reference id it was made for ($input, $ref) so it
    # is only honored while BOTH still match (see reviewer_ready()).
    reviewer_outcome <- shiny::reactiveVal(NULL)

    # Prefill the directory box from the last Generate, once, if empty.
    shiny::observe({
      dir <- package_dir_reactive()
      shiny::req(dir, nzchar(dir))
      if (!nzchar(trimws(input$upload_dir %||% ""))) {
        shiny::updateTextInput(session, "upload_dir", value = dir)
      }
    })

    upload_dir <- shiny::reactive({
      trimws(input$upload_dir %||% "")
    })

    file_summary <- shiny::reactive({
      summarize_package_files(upload_dir())
    })

    # Reference derived (read-only) from the directory's *_metadata.xml.
    ref_info <- shiny::reactive({
      reference_from_directory(upload_dir())
    })

    resolved_reference_id <- shiny::reactive({
      ri <- ref_info()
      if (isTRUE(ri$ok)) ri$reference_id else NA_integer_
    })

    do_check <- function() {
      ref <- resolved_reference_id()
      if (is.na(ref)) {
        ref_status(list(ok = FALSE, count = NA_integer_, files = character(0),
                        message = "No reference available to check."))
        return(invisible(NULL))
      }
      shiny::withProgress(message = "Checking reference on DataStore...", value = 0.5, {
        ref_status(reference_file_status(ref))
      })
      last_checked_ref(ref)
      invisible(NULL)
    }

    # AUTO-CHECK: fire once per derived reference id. Changing the directory
    # (hence the derived reference) clears stale state and re-checks.
    shiny::observe({
      ref <- resolved_reference_id()
      shiny::isolate({
        if (!identical(ref, last_checked_ref())) {
          upload_outcome(NULL)
          ref_status(NULL)
          just_uploaded_ref(NA_integer_)
        }
      })
      if (is.na(ref)) return()
      if (identical(ref, shiny::isolate(last_checked_ref()))) return()
      do_check()
    })

    output$file_list <- shiny::renderTable({
      fs <- file_summary()
      if (nrow(fs$files) == 0) return(NULL)
      out <- fs$files[, c("name", "size_human")]
      names(out) <- c("File", "Size")
      out
    })

    # Read-only derived-reference display (or the reason it couldn't derive).
    # Links to the reference PROFILE URL (works pre-activation), not the DOI.
    output$derived_reference <- shiny::renderUI({
      dir <- upload_dir()
      if (!nzchar(dir)) {
        return(shiny::helpText("Enter a data package directory above."))
      }
      ri <- ref_info()
      if (!isTRUE(ri$ok)) {
        return(shiny::tags$div(class = "alert alert-warning mt-2",
                               shiny::tags$strong("No reference id available: "), ri$message))
      }
      url <- datastore_reference_url(ri$reference_id, is_datastore_dev())
      shiny::tags$p(
        shiny::tags$strong("DataStore reference (from metadata): "),
        shiny::tags$code(ri$reference_id),
        shiny::tags$span(" \u2013 "),
        shiny::tags$a(href = url, target = "_blank", url)
      )
    })

    output$reference_status <- shiny::renderUI({
      st <- ref_status()
      ref <- resolved_reference_id()
      if (is.na(ref)) return(NULL)  # derived_reference already explains why

      # If we JUST uploaded successfully to THIS reference, show the success
      # message (output$upload_result) instead of the pre-upload status - do
      # not warn about the files we just put there. == (NA-safe) rather than
      # identical() (type-strict).
      jur <- just_uploaded_ref()
      if (!is.na(jur) && !is.na(ref) && jur == ref) return(NULL)

      if (is.null(st)) {
        return(shiny::helpText("Checking the reference on DataStore..."))
      }
      if (!isTRUE(st$ok)) {
        return(shiny::tags$div(class = "alert alert-danger mt-2",
                               shiny::tags$strong("Could not check the reference: "), st$message,
                               shiny::tags$div(class = "mt-1", "Are you connected to the VPN? Use ",
                                               "Re-check below once connected.")))
      }
      if (st$count > 0) {
        return(shiny::tags$div(class = "alert alert-warning mt-2",
                               shiny::tags$strong("This reference already has ", st$count, " file(s) attached: "),
                               paste(st$files, collapse = ", "), ". ",
                               shiny::tags$div(class = "mt-1",
                                               "The DataStore API cannot re-upload to a reference that already has ",
                                               "files. Use the \u201cRemove existing files\u201d button to clear them, ",
                                               "then upload.")))
      }
      shiny::tags$div(class = "alert alert-success mt-2",
                      "Reference is empty and ready to receive this data package.")
    })

    output$reference_actions <- shiny::renderUI({
      st <- ref_status()
      ref <- resolved_reference_id()
      if (is.na(ref)) return(NULL)

      # After a successful upload to this reference, suppress the re-check /
      # remove-files actions. == (NA-safe), same reasoning as reference_status.
      jur <- just_uploaded_ref()
      if (!is.na(jur) && !is.na(ref) && jur == ref) return(NULL)

      shiny::tagList(
        shiny::actionButton(session$ns("recheck_reference"), "Re-check reference",
                            class = "btn-outline-secondary btn-sm"),
        if (!is.null(st) && isTRUE(st$ok) && st$count > 0) {
          shiny::actionButton(session$ns("remove_files"), "Remove existing files",
                              class = "btn-outline-danger btn-sm")
        }
      )
    })

    shiny::observeEvent(input$recheck_reference, { do_check() })

    # ---- Remove existing files ----
    shiny::observeEvent(input$remove_files, {
      st <- ref_status()
      ref <- resolved_reference_id()
      if (is.na(ref) || is.null(st) || !isTRUE(st$ok) || st$count == 0) return()
      shiny::showModal(shiny::modalDialog(
        title = "Remove all files from this reference?",
        shiny::tags$pre(paste0(
          "This will permanently delete ALL ", st$count, " file(s) currently ",
          "attached to DataStore reference ", ref, ":\n\n  ",
          paste(st$files, collapse = "\n  "), "\n\n",
          "This cannot be undone. (Files cannot be removed from a reference ",
          "that has already been activated.) Continue?"
        )),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(session$ns("confirm_remove"), "Remove files",
                              class = "btn-danger")
        )
      ))
    })

    shiny::observeEvent(input$confirm_remove, {
      shiny::removeModal()
      ref <- resolved_reference_id()
      dev <- is_datastore_dev()
      res <- shiny::withProgress(message = "Removing files from DataStore...", value = 0.5, {
        tryCatch(
          {
            EMLeditor::remove_datastore_files(as.numeric(ref), force = TRUE, dev = dev)
            list(ok = TRUE, message = NULL)
          },
          error = function(e) list(ok = FALSE, message = strip_ansi(conditionMessage(e)))
        )
      })
      if (isTRUE(res$ok)) {
        shiny::showNotification("Files removed from the reference.", type = "message")
        upload_outcome(NULL)
      } else {
        shiny::showNotification(paste0("Could not remove files: ", res$message),
                                type = "error", duration = 10)
      }
      do_check()
    })

    # ---- Upload button ----
    output$upload_button <- shiny::renderUI({
      st <- ref_status()
      fs <- file_summary()
      can_upload <- !is.null(st) && isTRUE(st$ok) && st$count == 0 &&
        isTRUE(fs$has_metadata) && fs$metadata_count == 1
      shiny::actionButton(session$ns("do_upload"), "Upload to DataStore",
                          class = "btn-primary",
                          disabled = if (!can_upload) NA else NULL)
    })

    shiny::observeEvent(input$do_upload, {
      dir <- upload_dir()
      ref <- resolved_reference_id()
      dev <- is_datastore_dev()
      fs <- file_summary()

      if (!nzchar(dir) || !dir.exists(dir)) {
        shiny::showNotification("The directory does not exist.", type = "error")
        return()
      }
      if (!isTRUE(fs$has_metadata) || fs$metadata_count != 1) {
        shiny::showNotification("The directory must contain exactly one *_metadata.xml.",
                                type = "error")
        return()
      }

      n <- nrow(fs$files)
      shiny::showModal(shiny::modalDialog(
        title = "Upload data package to DataStore?",
        shiny::tags$pre(paste0(
          "This will upload ", n, " file", if (n == 1) "" else "s",
          " to DataStore reference ", ref, if (isTRUE(dev)) " (DEV)" else "", ".\n\n",
          "Source directory:\n", dir, "\n\n",
          "Requires an active VPN connection. Continue?"
        )),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(session$ns("confirm_upload"), "Upload", class = "btn-primary")
        )
      ))
    })

    shiny::observeEvent(input$confirm_upload, {
      shiny::removeModal()
      dir <- upload_dir()
      ref <- resolved_reference_id()
      dev <- is_datastore_dev()

      result <- shiny::withProgress(message = "Uploading to DataStore...", value = 0.4, {
        tryCatch(
          EMLeditor::upload_data_package(directory = dir, force = TRUE, dev = dev),
          error = function(e) list(status = "error", message = strip_ansi(conditionMessage(e)))
        )
      })
      upload_outcome(result)

      # Record a successful upload against this reference so the success view
      # persists through the post-upload re-check (which will now see files
      # and must NOT re-raise the "already has files" warning).
      if (identical(result$status %||% "", "uploaded")) {
        just_uploaded_ref(ref)
      }

      do_check()
    })

    output$upload_result <- shiny::renderUI({
      result <- upload_outcome()
      if (is.null(result)) return(NULL)
      status <- result$status %||% "error"

      switch(status,
             "uploaded" = {
               urls <- result$uploaded_urls
               files <- result$files
               shiny::tags$div(class = "alert alert-success mt-2",
                               shiny::tags$div(style = "font-size: 1.1rem; font-weight: 700;",
                                               "\u2713 Uploaded ", result$n_files, " file(s) to reference ",
                                               result$reference_id, "."),
                               if (!is.null(files) && length(files) > 0) {
                                 shiny::tags$ul(class = "mt-2",
                                                lapply(seq_along(files), function(i) {
                                                  u <- if (!is.null(urls) && length(urls) >= i) urls[i] else NA
                                                  if (!is.na(u)) {
                                                    shiny::tags$li(files[i], " \u2013 ",
                                                                   shiny::tags$a(href = u, target = "_blank", "download link"))
                                                  } else {
                                                    shiny::tags$li(files[i])
                                                  }
                                                })
                                 )
                               }
               )
             },
             "has_files" = shiny::tags$div(class = "alert alert-warning mt-2",
                                           shiny::tags$strong("Reference already has ", result$file_count, " file(s). "),
                                           "Nothing was uploaded. Remove the existing files above, then upload again."),
             "no_reference" = shiny::tags$div(class = "alert alert-danger mt-2",
                                              shiny::tags$strong("No draft reference exists for this package's DOI. "),
                                              "Create one during Generate (Tab 9) before uploading."),
             "doi_mismatch" = shiny::tags$div(class = "alert alert-danger mt-2",
                                              shiny::tags$strong("DOI / reference ID mismatch. "),
                                              "The metadata's DOI does not match reference ", result$reference_id,
                                              ". Nothing was uploaded."),
             "upload_failed" = shiny::tags$div(class = "alert alert-danger mt-2",
                                               shiny::tags$strong("Upload failed on ", result$file, ". "),
                                               result$message,
                                               shiny::tags$div(class = "mt-1",
                                                               "Any files listed before this one may have already uploaded - ",
                                                               "check the reference and, if needed, remove partial files before retrying.")),
             "cancelled" = shiny::tags$div(class = "alert alert-info mt-2",
                                           "Upload cancelled. Nothing was uploaded."),
             shiny::tags$div(class = "alert alert-danger mt-2",
                             shiny::tags$strong("Upload error: "),
                             result$message %||% "An unexpected error occurred during upload.")
      )
    })

    output$extract_reminder <- shiny::renderUI({
      result <- upload_outcome()
      if (is.null(result) || !identical(result$status %||% "", "uploaded")) return(NULL)
      ref <- resolved_reference_id()
      if (is.na(ref)) return(NULL)
      url <- datastore_reference_url(ref, is_datastore_dev())
      shiny::tags$div(class = "alert alert-warning mt-2",
                      shiny::tags$p(shiny::tags$strong("Manual step required: "),
                                    "there is currently no API to extract the EML and populate the ",
                                    "DataStore landing page (planned for a future DataStore release). ",
                                    "Open the reference and extract the metadata manually:"),
                      shiny::tags$a(href = url, target = "_blank", url))
    })

    # ---- Extract metadata (stub) ----
    shiny::observeEvent(input$extract_metadata, {
      ref <- resolved_reference_id()
      # TODO: wire the REAL extract-metadata call once DataStore exposes it.
      output$extract_result <- shiny::renderUI({
        shiny::tags$div(class = "alert alert-info mt-2",
                        "STUB: metadata extraction not yet available. Would trigger ",
                        "extraction for reference ", shiny::tags$code(ref), ".")
      })
    })

    # ---- Request a review: A. verify reviewer + add as editor ----
    reviewer_parsed <- shiny::reactive({
      parse_reviewer_identifier(input$reviewer_email)
    })

    # TRUE only while the last SUCCESSFUL verify+add still matches BOTH the
    # identifier currently in the box and the current reference id. Editing
    # either one silently invalidates it (no explicit clearing needed).
    # == (NA-safe) for the reference comparison, per the v5 lesson.
    reviewer_ready <- shiny::reactive({
      o <- reviewer_outcome()
      ref <- resolved_reference_id()
      if (is.null(o) || !isTRUE(o$ok)) return(FALSE)
      if (is.na(ref) || is.null(o$ref) || is.na(o$ref)) return(FALSE)
      identical(o$input, reviewer_parsed()$input) && o$ref == ref
    })

    output$verify_button <- shiny::renderUI({
      parsed <- reviewer_parsed()
      ref <- resolved_reference_id()
      can_verify <- isTRUE(parsed$ok) && !is.na(ref) && !reviewer_ready()

      hint <- if (is.na(ref)) {
        shiny::helpText("A DataStore reference is needed first - set the data ",
                        "package directory in step 1.")
      } else if (nzchar(parsed$input) && !isTRUE(parsed$ok)) {
        shiny::helpText(parsed$message)
      }

      shiny::tagList(
        shiny::actionButton(session$ns("verify_reviewer"), "Verify & add reviewer",
                            class = "btn-primary",
                            disabled = if (!can_verify) NA else NULL),
        hint
      )
    })

    shiny::observeEvent(input$verify_reviewer, {
      ref <- resolved_reference_id()
      parsed <- reviewer_parsed()
      if (is.na(ref) || !isTRUE(parsed$ok)) return()
      dev <- is_datastore_dev()

      outcome <- shiny::withProgress(message = "Verifying reviewer in Active Directory...",
                                     value = 0.3, {
                                       lk <- lookup_reviewer(parsed)
                                       if (!isTRUE(lk$ok)) {
                                         lk
                                       } else {
                                         shiny::setProgress(value = 0.7, message = "Adding reviewer to the reference...")
                                         ad <- add_reviewer_to_reference(ref, parsed, dev)
                                         if (!isTRUE(ad$ok)) {
                                           list(ok = FALSE, reason = "error", stage = "add", message = ad$message)
                                         } else {
                                           c(lk, list(n_editors = ad$n_editors, warnings = ad$warnings))
                                         }
                                       }
                                     })

      outcome$input <- parsed$input
      outcome$ref <- ref
      reviewer_outcome(outcome)
    })

    output$reviewer_result <- shiny::renderUI({
      o <- reviewer_outcome()
      ref <- resolved_reference_id()
      if (is.null(o)) return(NULL)
      # Hide results that belong to a different identifier/reference.
      if (is.na(ref) || is.null(o$ref) || is.na(o$ref) || o$ref != ref ||
          !identical(o$input, reviewer_parsed()$input)) {
        return(NULL)
      }

      if (!isTRUE(o$ok)) {
        headline <- if (identical(o$stage, "add")) {
          "Reviewer verified, but could not be added to the reference: "
        } else if (identical(o$reason, "not_found")) {
          "Reviewer not found: "
        } else if (identical(o$reason, "disabled")) {
          "Reviewer account is deactivated: "
        } else {
          "Could not verify the reviewer: "
        }
        return(shiny::tags$div(class = "alert alert-danger mt-2",
                               shiny::tags$strong(headline), o$message,
                               if (identical(o$reason, "error")) {
                                 shiny::tags$div(class = "mt-1",
                                                 "Are you connected to the VPN? Click the button again once connected.")
                               }))
      }

      who <- if (.nz(o$display_name)) paste0(o$display_name, " (", o$mail, ")") else o$mail
      shiny::tags$div(class = "alert alert-success mt-2",
                      shiny::tags$strong("\u2713 "), who,
                      " was verified in Active Directory and added as an editor on reference ",
                      shiny::tags$code(o$ref), ".",
                      if (length(o$warnings) > 0) {
                        shiny::tags$div(class = "mt-1",
                                        shiny::tags$strong("Note: "),
                                        paste(o$warnings, collapse = " "))
                      })
    })

    # ---- Request a review: B. generate the draft email (mailto:) ----
    custom_text <- shiny::reactive({
      x <- input$invite_custom_text
      if (is.null(x) || length(x) == 0 || is.na(x[1])) x <- ""
      substr(x[1], 1, REVIEW_CUSTOM_TEXT_MAX)
    })

    output$invite_char_count <- shiny::renderUI({
      shiny::helpText(nchar(custom_text()), " / ", REVIEW_CUSTOM_TEXT_MAX,
                      " characters. Your message appears near the start of the email.")
    })

    # The email, built from current inputs. Requires the reviewer to be
    # verified/added (the recipient is the AD-verified address).
    invite_email <- shiny::reactive({
      shiny::req(reviewer_ready())
      o <- reviewer_outcome()
      em <- build_reviewer_invite_email(
        reference_id = o$ref,
        dev = is_datastore_dev(),
        package_title = package_title_reactive(),
        custom_text = custom_text(),
        reviewer_given_name = o$given_name
      )
      em$to <- o$mail
      em$mailto <- build_reviewer_mailto(o$mail, em$subject, em$body)
      em
    })

    output$send_button <- shiny::renderUI({
      if (!reviewer_ready()) {
        return(shiny::tagList(
          shiny::tags$button(type = "button", class = "btn btn-secondary",
                             disabled = "disabled", "Generate Draft Email"),
          shiny::helpText("Complete step A first.")
        ))
      }
      em <- invite_email()
      if (nchar(em$mailto) > MAILTO_MAX_CHARS) {
        return(shiny::tagList(
          shiny::tags$button(type = "button", class = "btn btn-secondary",
                             disabled = "disabled", "Generate Draft Email"),
          shiny::tags$div(class = "alert alert-warning mt-2",
                          "The email is too long to open reliably in an email program. ",
                          "Shorten the custom message (line breaks count extra) and try again.")
        ))
      }
      shiny::tagList(
        # A plain mailto: link styled as a button - opens a DRAFT in the
        # user's own email program; they review it and click Send there.
        shiny::tags$a(class = "btn btn-primary", href = em$mailto,
                      "Generate Draft Email"),
        shiny::helpText("Opens a draft email addressed to the reviewer in your ",
                        "email program (e.g. Outlook). Review it and click Send ",
                        "there - breezeml does not send the email itself.")
      )
    })

    # ---- Activate button: gray + disabled until certified, then green ----
    # The disabled attribute is added with htmltools::tagAppendAttributes()
    # rather than actionButton(disabled = ...). Current Shiny versions
    # treat actionButton(disabled = NA) as "not disabled" (they test
    # isTRUE(disabled)), which is why the button was clickable before
    # certification. tagAppendAttributes() writes the attribute directly,
    # so it works the same on every Shiny version.
    output$activate_button <- shiny::renderUI({
      certified <- isTRUE(input$certified_review)
      if (certified) {
        return(shiny::actionButton(
          session$ns("do_activate"), "Activate reference",
          class = "btn-success"
        ))
      }
      htmltools::tagAppendAttributes(
        shiny::actionButton(
          session$ns("do_activate"), "Activate reference",
          class = "btn-unavailable"
        ),
        disabled = NA,
        `aria-disabled` = "true",
        title = "Check the certification box above to enable activation."
      )
    })

    # ---- Activate / publish ----
    shiny::observeEvent(input$do_activate, {
      ref <- resolved_reference_id()
      dev <- is_datastore_dev()
      conf <- build_activation_confirmation(ref, dev, isTRUE(input$certified_review))
      if (!isTRUE(conf$ok_to_proceed)) {
        shiny::showModal(shiny::modalDialog(
          title = conf$title, conf$message, easyClose = TRUE,
          footer = shiny::modalButton("OK")
        ))
        return()
      }
      shiny::showModal(shiny::modalDialog(
        title = conf$title, conf$message,
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(session$ns("confirm_activate"), "Activate",
                              class = "btn-warning")
        )
      ))
    })

    shiny::observeEvent(input$confirm_activate, {
      shiny::removeModal()
      ref <- resolved_reference_id()
      # TODO: wire the REAL activation/publish call. NOT yet confirmed.
      output$activate_result <- shiny::renderUI({
        shiny::tags$div(class = "alert alert-info mt-2",
                        "STUB: activation not yet wired. Would activate reference ",
                        shiny::tags$code(ref), ".")
      })
    })

    # ---- Module return contract ----
    data <- shiny::reactive({
      list(
        upload_dir = upload_dir(),
        reference_id = resolved_reference_id(),
        certified_review = isTRUE(input$certified_review)
      )
    })

    restore <- function(saved) {
      if (is.null(saved)) return(invisible(NULL))
      if (!is.null(saved$upload_dir) && nzchar(saved$upload_dir %||% "")) {
        shiny::updateTextInput(session, "upload_dir", value = saved$upload_dir)
      }
      invisible(NULL)
    }

    list(data = data, restore = restore)
  })
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && a == "")) b else a
