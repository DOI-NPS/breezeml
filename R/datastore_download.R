# datastore_download.R v1
#
# Helpers for the "Revise an existing data package" start screen when the
# user chooses to fetch the package from DataStore instead of selecting a
# folder on their own computer (start_screen.R, app_server.R).
#
# Three jobs, kept out of the server file so they can be read and tested
# on their own:
#
#   1. ds_parse_reference_id()  - read what the user typed.
#   2. ds_check_draft_reference() - before downloading anything, confirm
#      the reference EXISTS, is a DATA PACKAGE, is in DRAFT lifecycle, and
#      that the user is listed as an EDITOR on it.
#   3. ds_download_data_package() - call NPSutils::get_data_package() and
#      report where the files landed.
#
# All of these need the NPS VPN (or an NPS office network): they use the
# secure DataStore API. Every call is wrapped so a VPN/network failure
# comes back as a message to show the user, never an unhandled error.
#
# WHAT IS CHECKED, AND HOW CONFIDENT EACH CHECK IS
#
#   exists      NPSutils::check_ref_exists(secure = TRUE)      - reliable.
#   is package  NPSutils::check_is_data_package(secure = TRUE) - reliable.
#   DRAFT       NPSdatastore::get_lifecycle_info()$lifecycle,
#               falling back to NPSutils::get_ref_info(id, "Lifecycle").
#               The field is compared case-insensitively to "draft". If
#               NEITHER call yields a usable value the download is blocked
#               and the user is told the status could not be confirmed -
#               it is never assumed to be a draft.
#   editor      NPSdatastore::get_reference_editors() returns the list of
#               editors. Matching "me" against that list is the WEAKEST
#               part of this file: DataStore returns usernames/emails, and
#               the app has no authenticated identity of its own, so the
#               operating-system account name (and its NPS email forms) is
#               what gets compared. If the account name cannot be
#               determined, or the list has no recognisable username
#               column, the result is "unconfirmed" rather than "not an
#               editor", and the user is warned but not blocked. A
#               confident NON-match does block, because downloading would
#               fail anyway.
#
# NOT YET RUN against a live DataStore. The function names and arguments
# come from the NPSutils and NPSdatastore sources; the exact column names
# in get_reference_editors() and the exact lifecycle string are the two
# details most likely to need a small fix after the first real test.

#' Read a DataStore reference id out of what the user typed.
#'
#' Accepts the bare digits only (DataStore reference ids are 6-7 digit
#' whole numbers). NPSdatastore's own .validate_ref_id() rejects anything
#' non-numeric, so this catches it first with a friendlier message.
#'
#' @param x the raw text from the input box
#' @return list(ok, id = numeric or NA, message)
#' @noRd
ds_parse_reference_id <- function(x) {
  txt <- trimws(if (is.null(x) || length(x) == 0 || is.na(x[1])) "" else as.character(x[1]))

  if (!nzchar(txt)) {
    return(list(ok = FALSE, id = NA_real_,
                message = "Enter a DataStore reference id."))
  }
  if (!grepl("^[0-9]+$", txt)) {
    return(list(ok = FALSE, id = NA_real_,
                message = paste0("A DataStore reference id is digits only, ",
                                 "for example 2300498.")))
  }
  id <- suppressWarnings(as.numeric(txt))
  if (is.na(id) || id <= 0) {
    return(list(ok = FALSE, id = NA_real_,
                message = "That is not a usable reference id."))
  }
  if (nchar(txt) < 5 || nchar(txt) > 8) {
    return(list(ok = FALSE, id = NA_real_,
                message = paste0("A DataStore reference id is normally 6 or 7 ",
                                 "digits; that one has ", nchar(txt), ".")))
  }
  list(ok = TRUE, id = id, message = NULL)
}

#' Candidate names for the person running the app, used to look for them
#' in a reference's editor list. Returns the operating-system account
#' name plus the NPS email/UPN forms built from it, lower-cased. May be
#' empty, which callers must treat as "identity unknown".
#' @noRd
ds_current_user_names <- function() {
  raw <- c(
    Sys.getenv("USERNAME"),
    Sys.getenv("USER"),
    tryCatch(Sys.info()[["user"]], error = function(e) "")
  )
  raw <- raw[!is.na(raw) & nzchar(raw)]
  # A Windows account can arrive as DOI\flast or flast@nps.gov.
  raw <- sub("^.*\\\\", "", raw)
  raw <- unique(tolower(trimws(raw)))
  if (length(raw) == 0) return(character(0))
  bare <- sub("@.*$", "", raw)
  unique(c(raw, bare, paste0(bare, "@nps.gov"), paste0(bare, "@doi.gov")))
}

#' Pull the lifecycle string out of whatever get_lifecycle_info() returned.
#' The response is a parsed JSON list; the documented use is
#' `lifecycle_info$lifecycle`, but it is read defensively here (top level,
#' then one level down) so an unexpected shape yields NA rather than an
#' error.
#' @noRd
ds_lifecycle_value <- function(info) {
  pick <- function(x) {
    if (is.null(x) || !is.list(x)) return(NA_character_)
    nm <- names(x)
    if (is.null(nm)) return(NA_character_)
    hit <- which(tolower(nm) == "lifecycle")
    if (length(hit) == 0) return(NA_character_)
    val <- x[[hit[1]]]
    if (is.character(val) && length(val) >= 1 && nzchar(val[1])) val[1] else NA_character_
  }

  val <- pick(info)
  if (!is.na(val)) return(val)

  if (is.list(info)) {
    for (el in info) {
      val <- pick(el)
      if (!is.na(val)) return(val)
    }
  }
  NA_character_
}

#' Does an editor table contain one of this user's names?
#'
#' @param editors whatever get_reference_editors() returned
#' @param me character vector from ds_current_user_names()
#' @return list(status = "match"|"no_match"|"unconfirmed", editors = chr,
#'   message = chr or NULL). "editors" is a display list of the editors
#'   found, for showing the user who does have access.
#' @noRd
ds_match_editor <- function(editors, me = ds_current_user_names()) {
  if (!is.data.frame(editors) || nrow(editors) == 0) {
    return(list(status = "unconfirmed", editors = character(0),
                message = "DataStore returned no editor list for this reference."))
  }

  nm <- names(editors)
  id_cols <- nm[grepl("user|email|mail|upn|login|account", nm, ignore.case = TRUE)]

  # Build a display list from whichever name-ish columns exist.
  show_cols <- nm[grepl("user|email|mail|name", nm, ignore.case = TRUE)]
  display <- if (length(show_cols) > 0) {
    apply(editors[, show_cols, drop = FALSE], 1, function(row) {
      paste(trimws(as.character(row[!is.na(row) & nzchar(as.character(row))])),
            collapse = " ")
    })
  } else {
    character(0)
  }
  display <- unique(display[nzchar(display)])

  if (length(id_cols) == 0) {
    return(list(status = "unconfirmed", editors = display,
                message = paste0("The editor list has no username or email ",
                                 "column, so your access could not be checked.")))
  }
  if (length(me) == 0) {
    return(list(status = "unconfirmed", editors = display,
                message = paste0("Your computer account name could not be ",
                                 "determined, so your access could not be checked.")))
  }

  values <- unlist(lapply(id_cols, function(cn) as.character(editors[[cn]])))
  values <- tolower(trimws(values[!is.na(values) & nzchar(values)]))
  # Compare both as given and with any domain removed.
  values <- unique(c(values, sub("@.*$", "", values)))

  if (any(values %in% me)) {
    list(status = "match", editors = display, message = NULL)
  } else {
    list(status = "no_match", editors = display, message = NULL)
  }
}

#' Check a reference before downloading it: exists, is a data package, is
#' in DRAFT, and the user is an editor.
#'
#' @param reference_id numeric, from ds_parse_reference_id()
#' @param dev logical, is_datastore_dev()
#' @return list(
#'   ok          TRUE only if every blocking check passed,
#'   can_download same as ok (kept separate for readability at call sites),
#'   exists, is_package, lifecycle, is_draft,
#'   editor_status "match"|"no_match"|"unconfirmed",
#'   editors      chr vector of editors to show the user,
#'   title        reference title if it could be read, else NA,
#'   blockers     chr vector - why the download is not allowed,
#'   warnings     chr vector - things to tell the user but not block on,
#'   failed_stage NA, or the stage that errored ("exists", "package",
#'                "lifecycle", "editors")
#' )
#' @noRd
ds_check_draft_reference <- function(reference_id, dev = FALSE) {
  out <- list(ok = FALSE, can_download = FALSE, exists = NA,
              is_package = NA, lifecycle = NA_character_, is_draft = NA,
              editor_status = "unconfirmed", editors = character(0),
              title = NA_character_, blockers = character(0),
              warnings = character(0), failed_stage = NA_character_)

  fail <- function(out, stage, msg) {
    out$failed_stage <- stage
    out$blockers <- c(out$blockers, msg)
    out
  }

  # ---- 1. does it exist? ----
  exists <- tryCatch(
    NPSutils::check_ref_exists(reference_id = reference_id,
                               secure = TRUE, dev = dev),
    error = function(e) e
  )
  if (inherits(exists, "condition")) {
    return(fail(out, "exists", paste0(
      "DataStore could not be reached: ", strip_ansi(conditionMessage(exists)),
      " Check that you are connected to the NPS VPN."
    )))
  }
  out$exists <- isTRUE(exists)
  if (!out$exists) {
    return(fail(out, NA_character_, paste0(
      "Reference ", format(reference_id, scientific = FALSE),
      " was not found on DataStore. Check the id, and check that you are ",
      "connected to the NPS VPN."
    )))
  }

  # ---- 2. is it a data package? ----
  is_pkg <- tryCatch(
    NPSutils::check_is_data_package(reference_id = reference_id,
                                    secure = TRUE, dev = dev),
    error = function(e) e
  )
  if (inherits(is_pkg, "condition")) {
    out$warnings <- c(out$warnings, paste0(
      "Could not confirm that this reference is a data package: ",
      strip_ansi(conditionMessage(is_pkg))
    ))
  } else {
    out$is_package <- isTRUE(is_pkg)
    if (!out$is_package) {
      out$blockers <- c(out$blockers, paste0(
        "Reference ", format(reference_id, scientific = FALSE),
        " is not a data package on DataStore."
      ))
    }
  }

  # ---- 3. is it a DRAFT? ----
  life <- tryCatch(
    NPSdatastore::get_lifecycle_info(reference_id = reference_id, dev = dev),
    error = function(e) e
  )
  lifecycle <- if (inherits(life, "condition")) NA_character_ else ds_lifecycle_value(life)

  if (is.na(lifecycle)) {
    # Fall back to the reference profile's own Lifecycle field.
    alt <- tryCatch(
      NPSutils::get_ref_info(reference_id = reference_id, field = "Lifecycle"),
      error = function(e) e
    )
    if (!inherits(alt, "condition") && is.character(alt) && length(alt) >= 1 &&
        nzchar(alt[1])) {
      lifecycle <- alt[1]
    }
  }

  out$lifecycle <- lifecycle
  if (is.na(lifecycle)) {
    out$blockers <- c(out$blockers, paste0(
      "The reference's status could not be read from DataStore, so it ",
      "cannot be confirmed as a draft."
    ))
  } else {
    out$is_draft <- identical(tolower(trimws(lifecycle)), "draft")
    if (!out$is_draft) {
      out$blockers <- c(out$blockers, paste0(
        "Reference ", format(reference_id, scientific = FALSE),
        " has the status ", lifecycle, ". Only a draft data package can be ",
        "revised here; an activated reference is published and cannot be ",
        "changed."
      ))
    }
  }

  # ---- 4. is the user an editor? ----
  editors <- tryCatch(
    NPSdatastore::get_reference_editors(reference_id = reference_id, dev = dev),
    error = function(e) e
  )
  if (inherits(editors, "condition")) {
    out$editor_status <- "unconfirmed"
    out$warnings <- c(out$warnings, paste0(
      "The editor list could not be read, so your access could not be ",
      "checked: ", strip_ansi(conditionMessage(editors))
    ))
  } else {
    m <- ds_match_editor(editors)
    out$editor_status <- m$status
    out$editors <- m$editors
    if (identical(m$status, "no_match")) {
      out$blockers <- c(out$blockers, paste0(
        "You are not listed as an editor on reference ",
        format(reference_id, scientific = FALSE),
        ", so DataStore will not release its files to you. Ask one of its ",
        "editors to add you."
      ))
    } else if (identical(m$status, "unconfirmed") && !is.null(m$message)) {
      out$warnings <- c(out$warnings, m$message)
    }
  }

  # ---- title, for confirming the right package ----
  ttl <- tryCatch(
    NPSutils::get_ref_info(reference_id = reference_id, field = "Title"),
    error = function(e) e
  )
  if (!inherits(ttl, "condition") && is.character(ttl) && length(ttl) >= 1) {
    out$title <- ttl[1]
  }

  out$ok <- length(out$blockers) == 0
  out$can_download <- out$ok
  out
}

#' The folder NPSutils::get_data_package() will write a package into:
#' <path>/data/<reference_id>. Used to tell the user where files are going
#' BEFORE anything is downloaded.
#' @noRd
ds_download_dir_for <- function(path, reference_id) {
  file.path(normalizePath(path, winslash = "/", mustWork = FALSE),
            "data", format(reference_id, scientific = FALSE))
}

#' Download a data package from DataStore.
#'
#' Calls NPSutils::get_data_package(secure = TRUE, force = TRUE). force =
#' TRUE is required here because the app is not a console: the function's
#' interactive prompts (overwrite? newer version?) have nowhere to go.
#' With force = TRUE it overwrites an existing folder of the same name
#' without asking, which is why the screen warns the user first.
#'
#' @param reference_id numeric
#' @param path parent folder; the package lands in <path>/data/<id>
#' @param dev logical, is_datastore_dev()
#' @return list(ok, dir, files, csv_count, metadata_count, message)
#' @noRd
ds_download_data_package <- function(reference_id, path, dev = FALSE) {
  dest <- ds_download_dir_for(path, reference_id)

  if (!dir.exists(path)) {
    created <- dir.create(path, recursive = TRUE, showWarnings = FALSE)
    if (!created && !dir.exists(path)) {
      return(list(ok = FALSE, dir = dest, files = character(0),
                  csv_count = 0L, metadata_count = 0L,
                  message = paste0("The folder ", path,
                                   " does not exist and could not be created.")))
    }
  }

  res <- tryCatch(
    NPSutils::get_data_package(
      reference_id = reference_id,
      secure = TRUE,
      path = path,
      force = TRUE,
      dev = dev
    ),
    error = function(e) e
  )

  if (inherits(res, "condition")) {
    return(list(ok = FALSE, dir = dest, files = character(0),
                csv_count = 0L, metadata_count = 0L,
                message = strip_ansi(conditionMessage(res))))
  }

  if (!dir.exists(dest)) {
    return(list(ok = FALSE, dir = dest, files = character(0),
                csv_count = 0L, metadata_count = 0L,
                message = paste0(
                  "No folder was created at ", dest,
                  ". The download did not produce any files."
                )))
  }

  files <- list.files(dest, full.names = FALSE)
  csvs <- grep("\\.csv$", files, ignore.case = TRUE, value = TRUE)
  xmls <- grep("metadata\\.xml$", files, ignore.case = TRUE, value = TRUE)

  if (length(files) == 0) {
    return(list(ok = FALSE, dir = dest, files = files,
                csv_count = 0L, metadata_count = 0L,
                message = paste0(
                  "The folder ", dest, " is empty. DataStore released no ",
                  "files - this usually means the VPN dropped, or you do ",
                  "not have access to this reference."
                )))
  }

  list(ok = TRUE, dir = dest, files = files,
       csv_count = length(csvs), metadata_count = length(xmls),
       message = NULL)
}
