# 08_org_context.R v12
#
# Content Units now additionally validate against
# NPSdatastore::get_unit_geography() - a content unit must be a physical
# place with actual geographic boundaries (used to generate bounding box
# coordinates per skeleton.Rmd), unlike Producing Units, which may be
# administrative entities (e.g. a network or regional office) with no
# geographic footprint and validate only against get_all_nps_units().
#
# get_unit_geography() returns a 0-row result for BOTH "not a valid unit
# code at all" and "valid unit but has no geography" - these are
# indistinguishable from that call alone. To give a precise error message,
# Content Units are checked against get_all_nps_units() FIRST (same as
# Producing Units) to establish "recognized unit code", and only THEN
# checked against get_unit_geography() to establish "has geography" -
# so a rejected code can be told apart as either "not a recognized NPS
# unit code" or "recognized unit but has no associated geography".
#
# CONFIRMED (v9): when NONE of the codes queried in a single
# get_unit_geography() call have geography, the API returns a 0x0 tibble
# (zero rows AND zero columns) - not a 0-row tibble with the expected
# Code/Geography columns intact. Extracting $Code from a 0x0 result does
# not behave the same as extracting $Code from a proper 0-row tibble, and
# previously caused affected codes (e.g. "IMD") to be incorrectly treated
# as having geography and added anyway. The fix (see has_geography block
# in unit_category_server()) explicitly checks nrow()/column presence
# before attempting to extract or match against the Code column, rather
# than assuming the result always has the expected shape.
#
# Unit codes are matched case-insensitively (so "acad" or "Acad" both
# resolve to "ACAD") but always stored/displayed as the canonical
# uppercase code returned by the API.
#
# DataStore Project (v10/v11): EML schema only allows a single Project
# link per data package (unlike Cross-references, which allow many), so -
# even though NPSdatastore::search_references_by_id_basic() itself can
# accept and return info on multiple reference IDs at once - the UI
# enforces a hard cap of exactly one accepted project. Attempting to add
# a second while one is already present is rejected outright (with a
# message to remove the existing one first) without even calling the
# API. A candidate ID is validated with three sequential checks, each
# with a distinct rejection reason:
#   1. search_references_by_id_basic() must not error (an error means the
#      reference ID doesn't exist / can't be found at all - see its
#      documented behavior of erroring outright when NONE of the
#      requested IDs are found)
#   2. referenceType must be exactly "Project" (not e.g. "Data Package")
#   3. lifecycle must be exactly "Active" (not e.g. "Draft" or "Inactive")
# No client-side format check (e.g. requiring exactly 7 digits) is done
# before the API call - malformed input is simply passed through and
# reported via whatever the API naturally returns (typically a
# not-found-style error), rather than pre-empting it with a separate
# format-specific message.
#
# CONFIRMED (v11): the reference's human-readable title is in the
# search_references_by_id_basic() result's `title` column, NOT
# `publicationTitle` (the latter is unreliable/often NA - a possible
# upstream NPSdatastore inconsistency, not something to work around here).
# The accepted project's title is fetched once at add-time (from the same
# API response already used for validation, no extra call) and stored
# alongside its reference ID for display in the project table.
#
# Cross-references (v12): unlike the Project field, ANY referenceType is
# acceptable here - the only requirements are that the reference exists
# and is Active. Also unlike Project, multiple may be added at once via
# comma-separated batch entry (same input pattern as Content/Producing
# Units), rather than one at a time.
#
# search_references_by_id_basic() behaves differently for batches than
# for single IDs: a single not-found ID errors outright, but in a batch
# of multiple IDs it instead returns rows for whichever WERE found and
# emits a warning (not an error) naming the ones that weren't - so "not
# found" for a batch is determined by diffing the candidate IDs against
# the returned referenceId column, not by catching an error. An error is
# only expected here if NONE of the batch's eligible candidates were
# found, matching the single-ID case. The API's own warning about missing
# IDs is suppressed (via withCallingHandlers/invokeRestart) since the
# same information is already being derived and reported through the
# app's own skip-reason mechanism - surfacing it twice would be
# redundant. Reference titles (from `title`, not `publicationTitle` - see
# Project note above) are captured per-ID from the same lookup and stored
# alongside each accepted cross-reference for display.
#
# Corresponds to skeleton.Rmd's "Add content unit links" (park units,
# EMLeditor::set_content_units()), "Add the Producing Unit(s)"
# (EMLeditor::set_producing_units()), "Add your data package to a
# DataStore project" (EMLeditor::set_project()), and "Add cross
# references" (EMLeditor::set_cross_reference()) sections.
#
# All four are optional per skeleton.Rmd - a data package may have no
# specific location (species list, lab samples), no project link, and no
# cross-references.
#
# Like 07_permissions.R, this tab's state is applied to the EML object via
# EMLeditor::set_*() calls in run_generation() (09_generate.R), not
# written to a .txt template.

#' Fetch the live list of NPS units for validation/lookup. Falls back to
#' an empty tibble (with a warning) if the API call fails - unit entry
#' will then reject everything until the underlying issue is resolved,
#' rather than silently accepting unvalidated codes.
#'
#' @return tibble with at least UnitCode, UnitName, FullName columns, or
#'   an empty tibble with those columns if the API call failed
#' @noRd
get_nps_units_table <- function() {
  units <- tryCatch({
    NPSdatastore::get_all_nps_units()
  }, error = function(e) {
    shiny::showNotification(
      paste0("Could not load NPS unit list from NPSdatastore::get_all_nps_units(): ",
             conditionMessage(e), ". Unit codes cannot be validated until this is resolved."),
      type = "warning", duration = 10
    )
    NULL
  })

  if (is.null(units) || nrow(units) == 0) {
    return(tibble::tibble(UnitCode = character(), UnitName = character(), FullName = character()))
  }

  units
}

#' Reusable UI for one unit-entry category (Content Units or Producing
#' Units) - matches person_category_ui()'s pattern in 02_people.R.
#' @noRd
unit_category_ui <- function(id, header, help_text) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header(header),
    shiny::helpText(help_text),
    bslib::layout_columns(
      shiny::textInput(ns("new_code"), NULL,
                       placeholder = "Enter one or more unit codes, separated by commas", width = "100%"),
      shiny::actionButton(ns("add_code"), "Add", class = "btn-primary btn-sm"),
      col_widths = c(10, 2)
    ),
    DT::DTOutput(ns("units_table"))
  )
}

#' Reusable server for one unit-entry category. Matches
#' person_category_server()'s pattern in 02_people.R: type/paste codes,
#' validate in a batch against the live unit table (and, if
#' require_geography is TRUE, additionally against
#' NPSdatastore::get_unit_geography()), add valid ones, report skipped
#' ones with a specific reason, allow per-row removal.
#'
#' @param id module id
#' @param units_table the tibble from get_nps_units_table(), shared across
#'   both categories so get_all_nps_units() is only called once per session
#' @param require_geography if TRUE (Content Units), codes must ALSO have
#'   an entry in NPSdatastore::get_unit_geography() to be accepted - a
#'   recognized-but-geography-less code is rejected with a distinct reason
#'   from an unrecognized code entirely. If FALSE (Producing Units), only
#'   units_table membership is checked.
#' @return list(data = reactive() character vector of UnitCode, valid = reactive() TRUE)
#' @noRd
unit_category_server <- function(id, units_table, require_geography = FALSE) {
  shiny::moduleServer(id, function(input, output, session) {
    codes <- shiny::reactiveVal(character(0))

    shiny::observeEvent(input$add_code, {
      shiny::req(input$new_code)

      raw_entries <- strsplit(input$new_code, ",")[[1]]
      candidates <- toupper(trimws(raw_entries))
      candidates <- candidates[nzchar(candidates)]
      shiny::req(length(candidates) > 0)

      current <- codes()
      valid_lookup <- toupper(units_table$UnitCode)

      is_recognized <- candidates %in% valid_lookup
      already_present <- candidates %in% current
      dupe_within_batch <- duplicated(candidates)

      # Only check geography for codes that are otherwise eligible
      # (recognized, not already present, not a within-batch duplicate) -
      # no point spending an API call validating something that's going
      # to be rejected for a different reason anyway.
      has_geography <- rep(TRUE, length(candidates))
      if (require_geography) {
        needs_geo_check <- is_recognized & !already_present & !dupe_within_batch
        if (any(needs_geo_check)) {
          geo_result <- tryCatch(
            NPSdatastore::get_unit_geography(candidates[needs_geo_check]),
            error = function(e) {
              shiny::showNotification(
                paste0("Could not verify unit geography: ", conditionMessage(e),
                       ". Rejecting affected codes until this is resolved."),
                type = "warning"
              )
              tibble::tibble(Code = character())
            }
          )

          # CONFIRMED BUG SOURCE (v9): when none of the queried codes have
          # geography, get_unit_geography() returns a 0x0 tibble (zero
          # columns, not just zero rows). Extracting $Code from that shape
          # does not reliably behave like extracting $Code from a normal
          # 0-row tibble with the Code column intact, and previously let
          # geography-less codes (e.g. "IMD") pass through as if they had
          # geography. Guard explicitly on row count AND column presence
          # before attempting the $Code extraction, so a fully-empty
          # result is always treated as "nothing in this batch has
          # geography" rather than silently mismatching.
          if (nrow(geo_result) == 0 || !"Code" %in% names(geo_result)) {
            found_geo_codes <- character(0)
          } else {
            found_geo_codes <- toupper(geo_result$Code)
          }

          has_geography[needs_geo_check] <- candidates[needs_geo_check] %in% found_geo_codes
        }
      }

      skip_reason <- dplyr::case_when(
        !is_recognized ~ "not a recognized NPS unit code",
        already_present ~ "already added",
        dupe_within_batch ~ "duplicate in list",
        require_geography & !has_geography ~ "recognized unit but has no associated geography",
        TRUE ~ NA_character_
      )

      valid_new <- candidates[is.na(skip_reason)]
      skipped <- tibble::tibble(code = candidates[!is.na(skip_reason)],
                                reason = skip_reason[!is.na(skip_reason)])

      if (length(valid_new) > 0) {
        codes(c(current, valid_new))
      }
      shiny::updateTextInput(session, "new_code", value = "")

      msg <- paste0("Added ", length(valid_new), " unit(s).")
      if (nrow(skipped) > 0) {
        msg <- paste0(msg, " Skipped ", nrow(skipped), ": ",
                      paste0(skipped$code, " (", skipped$reason, ")", collapse = ", "))
      }
      shiny::showNotification(msg, type = if (nrow(skipped) > 0) "warning" else "message")
    })

    output$units_table <- DT::renderDT({
      current <- codes()
      display_df <- tibble::tibble(code = current)

      if (nrow(display_df) > 0) {
        match_idx <- match(display_df$code, toupper(units_table$UnitCode))
        display_df$name <- ifelse(
          !is.na(match_idx) & nzchar(units_table$FullName[match_idx]),
          units_table$FullName[match_idx],
          ifelse(!is.na(match_idx), units_table$UnitName[match_idx], NA_character_)
        )
        display_df$remove <- vapply(seq_len(nrow(display_df)), function(i) {
          as.character(
            shiny::actionButton(session$ns(paste0("remove_", i)), "Remove",
                                class = "btn-danger btn-sm",
                                onclick = sprintf(
                                  'Shiny.setInputValue(\"%s\", %d, {priority: \"event\"})',
                                  session$ns("remove_code"), i
                                ))
          )
        }, character(1))
      } else {
        display_df$name <- character(0)
        display_df$remove <- character(0)
      }

      DT::datatable(
        display_df,
        rownames = FALSE,
        selection = "none",
        colnames = c("Code", "Name", ""),
        escape = which(names(display_df) != "remove") - 1,
        options = list(dom = 't', pageLength = -1)
      )
    })

    shiny::observeEvent(input$remove_code, {
      idx <- input$remove_code
      current <- codes()
      shiny::req(idx >= 1, idx <= length(current))
      codes(current[-idx])
    })

    list(data = codes, valid = shiny::reactive(TRUE))
  })
}

org_context_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    unit_category_ui(
      ns("content_units"), "Park Units (content) - optional",
      paste0("Park units where the DATA were collected. Must be units with ",
             "actual geographic boundaries (bounding box coordinates are ",
             "generated per unit) - administrative-only units without ",
             "geography will be rejected. If data span multiple units ",
             "within a network, list each unit individually rather than ",
             "the network. Enter 4-letter unit codes (e.g. ACAD, YELL).")
    ),
    unit_category_ui(
      ns("producing_units"), "Producing Unit(s)",
      paste0("The unit(s) responsible for generating the data package - ",
             "may be a single park, a network, or an administrative unit ",
             "with no geographic footprint. May overlap with, or differ ",
             "entirely from, the content units above.")
    ),
    bslib::card(
      bslib::card_header("DataStore Project (optional)"),
      shiny::helpText("DataStore only supports one project connection per data ",
                      "package. The project must already exist, be Active, and ",
                      "be of reference type \"Project\". Remove the current ",
                      "project before adding a different one."),
      bslib::layout_columns(
        shiny::numericInput(ns("new_project_id"), NULL, value = NA, width = "100%"),
        shiny::actionButton(ns("add_project"), "Add", class = "btn-primary btn-sm"),
        col_widths = c(10, 2)
      ),
      DT::DTOutput(ns("project_table"))
    ),
    bslib::card(
      bslib::card_header("Cross-references (optional)"),
      shiny::helpText("Reference IDs for related DataStore items (e.g. a sampling ",
                      "scheme diagram) not included in the data package itself. ",
                      "Must be valid, Active DataStore references (any reference ",
                      "type is allowed)."),
      bslib::layout_columns(
        shiny::textInput(ns("new_xref"), NULL,
                         placeholder = "Enter one or more reference IDs, separated by commas", width = "100%"),
        shiny::actionButton(ns("add_xref"), "Add", class = "btn-primary btn-sm"),
        col_widths = c(10, 2)
      ),
      DT::DTOutput(ns("xref_table"))
    ),
    col_widths = c(-2, 8, -2), fill = FALSE
  )
}

#' @return reactive() list:
#'   $content_units, $producing_units - character vectors of UnitCode (may be empty)
#'   $project_id - integer or NA
#'   $cross_references - integer vector (may be empty)
#'   $valid, $errors
#' @noRd
org_contextServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    units_table <- get_nps_units_table()

    content_units <- unit_category_server("content_units", units_table, require_geography = TRUE)
    producing_units <- unit_category_server("producing_units", units_table, require_geography = FALSE)

    # DataStore Project - single-entry only (EML schema allows exactly one
    # Project link per data package). Validated against
    # NPSdatastore::search_references_by_id_basic(): the candidate ID must
    # resolve (API errors outright if not found at all - see its
    # documented behavior), must have referenceType == "Project", and must
    # have lifecycle == "Active". No pre-API format check is performed;
    # malformed input is simply passed to the API and whatever it reports
    # (typically not-found) is surfaced as the skip reason. The project's
    # title (from the `title` column, NOT `publicationTitle` - see file
    # header note) is captured at add-time for display alongside its ID.
    project_id <- shiny::reactiveVal(NA_integer_)
    project_title <- shiny::reactiveVal(NA_character_)

    shiny::observeEvent(input$add_project, {
      shiny::req(input$new_project_id, !is.na(input$new_project_id))
      candidate <- as.integer(input$new_project_id)

      if (!is.na(project_id())) {
        shiny::showNotification(
          "A project is already linked. Remove it before adding a new one.",
          type = "warning"
        )
        shiny::updateNumericInput(session, "new_project_id", value = NA)
        return()
      }

      ref_result <- tryCatch(
        NPSdatastore::search_references_by_id_basic(candidate),
        error = function(e) {
          NULL
        }
      )

      if (is.null(ref_result) || nrow(ref_result) == 0) {
        shiny::showNotification(
          paste0(candidate, ": reference ID not found on DataStore."),
          type = "warning"
        )
      } else if (ref_result$referenceType[1] != "Project") {
        shiny::showNotification(
          paste0(candidate, ": reference exists but is a \"",
                 ref_result$referenceType[1], "\", not a Project."),
          type = "warning"
        )
      } else if (ref_result$lifecycle[1] != "Active") {
        shiny::showNotification(
          paste0(candidate, ": reference is a Project but is not Active ",
                 "(status: \"", ref_result$lifecycle[1], "\")."),
          type = "warning"
        )
      } else {
        project_id(candidate)
        project_title(ref_result$title[1])
        shiny::showNotification(paste0("Added project ", candidate, "."), type = "message")
      }

      shiny::updateNumericInput(session, "new_project_id", value = NA)
    })

    output$project_table <- DT::renderDT({
      current <- project_id()
      display_df <- if (!is.na(current)) {
        tibble::tibble(reference_id = current, title = project_title())
      } else {
        tibble::tibble(reference_id = integer(0), title = character(0))
      }

      if (nrow(display_df) > 0) {
        display_df$remove <- vapply(seq_len(nrow(display_df)), function(i) {
          as.character(
            shiny::actionButton(session$ns(paste0("remove_project_", i)), "Remove",
                                class = "btn-danger btn-sm",
                                onclick = sprintf(
                                  'Shiny.setInputValue(\"%s\", %d, {priority: \"event\"})',
                                  session$ns("remove_project"), i
                                ))
          )
        }, character(1))
      } else {
        display_df$remove <- character(0)
      }

      DT::datatable(display_df, rownames = FALSE, selection = "none",
                    colnames = c("Reference ID", "Title", ""), escape = FALSE,
                    options = list(dom = 't', pageLength = -1))
    })

    shiny::observeEvent(input$remove_project, {
      project_id(NA_integer_)
      project_title(NA_character_)
    })

    # Cross-references - like Content/Producing Units, multiple may be
    # added at once via comma-separated batch entry, and (unlike the
    # Project field) any referenceType is acceptable; the only
    # requirements are that the reference exists and is Active.
    #
    # search_references_by_id_basic() behaves differently for batches than
    # for single IDs: a single not-found ID errors outright, but in a
    # batch of multiple IDs it instead returns rows for whichever WERE
    # found and emits a warning (not an error) naming the ones that
    # weren't - so "not found" for a batch is determined by diffing the
    # candidate IDs against the returned referenceId column, not by
    # catching an error. An error is only expected here if NONE of the
    # batch's eligible candidates were found, matching the single-ID case.
    # The API's own warning about missing IDs is suppressed (via
    # withCallingHandlers) since the same information is already being
    # derived and reported through the app's own skip-reason mechanism -
    # surfacing it twice would be redundant.
    xrefs <- shiny::reactiveVal(integer(0))
    xref_titles <- shiny::reactiveVal(character(0))

    shiny::observeEvent(input$add_xref, {
      shiny::req(input$new_xref)

      raw_entries <- strsplit(input$new_xref, ",")[[1]]
      candidate_strs <- trimws(raw_entries)
      candidate_strs <- candidate_strs[nzchar(candidate_strs)]
      shiny::req(length(candidate_strs) > 0)

      candidates <- suppressWarnings(as.integer(candidate_strs))
      not_numeric <- is.na(candidates)

      current <- xrefs()
      already_present <- candidates %in% current
      dupe_within_batch <- duplicated(candidates)

      needs_lookup <- !not_numeric & !already_present & !dupe_within_batch

      found_ids <- integer(0)
      active_ids <- integer(0)
      inactive_lookup <- character(0)
      titles_by_id <- character(0)

      if (any(needs_lookup, na.rm = TRUE)) {
        lookup_ids <- candidates[needs_lookup]
        ref_result <- withCallingHandlers(
          tryCatch(
            NPSdatastore::search_references_by_id_basic(lookup_ids),
            error = function(e) NULL
          ),
          warning = function(w) invokeRestart("muffleWarning")
        )

        if (!is.null(ref_result) && nrow(ref_result) > 0) {
          found_ids <- ref_result$referenceId
          active_mask <- ref_result$lifecycle == "Active"
          active_ids <- ref_result$referenceId[active_mask]
          inactive_lookup <- setNames(ref_result$lifecycle[!active_mask],
                                      ref_result$referenceId[!active_mask])
          titles_by_id <- setNames(ref_result$title, ref_result$referenceId)
        }
      }

      skip_reason <- rep(NA_character_, length(candidates))
      skip_reason[not_numeric] <- "not a valid reference ID"
      skip_reason[already_present & is.na(skip_reason)] <- "already added"
      skip_reason[dupe_within_batch & is.na(skip_reason)] <- "duplicate in list"
      skip_reason[needs_lookup & !(candidates %in% found_ids) & is.na(skip_reason)] <-
        "not found on DataStore"
      skip_reason[needs_lookup & (candidates %in% found_ids) & !(candidates %in% active_ids) & is.na(skip_reason)] <-
        vapply(candidates[needs_lookup & (candidates %in% found_ids) & !(candidates %in% active_ids)],
               function(id) paste0("not Active (status: \"", inactive_lookup[[as.character(id)]], "\")"),
               character(1))

      valid_new <- candidates[is.na(skip_reason)]
      valid_new_titles <- vapply(valid_new, function(id) {
        t <- titles_by_id[[as.character(id)]]
        if (is.null(t)) NA_character_ else t
      }, character(1))

      skipped <- tibble::tibble(code = candidate_strs[!is.na(skip_reason)],
                                reason = skip_reason[!is.na(skip_reason)])

      if (length(valid_new) > 0) {
        xrefs(c(current, valid_new))
        xref_titles(c(xref_titles(), valid_new_titles))
      }
      shiny::updateTextInput(session, "new_xref", value = "")

      msg <- paste0("Added ", length(valid_new), " reference(s).")
      if (nrow(skipped) > 0) {
        msg <- paste0(msg, " Skipped ", nrow(skipped), ": ",
                      paste0(skipped$code, " (", skipped$reason, ")", collapse = ", "))
      }
      shiny::showNotification(msg, type = if (nrow(skipped) > 0) "warning" else "message")
    })

    output$xref_table <- DT::renderDT({
      refs <- xrefs()
      titles <- xref_titles()
      display_df <- tibble::tibble(reference_id = refs, title = titles)
      if (nrow(display_df) > 0) {
        display_df$remove <- vapply(seq_len(nrow(display_df)), function(i) {
          as.character(
            shiny::actionButton(session$ns(paste0("remove_xref_", i)), "Remove",
                                class = "btn-danger btn-sm",
                                onclick = sprintf(
                                  'Shiny.setInputValue(\"%s\", %d, {priority: \"event\"})',
                                  session$ns("remove_xref"), i
                                ))
          )
        }, character(1))
      } else {
        display_df$remove <- character(0)
      }
      DT::datatable(display_df, rownames = FALSE, selection = "none",
                    colnames = c("Reference ID", "Title", ""), escape = FALSE,
                    options = list(dom = 't', pageLength = -1))
    })

    shiny::observeEvent(input$remove_xref, {
      idx <- input$remove_xref
      current <- xrefs()
      shiny::req(idx >= 1, idx <= length(current))
      xrefs(current[-idx])
      xref_titles(xref_titles()[-idx])
    })

    shiny::reactive({
      list(
        content_units = content_units$data(),
        producing_units = producing_units$data(),
        project_id = project_id(),
        cross_references = xrefs(),
        valid = TRUE,
        errors = character(0)
      )
    })
  })
}

#' Apply this tab's state to an in-memory EML object. Called from
#' run_generation() alongside apply_permissions_to_eml(), after make_eml()
#' and before write_eml(). Every piece here is optional, so each call is
#' skipped if its corresponding state is empty/NA.
#'
#' force = TRUE / NPS = TRUE on every call, matching apply_permissions_to_eml()
#' - see that function's docstring in 07_permissions.R for the rationale.
#'
#' @param my_metadata the in-memory EML object (post make_eml(), pre write_eml())
#' @param state the list returned by org_contextServer()'s reactive,
#'   evaluated (i.e. state <- org_context_reactive())
#' @return the EML object with content units, producing units, project,
#'   and cross-reference links applied
apply_org_context_to_eml <- function(my_metadata, state) {
  if (length(state$content_units) > 0) {
    my_metadata <- EMLeditor::set_content_units(my_metadata, state$content_units, force = TRUE, NPS = TRUE)
  }
  if (length(state$producing_units) > 0) {
    my_metadata <- EMLeditor::set_producing_units(my_metadata, state$producing_units, force = TRUE, NPS = TRUE)
  }
  if (!is.na(state$project_id)) {
    my_metadata <- EMLeditor::set_project(my_metadata, state$project_id, force = TRUE, NPS = TRUE)
  }
  for (ref in state$cross_references) {
    my_metadata <- EMLeditor::set_cross_reference(my_metadata, ref, force = TRUE, NPS = TRUE)
  }
  my_metadata
}
