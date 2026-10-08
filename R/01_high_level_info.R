# 01_high_level_info.R v18
#
# v18: added restore() to highLevelServer()'s return value, for the
# Save/Load session feature (app_state.R / app_server.R). Restoring here
# has NO dependency on Tab 3's uploaded files - this tab's fields are all
# free-standing scalars/keywords, so restore() can run immediately on
# load, unlike Tabs 3/4/5/6.
#
# restore() takes a list matching app_state.R's
# default_app_state()$high_level_info shape and pushes it into this
# module's actual inputs/reactiveVals:
#   - plain text/radio/date inputs -> update*Input()
#   - keywords (saved as a plain list-of-rows, since JSON has no native
#     tibble type) -> reconstructed into the keywords tibble via
#     tibble::as_tibble(), then pushed into the keywords reactiveVal
#     directly (no Shiny input backs this - DT table is rendered FROM it)
#
# Dates round-trip through JSON as ISO8601 strings (see app_state.R's
# save_app_state()/jsonlite::write_json()), so restore() parses them back
# to Date via as.Date() before calling updateDateInput() - passing a raw
# string to `value` would not display correctly.

MIN_ABSTRACT_WORDS <- 20

word_count <- function(x) {
  x <- trimws(x)
  if (!nzchar(x)) return(0)
  length(strsplit(x, "\\s+")[[1]])
}

highLevelInput <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    bslib::card(
      class = "mb-2",
      bslib::card_header("Metadata & Package Identifiers"),
      shiny::textInput(ns("metadata_id"), "Metadata filename (Required)",
                       placeholder = "e.g. EVER_AA", width = "100%",
                       updateOn = "blur"),
      shiny::helpText(
        "Metadata filename becomes the .xml filename (",
        shiny::HTML("<code>&lt;name&gt;_metadata.xml</code>"),
        " - do not include the extension). The text entered here will also",
        " be the name of the directory containing all of the app output that ",
        "will be written to your working directory."),
      shiny::textInput(ns("package_title"), "Package title (Required)", width = "100%", updateOn = "blur"),
      shiny::helpText("Must be at least 5 words long. Your title should answer basic questions such as ", shiny::HTML("<b>what</b>"), ", ", shiny::HTML("<b>when</b>"), ", and ", shiny::HTML("<b>where</b>"), ". Spell out acroonyms. For example, 'Pacific Island Network Focal Terrestrial Plant Communities Monitoring Data Package 2010-2022'"),
      shiny::radioButtons(ns("data_status"), "Data collection status",
                          choices = c("Complete" = "complete", "Ongoing" = "ongoing"),
                          inline = TRUE),
      bslib::layout_columns(
        shiny::dateInput(ns("start_date"), "Collection start date",
                         format = "yyyy-mm-dd", width = "100%"),
        shiny::dateInput(ns("end_date"), "Collection end date",
                         format = "yyyy-mm-dd", width = "100%"),
        col_widths = c(6, 6)
      ),
      shiny::helpText("Date of the first and last data point across all files in ",
                      "the package (not planning or processing time). ISO 8601 ",
                      "format (YYYY-MM-DD). Dates in the future will cause errors ",
                      "downstream.")
    ),
    bslib::card(
      class = "mb-2",
      bslib::card_header("Abstract (Required)"),
      shiny::textAreaInput(ns("abstract"), NULL, width = "100%", rows = 5,
                           resize = "vertical", updateOn = "blur"),
      shiny::textOutput(ns("abstract_word_count")),
      shiny::helpText("Should let a non-expert understand ",
                      shiny::HTML("<b>Why</b>"), ", ", shiny::HTML("<b>How</b>"), ", ",
                      shiny::HTML("<b>Where</b>"), ", ", shiny::HTML("<b>When</b>"), ", and ",
                      shiny::HTML("<b>What</b>"),
                      " data were collected. Must be more than ", MIN_ABSTRACT_WORDS,
                      " words; ~250 words or fewer is typical.")
    ),
    bslib::card(
      class = "mb-2",
      bslib::card_header("Methods (Required)"),
      shiny::textAreaInput(ns("methods"), NULL, width = "100%", rows = 5,
                           resize = "vertical", updateOn = "blur"),
      shiny::helpText("Should contain sufficient detail that an expert could ",
                      "repeat the study. ", shiny::HTML("<b>Only citing SOPs or Protocols ",
                                                        "is insufficient</b>"),
                      " - include experimental design, data collection, and QA/QC.")
    ),
    bslib::card(
      class = "mb-2",
      bslib::card_header("Keywords (Required)"),
      bslib::layout_columns(
        shiny::textInput(ns("new_keyword"), NULL,
                         placeholder = "Add one or more keywords, separated by commas", width = "100%"),
        shiny::actionButton(ns("add_keyword"), "Add", class = "btn-primary btn-sm"),
        col_widths = c(10, 2)
      ),
      DT::DTOutput(ns("keywords_table")),
      shiny::helpText("At least one keyword is required. A thesaurus is optional and can be ",
                      "set per keyword by editing the 'Thesaurus' column above - if left blank, ",
                      "no thesaurus is included for that keyword in the metadata.")
    ),
    bslib::card(
      class = "mb-2",
      bslib::card_header("Additional notes (Optional)"),
      shiny::textAreaInput(ns("additional_notes"), NULL, width = "100%", rows = 3,
                           resize = "vertical", updateOn = "blur"),
      shiny::helpText("Anything useful to a data user not included elsewhere - ",
                      "e.g. full citations/URLs for resources referenced in Methods or acknowledgments of contributors that did not rise to the level of authors/creators.")
    ),
    col_widths = c(-2, 8, -2), fill = FALSE
  )
}

#' @return reactive() list:
#'   $metadata_id, $package_title, $data_status, $abstract, $methods,
#'   $additional_notes - character scalars
#'   $keywords - character vector
#'   $valid - logical
#'   $errors - character vector, empty if valid
#' Also returns $restore (function) as an ATTRIBUTE on the module's return
#' value in app_server.R's wiring - see highLevelServer()'s final return,
#' which is now list(reactive_fn, restore = function(saved) {...}) rather
#' than a bare reactive(). Callers use high_level()$<field> unchanged if
#' they call the reactive component directly; app_server.R is updated to
#' call high_level$data() instead of high_level() - see accompanying
#' app_server.R notes.
#' @noRd
highLevelServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    # keywords is now a tibble(keyword, keywordThesaurus) rather than a
    # bare character vector - thesaurus is optional and set per-keyword
    # (blank/NA if not specified), not a single value forced onto every
    # keyword. Previously every keyword got a hardcoded "NPS Data Package"
    # thesaurus with no way to change or omit it per-keyword.
    keywords <- shiny::reactiveVal(
      tibble::tibble(keyword = character(0), keywordThesaurus = character(0))
    )

    shiny::observeEvent(input$add_keyword, {
      shiny::req(input$new_keyword)

      raw_entries <- strsplit(input$new_keyword, ",")[[1]]
      candidates <- trimws(raw_entries)
      candidates <- candidates[nzchar(candidates)]
      shiny::req(length(candidates) > 0)

      current <- keywords()
      already_present <- candidates %in% current$keyword
      dupe_within_batch <- duplicated(candidates)
      skip <- already_present | dupe_within_batch

      new_keywords <- candidates[!skip]
      skipped <- candidates[skip]

      if (length(new_keywords) > 0) {
        new_rows <- tibble::tibble(keyword = new_keywords, keywordThesaurus = NA_character_)
        keywords(rbind(current, new_rows))
      }
      shiny::updateTextInput(session, "new_keyword", value = "")

      if (length(skipped) > 0) {
        shiny::showNotification(
          paste0("Added ", length(new_keywords), " keyword(s). Skipped ",
                 length(skipped), " already in the list: ",
                 paste(skipped, collapse = ", ")),
          type = "warning"
        )
      }
    })

    output$keywords_table <- DT::renderDT({
      display_df <- keywords()

      if (nrow(display_df) > 0) {
        display_df$remove <- vapply(seq_len(nrow(display_df)), function(i) {
          as.character(
            shiny::actionButton(session$ns(paste0("remove_kw_", i)), "Remove",
                                class = "btn-danger btn-sm",
                                onclick = sprintf(
                                  'Shiny.setInputValue(\"%s\", %d, {priority: \"event\"})',
                                  session$ns("remove_keyword"), i
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
        colnames = c("Keyword", "Thesaurus (optional)", ""),
        escape = which(names(display_df) != "remove") - 1,
        options = list(dom = 't', pageLength = -1),
        # keyword (col 0) and remove button (last col) are locked; only
        # keywordThesaurus (col 1) is editable
        editable = list(target = "cell", disable = list(columns = c(0, ncol(display_df) - 1)))
      )
    })

    shiny::observeEvent(input$keywords_table_cell_edit, {
      edit <- input$keywords_table_cell_edit
      current <- keywords()
      if (edit$col >= ncol(current)) return(invisible(NULL))
      updated <- DT::editData(current, edit, rownames = FALSE)
      # treat a blank/whitespace-only entry the same as "not specified"
      updated$keywordThesaurus[!nzchar(trimws(updated$keywordThesaurus %||% ""))] <- NA_character_
      keywords(updated)
    })

    shiny::observeEvent(input$remove_keyword, {
      idx <- input$remove_keyword
      current <- keywords()
      shiny::req(idx >= 1, idx <= nrow(current))
      keywords(current[-idx, , drop = FALSE])
    })

    output$abstract_word_count <- shiny::renderText({
      n <- word_count(input$abstract %||% "")
      status <- if (n < MIN_ABSTRACT_WORDS) " (minimum 20 required)" else ""
      paste0(n, " words", status)
    })

    data <- shiny::reactive({
      errors <- character(0)

      metadata_id <- trimws(input$metadata_id %||% "")
      package_title <- trimws(input$package_title %||% "")
      abstract <- trimws(input$abstract %||% "")
      methods <- trimws(input$methods %||% "")
      kw <- keywords()
      start_date <- input$start_date
      end_date <- input$end_date

      if (!nzchar(metadata_id)) errors <- c(errors, "Metadata filename is required.")
      if (grepl("[^A-Za-z0-9_\\-]", metadata_id)) {
        errors <- c(errors, "Metadata filename should only contain letters, numbers, underscores, and hyphens.")
      }
      if (!nzchar(package_title)) errors <- c(errors, "Package title is required.")
      if (!nzchar(abstract)) {
        errors <- c(errors, "Abstract is required.")
      } else if (word_count(abstract) < MIN_ABSTRACT_WORDS) {
        errors <- c(errors, paste0("Abstract must be at least ", MIN_ABSTRACT_WORDS, " words."))
      }
      if (!nzchar(methods)) errors <- c(errors, "Methods is required.")
      if (nrow(kw) == 0) errors <- c(errors, "At least one keyword is required.")

      if (is.null(start_date) || is.na(start_date)) {
        errors <- c(errors, "Collection start date is required.")
      }
      if (is.null(end_date) || is.na(end_date)) {
        errors <- c(errors, "Collection end date is required.")
      }
      if (!is.null(start_date) && !is.null(end_date) &&
          !is.na(start_date) && !is.na(end_date)) {
        if (end_date < start_date) {
          errors <- c(errors, "Collection end date cannot be before the start date.")
        }
        if (end_date > Sys.Date() || start_date > Sys.Date()) {
          errors <- c(errors, "Collection dates cannot be in the future.")
        }
      }

      list(
        metadata_id = metadata_id,
        package_title = package_title,
        data_status = input$data_status %||% "complete",
        abstract = abstract,
        methods = methods,
        additional_notes = trimws(input$additional_notes %||% ""),
        keywords = kw,
        start_date = start_date,
        end_date = end_date,
        valid = length(errors) == 0,
        errors = errors
      )
    })

    #' Push saved state into this module's inputs/reactiveVals. Called
    #' once by app_server.R right after a JSON load, with
    #' saved_state$high_level_info (see app_state.R). No dependency on
    #' Tab 3's uploaded files - safe to call immediately.
    #'
    #' @param saved list matching default_app_state()$high_level_info's
    #'   shape (as deserialized by jsonlite::read_json(simplifyVector=TRUE))
    restore <- function(saved) {
      if (is.null(saved)) return(invisible(NULL))

      shiny::updateTextInput(session, "metadata_id", value = saved$metadata_id %||% "")
      shiny::updateTextInput(session, "package_title", value = saved$package_title %||% "")
      shiny::updateRadioButtons(session, "data_status", selected = saved$data_status %||% "complete")
      shiny::updateTextAreaInput(session, "abstract", value = saved$abstract %||% "")
      shiny::updateTextAreaInput(session, "methods", value = saved$methods %||% "")
      shiny::updateTextAreaInput(session, "additional_notes", value = saved$additional_notes %||% "")

      # dates round-tripped through JSON as ISO8601 strings - parse back
      # to Date before handing to updateDateInput(); NA/missing left as
      # the widget's own default (today) rather than forcing an invalid value
      if (!is.null(saved$start_date) && !is.na(saved$start_date)) {
        shiny::updateDateInput(session, "start_date", value = as.Date(saved$start_date))
      }
      if (!is.null(saved$end_date) && !is.na(saved$end_date)) {
        shiny::updateDateInput(session, "end_date", value = as.Date(saved$end_date))
      }

      # keywords saved as a plain list-of-columns (jsonlite's
      # simplifyVector shape for a data-frame-like list) - reconstruct as
      # a tibble with the right columns/types even if saved with zero rows.
      #
      # jsonlite::write_json(auto_unbox = TRUE) collapses any length-1
      # vector to a bare JSON scalar rather than a one-element array - so
      # a save file with EXACTLY ONE keyword round-trips as
      # kw$keyword == "foo" (length-1 character), not c("foo"), and
      # kw$keywordThesaurus == NULL (not NA_character_) if that one
      # keyword had no thesaurus. Both sides are coerced defensively here
      # (NULL -> NA before as.character(), and both are already
      # length-compatible once that's fixed) rather than relying on
      # write_json to always emit arrays - the same one-element collapse
      # can happen for any saved vector field, not just this one.
      kw <- saved$keywords
      kw_keyword <- kw$keyword %||% character(0)
      kw_thesaurus <- kw$keywordThesaurus
      if (is.null(kw_thesaurus)) kw_thesaurus <- rep(NA_character_, length(kw_keyword))

      if (length(kw_keyword) == 0) {
        keywords(tibble::tibble(keyword = character(0), keywordThesaurus = character(0)))
      } else {
        keywords(tibble::tibble(
          keyword = as.character(kw_keyword),
          keywordThesaurus = as.character(kw_thesaurus)
        ))
      }

      invisible(NULL)
    }

    list(data = data, restore = restore)
  })
}

#' Emit the chunk that writes abstract.txt, methods.txt, keywords.txt, and
#' additional_info.txt, overwriting the blank templates that
#' EMLassemblyline::template_core_metadata() produces - same pattern as
#' emit_fields_chunk() / emit_people_chunk(). Pure function.
#'
#' @param state the list returned by highLevelServer()'s $data reactive,
#'   evaluated (i.e. state <- high_level$data())
#' @param working_folder_var name of the R variable holding the working
#'   folder path in the generated script (default "working_folder")
#' @return character - the R code chunk to write these .txt files, or an
#'   explanatory comment if state is incomplete/invalid
emit_high_level_chunk <- function(state, working_folder_var = "working_folder") {
  if (is.null(state) || !isTRUE(state$valid)) {
    return(paste0(
      "# High-level package information is incomplete - resolve the\n",
      "# following before generating a final script:\n",
      paste0("#   - ", state$errors, collapse = "\n"), "\n"
    ))
  }

  # deparse() lets R handle all escaping (quotes, backslashes, embedded
  # newlines) correctly, rather than hand-rolling string-safe substitution.
  metadata_id_r <- deparse(state$metadata_id)
  package_title_r <- deparse(state$package_title)
  abstract_r <- deparse(state$abstract)
  methods_r <- deparse(state$methods)
  notes_r <- deparse(state$additional_notes)
  start_date_r <- sprintf('lubridate::ymd("%s")', format(state$start_date, "%Y-%m-%d"))
  end_date_r <- sprintf('lubridate::ymd("%s")', format(state$end_date, "%Y-%m-%d"))

  # state$keywords is a tibble(keyword, keywordThesaurus) - thesaurus is
  # optional per-keyword (NA if not specified by the user), NOT a single
  # constant value applied to every keyword. tibble_to_r_tribble() (from
  # 04_fields.R) already handles NA -> NA_character_ correctly, so the
  # emitted script reproduces exactly which keywords do/don't have a
  # thesaurus, rather than forcing one onto all of them.
  keywords_tribble_r <- tibble_to_r_tribble(state$keywords)

  glue::glue(
    'metadata_id <- {metadata_id_r}\n',
    'package_title <- {package_title_r}\n',
    'data_type <- "{state$data_status}"\n',
    'startdate <- {start_date_r}\n',
    'enddate <- {end_date_r}\n\n',
    'EMLassemblyline::template_core_metadata(path = {working_folder_var}, license = "CC0")\n\n',
    'writeLines({abstract_r}, file.path({working_folder_var}, "abstract.txt"))\n',
    'writeLines({methods_r}, file.path({working_folder_var}, "methods.txt"))\n',
    'writeLines({notes_r}, file.path({working_folder_var}, "additional_info.txt"))\n\n',
    'keywords_df <- {keywords_tribble_r}\n',
    'readr::write_tsv(keywords_df, file.path({working_folder_var}, "keywords.txt"), na = "")\n',
    .trim = FALSE
  )
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && a == "")) b else a
