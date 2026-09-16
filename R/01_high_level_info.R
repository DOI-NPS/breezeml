# 01_high_level_info.R v16
#
# Added @noRd to highLevelServer() - it's an internal Shiny module server,
# not meant to have a public help page. Resolves roxygen2's "Skipping; no
# name and/or title" note.
#
# Corresponds to skeleton.Rmd's package-level scalars (title, package.id,
# temporal coverage handled elsewhere) plus the core metadata .txt files
# from FUNCTION 1 - template_core_metadata: abstract.txt, methods.txt,
# additional_info.txt, keywords.txt. (intellectual_rights.txt and
# custom_units.txt are handled later via EMLeditor::set_int_rights() and
# are not user-entered here, per skeleton.Rmd's guidance.)
#
# Abstract must be > 20 words per skeleton.Rmd; keywords require at least
# one entry (EMLassemblyline expects a keyword/thesaurus pair table).

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
      bslib::card_header("Metadata & Package Identifiers"),
      shiny::textInput(ns("metadata_id"), "Metadata filename",
                       placeholder = "e.g. EVER_AA_metadata", width = "100%",
                       updateOn = "blur"),
      shiny::helpText("Becomes the .xml filename. Must end up as ",
                      shiny::HTML("<code>&lt;name&gt;_metadata.xml</code>"),
                      " - do not include the extension here."),
      shiny::textInput(ns("package_title"), "Package title", width = "100%", updateOn = "blur"),
      shiny::helpText("FAIR principles suggest 7-20 words. Avoid acronyms: spell ",
                      "out park and network units."),
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
      bslib::card_header("Abstract"),
      shiny::textAreaInput(ns("abstract"), NULL, width = "100%", rows = 6,
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
      bslib::card_header("Methods"),
      shiny::textAreaInput(ns("methods"), NULL, width = "100%", rows = 6,
                           resize = "vertical", updateOn = "blur"),
      shiny::helpText("Should contain sufficient detail that an expert could ",
                      "repeat the study. ", shiny::HTML("<b>Only citing SOPs or Protocols ",
                                                        "is insufficient</b>"),
                      " - include experimental design, data collection, and QA/QC.")
    ),
    bslib::card(
      bslib::card_header("Keywords"),
      bslib::layout_columns(
        shiny::textInput(ns("new_keyword"), NULL,
                         placeholder = "Add one or more keywords, separated by commas", width = "100%"),
        shiny::actionButton(ns("add_keyword"), "Add", class = "btn-primary btn-sm"),
        col_widths = c(10, 2)
      ),
      DT::DTOutput(ns("keywords_table")),
      shiny::helpText("At least one keyword is required. A generic thesaurus of ",
                      "'NPS Data Package' is applied automatically.")
    ),
    bslib::card(
      bslib::card_header("Additional notes"),
      shiny::textAreaInput(ns("additional_notes"), NULL, width = "100%", rows = 4,
                           resize = "vertical", updateOn = "blur"),
      shiny::helpText("Anything useful to a data user not included elsewhere - ",
                      "e.g. full citations/URLs for resources referenced in Methods.")
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
#' @noRd
highLevelServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    keywords <- shiny::reactiveVal(character(0))

    shiny::observeEvent(input$add_keyword, {
      shiny::req(input$new_keyword)

      raw_entries <- strsplit(input$new_keyword, ",")[[1]]
      candidates <- trimws(raw_entries)
      candidates <- candidates[nzchar(candidates)]
      shiny::req(length(candidates) > 0)

      current <- keywords()
      already_present <- candidates %in% current
      dupe_within_batch <- duplicated(candidates)
      skip <- already_present | dupe_within_batch

      new_keywords <- candidates[!skip]
      skipped <- candidates[skip]

      if (length(new_keywords) > 0) {
        keywords(c(current, new_keywords))
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
      kws <- keywords()
      display_df <- tibble::tibble(keyword = kws)

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
        colnames = c("Keyword", ""),
        escape = FALSE,
        options = list(dom = 't', pageLength = -1)
      )
    })

    shiny::observeEvent(input$remove_keyword, {
      idx <- input$remove_keyword
      current <- keywords()
      shiny::req(idx >= 1, idx <= length(current))
      keywords(current[-idx])
    })

    output$abstract_word_count <- shiny::renderText({
      n <- word_count(input$abstract %||% "")
      status <- if (n < MIN_ABSTRACT_WORDS) " (minimum 20 required)" else ""
      paste0(n, " words", status)
    })

    shiny::reactive({
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
      if (length(kw) == 0) errors <- c(errors, "At least one keyword is required.")

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
  })
}

#' Emit the chunk that writes abstract.txt, methods.txt, keywords.txt, and
#' additional_info.txt, overwriting the blank templates that
#' EMLassemblyline::template_core_metadata() produces - same pattern as
#' emit_fields_chunk() / emit_people_chunk(). Pure function.
#'
#' @param state the list returned by highLevelServer()'s reactive,
#'   evaluated (i.e. state <- high_level_reactive())
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
  kw_r <- deparse(state$keywords)
  start_date_r <- sprintf('lubridate::ymd("%s")', format(state$start_date, "%Y-%m-%d"))
  end_date_r <- sprintf('lubridate::ymd("%s")', format(state$end_date, "%Y-%m-%d"))

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
    'keywords_df <- tibble::tibble(\n',
    '  keyword = {kw_r},\n',
    '  keywordThesaurus = "NPS Data Package"\n',
    ')\n',
    'readr::write_tsv(keywords_df, file.path({working_folder_var}, "keywords.txt"), na = "")\n',
    .trim = FALSE
  )
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && a == "")) b else a
