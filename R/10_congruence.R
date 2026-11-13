# 10_congruence.R v6
#
# v6: DPchecker's console output is now suppressed while the checks run.
# run_congruence_checks() has no quiet/verbose argument; all of its
# console text is produced by cli (cli_h1(), cli_h2(), cli_bullets(),
# cli_alert_*(), ...), which is emitted as R message conditions, so the
# call is wrapped in suppressMessages(). The tab never used that output:
# it renders the structured list DPchecker returns (attr(res, "checks")),
# or e$checks after a gatekeeper abort. A gatekeeper abort is an ERROR,
# not a message, so suppressMessages() does not hide it and the existing
# tryCatch(error = ...) recovery is unchanged. The output_filename
# argument is deliberately NOT used: it would also open a log file in a
# viewer (file.show()). No option to re-enable the output was added; if
# console output is needed for debugging, remove suppressMessages() below
# or add an option then. NOT yet run against a live DPchecker - test it
# in the app. No other behavior changed vs v5.
#
# v5: congruence checks can now be run on an EXISTING data package, not
# only on the one just generated.
#   - The tab still DEFAULTS to the newly generated data_package/ folder.
#     When a Generate succeeds, that folder is selected automatically and
#     any folder the user picked earlier is dropped (a fresh Generate wins).
#   - If no generated package exists (nothing generated this session, the
#     folder was deleted, or an EML import cleared it), the tab asks the
#     user to select the folder that holds their data package.
#   - Folders are chosen with the operating system's own folder window
#     (Windows Explorer dialog / macOS Finder dialog), not by typing a
#     path. The window code lives in choose_directory.R so Tab 11 can
#     reuse it. A typed-path box appears ONLY as a fallback, when no
#     folder window can be opened (see "Where the window opens" in
#     choose_directory.R) or the app is not running on the user's own
#     computer.
#   - "Use newly generated package" returns to the default after the user
#     has picked a different folder.
#   - If the chosen folder has no *_metadata.xml but contains a
#     data_package/ subfolder that does (for example the folder breezeml
#     made for the package, one level above data_package/), the
#     data_package/ subfolder is checked and the tab says so.
#   - Before running, the tab lists how many data files and metadata
#     files it found. The Run button is unavailable (gray, with an
#     explanation) unless there is exactly one *_metadata.xml.
#   - Results are cleared whenever the folder being checked changes, and
#     the results state which folder they describe.
#   - The module signature is unchanged: congruenceServer(id, package_dir).
#     No change to app_server.R is needed.
#
# v4: strip terminal control codes from check messages before rendering.
# DPchecker's messages (captured from cli conditions) arrive carrying raw
# ANSI SGR color codes (e.g. \033[33m) and OSC 8 hyperlink escape
# sequences (e.g. \033]8;;ide:help:EMLeditor::set_abstract\033\\...\033]8;;
# \033\\) - terminal control codes that are gibberish in HTML. Each
# message line is now run through clean_message_line(), which uses
# cli::ansi_strip() (which removes BOTH ANSI SGR codes AND OSC 8
# hyperlinks, keeping the visible link text) with a regex fallback, so the
# browser shows clean readable text like "Use `EMLeditor::set_abstract()`
# to revise." instead of escape-code noise. The cli VISUAL formatting
# (colors, clickable links) is intentionally not reproduced - just the
# readable text content, per the agreed approach.
#
# v3: render the per-check MESSAGE for warn/fail rows. v2 drew only the
# label + pass/warn/fail badge, discarding the reason - which made a
# "Fail" row useless for troubleshooting (the user saw THAT a check
# failed but not WHY or how to fix it). DPchecker's breezeml_dev branch
# now returns a `message` field on each check record (the raw
# c(cond$message, cond$body) text the check raised - the same content the
# console prints, minus cli's visual formatting), so this version shows it
# inline beneath any warn or fail row. Pass rows show no message (there
# isn't one). The message is a character vector (one element per bulleted
# line the check emitted), rendered one line per element.
#
# v2: rewritten against DPchecker's structured-output contract (branch
# doi-nps/DPchecker@breezeml_dev) - no registry, no by-name function
# resolution, no drift alarm. Thin renderer over attr(result, "checks").
#
# ---- The DPchecker contract this module consumes ----
#
# run_congruence_checks(directory, check_metadata_only = FALSE) still
# returns (invisibly) c(errors = <n>, warnings = <n>) for console/script
# callers, UNCHANGED. Additionally:
#
#   - On a run that completes (no gatekeeper abort): the return value
#     carries a "checks" attribute - attr(result, "checks") - a list with
#     one element per check that ran, each list(check = <name>,
#     status = "pass"/"warn"/"fail", message = <chr or NULL>), in
#     execution order. message is NULL for a pass.
#
#   - On a GATEKEEPER failure (load_metadata, validate_schema,
#     dup_meta_entries, file_name_match, fields_match): the function
#     ABORTS (throws) rather than returning. The same partial per-check
#     list (every check that ran, plus the failing gatekeeper marked
#     "fail" with its message) is attached to the thrown condition as
#     e$checks, recoverable via tryCatch(error = function(e) e$checks).
#
# So this module wraps the call in tryCatch and reads the list from
# EITHER attr(result, "checks") (success) OR e$checks (abort).
#
# ---- This module's behavior ----
#
# - PURELY INFORMATIONAL. Never blocks anything. Can run at any time on
#   any data package folder; it does not need a Generate first (v5).
# - Runs on BUTTON PRESS; previous results AUTO-CLEAR when the folder
#   being checked changes (a fresh Generate, or the user picking another
#   folder), so stale results are never shown.
# - Runs the FULL check (check_metadata_only = FALSE): a data package
#   folder has the .csv data files AND the *_metadata.xml, so data-file
#   checks run too. The data-file gatekeepers (file_name_match,
#   fields_match) are therefore reachable and can abort - handled via the
#   e$checks recovery path.
# - KNOWN LIMITATION (unchanged from v4): if Generate is run again and
#   writes to the SAME folder as before, the package_dir reactive value
#   does not change, so earlier results are not cleared automatically.
#   Press "Run congruence checks" again after regenerating.
#
# ---- Dependency ----
#
# Requires DPchecker (>= the breezeml_dev branch that returns the "checks"
# attribute with per-check `message`, and the e$checks abort field).
# DESCRIPTION must carry:
#   Imports: DPchecker
#   Remotes: doi-nps/DPchecker@breezeml_dev
# until that branch is merged to DPchecker's main/release.
#
# Also uses summarize_package_files() from 11_upload_publish.R and
# choose_directory_native() from choose_directory.R. Both are plain
# functions in this package, called only while the app is running, so file
# load order does not matter.

# Human-readable labels for each check_id DPchecker reports. Purely
# cosmetic. An unknown id (e.g. DPchecker added a new check) renders as
# its raw id via label_for() - a visible, harmless "add a label" signal,
# not an error. Display order comes from the order DPchecker returns the
# checks in, not from this list.
CONGRUENCE_CHECK_LABELS <- c(
  load_metadata            = "Metadata loads without error",
  validate_schema          = "Metadata schema validates",
  dup_meta_entries         = "No duplicate data table entries in metadata",
  metadata_version         = "Metadata version is current",
  delimiter                = "Delimiter is documented correctly",
  header_num               = "Header row count is documented correctly",
  footer                   = "Footer rows are documented correctly",
  taxonomic_cov            = "Taxonomic coverage is valid",
  geographic_cov           = "Geographic coverage is valid",
  content_units            = "Content units are documented",
  doi                      = "DOI is present",
  doi_format               = "DOI format is valid",
  datatable_urls           = "Data table URLs are present",
  datatable_urls_doi       = "Data table URLs match the DOI",
  datatable_url_attributes = "Data table URL attributes are valid",
  publisher                = "Publisher is documented",
  valid_fieldnames         = "Field names are valid",
  valid_filenames          = "File names are valid",
  pii_meta_emails          = "No personal emails in metadata",
  creator                  = "Creator is documented",
  pub_date                 = "Publication date is documented",
  dp_title                 = "Data package title is documented",
  keywords                 = "Keywords are documented",
  by_for_nps               = "NPS attribution is documented",
  publisher_name           = "Publisher name is documented",
  publisher_state          = "Publisher state is documented",
  publisher_city           = "Publisher city is documented",
  dp_abstract              = "Abstract is documented",
  methods                  = "Methods are documented",
  file_descript            = "File description is documented",
  access                   = "Access level is documented",
  cui_label                = "CUI label is documented",
  cui_email                = "CUI contact email is documented",
  cui_designator           = "CUI designator is documented",
  license                  = "License is documented",
  int_rights               = "Intellectual rights are documented",
  attribute_defs           = "Attribute definitions are present",
  storage_type             = "Storage type is documented",
  orcid_exists             = "ORCID is present (optional)",
  orcid_format             = "ORCID format is valid (optional)",
  orcid_resolves           = "ORCID resolves (optional)",
  orcid_match              = "ORCID matches creator name (optional)",
  notes                    = "Notes are documented (optional)",
  project                  = "Project is documented (optional)",
  file_name_match          = "File names match between metadata and package",
  fields_match             = "Column names match between metadata and data files",
  missing_data             = "Missing data codes are consistent with data",
  numeric_fields           = "Numeric fields contain only numeric data",
  dates_parse              = "Dates parse correctly",
  date_range               = "Dates fall within the documented date range",
  pii_data_emails          = "No personal emails in data files"
)

# Map a check_id to its display label, falling back to the raw id if we
# don't have a nicer label on file (e.g. DPchecker added a new check).
label_for <- function(check_id) {
  lbl <- CONGRUENCE_CHECK_LABELS[[check_id]]
  if (is.null(lbl)) check_id else lbl
}

# Strip terminal control codes from a message string so it renders as
# clean text in the browser. DPchecker's messages (captured from cli
# conditions) can carry raw ANSI SGR color codes and OSC 8 hyperlink
# escape sequences - see v4 note. cli::ansi_strip() handles both (removing
# the escapes, keeping the visible link text); the manual regex fallback
# covers the (unlikely) case cli::ansi_strip is unavailable, stripping
# ANSI CSI sequences (ESC [ ... <letter>) and OSC sequences (ESC ] ...
# terminated by BEL or ESC \) while leaving their visible text. Vectorized
# over the input (a message may be multiple lines).
clean_message_line <- function(x) {
  if (length(x) == 0) return(x)
  x <- as.character(x)
  stripped <- tryCatch(
    cli::ansi_strip(x),
    error = function(e) NULL
  )
  if (!is.null(stripped)) {
    return(stripped)
  }
  # Fallback: no cli::ansi_strip available. Remove OSC 8 hyperlink wrappers
  # first (keeping the display text between the two OSC sequences), then any
  # remaining ANSI CSI/OSC escapes.
  esc <- "\033"
  # OSC: ESC ] ... (BEL | ESC \)  -- the link target + terminators; the
  # visible text sits between an opening and closing OSC, so removing the
  # OSC sequences themselves leaves the text intact.
  x <- gsub(paste0(esc, "\\].*?(\007|", esc, "\\\\)"), "", x, perl = TRUE)
  # CSI: ESC [ ... <final byte in @-~>
  x <- gsub(paste0(esc, "\\[[0-9;]*[A-Za-z]"), "", x, perl = TRUE)
  x
}

# Per-status display styling. Kept in one place so the pass/warn/fail/
# unknown visual language is consistent and easy to adjust.
congruence_status_style <- function(status) {
  switch(status,
         pass = list(icon = "\u2713", colour = "#198754", label = "Pass"),   # green check
         warn = list(icon = "\u26A0", colour = "#fd7e14", label = "Warning"),# amber warning
         fail = list(icon = "\u2717", colour = "#dc3545", label = "Fail"),   # red cross
         list(icon = "\u2022", colour = "#6c757d", label = status)           # neutral fallback
  )
}

# ---- v5 helpers: choosing which data package to check -------------------

#' TRUE if x is a single, non-empty path to a folder that exists.
#' @noRd
congruence_dir_is_valid <- function(x) {
  !is.null(x) && length(x) == 1 && !is.na(x) && nzchar(x) && dir.exists(x)
}

#' Decide which folder to check, given the one selected. Pure function
#' (filesystem read only).
#'
#' If the selected folder holds no *_metadata.xml but has a data_package/
#' subfolder that does, the subfolder is used. This covers the common case
#' of selecting the folder breezeml created for the package instead of the
#' data_package/ folder inside it.
#'
#' @param dir the folder as selected
#' @return list(dir = folder to check, note = chr message or NULL)
#' @noRd
resolve_package_dir <- function(dir) {
  out <- list(dir = dir, note = NULL)
  if (!congruence_dir_is_valid(dir)) return(out)

  has_metadata <- function(d) {
    length(list.files(d, pattern = "metadata\\.xml$", ignore.case = TRUE)) > 0
  }

  if (!has_metadata(dir)) {
    sub <- file.path(dir, "data_package")
    if (dir.exists(sub) && has_metadata(sub)) {
      out$dir <- normalizePath(sub, winslash = "/", mustWork = FALSE)
      out$note <- paste0(
        "The folder you selected contains a data_package subfolder, so that ",
        "subfolder is the one being checked."
      )
    }
  }
  out
}

#' Is this Shiny session running on the user's own computer? The native
#' folder window opens on the machine running R, so it is only useful when
#' that machine is the user's. breezeml is a local app (it is launched with
#' run_breezeml()), so this is normally TRUE. Must be called inside a
#' reactive context; the browser reports its host name a moment after the
#' session starts, so the first answer can be FALSE and then change.
#' @noRd
congruence_is_local_session <- function(session) {
  host <- session$clientData$url_hostname
  isTRUE(host %in% c("127.0.0.1", "localhost", "::1", "[::1]"))
}

#' A button that is plainly unavailable: gray (btn-unavailable, see
#' breezeml.css), the real disabled attribute, announced as disabled to
#' screen readers, and a hover tip saying why. Same pattern as the
#' Activate button in 11_upload_publish.R.
#' @noRd
congruence_unavailable_button <- function(inputId, label, why, icon = NULL) {
  btn <- shiny::actionButton(inputId, label, icon = icon,
                             class = "btn-unavailable", title = why)
  htmltools::tagAppendAttributes(btn, disabled = "disabled",
                                 `aria-disabled` = "true")
}

congruenceUI <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    bslib::card(
      bslib::card_header("Congruence checks (DPchecker)"),
      shiny::helpText(
        "This step runs the ",
        shiny::a("DPchecker R package's",
                 href = "https://doi-nps.github.io/DPchecker/",
                 target = "_blank"),
        " congruence checks against a data package folder (the .csv data ",
        "files and the finalized metadata .xml). By default it checks the ",
        "data package you just generated. You can instead select any ",
        "existing data package folder on your computer, including one made ",
        "in an earlier session or outside breezeml, without generating ",
        "anything. This is informational - it does not change your ",
        "package or block anything. For any check that warns or fails, the ",
        "specific reason (and usually how to fix it) is shown beneath that ",
        "check."
      ),
      shiny::uiOutput(ns("run_controls")),
      shiny::uiOutput(ns("typed_fallback")),
      shiny::uiOutput(ns("summary")),
      shiny::uiOutput(ns("results"))
    ),
    col_widths = c(-1, 10, -1), fill = FALSE
  )
}

#' Congruence checks tab server.
#'
#' @param id module id
#' @param package_dir a reactive() returning the path to the generated
#'   data package directory (the folder containing the .csv files and the
#'   *_metadata.xml), or NULL/"" if no package has been generated yet this
#'   session. Typically shiny::reactive(dirname(last_result$xml_path)) set
#'   from app_server.R after a successful Generate. This is the DEFAULT
#'   folder to check; the user may choose a different one.
#' @return invisible(NULL) - self-contained informational panel.
#' @keywords internal
congruenceServer <- function(id, package_dir) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Holds the most recent run, or NULL if not yet run / cleared because
    # the folder being checked changed. Shape:
    #   list(checks = <list of list(check=, status=, message=)>,
    #        aborted = <logical>, abort_message = <chr or NULL>,
    #        dir = <folder that was checked>, ran_at = <POSIXct>)
    results <- shiny::reactiveVal(NULL)

    # Folder the user picked with the folder window (or the typed
    # fallback). NULL means "use the newly generated package".
    chosen_dir <- shiny::reactiveVal(NULL)

    # TRUE once the typed-path fallback should be offered.
    show_typed <- shiny::reactiveVal(FALSE)

    is_local <- shiny::reactive(congruence_is_local_session(session))

    # The newly generated package folder, if it still exists on disk.
    generated_dir <- shiny::reactive({
      d <- package_dir()
      if (congruence_dir_is_valid(d)) d else NULL
    })

    # The folder to check: the user's choice if any, otherwise the newly
    # generated package. info$dir is NULL when there is neither.
    target_info <- shiny::reactive({
      picked <- chosen_dir()
      if (!is.null(picked)) {
        res <- resolve_package_dir(picked)
        return(list(dir = res$dir, note = res$note, source = "chosen"))
      }
      gen <- generated_dir()
      if (!is.null(gen)) {
        return(list(dir = gen, note = NULL, source = "generated"))
      }
      list(dir = NULL, note = NULL, source = "none")
    })

    target_dir <- shiny::reactive(target_info()$dir)

    file_summary <- shiny::reactive({
      summarize_package_files(target_dir())
    })

    # A fresh Generate (the generated package folder changed) puts the
    # tab back on the default: drop any folder picked earlier. ignoreNULL
    # = FALSE so clearing the dir (e.g. after an EML import) also resets;
    # ignoreInit = TRUE so we don't fire on module startup.
    shiny::observeEvent(package_dir(), {
      chosen_dir(NULL)
      show_typed(FALSE)
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # Never show results for a different folder than the one now selected.
    shiny::observeEvent(target_dir(), {
      results(NULL)
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # ---- Choosing a folder ------------------------------------------

    shiny::observeEvent(input$browse, {
      res <- choose_directory_native(
        caption = "Select the data package folder to check",
        default = target_dir()
      )
      if (identical(res$status, "ok")) {
        chosen_dir(res$path)
      } else if (identical(res$status, "unavailable")) {
        show_typed(TRUE)
        shiny::showNotification(
          paste0("A folder window could not be opened on this computer. ",
                 "Type the folder path in the box below instead."),
          type = "warning", duration = 10
        )
      }
      # "cancelled": the user closed the window; nothing to do.
    })

    shiny::observeEvent(input$use_generated, {
      chosen_dir(NULL)
    })

    shiny::observeEvent(input$use_typed, {
      typed <- trimws(input$typed_dir %||% "")
      if (!nzchar(typed) || !dir.exists(typed)) {
        shiny::showNotification("That folder does not exist.", type = "error")
        return(invisible(NULL))
      }
      chosen_dir(normalizePath(typed, winslash = "/", mustWork = FALSE))
    })

    # ---- Controls ----------------------------------------------------

    output$run_controls <- shiny::renderUI({
      info <- target_info()
      local <- is_local()

      # An unavailable Browse button is never shown; when this is not the
      # user's own computer the typed fallback (typed_fallback) is used.
      browse_button <- function(label) {
        if (!local) return(NULL)
        shiny::actionButton(ns("browse"), label,
                            class = "btn-outline-primary",
                            icon = shiny::icon("folder-open"))
      }

      # No generated package and nothing chosen yet: ask for a folder.
      if (is.null(info$dir)) {
        return(shiny::tagList(
          shiny::tags$div(
            class = "alert alert-info",
            shiny::tags$strong("No newly generated data package was found. "),
            "Select the folder that contains the data package you want to ",
            "check: the .csv data files together with the *_metadata.xml. ",
            "Or generate a package first (Generate tab) and return here."
          ),
          browse_button("Select data package folder...")
        ))
      }

      fs <- file_summary()
      n_data <- nrow(fs$files) - fs$metadata_count

      source_text <- if (identical(info$source, "chosen")) {
        "folder you selected"
      } else {
        "newly generated package"
      }

      folder_line <- shiny::tags$div(
        class = "mb-2",
        shiny::tags$strong("Data package folder: "),
        shiny::tags$code(info$dir),
        shiny::tags$span(class = "text-muted ms-2",
                         style = "font-size: 0.85em;",
                         paste0("(", source_text, ")"))
      )

      note_line <- if (!is.null(info$note)) {
        shiny::tags$div(class = "text-muted mb-2",
                        style = "font-size: 0.85em;", info$note)
      }

      # What is in the folder, and whether the checks can run on it.
      can_run <- fs$metadata_count == 1
      problem <- if (fs$metadata_count == 0) {
        paste0("No *_metadata.xml file was found in this folder. Select the ",
               "folder that holds both the data files and the metadata .xml ",
               "(for a package made by breezeml, the data_package folder).")
      } else if (fs$metadata_count > 1) {
        paste0("More than one *_metadata.xml file was found in this folder. ",
               "A data package may contain only one.")
      } else {
        NULL
      }

      contents_line <- if (can_run) {
        shiny::tags$div(
          class = "text-muted mb-2", style = "font-size: 0.85em;",
          paste0("Found ", n_data, " data file", if (n_data == 1) "" else "s",
                 " (.csv) and 1 metadata file."),
          if (n_data == 0) {
            " No .csv data files are present, so the data-file checks will not be able to pass."
          }
        )
      } else {
        shiny::tags$div(class = "alert alert-warning mb-2", problem)
      }

      run_button <- if (can_run) {
        shiny::actionButton(ns("run"), "Run congruence checks",
                            class = "btn-primary", icon = shiny::icon("play"))
      } else {
        congruence_unavailable_button(
          ns("run"), "Run congruence checks",
          why = "Select a folder that contains exactly one *_metadata.xml.",
          icon = shiny::icon("play")
        )
      }

      back_button <- if (identical(info$source, "chosen") &&
                         !is.null(generated_dir())) {
        shiny::actionButton(ns("use_generated"),
                            "Use newly generated package",
                            class = "btn-outline-primary")
      }

      shiny::tagList(
        folder_line,
        note_line,
        contents_line,
        shiny::tags$div(
          class = "d-flex flex-wrap gap-2",
          run_button,
          browse_button("Select a different folder..."),
          back_button
        )
      )
    })

    # Typed-path fallback. Shown only when a folder window cannot be used
    # (see choose_directory.R) or this is not the user's own computer.
    output$typed_fallback <- shiny::renderUI({
      if (!(isTRUE(show_typed()) || !is_local())) return(NULL)
      shiny::tags$div(
        class = "mt-3",
        shiny::textInput(ns("typed_dir"), "Data package folder (type the path)",
                         placeholder = "path to the folder containing the .csv files and *_metadata.xml",
                         width = "100%"),
        shiny::actionButton(ns("use_typed"), "Use this folder",
                            class = "btn-outline-primary")
      )
    })

    # ---- Running the checks -------------------------------------------

    shiny::observeEvent(input$run, {
      dir <- target_dir()
      if (!congruence_dir_is_valid(dir)) {
        shiny::showNotification(
          paste0("No data package folder to check. Generate a package or ",
                 "select a folder first."),
          type = "error"
        )
        return(invisible(NULL))
      }
      fs <- summarize_package_files(dir)
      if (fs$metadata_count != 1) {
        shiny::showNotification(
          "The folder must contain exactly one *_metadata.xml.",
          type = "error"
        )
        return(invisible(NULL))
      }

      shiny::withProgress(message = "Running congruence checks...", value = 0.4, {
        # DPchecker's usual cli console output is suppressed (v6); we only
        # consume the structured list here. tryCatch handles a gatekeeper
        # abort (which THROWS): on abort the partial list is recovered from
        # e$checks and the abort's own message is kept to show why it
        # stopped.
        run_outcome <- tryCatch(
          {
            # suppressMessages() hides DPchecker's cli console output (see
            # the v6 note in the header). Errors still propagate.
            res <- suppressMessages(
              DPchecker::run_congruence_checks(
                directory = dir,
                check_metadata_only = FALSE
              )
            )
            list(
              checks = attr(res, "checks") %||% list(),
              aborted = FALSE,
              abort_message = NULL
            )
          },
          error = function(e) {
            list(
              checks = e$checks %||% list(),
              aborted = TRUE,
              abort_message = conditionMessage(e)
            )
          }
        )
        shiny::incProgress(0.6)
        run_outcome$dir <- dir
        run_outcome$ran_at <- Sys.time()
        results(run_outcome)
      })
    })

    output$summary <- shiny::renderUI({
      r <- results()
      if (is.null(r)) return(NULL)

      checks <- r$checks
      statuses <- vapply(checks, function(c) c$status %||% "unknown", character(1))
      n_pass <- sum(statuses == "pass")
      n_warn <- sum(statuses == "warn")
      n_fail <- sum(statuses == "fail")

      badge <- function(n, style) {
        shiny::tags$span(
          style = paste0(
            "display:inline-block; margin-right:0.75rem; font-weight:600; color:",
            style$colour, ";"
          ),
          paste0(style$icon, " ", n, " ", style$label, if (n == 1) "" else "s")
        )
      }

      shiny::tagList(
        shiny::tags$hr(),
        shiny::tags$div(
          class = "text-muted mb-1", style = "font-size: 0.85em;",
          paste0("Results for ", r$dir, " (run at ",
                 format(r$ran_at, "%H:%M:%S"), ")")
        ),
        shiny::tags$div(
          class = "mb-2",
          badge(n_pass, congruence_status_style("pass")),
          badge(n_warn, congruence_status_style("warn")),
          badge(n_fail, congruence_status_style("fail"))
        ),
        if (isTRUE(r$aborted)) {
          shiny::tags$div(
            class = "alert alert-danger",
            style = "font-size: 0.9em;",
            shiny::tags$strong("The check run stopped early. "),
            "A required (gatekeeper) check failed, so the remaining checks ",
            "could not run. Fix the issue below, then re-run the congruence ",
            "checks. (For a package made in breezeml, correct the entry and ",
            "generate again first.)",
            if (!is.null(r$abort_message) && nzchar(r$abort_message)) {
              shiny::tags$div(class = "mt-2", shiny::tags$em(r$abort_message))
            }
          )
        }
      )
    })

    output$results <- shiny::renderUI({
      r <- results()
      if (is.null(r)) return(NULL)
      checks <- r$checks
      if (length(checks) == 0) {
        return(shiny::tags$div(
          class = "text-muted mt-2",
          "No per-check results were returned."
        ))
      }

      rows <- lapply(checks, function(chk) {
        status <- chk$status %||% "unknown"
        style <- congruence_status_style(status)
        msg <- clean_message_line(chk$message)  # ANSI/OSC stripped; chr vec or NULL

        # Header row: icon + label + status word.
        header_row <- shiny::tags$div(
          style = paste0(
            "display:flex; align-items:center; padding:0.35rem 0 0.1rem 0;"
          ),
          shiny::tags$span(
            style = paste0(
              "flex:0 0 1.5rem; font-weight:700; color:", style$colour, ";"
            ),
            style$icon
          ),
          shiny::tags$span(
            style = "flex:1 1 auto;",
            label_for(chk$check %||% "unknown")
          ),
          shiny::tags$span(
            style = paste0(
              "flex:0 0 auto; font-size:0.8em; font-weight:600; color:",
              style$colour, ";"
            ),
            style$label
          )
        )

        # Detail block: only for warn/fail with a non-empty message.
        # Indented to align under the label, de-emphasized but readable.
        # Each element of the message vector is its own line.
        show_detail <- status %in% c("warn", "fail") &&
          !is.null(msg) && length(msg) > 0 && any(nzchar(msg))
        detail_block <- if (show_detail) {
          shiny::tags$div(
            style = paste0(
              "margin:0 0 0.35rem 1.5rem; padding:0.4rem 0.6rem; ",
              "font-size:0.85em; color:#333; background-color:#f8f9fa; ",
              "border-left:3px solid ", style$colour, "; border-radius:2px;"
            ),
            lapply(msg[nzchar(msg)], function(line) {
              shiny::tags$div(line)
            })
          )
        } else {
          NULL
        }

        shiny::tags$div(
          style = "border-bottom:1px solid #eee; padding-bottom:0.15rem;",
          header_row,
          detail_block
        )
      })

      shiny::tagList(
        shiny::tags$div(class = "mt-2", rows)
      )
    })

    invisible(NULL)
  })
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a
