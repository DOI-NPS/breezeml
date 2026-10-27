# 10_congruence.R v4
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
# - PURELY INFORMATIONAL. Never blocks anything. Runs only AFTER Generate
#   (it checks the finalized data_package/ directory Generate writes).
# - Runs on BUTTON PRESS; previous results AUTO-CLEAR when a new package
#   directory appears (a fresh Generate), so stale results are never shown.
# - Runs the FULL check (check_metadata_only = FALSE): the data_package/
#   directory has the .csv data files AND the *_metadata.xml, so data-file
#   checks run too. The data-file gatekeepers (file_name_match,
#   fields_match) are therefore reachable and can abort - handled via the
#   e$checks recovery path.
#
# ---- Dependency ----
#
# Requires DPchecker (>= the breezeml_dev branch that returns the "checks"
# attribute with per-check `message`, and the e$checks abort field).
# DESCRIPTION must carry:
#   Imports: DPchecker
#   Remotes: doi-nps/DPchecker@breezeml_dev
# until that branch is merged to DPchecker's main/release.

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
        "congruence checks against the data package that ",
        "was just generated (the data_package/ folder, including your data ",
        "files and the finalized metadata .xml). This is informational - it ",
        "does not change your package or block anything. Generate the metadata ",
        "first, then run these checks to review the result before uploading to ",
        "DataStore. For any check that warns or fails, the specific reason ",
        "(and usually how to fix it) is shown beneath that check."
      ),
      shiny::uiOutput(ns("run_controls")),
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
#'   from app_server.R after a successful Generate.
#' @return invisible(NULL) - self-contained informational panel.
#' @keywords internal
congruenceServer <- function(id, package_dir) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Holds the most recent run, or NULL if not yet run / cleared because
    # a new package was generated. Shape:
    #   list(checks = <list of list(check=, status=, message=)>,
    #        aborted = <logical>, abort_message = <chr or NULL>)
    results <- shiny::reactiveVal(NULL)

    # AUTO-CLEAR on a new Generate: when the package directory changes,
    # drop prior results so the panel never shows stale checks against a
    # newer package. ignoreNULL = FALSE so clearing the dir also resets;
    # ignoreInit = TRUE so we don't fire on module startup.
    shiny::observeEvent(package_dir(), {
      results(NULL)
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    output$run_controls <- shiny::renderUI({
      dir <- package_dir()
      if (is.null(dir) || !nzchar(dir) || !dir.exists(dir)) {
        return(shiny::tags$div(
          class = "text-muted",
          "No generated data package found yet. Generate a data package first ",
          "(Generate tab), then return here to run the congruence checks."
        ))
      }
      shiny::tagList(
        shiny::actionButton(ns("run"), "Run congruence checks",
                            class = "btn-primary", icon = shiny::icon("play")),
        shiny::tags$span(class = "text-muted ms-2",
                         style = "font-size: 0.85em;",
                         paste0("Target: ", dir))
      )
    })

    shiny::observeEvent(input$run, {
      dir <- package_dir()
      if (is.null(dir) || !nzchar(dir) || !dir.exists(dir)) {
        shiny::showNotification(
          "No generated data package directory to check. Generate first.",
          type = "error"
        )
        return(invisible(NULL))
      }

      shiny::withProgress(message = "Running congruence checks...", value = 0.4, {
        # DPchecker prints its usual cli output to the console/log; we only
        # consume the structured list here. tryCatch handles a gatekeeper
        # abort (which THROWS): on abort the partial list is recovered from
        # e$checks and the abort's own message is kept to show why it
        # stopped.
        run_outcome <- tryCatch(
          {
            res <- DPchecker::run_congruence_checks(
              directory = dir,
              check_metadata_only = FALSE
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
            "could not run. Fix the issue below and generate again, then ",
            "re-run the congruence checks.",
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
