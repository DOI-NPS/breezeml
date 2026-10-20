# 10_congruence.R v1
#
# NEW optional tab (Tab 10), placed AFTER Generate (Tab 9). Runs DPchecker's
# full congruence/compliance suite against the data package that Generate
# wrote, and presents the results as a STRUCTURED per-check pass/warn/fail
# list. Purely informational: it NEVER blocks anything - the user reads the
# report and goes back to fix tabs as needed, then re-generates.
#
# NEW DEPENDENCY: DPchecker - add to DESCRIPTION's Imports.
#
# ---- Why this re-implements run_congruence_checks()'s orchestration ----
#
# DPchecker::run_congruence_checks() returns ONLY c(errors = N, warnings = M)
# - just counts. Its per-check detail is emitted as printed cli output, and
# those printed bullets are NOT labeled with which test_*() produced them.
# So that function cannot yield a STRUCTURED per-check list (which is what
# this tab needs). The only way to get one is to run each test_*()
# individually and classify its outcome. That is exactly what
# run_congruence_checks() does internally in its own tryCatch() blocks; we
# mirror it here:
#     error signalled   -> "fail"
#     warning signalled -> "warning"
#     neither           -> "pass"
#
# KNOWN DRIFT RISK (documented, with a built-in alarm): the registry below
# replicates run_congruence_checks()'s list of checks and their argument
# shapes, read directly off that function's body. If DPchecker adds,
# renames, or removes a check, this registry can drift out of sync - the
# SAME class of duplication risk as 09_generate.R's personnel.txt logic.
# Two mitigations: (1) the checks are a single declarative registry
# (congruence_check_registry()) that is easy to diff against the DPchecker
# source; (2) each test is resolved BY NAME from the DPchecker namespace at
# run time (get_dpc_fn()), so a test that no longer exists surfaces as a
# visible "not found in installed DPchecker" row rather than crashing - i.e.
# drift announces itself instead of failing silently.
#
# Gatekeeper-abort chain faithfully replicated: in run_congruence_checks(),
# a failure of load_metadata, test_validate_schema, test_dup_meta_entries,
# test_file_name_match, or test_fields_match cli_abort()s and stops the run.
# Here, a "fail" on any gatekeeper marks every SUBSEQUENT check "skipped
# (blocked by ...)" rather than running it (downstream checks would produce
# misleading cascade errors against metadata/data already known bad).
#
# Requires the DOI/URL-finalized .xml, so it can only run after Generate
# has written the package. Points at the data_package/ directory Generate
# produced (app_server.R persists that path in a reactiveVal after a
# successful generate and passes it in as package_dir_reactive). If nothing
# has been generated yet this session, the tab says so instead of erroring.
#
# CAVEAT to verify in testing: the detail text shown per check comes from
# each test's signalled condition ($message / $body, as run_congruence_checks
# itself consumes them). We strip ANSI escape codes; residual inline cli
# markup (e.g. "{.file x}") - if any survives into the condition object -
# is displayed as-is. If that looks noisy in practice, extend clean_cli_text().

# ---- helpers (pure; unit-testable without Shiny) ----

# Strip ANSI escape sequences and drop empty lines from captured cli text.
clean_cli_text <- function(x) {
  if (length(x) == 0) return(character(0))
  x <- as.character(x)
  x <- x[!is.na(x)]
  x <- gsub("\033\\[[0-9;]*m", "", x)   # ANSI colour codes
  x <- trimws(x)
  x[nzchar(x)]
}

# Resolve a DPchecker function BY NAME whether exported or not; NULL if it
# doesn't exist in the installed DPchecker (the drift alarm - see header).
get_dpc_fn <- function(name) {
  tryCatch(
    get(name, envir = asNamespace("DPchecker"), mode = "function"),
    error = function(e) NULL
  )
}

# Parse a user-entered comma-separated skip_cols string into the shape
# DPchecker::test_date_range() expects. Empty -> NA (its documented default).
parse_skip_cols <- function(x) {
  if (is.null(x)) return(NA)
  parts <- trimws(strsplit(x, ",")[[1]])
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0) NA else parts
}

# Declarative registry of every check run_congruence_checks() performs, in
# order, with the argument shape each test takes (read off that function's
# body) and whether it is a gatekeeper / a data-file check.
#
# argshape:
#   "meta"         -> test(metadata)
#   "dir"          -> test(directory)
#   "dirmeta"      -> test(directory, metadata)
#   "dirmeta_skip" -> test(directory, metadata, skip_cols = skip_cols)
congruence_check_registry <- function() {
  e <- function(test, label, section, argshape = "meta",
                gatekeeper = FALSE, data_check = FALSE) {
    list(test = test, label = label, section = section, argshape = argshape,
         gatekeeper = gatekeeper, data_check = data_check)
  }

  M    <- "Metadata compliance"
  DS   <- "Required for DataStore extraction"
  OPT  <- "Additional / optional metadata"
  DATA <- "Consistency with data files"
  COMP <- "Data & metadata compliance"

  list(
    e("test_validate_schema",         "Metadata schema validates",        M, gatekeeper = TRUE),
    e("test_dup_meta_entries",        "No duplicate data-table names",     M, gatekeeper = TRUE),
    e("test_metadata_version",        "EML metadata version",              M),
    e("test_delimiter",               "Field delimiter declared",          M),
    e("test_header_num",              "Header row count",                  M),
    e("test_footer",                  "Footer declaration",                M),
    e("test_taxonomic_cov",           "Taxonomic coverage",                M),
    e("test_geographic_cov",          "Geographic coverage",               M),
    e("test_content_units",           "Content (park) units",              M),
    e("test_doi",                     "DOI present",                       M),
    e("test_doi_format",              "DOI format",                        M),
    e("test_datatable_urls",          "Data-table URLs present",           M),
    e("test_datatable_urls_doi",      "Data-table URLs match DOI",         M),
    e("test_datatable_url_attributes","Data-table URL attributes",         M),
    e("test_publisher",               "Publisher element",                 M),
    e("test_valid_fieldnames",        "Valid field names",                 M),
    e("test_valid_filenames",         "Valid file names",                  M),
    e("test_pii_meta_emails",         "No PII emails in metadata",         M, argshape = "dir"),

    e("test_creator",                 "Creator(s) present",                DS),
    e("test_pub_date",                "Publication date",                  DS),
    e("test_dp_title",                "Data-package title",                DS),
    e("test_keywords",                "Keywords",                          DS),
    e("test_by_for_nps",              "'By or for NPS' set",               DS),
    e("test_publisher_name",          "Publisher name",                    DS),
    e("test_publisher_state",         "Publisher state",                   DS),
    e("test_publisher_city",          "Publisher city",                    DS),
    e("test_dp_abstract",             "Abstract",                          DS),
    e("test_methods",                 "Methods",                           DS),
    e("test_file_descript",           "File descriptions",                 DS),
    e("test_permissions",             "Permissions / access",              DS),
    e("test_license",                 "License",                           DS),
    e("test_int_rights",              "Intellectual rights",               DS),
    e("test_attribute_defs",          "Attribute definitions",             DS),
    e("test_storage_type",            "Storage type",                      DS),

    e("test_orcid_exists",            "ORCID present",                     OPT),
    e("test_orcid_format",            "ORCID format",                     OPT),
    e("test_orcid_resolves",          "ORCID resolves",                   OPT),
    e("test_orcid_match",             "ORCID matches name",               OPT),
    e("test_notes",                   "Notes",                            OPT),
    e("test_project",                 "Project",                          OPT),

    e("test_file_name_match",         "Data file names match metadata",    DATA, argshape = "dirmeta",      gatekeeper = TRUE, data_check = TRUE),
    e("test_fields_match",            "Data columns match metadata",       DATA, argshape = "dirmeta",      gatekeeper = TRUE, data_check = TRUE),
    e("test_missing_data",            "Missing-value codes",               DATA, argshape = "dirmeta",      data_check = TRUE),
    e("test_numeric_fields",          "Numeric fields are numeric",        DATA, argshape = "dirmeta",      data_check = TRUE),
    e("test_dates_parse",             "Dates parse",                       DATA, argshape = "dirmeta",      data_check = TRUE),
    e("test_date_range",              "Dates within declared range",       DATA, argshape = "dirmeta_skip", data_check = TRUE),

    e("test_pii_data_emails",         "No PII emails in data",             COMP, argshape = "dir", data_check = TRUE)
  )
}

# Run one resolved test function with the right argument shape, classifying
# the outcome exactly as run_congruence_checks() does (error/warning/clean).
run_one_check <- function(fn, argshape, metadata, directory, skip_cols) {
  call_it <- switch(
    argshape,
    meta         = function() fn(metadata),
    dir          = function() fn(directory),
    dirmeta      = function() fn(directory, metadata),
    dirmeta_skip = function() fn(directory, metadata, skip_cols = skip_cols),
    function() fn(metadata)
  )
  tryCatch(
    {
      call_it()
      list(status = "pass", detail = character(0))
    },
    error = function(e) list(status = "fail",    detail = c(conditionMessage(e), e$body)),
    warning = function(w) list(status = "warning", detail = c(conditionMessage(w), w$body))
  )
}

# Assemble accumulated per-check results into a tibble + a status summary.
finalize_congruence_results <- function(results) {
  if (length(results) == 0) {
    df <- tibble::tibble(section = character(), check = character(),
                         status = character(), detail = character())
  } else {
    df <- tibble::tibble(
      section = purrr::map_chr(results, "section"),
      check   = purrr::map_chr(results, "check"),
      status  = purrr::map_chr(results, "status"),
      detail  = purrr::map_chr(results, "detail")
    )
  }
  summary <- c(
    pass    = sum(df$status == "pass"),
    warning = sum(df$status == "warning"),
    fail    = sum(df$status == "fail"),
    skipped = sum(df$status == "skipped")
  )
  list(results = df, summary = summary)
}

#' Run DPchecker's full suite against `directory`, returning STRUCTURED
#' per-check results (not just counts). Pure-ish: no Shiny dependency, but
#' does real file/network work via the DPchecker tests. Never throws for a
#' failing CHECK (those become "fail"/"warning" rows); only truly
#' exceptional conditions (e.g. DPchecker missing entirely) propagate.
#'
#' @param directory path to the data package to check (the Generate tab's
#'   data_package/ folder: the .xml plus the data .csv files)
#' @param check_metadata_only logical - skip the data-file checks
#' @param skip_cols NA or character vector of data columns to exclude from
#'   the date-range check (passed straight to test_date_range)
#' @return list(results = tibble(section, check, status, detail),
#'              summary = c(pass=, warning=, fail=, skipped=))
run_structured_congruence_checks <- function(directory,
                                             check_metadata_only = FALSE,
                                             skip_cols = NA) {
  results <- list()
  add <- function(section, label, status, detail = character(0)) {
    results[[length(results) + 1]] <<- list(
      section = section, check = label, status = status,
      detail = paste(clean_cli_text(detail), collapse = "\n")
    )
  }

  # --- Gatekeeper 0: load the metadata (everything else depends on it) ---
  load_fn <- get_dpc_fn("load_metadata")
  if (is.null(load_fn)) {
    add("Reading metadata", "Load metadata", "fail",
        "DPchecker::load_metadata() not found in the installed DPchecker.")
    return(finalize_congruence_results(results))
  }

  # Capture warnings without aborting (so we still get the metadata object),
  # but treat a hard error as a fatal gatekeeper failure.
  load_warnings <- character(0)
  metadata <- withCallingHandlers(
    tryCatch(load_fn(directory), error = function(e) e),
    warning = function(w) {
      load_warnings <<- c(load_warnings, conditionMessage(w), w$body)
      invokeRestart("muffleWarning")
    }
  )
  if (inherits(metadata, "error") || inherits(metadata, "condition")) {
    add("Reading metadata", "Load metadata", "fail",
        c(conditionMessage(metadata), metadata$body))
    return(finalize_congruence_results(results))
  }
  add("Reading metadata", "Load metadata",
      if (length(load_warnings) > 0) "warning" else "pass",
      load_warnings)

  # --- Run every registered check, honouring the gatekeeper-abort chain ---
  blocked_by <- NULL
  for (chk in congruence_check_registry()) {
    if (chk$data_check && isTRUE(check_metadata_only)) next

    if (!is.null(blocked_by)) {
      add(chk$section, chk$label, "skipped",
          paste0("Skipped - blocked by an earlier failed check (", blocked_by, ")."))
      next
    }

    fn <- get_dpc_fn(chk$test)
    if (is.null(fn)) {
      # drift alarm: a check in our registry no longer exists in DPchecker
      add(chk$section, chk$label, "skipped",
          paste0("Check '", chk$test, "' was not found in the installed DPchecker ",
                 "(version mismatch - this tab's check list may need updating)."))
      next
    }

    res <- run_one_check(fn, chk$argshape, metadata, directory, skip_cols)
    add(chk$section, chk$label, res$status, res$detail)

    if (isTRUE(chk$gatekeeper) && identical(res$status, "fail")) {
      blocked_by <- chk$label
    }
  }

  finalize_congruence_results(results)
}

# ---- Shiny module ----

STATUS_BG <- c(
  pass    = "#d1e7dd",  # green
  warning = "#fff3cd",  # amber (matches Tab 4's required-cell highlight)
  fail    = "#f8d7da",  # red
  skipped = "#e2e3e5"   # grey
)
STATUS_LABEL <- c(
  pass = "Pass", warning = "Warning", fail = "Fail", skipped = "Skipped"
)

congruenceUI <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    bslib::card(
      bslib::card_header("Congruence & compliance checks (optional)"),
      shiny::helpText(
        "Run DPchecker's full suite of checks against the data package you ",
        "generated on the previous tab - comparing the metadata (.xml) ",
        "against the data files (.csv), and checking the metadata for the ",
        "elements DataStore and NPS policy require. This is ", shiny::HTML("<b>informational only</b>"),
        ": nothing here blocks anything. Use the results to decide whether to ",
        "go back, fix something, and generate again."
      ),
      shiny::helpText(
        "You must generate the data package first (previous tab) - these ",
        "checks run against the finalized .xml (including its DOI and data ",
        "table URLs) and the copied .csv files."
      ),
      shiny::checkboxInput(ns("check_metadata_only"),
                           "Skip data-file checks (check metadata only - faster)",
                           value = FALSE),
      shiny::textInput(ns("skip_cols"),
                       "Columns to skip in the date-range check (optional, comma-separated)",
                       placeholder = "e.g. QC_date, reviewed_on", width = "100%"),
      shiny::helpText("Useful if some date columns (e.g. QA/QC dates) are expected to ",
                      "fall outside the data's declared temporal coverage."),
      shiny::actionButton(ns("run_checks"), "Run congruence checks",
                          class = "btn-primary"),
      shiny::uiOutput(ns("no_package")),
      shiny::uiOutput(ns("summary")),
      shiny::br(),
      DT::DTOutput(ns("results"))
    ),
    col_widths = c(-1, 10, -1), fill = FALSE
  )
}

#' @param id module id
#' @param package_dir_reactive a reactive() returning the path to the last
#'   successfully-generated data_package/ directory, or NULL if none this
#'   session (app_server.R sets this after a successful generate).
#'
#' @noRd
#'
congruenceServer <- function(id, package_dir_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    checks_result <- shiny::reactiveVal(NULL)

    output$no_package <- shiny::renderUI({
      dir <- tryCatch(package_dir_reactive(), error = function(e) NULL)
      if (is.null(dir) || !nzchar(dir) || !dir.exists(dir)) {
        shiny::tags$div(
          class = "alert alert-secondary mt-2",
          style = "font-size: 0.9em;",
          "No generated data package found yet this session. Generate the ",
          "data package on the previous tab, then run the checks here."
        )
      } else {
        NULL
      }
    })

    shiny::observeEvent(input$run_checks, {
      dir <- tryCatch(package_dir_reactive(), error = function(e) NULL)
      if (is.null(dir) || !nzchar(dir) || !dir.exists(dir)) {
        shiny::showNotification(
          "No generated data package found yet. Generate it on the previous tab first.",
          type = "error"
        )
        return(invisible(NULL))
      }

      skip <- parse_skip_cols(input$skip_cols)

      shiny::withProgress(message = "Running congruence checks...", value = 0.3, {
        res <- tryCatch(
          run_structured_congruence_checks(
            directory = dir,
            check_metadata_only = isTRUE(input$check_metadata_only),
            skip_cols = skip
          ),
          error = function(e) {
            shiny::showNotification(
              paste0("Congruence checks could not run: ", conditionMessage(e)),
              type = "error", duration = 10
            )
            NULL
          }
        )
        shiny::incProgress(0.7)
        checks_result(res)
      })
    })

    output$summary <- shiny::renderUI({
      res <- checks_result()
      if (is.null(res)) return(NULL)
      s <- res$summary

      overall_class <- if (s[["fail"]] > 0) {
        "alert alert-danger"
      } else if (s[["warning"]] > 0) {
        "alert alert-warning"
      } else {
        "alert alert-success"
      }
      overall_msg <- if (s[["fail"]] > 0) {
        sprintf("%d check(s) failed, %d warning(s).", s[["fail"]], s[["warning"]])
      } else if (s[["warning"]] > 0) {
        sprintf("No failures, but %d warning(s) to look into.", s[["warning"]])
      } else {
        "All checks passed."
      }

      shiny::tags$div(
        class = paste(overall_class, "mt-3"),
        shiny::tags$strong(overall_msg),
        shiny::tags$div(
          class = "text-muted", style = "font-size: 0.85em;",
          sprintf("%d passed \u00b7 %d warning \u00b7 %d failed \u00b7 %d skipped",
                  s[["pass"]], s[["warning"]], s[["fail"]], s[["skipped"]])
        )
      )
    })

    output$results <- DT::renderDT({
      res <- checks_result()
      shiny::req(res)
      df <- res$results
      shiny::req(nrow(df) > 0)

      # Human-friendly status label + HTML-safe, line-broken detail.
      disp <- df
      disp$status <- unname(STATUS_LABEL[disp$status])
      disp$detail <- vapply(df$detail, function(d) {
        if (!nzchar(d)) return("")
        gsub("\n", "<br>", htmltools::htmlEscape(d), fixed = TRUE)
      }, character(1))

      disp <- disp[, c("section", "check", "status", "detail")]

      DT::datatable(
        disp,
        rownames = FALSE,
        selection = "none",
        colnames = c("Section", "Check", "Status", "Details"),
        # only the Details column holds (pre-escaped) HTML; escape the rest
        escape = which(names(disp) != "detail") - 1,
        options = list(
          pageLength = -1,
          dom = 'ft',
          order = list(),           # preserve run order, don't auto-sort
          columnDefs = list(list(targets = 3, width = "45%"))
        )
      ) |>
        DT::formatStyle(
          "status",
          backgroundColor = DT::styleEqual(
            unname(STATUS_LABEL),
            unname(STATUS_BG[names(STATUS_LABEL)])
          ),
          fontWeight = "bold"
        )
    })
  })
}
