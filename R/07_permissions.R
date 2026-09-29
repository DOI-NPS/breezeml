# 07_permissions.R v7
#
# v7: added restore() to permissionsServer()'s return value, for the
# Save/Load session feature (app_state.R / app_server.R). No dependency
# on Tab 3's uploaded files - every field here is a plain Shiny input, so
# restore() is just a sequence of update*Input() calls and can run
# immediately on load.
#
# One subtlety: legal_authority_id's choices are populated asynchronously
# from NPSdatastore::get_legal_authority() (see the shiny::observe() block
# below) - the selectInput starts with choices = NULL and only gets real
# choices once that API call resolves. If restore() calls
# updateSelectInput(..., selected = saved$legal_authority_id) BEFORE that
# observe() has populated choices, the selected value has nothing to
# attach to and is silently dropped. restore() re-applies the saved
# selection inside a shiny::observe() gated on legal_authorities being
# loaded AND non-empty, so it fires once choices actually exist,
# regardless of which happens first (API response vs. restore() being
# called) - this mirrors the existing pattern already used for
# legal_authority_description's rendering.
#
# permissionsServer()'s return value changes from a bare reactive() to
# list(data = <reactive>, restore = <function>).
#
# NEW: emit_permissions_chunk() - the "Preview script" button and the
# generation_script.R written to disk previously stopped right after
# make_eml()/eml_validate()/write_eml() and never included ANY of the
# EMLeditor::set_*() calls that actually get applied at runtime (Tabs
# 7-9's post-make_eml() edits). This mirrors the existing
# emit_high_level_chunk()/emit_people_chunk()/etc. pattern: a pure
# function that emits the R code TEXT equivalent of what
# apply_permissions_to_eml() actually DOES at runtime, so the generated
# script is a genuinely complete, standalone reproduction - not just the
# make_eml() portion.
#
# Kept in exact sync with apply_permissions_to_eml()'s real logic below -
# if that function's behavior ever changes, this emit function must be
# updated to match, or the "preview"/saved script will silently drift from
# what the app actually does.
#
# Fixed bare setNames() -> stats::setNames() to resolve R CMD check's
# "no visible global function definition" note.
#
# Added @noRd to permissionsServer() - internal Shiny module server, not
# meant to have a public help page. Resolves roxygen2's "Skipping; no
# name and/or title" note.
#
# Corresponds to skeleton.Rmd's "Add file access permissions and
# justifications" (EMLeditor::set_permissions()), "Intellectual Rights"
# (EMLeditor::set_int_rights()), and "Set the language"
# (EMLeditor::set_language()) sections.
#
# These three are grouped together because access level and intellectual
# rights are cross-validated per skeleton.Rmd's explicit warning: a PUBLIC
# access level cannot have a "restricted" license, and an INTERNAL/
# RESTRICTED package cannot have a public-domain/CC0 license. This tab
# enforces that agreement rather than letting the two drift apart and only
# discovering the mismatch when EMLeditor::set_int_rights() is applied.
#
# Unlike Tabs 1-6, this tab's state does NOT get written to .txt templates
# consumed by make_eml() - it gets applied to the EML object AFTER
# make_eml() succeeds, via EMLeditor::set_*() calls, as part of
# run_generation()'s post-make_eml() step. See 09_generate.R.

ACCESS_LEVELS <- c(
  "Public - no restrictions" = "PUBLIC",
  "Internal - requires NPS network/VPN" = "INTERNAL",
  "Restricted - limited to named individuals" = "RESTRICTED"
)

INTELLECTUAL_RIGHTS_OPTIONS <- c(
  "CC0 (public domain dedication) - default for public NPS data" = "CC0",
  "Public domain (no CUI, no copyright/license)" = "public",
  "Restricted (contains CUI, internal use only)" = "restricted"
)

permissionsUI <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    bslib::card(
      bslib::card_header("File Access Permissions"),
      shiny::helpText("Indicate the access level for this data package. This is ",
                      "required even for PUBLIC packages, so users can see that ",
                      "the absence of CUI was a deliberate determination rather ",
                      "than an oversight."),
      shiny::radioButtons(ns("access_level"), "Access level",
                          choices = ACCESS_LEVELS, selected = "PUBLIC"),

      shiny::conditionalPanel(
        condition = "input.access_level != 'PUBLIC'",
        ns = ns,
        shiny::selectInput(ns("legal_authority_id"), "Justification (legal authority)",
                           choices = NULL, width = "100%"),
        shiny::uiOutput(ns("legal_authority_description")),
        shiny::textInput(ns("contact_email"), "Contact email for access requests",
                         placeholder = "e.g. a group email such as park_data@nps.gov",
                         width = "100%"),
        shiny::helpText("A group email is recommended over an individual's, given ",
                        "staff turnover."),
        shiny::textInput(ns("authority_designator"), "Person responsible for restriction decisions",
                         width = "100%")
      )
    ),
    bslib::card(
      bslib::card_header("Intellectual Rights"),
      shiny::helpText("Must agree with the access level above: PUBLIC packages ",
                      "cannot use a 'restricted' license, and INTERNAL/RESTRICTED ",
                      "packages cannot use CC0 or public domain."),
      shiny::radioButtons(ns("int_rights"), "License", choices = INTELLECTUAL_RIGHTS_OPTIONS,
                          selected = "CC0"),
      shiny::uiOutput(ns("rights_mismatch_warning"))
    ),
    bslib::card(
      bslib::card_header("Language"),
      shiny::helpText("The human language the data package and metadata are ",
                      "written in."),
      shiny::selectInput(ns("language"), NULL,
                         choices = c("English", "Spanish", "French", "German",
                                     "Navajo", "Hawaiian", "Other"),
                         selected = "English", width = "100%"),
      shiny::conditionalPanel(
        condition = "input.language == 'Other'",
        ns = ns,
        shiny::textInput(ns("language_other"), "Specify language",
                         placeholder = "Use the English Name of Language, e.g. 'Zuni'",
                         width = "100%")
      )
    ),
    col_widths = c(-2, 8, -2), fill = FALSE
  )
}

#' @return list(data = <reactive() list>, restore = <function>)
#'   data() returns:
#'     $access_level, $legal_authority_id, $contact_email,
#'     $authority_designator, $int_rights, $language
#'     $valid, $errors
#' @noRd
permissionsServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    legal_authorities <- tryCatch({
      df <- NPSdatastore::get_legal_authority()
      df[isTRUE(df$active) | df$active == TRUE, ]
    }, error = function(e) {
      shiny::showNotification(
        paste0("Could not load legal authority justifications from DataStore: ",
               conditionMessage(e), ". You can still proceed if Access level is Public."),
        type = "warning", duration = 10
      )
      NULL
    })

    shiny::observe({
      if (!is.null(legal_authorities) && nrow(legal_authorities) > 0) {
        choices <- stats::setNames(legal_authorities$id, legal_authorities$label)
        shiny::updateSelectInput(session, "legal_authority_id", choices = choices)
      }
    })

    output$legal_authority_description <- shiny::renderUI({
      shiny::req(input$legal_authority_id, legal_authorities)
      row <- legal_authorities[legal_authorities$id == as.integer(input$legal_authority_id), ]
      if (nrow(row) == 0) return(NULL)
      shiny::helpText(row$description[1])
    })

    # Cross-validation: PUBLIC <-> CC0/public only; INTERNAL/RESTRICTED <-> restricted only
    mismatch <- shiny::reactive({
      access <- input$access_level %||% "PUBLIC"
      rights <- input$int_rights %||% "CC0"

      if (access == "PUBLIC" && rights == "restricted") {
        return("A PUBLIC data package cannot use a 'restricted' license - choose CC0 or public domain, or change the access level.")
      }
      if (access != "PUBLIC" && rights %in% c("CC0", "public")) {
        return("An INTERNAL or RESTRICTED data package cannot use a public-domain/CC0 license - choose 'restricted', or change the access level to Public.")
      }
      NULL
    })

    output$rights_mismatch_warning <- shiny::renderUI({
      m <- mismatch()
      if (is.null(m)) return(NULL)
      shiny::tags$div(class = "alert alert-danger mt-2", m)
    })

    data <- shiny::reactive({
      access <- input$access_level %||% "PUBLIC"
      rights <- input$int_rights %||% "CC0"
      language <- if (identical(input$language, "Other")) trimws(input$language_other %||% "") else input$language

      errors <- character(0)
      m <- mismatch()
      if (!is.null(m)) errors <- c(errors, m)

      if (access != "PUBLIC") {
        if (is.null(input$legal_authority_id) || !nzchar(input$legal_authority_id)) {
          errors <- c(errors, "A justification (legal authority) is required for Internal or Restricted access.")
        }
        if (!nzchar(trimws(input$contact_email %||% ""))) {
          errors <- c(errors, "A contact email is required for Internal or Restricted access.")
        }
        if (!nzchar(trimws(input$authority_designator %||% ""))) {
          errors <- c(errors, "The person responsible for restriction decisions is required for Internal or Restricted access.")
        }
      }

      if (!nzchar(language)) {
        errors <- c(errors, "Language is required.")
      }

      list(
        access_level = access,
        legal_authority_id = if (access != "PUBLIC") as.integer(input$legal_authority_id) else NA_integer_,
        contact_email = trimws(input$contact_email %||% ""),
        authority_designator = trimws(input$authority_designator %||% ""),
        int_rights = rights,
        language = language,
        valid = length(errors) == 0,
        errors = errors
      )
    })

    # Holds a pending restored legal_authority_id until legal_authorities'
    # choices are actually populated - see restore()/the observe() below.
    pending_legal_authority_id <- shiny::reactiveVal(NULL)

    # Applies pending_legal_authority_id() once legal_authorities has
    # loaded AND its choices have been pushed into the selectInput by the
    # observe() above. Ordering between "restore() was called" and "the
    # API call to get_legal_authority() resolved" is not guaranteed either
    # way, so this fires on EITHER becoming ready and simply no-ops if the
    # other piece isn't ready yet.
    shiny::observe({
      pending <- pending_legal_authority_id()
      shiny::req(pending, legal_authorities, nrow(legal_authorities) > 0)
      shiny::updateSelectInput(session, "legal_authority_id", selected = as.character(pending))
      pending_legal_authority_id(NULL)
    })

    #' Push saved state into this module's inputs. Called once by
    #' app_server.R right after a JSON load, with
    #' saved_state$permissions (see app_state.R). No dependency on Tab
    #' 3's uploaded files - safe to call immediately.
    #'
    #' @param saved list matching default_app_state()$permissions's shape
    restore <- function(saved) {
      if (is.null(saved)) return(invisible(NULL))

      shiny::updateRadioButtons(session, "access_level", selected = saved$access_level %||% "PUBLIC")
      shiny::updateRadioButtons(session, "int_rights", selected = saved$int_rights %||% "CC0")
      shiny::updateTextInput(session, "contact_email", value = saved$contact_email %||% "")
      shiny::updateTextInput(session, "authority_designator", value = saved$authority_designator %||% "")

      saved_language <- saved$language %||% "English"
      known_languages <- c("English", "Spanish", "French", "German", "Navajo", "Hawaiian")
      if (saved_language %in% known_languages) {
        shiny::updateSelectInput(session, "language", selected = saved_language)
      } else if (nzchar(saved_language)) {
        # anything not in the fixed dropdown list was originally entered
        # via the "Other" + free-text path - restore both pieces so the
        # conditionalPanel shows the right custom value again
        shiny::updateSelectInput(session, "language", selected = "Other")
        shiny::updateTextInput(session, "language_other", value = saved_language)
      }

      if (!is.null(saved$legal_authority_id) && !is.na(saved$legal_authority_id)) {
        # deferred - see pending_legal_authority_id()/observe() above,
        # since legal_authorities' choices may not be populated yet
        pending_legal_authority_id(saved$legal_authority_id)
      }

      invisible(NULL)
    }

    list(data = data, restore = restore)
  })
}

#' Apply this tab's state to an in-memory EML object via EMLeditor's set_*()
#' functions. Called from run_generation() AFTER make_eml() succeeds and
#' BEFORE write_eml() - this is the "edit before writing to disk" step that
#' replaces the CLI workflow's separate post-hoc editing pass. Returns the
#' modified EML object, or throws (caller wraps in tryCatch).
#'
#' set_permissions()'s real signature (as of writing, still in development,
#' not yet merged to EMLeditor main):
#'   set_permissions(eml_object, access = c("PUBLIC","RESTRICTED","INTERNAL"),
#'                    legal_authority_id = NULL, contact_email = NULL,
#'                    authority_designator = NULL, force = FALSE, NPS = TRUE)
#' For PUBLIC access, legal_authority_id/contact_email/authority_designator
#' must be left NULL (not just empty strings) - they are simply not
#' applicable to a public data package, and passing empty strings instead
#' of NULL could be misinterpreted as actual (blank) values rather than
#' "not applicable".
#'
#' force = TRUE is used on every EMLeditor set_*() call in this app: it
#' suppresses interactive/console-oriented behavior (confirmation prompts,
#' verbose output) that doesn't apply in a non-interactive Shiny server
#' context - there is no console for a prompt to appear in.
#'
#' NPS = TRUE is used on every EMLeditor set_*() call that supports it: it
#' triggers EMLeditor::.set_for_by_nps(), which adds standard, always-true
#' NPS attribution metadata to the EML object. This app is exclusively for
#' NPS data packages, so NPS = TRUE is hardcoded rather than exposed as a
#' setting - per current guidance, exposing NPS = FALSE as a toggle may be
#' a future feature, but is not needed now.
#'
#' @param my_metadata the in-memory EML object (post make_eml(), pre write_eml())
#' @param state the list returned by permissionsServer()'s $data reactive,
#'   evaluated (i.e. state <- permissions$data())
#' @return the EML object with permissions, intellectual rights, and
#'   language applied
apply_permissions_to_eml <- function(my_metadata, state) {
  if (state$access_level == "PUBLIC") {
    my_metadata <- EMLeditor::set_permissions(
      my_metadata,
      access = "PUBLIC",
      legal_authority_id = NULL,
      contact_email = NULL,
      authority_designator = NULL,
      force = TRUE,
      NPS = TRUE
    )
  } else {
    my_metadata <- EMLeditor::set_permissions(
      my_metadata,
      access = state$access_level,
      legal_authority_id = state$legal_authority_id,
      contact_email = state$contact_email,
      authority_designator = state$authority_designator,
      force = TRUE,
      NPS = TRUE
    )
  }

  my_metadata <- EMLeditor::set_int_rights(my_metadata, state$int_rights, force = TRUE, NPS = TRUE)
  my_metadata <- EMLeditor::set_language(my_metadata, state$language, force = TRUE, NPS = TRUE)

  my_metadata
}

#' Emit the R code chunk that reproduces apply_permissions_to_eml()'s
#' calls as literal script text, for the "Preview script" modal and the
#' generation_script.R written to disk. Pure function - must be kept in
#' exact sync with apply_permissions_to_eml() above; if that function's
#' logic changes, update this to match.
#'
#' @param state the list returned by permissionsServer()'s $data reactive,
#'   evaluated (i.e. state <- permissions$data())
#' @return character - the R code chunk applying permissions/rights/language
emit_permissions_chunk <- function(state) {
  if (is.null(state) || !isTRUE(state$valid)) {
    return(paste0(
      "# Permissions/rights/language information is incomplete - resolve\n",
      "# the following before generating a final script:\n",
      paste0("#   - ", state$errors, collapse = "\n"), "\n"
    ))
  }

  access_r <- deparse(state$access_level)
  int_rights_r <- deparse(state$int_rights)
  language_r <- deparse(state$language)

  if (state$access_level == "PUBLIC") {
    permissions_call <- glue::glue(
      'my_metadata <- EMLeditor::set_permissions(\n',
      '  my_metadata,\n',
      '  access = {access_r},\n',
      '  legal_authority_id = NULL,\n',
      '  contact_email = NULL,\n',
      '  authority_designator = NULL,\n',
      '  force = TRUE,\n',
      '  NPS = TRUE\n',
      ')\n'
    )
  } else {
    legal_authority_r <- deparse(state$legal_authority_id)
    contact_email_r <- deparse(state$contact_email)
    authority_designator_r <- deparse(state$authority_designator)
    permissions_call <- glue::glue(
      'my_metadata <- EMLeditor::set_permissions(\n',
      '  my_metadata,\n',
      '  access = {access_r},\n',
      '  legal_authority_id = {legal_authority_r},\n',
      '  contact_email = {contact_email_r},\n',
      '  authority_designator = {authority_designator_r},\n',
      '  force = TRUE,\n',
      '  NPS = TRUE\n',
      ')\n'
    )
  }

  glue::glue(
    '# --- Permissions, intellectual rights, language (Tab 7) ---\n',
    '{permissions_call}\n',
    'my_metadata <- EMLeditor::set_int_rights(my_metadata, {int_rights_r}, force = TRUE, NPS = TRUE)\n',
    'my_metadata <- EMLeditor::set_language(my_metadata, {language_r}, force = TRUE, NPS = TRUE)\n',
    .trim = FALSE
  )
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && a == "")) b else a
