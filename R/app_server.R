# app_server.R v2
#
# FIXED: fieldsServer() (04_fields.R v20) now returns
# list(tables=, valid=, errors=) instead of just the bare named list of
# tables - it previously computed no validation at all, meaning Tab 4
# (numeric columns missing units, Date columns missing format strings,
# categorical codes missing definitions, blank attribute definitions)
# could NEVER block Generate, regardless of content. Confirmed via live
# testing: a numeric column with no unit was silently accepted, "Generate"
# reported everything complete, and the resulting EML silently dropped
# that entire data table and failed schema validation.
#
# This required two changes here:
#   1. all_errors() now includes fields()'s $valid/$errors, same pattern
#      as every other tab.
#   2. Every other place that consumed fields() expecting the OLD bare
#      list-of-tables shape now uses fields()$tables instead (the preview
#      script handler and do_generate()'s call into run_generation()).
#
# The top-level Shiny server function, converted from app.R's top-level
# `server <- function(input, output, session) {...}` script variable -
# same packaging rationale as app_ui.R.

#' bReezEML server logic
#' @param input,output,session standard Shiny server arguments
#' @keywords internal
app_server <- function(input, output, session) {
  high_level <- highLevelServer("high_level")
  people <- peopleServer("people")
  tables <- tableMetadataServer("table_metadata")

  # shared reactive of just the parsed data.frames, keyed by file name -
  # this is what downstream modules (fields, geography, taxonomy) consume
  table_data <- shiny::reactive({
    tables()$data
  })

  fields <- fieldsServer("fields", table_data)
  geography <- geographyServer("geography", table_data)
  taxonomy <- taxonomyServer("taxonomy", table_data)
  permissions <- permissionsServer("permissions")
  org_context <- org_contextServer("org_context")

  # Tracks the most recent successfully-created DataStore draft reference
  # this session, if any. NULL until a create-new-DOI generate succeeds
  # once. Persists across regenerates so the same draft/DOI is reused
  # rather than silently creating duplicates.
  doi_state <- shiny::reactiveVal(NULL)

  # Collect every tab's validation errors in one place so "Generate" has a
  # single, clear gate rather than failing deep inside make_eml().
  all_errors <- shiny::reactive({
    hl <- high_level()
    pp <- people()
    tb <- tables()
    fl <- fields()
    pm <- permissions()
    oc <- org_context()
    c(
      if (!isTRUE(hl$valid)) hl$errors,
      if (!isTRUE(pp$valid)) pp$errors,
      if (!isTRUE(tb$valid)) tb$errors,
      if (!isTRUE(fl$valid)) fl$errors,
      if (!isTRUE(pm$valid)) pm$errors,
      if (!isTRUE(oc$valid)) oc$errors
    )
  })

  output$generate_status <- shiny::renderUI({
    errs <- all_errors()
    if (length(errs) == 0) {
      return(shiny::tags$div(class = "text-success", "All required information is complete. Ready to generate."))
    }
    shiny::tagList(
      shiny::tags$div(class = "text-danger", "Resolve the following before generating:"),
      shiny::tags$ul(lapply(errs, shiny::tags$li))
    )
  })

  shiny::observeEvent(input$preview_script, {
    script <- build_generation_script(
      high_level(), people(), tables(), fields()$tables, geography(), taxonomy()
    )
    shiny::showModal(shiny::modalDialog(
      title = "Generated script (preview)",
      shiny::tags$pre(script, style = "white-space: pre-wrap; max-height: 500px; overflow-y: auto;"),
      size = "l", easyClose = TRUE, footer = shiny::modalButton("Close")
    ))
  })

  output$generation_result <- shiny::renderUI({ NULL })

  # Actually runs the generation pipeline exactly once.
  # @param create_new_doi passed straight through to run_generation() -
  #   TRUE only right after the user has confirmed creating/replacing a
  #   DataStore draft reference; FALSE (the default) reuses whatever
  #   doi_state() already holds, if anything.
  do_generate <- function(create_new_doi = FALSE) {
    wf <- input$working_folder
    if (!nzchar(wf) || !dir.exists(wf)) {
      shiny::showNotification("Working folder does not exist or is not accessible.", type = "error")
      return(invisible(NULL))
    }

    shiny::withProgress(message = "Generating EML...", value = 0.3, {
      result <- run_generation(
        parent_folder = wf,
        high_level_state = high_level(),
        people_state = people(),
        tables_state = tables(),
        fields_state = fields()$tables,
        geo_state = geography(),
        taxonomy_state = taxonomy(),
        permissions_state = permissions(),
        org_context_state = org_context(),
        doi_state = doi_state(),
        create_new_doi = create_new_doi
      )
      shiny::incProgress(0.7)

      if (isTRUE(result$success) && !is.null(result$doi_state)) {
        doi_state(result$doi_state)
      }

      output$generation_result <- shiny::renderUI({
        if (isTRUE(result$success)) {
          shiny::tagList(
            shiny::tags$div(class = "text-success mt-2", result$message),
            shiny::tags$div(class = "text-muted", paste0("Written to: ", result$xml_path)),
            if (!is.null(result$doi_state) && !is.na(result$doi_state$doi)) {
              shiny::tags$div(class = "text-muted", paste0("DataStore DOI: ", result$doi_state$doi))
            },
            if (!is.null(result$content_issues)) {
              shiny::tagList(
                shiny::tags$hr(),
                shiny::tags$details(
                  shiny::tags$summary(
                    style = "cursor: pointer; font-weight: 600;",
                    "Review notes from EMLassemblyline::issues() (click to expand)"
                  ),
                  shiny::tags$p(class = "text-muted mt-2",
                                "This always runs and often includes expected notes (e.g. no ",
                                "Principal Investigator listed, which NPS data packages don't ",
                                "require). Skim for anything indicating a field was skipped or ",
                                "not understood - if something you entered in Tabs 4-6 isn't ",
                                "reflected here, you can go fix it and generate again without ",
                                "losing any of your other entries."),
                  shiny::tags$pre(
                    style = "white-space: pre-wrap; max-height: 300px; overflow-y: auto;",
                    result$content_issues
                  )
                )
              )
            }
          )
        } else {
          shiny::tagList(
            shiny::tags$div(class = "text-danger mt-2", result$message),
            if (!is.null(result$validation_errors)) {
              shiny::tags$ul(lapply(result$validation_errors, shiny::tags$li))
            }
          )
        }
      })
    })
  }

  shiny::observeEvent(input$generate_script, {
    errs <- all_errors()
    if (length(errs) > 0) {
      shiny::showNotification("Cannot generate: required information is missing. See the list above.",
                              type = "error")
      return(invisible(NULL))
    }

    # DataStore draft reference creation is a real, non-idempotent side
    # effect - always confirm before creating or replacing one, every
    # time, whether this is the very first generate this session or a
    # regenerate that will replace an existing draft. There is no
    # "keep existing draft" option - every generate after the first
    # creates a brand-new draft/DOI and deletes the superseded one
    # (see cleanup_old_doi() in 09_generate.R). This was a deliberate
    # simplification: EMLeditor::set_doi() (the function that would let a
    # regenerate reuse an existing DOI without creating a new draft) does
    # not accept a `dev` parameter, unlike set_datastore_doi() and
    # delete_inactive_ref() - calling it with dev = is_datastore_dev() (as
    # an earlier version of this app did) threw "unused argument". Rather
    # than maintain a second, narrower code path just to reuse a DOI
    # between regenerates, every generate now simply always creates fresh.
    existing <- doi_state()
    confirmation <- build_doi_confirmation(existing)

    shiny::showModal(shiny::modalDialog(
      title = confirmation$title,
      confirmation$message,
      footer = shiny::tagList(
        shiny::modalButton("Cancel"),
        shiny::actionButton("confirm_create_doi", "Create draft & generate", class = "btn-primary")
      )
    ))
  })

  shiny::observeEvent(input$confirm_create_doi, {
    shiny::removeModal()
    do_generate(create_new_doi = TRUE)
  })
}
