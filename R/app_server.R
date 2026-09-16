# app_server.R v1
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
    pm <- permissions()
    oc <- org_context()
    c(
      if (!isTRUE(hl$valid)) hl$errors,
      if (!isTRUE(pp$valid)) pp$errors,
      if (!isTRUE(tb$valid)) tb$errors,
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
      high_level(), people(), tables(), fields(), geography(), taxonomy()
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
        fields_state = fields(),
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
    # effect - always confirm before creating or replacing one, even if
    # this is the very first generate this session.
    existing <- doi_state()

    if (is.null(existing)) {
      confirmation <- build_doi_confirmation(NULL)
      shiny::showModal(shiny::modalDialog(
        title = confirmation$title,
        confirmation$message,
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("confirm_create_doi", "Create draft & generate", class = "btn-primary")
        )
      ))
    } else {
      # a draft already exists - let the user choose to just regenerate
      # locally (reusing the existing DOI, no DataStore call at all) or
      # explicitly replace the draft
      shiny::showModal(shiny::modalDialog(
        title = "Regenerate metadata",
        paste0("You already have a draft reference (ID: ", existing$reference_id,
               ", DOI: ", existing$doi, "). How would you like to proceed?"),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("confirm_reuse_doi", "Regenerate (keep existing draft)", class = "btn-secondary"),
          shiny::actionButton("confirm_replace_doi", "Replace draft reference", class = "btn-danger")
        )
      ))
    }
  })

  shiny::observeEvent(input$confirm_create_doi, {
    shiny::removeModal()
    do_generate(create_new_doi = TRUE)
  })

  shiny::observeEvent(input$confirm_replace_doi, {
    shiny::removeModal()
    do_generate(create_new_doi = TRUE)
  })

  shiny::observeEvent(input$confirm_reuse_doi, {
    shiny::removeModal()
    do_generate(create_new_doi = FALSE)
  })
}
