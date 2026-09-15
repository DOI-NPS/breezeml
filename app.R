library(shiny)
library(NPSdataverse)
library(bslib)
library(shinyBS)
#sure would be nice to get rid fo this dependency at some point
library(DSbulkUploadR)

# Fail fast and loud, rather than deep inside a Generate click, if the
# cui_2026 branch's set_permissions() isn't installed yet. See dependency
# note above for the install command.
if (!exists("set_permissions", where = asNamespace("EMLeditor"), inherits = FALSE)) {
  stop(
    "EMLeditor::set_permissions() was not found. This app requires the ",
    "cui_2026 development branch of EMLeditor, which has not yet been ",
    "merged to main. Install it with:\n\n",
    '  remotes::install_github("DOI-NPS/EMLeditor", ref = "cui_2026")\n\n',
    "Once cui_2026 merges to EMLeditor main, this check (and this ",
    "install instruction) can be removed - a normal NPSdataverse install ",
    "will be sufficient."
  )
}

# app.R v22
#
# DEPENDENCY NOTE: EMLeditor::set_permissions() (used in 08_permissions.R)
# is still in development and NOT YET MERGED to EMLeditor's main branch as
# of this writing. Install the dev branch explicitly:
#
#   remotes::install_github("DOI-NPS/EMLeditor", ref = "cui_2026")
#
# Once cui_2026 merges to main, this app can go back to installing
# EMLeditor normally (e.g. via remotes::install_github("doi-nps/NPSdataverse")
# per skeleton.Rmd) - remove this note and the pinned install at that point.

# IMPORTANT: runApp() on a directory containing an R/ folder automatically
# runs shiny::loadSupport(), which sources every file in R/ ALPHABETICALLY
# before app.R's own code executes - independent of and in addition to the
# explicit source() calls below. That automatic pass does not know our
# dependency order (e.g. it would try 05_geography.R, which needs
# pickColsUI(), before card_pick_cols_from_table.R, since "0" sorts before
# "c" alphabetically) and can error out before app.R's own sourcing ever
# runs. Setting shiny.autoload.r = FALSE disables that automatic pass so
# our explicit, correctly-ordered source() calls are the ONLY sourcing
# that happens.
options(shiny.autoload.r = FALSE)

source("R/card_pick_cols_from_table.R")  # shared sub-module - must load first
source("R/utils.R")
source("R/01_high_level_info.R")
source("R/02_people.R")
source("R/03_data_tables.R")
source("R/04_fields.R")
source("R/05_geography.R")
source("R/06_taxonomy.R")
source("R/07_generate.R")
source("R/08_permissions.R")
source("R/09_org_context.R")
source("R/card_field_metadata_disp.R")
source("R/card_field_metadata_entry.R")
source("R/card_nps_unit.R")
source("R/card_people.R")

ui <- page_sidebar(
  
  theme = bs_theme(preset = "lumen"),  # bootstrap theme, lots to choose from
  
  title = "bReezEML",
  
  # ---- Sidebar ----
  sidebar = sidebar(
    width = "25%",
    title = "About this app",
    accordion(open = FALSE,
              accordion_panel("Overview",
                              helpText("bReezEML is a grahical interface tool for creating",
                                       a("Ecological Metadata Language",
                                         href = "https://eml.ecoinformatics.org/",
                                         target = "_blank"),
                                       " metadata using the",
                                       a("NPSdataverse R packages",
                                         href = "https://nationalparkservice.github.io/NPSdataverse/",
                                         target = "_blank"),
                                       ". bReezEML is maintained by the ",
                                       a("National Park Service",
                                         href = "",
                                         target = "_blank"),
                                       " and is designed to help create data packages to ",
                                       "uploaded to the NPS designated science repository,",
                                       a("DataStore",
                                         href="https://irma.nps.gov/DataStore/",
                                         target = "_blank"),
                                       ". For detailed information on NPS data package specifications ",
                                       "and construction, see",
                                       a("the NPS data publication best practices SharePoint site",
                                         href = paste0("https://doimspp.sharepoint.com/sites/nps-",
                                                       "nrss-imdiv/data-publication"),
                                         target = "_blank"),
                                       ".")
              ),
              accordion_panel("Help",
                              helpText("For additional information on constructing ",
                                       "data packages, see the NPS best practices for ",
                                       "data publication ",
                                       a("SharePoint site",
                                         href = paste0("https://doimspp.sharepoint.",
                                                       "com/sites/nps-nrss-imdiv/data-publication"),
                                         target = "_blank"),
                                       ".",
                                       br(),
                                       br(),
                                       "Maintainers: ",
                                       br(),
                                       a("sarah_wright@nps.gov",
                                         href = "mailto:sarah_wright@nps.gov"),
                                       br(),
                                       a("robert_baker@nps.gov",
                                         href = "mailto:robert_baker@nps.gov")
                              )
              ),
              accordion_panel("Cite bReezEML",
                              helpText("Baker et al. (2025). NPSdataverse: a suite of R packages for",
                                       " data processing, authoring Ecological Metadata Language ",
                                       "metadata, checking data-metadata congruence, and ",
                                       "accessing data. Journal of Open Source Software, 10(109),",
                                       " 8066, ",
                                       a("https://doi.org/10.21105/joss.08066",
                                         href = "https://doi.org/10.21105/joss.08066",
                                         target = "_blank")
                              )
              ),
              accordion_panel("Issues",
                              helpText("Please use github for all ",
                                       a("issues",
                                         href = paste0("https://github.com/nationalparkservice/",
                                                       "shinyEML/issues"),
                                         target = "_blank"),
                                       "."
                              )
              ),
              accordion_panel("Source Code",
                              helpText("Source available on ",
                                       a("GitHub.com",
                                         href = "https://github.com/nationalparkservice/shinyEML",
                                         target = "_blank"),
                                       ".")
              ),
              accordion_panel("License",
                              helpText("bReezEML is released under a ",
                                       a("CC0",
                                         href = "https://creativecommons.org/public-domain/cc0/",
                                         target = "_blank"),
                                       "license with no rights reserved.")
              )
    )
  ),
  # --- End sidebar ---
  
  # ---- Main section ----
  navset_underline(
    id = "main_panel",
    
    ## ---- Tab 1: High-level info ----
    nav_panel("1. High-level info",
              highLevelInput("high_level")
    ),
    # --- End Tab 1 ---
    
    ## ---- Tab 2: People ----
    nav_panel("2. People",
              peopleInput("people")
    ),
    # --- End tab 2 ---
    
    ## ---- Tab 3: Data tables ----
    nav_panel("3. Data tables",
              tableMetadataUI("table_metadata")
    ),
    # --- End tab 3 ---
    
    ## ---- Tab 4: Fields ----
    nav_panel("4. Fields",
              fieldsUI("fields")
    ),
    # --- End tab 4 ---
    
    ## ---- Tab 5: Geography ----
    nav_panel("5. Geography",
              geographyUI("geography")
    ),
    # --- End tab 5 ---
    
    ## ---- Tab 6: Taxonomy ----
    nav_panel("6. Taxonomy",
              taxonomyUI("taxonomy")
    ),
    # --- End tab 6 ---
    
    ## ---- Tab 7: Permissions ----
    nav_panel("7. Permissions & Rights",
              permissionsUI("permissions")
    ),
    # --- End tab 7 ---
    
    ## ---- Tab 8: Org context ----
    nav_panel("8. Units & Project",
              org_context_ui("org_context")
    ),
    # --- End tab 8 ---
    
    ## ---- Tab 9: Generate ----
    nav_panel("9. Generate",
              layout_columns(
                card(
                  card_header("Generate EML"),
                  uiOutput("generate_status"),
                  textInput("working_folder", "Parent output folder (existing, writable directory)",
                            value = getwd(), width = "100%"),
                  helpText("A subfolder named after the metadata filename (Tab 1) will be ",
                           "created here, containing:"),
                  tags$ul(
                    tags$li(HTML("<code>data_package/</code> - data files + the final .xml (ready for DataStore)")),
                    tags$li(HTML("<code>data_package_creation/</code> - the generation script and all .txt templates"))
                  ),
                  layout_columns(
                    actionButton("preview_script", "Preview script", class = "btn-outline-secondary", width = "100%"),
                    actionButton("generate_script", "Generate EML", class = "btn-success", width = "100%"),
                    col_widths = c(6, 6)
                  ),
                  uiOutput("generation_result")
                ),
                col_widths = c(-2, 8, -2), fill = FALSE
              )
    )
    # --- End tab 9 ---
  )
  # --- End main section ---
)

# ---- Server ----
server <- function(input, output, session) {
  high_level <- highLevelServer("high_level")
  people <- peopleServer("people")
  tables <- tableMetadataServer("table_metadata")
  
  # shared reactive of just the parsed data.frames, keyed by file name -
  # this is what downstream modules (fields, geography, taxonomy) consume
  table_data <- reactive({
    tables()$data
  })
  
  fields <- fieldsServer("fields", table_data)
  geography <- geographyServer("geography", table_data)
  taxonomy <- taxonomyServer("taxonomy", table_data)
  permissions <- permissionsServer("permissions")
  org_context <- org_contextServer("org_context")
  
  # Tracks the most recent successfully-created DataStore draft reference
  # this session, if any. NULL until create_or_replace_doi() succeeds once.
  # Persists across regenerates so the same draft/DOI is reused rather than
  # silently creating duplicates.
  doi_state <- reactiveVal(NULL)
  
  # Collect every tab's validation errors in one place so "Generate" has a
  # single, clear gate rather than failing deep inside make_eml().
  all_errors <- reactive({
    hl <- high_level()
    pp <- people()
    c(
      if (!isTRUE(hl$valid)) hl$errors,
      if (!isTRUE(pp$valid)) pp$errors
    )
  })
  
  output$generate_status <- renderUI({
    errs <- all_errors()
    if (length(errs) == 0) {
      return(tags$div(class = "text-success", "All required information is complete. Ready to generate."))
    }
    tagList(
      tags$div(class = "text-danger", "Resolve the following before generating:"),
      tags$ul(lapply(errs, tags$li))
    )
  })
  
  # Collect every tab's validation errors in one place so "Generate" has a
  # single, clear gate rather than failing deep inside make_eml().
  all_errors <- reactive({
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
  
  output$generate_status <- renderUI({
    errs <- all_errors()
    if (length(errs) == 0) {
      return(tags$div(class = "text-success", "All required information is complete. Ready to generate."))
    }
    tagList(
      tags$div(class = "text-danger", "Resolve the following before generating:"),
      tags$ul(lapply(errs, tags$li))
    )
  })
  
  observeEvent(input$preview_script, {
    script <- build_generation_script(
      high_level(), people(), tables(), fields(), geography(), taxonomy()
    )
    showModal(modalDialog(
      title = "Generated script (preview)",
      tags$pre(script, style = "white-space: pre-wrap; max-height: 500px; overflow-y: auto;"),
      size = "l", easyClose = TRUE, footer = modalButton("Close")
    ))
  })
  
  output$generation_result <- renderUI({ NULL })
  
  # Actually runs the generation pipeline exactly once.
  # @param create_new_doi passed straight through to run_generation() -
  #   TRUE only right after the user has confirmed creating/replacing a
  #   DataStore draft reference; FALSE (the default) reuses whatever
  #   doi_state() already holds, if anything.
  do_generate <- function(create_new_doi = FALSE) {
    wf <- input$working_folder
    if (!nzchar(wf) || !dir.exists(wf)) {
      showNotification("Working folder does not exist or is not accessible.", type = "error")
      return(invisible(NULL))
    }
    
    withProgress(message = "Generating EML...", value = 0.3, {
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
      incProgress(0.7)
      
      if (isTRUE(result$success) && !is.null(result$doi_state)) {
        doi_state(result$doi_state)
      }
      
      output$generation_result <- renderUI({
        if (isTRUE(result$success)) {
          tagList(
            tags$div(class = "text-success mt-2", result$message),
            tags$div(class = "text-muted", paste0("Written to: ", result$xml_path)),
            if (!is.null(result$doi_state) && !is.na(result$doi_state$doi)) {
              tags$div(class = "text-muted", paste0("DataStore DOI: ", result$doi_state$doi))
            },
            if (!is.null(result$content_issues)) {
              tagList(
                tags$hr(),
                tags$details(
                  tags$summary(
                    style = "cursor: pointer; font-weight: 600;",
                    "Review notes from EMLassemblyline::issues() (click to expand)"
                  ),
                  tags$p(class = "text-muted mt-2",
                         "This always runs and often includes expected notes (e.g. no ",
                         "Principal Investigator listed, which NPS data packages don't ",
                         "require). Skim for anything indicating a field was skipped or ",
                         "not understood - if something you entered in Tabs 4-6 isn't ",
                         "reflected here, you can go fix it and generate again without ",
                         "losing any of your other entries."),
                  tags$pre(
                    style = "white-space: pre-wrap; max-height: 300px; overflow-y: auto;",
                    result$content_issues
                  )
                )
              )
            }
          )
        } else {
          tagList(
            tags$div(class = "text-danger mt-2", result$message),
            if (!is.null(result$validation_errors)) {
              tags$ul(lapply(result$validation_errors, tags$li))
            }
          )
        }
      })
    })
  }
  
  observeEvent(input$generate_script, {
    errs <- all_errors()
    if (length(errs) > 0) {
      showNotification("Cannot generate: required information is missing. See the list above.",
                       type = "error")
      return(invisible(NULL))
    }
    
    # DataStore draft reference creation is a real, non-idempotent side
    # effect - always confirm before creating or replacing one, even if
    # this is the very first generate this session. If a DOI already
    # exists and the user just wants to regenerate the .xml (e.g. after
    # fixing a Fields issue) WITHOUT touching DataStore, they should
    # cancel this dialog - the existing DOI is still reused correctly via
    # do_generate()'s default create_new_doi = FALSE... but since
    # generate_script always shows this modal, add a clear third choice
    # for "just regenerate, don't touch DataStore" when a DOI already exists.
    existing <- doi_state()
    
    if (is.null(existing)) {
      confirmation <- build_doi_confirmation(NULL)
      showModal(modalDialog(
        title = confirmation$title,
        confirmation$message,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_create_doi", "Create draft & generate", class = "btn-primary")
        )
      ))
    } else {
      # a draft already exists - let the user choose to just regenerate
      # locally (reusing the existing DOI, no DataStore call at all) or
      # explicitly replace the draft
      showModal(modalDialog(
        title = "Regenerate metadata",
        paste0("You already have a draft reference (ID: ", existing$reference_id,
               ", DOI: ", existing$doi, "). How would you like to proceed?"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_reuse_doi", "Regenerate (keep existing draft)", class = "btn-secondary"),
          actionButton("confirm_replace_doi", "Replace draft reference", class = "btn-danger")
        )
      ))
    }
  })
  
  observeEvent(input$confirm_create_doi, {
    removeModal()
    do_generate(create_new_doi = TRUE)
  })
  
  observeEvent(input$confirm_replace_doi, {
    removeModal()
    do_generate(create_new_doi = TRUE)
  })
  
  observeEvent(input$confirm_reuse_doi, {
    removeModal()
    do_generate(create_new_doi = FALSE)
  })
}

shinyApp(ui, server)