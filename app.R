library(shiny)
library(NPSdataverse)
library(bslib)
library(shinyBS)
#sure would be nice to get rid fo this dependency at some point
library(DSbulkUploadR)

# Explicit sourcing in dependency order. Shiny's automatic sourcing of
# files in the R/ subfolder (via shiny::loadSupport()) is alphabetical,
# which silently breaks when a module (e.g. 05_geography.R) depends on a
# shared helper (e.g. card_pick_cols_from_table.R) that happens to sort
# after it. Sourcing explicitly here avoids relying on filename ordering.
source("R/card_pick_cols_from_table.R")  # shared sub-module - must load first
source("R/utils.R")
source("R/01_high_level_info.R")
source("R/02_people.R")
source("R/03_data_tables.R")
source("R/04_fields.R")
source("R/05_geography.R")
source("R/06_taxonomy.R")
source("R/07_generate.R")
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
    
    ## ---- Tab 7: Generate ----
    nav_panel("7. Generate",
              layout_columns(
                card(
                  card_header("Generate EML"),
                  uiOutput("generate_status"),
                  textInput("working_folder", "Working folder (existing, writable directory)",
                            value = getwd(), width = "100%"),
                  helpText("Data files, .txt templates, and the final .xml will be ",
                           "written here. The .xml will be named ",
                           HTML("<code>&lt;metadata filename&gt;_metadata.xml</code>"),
                           "."),
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
    # --- End tab 7 ---
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
    c(
      if (!isTRUE(hl$valid)) hl$errors,
      if (!isTRUE(pp$valid)) pp$errors,
      if (!isTRUE(tb$valid)) tb$errors
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
  
  observeEvent(input$generate_script, {
    errs <- all_errors()
    if (length(errs) > 0) {
      showNotification("Cannot generate: required information is missing. See the list above.",
                       type = "error")
      return(invisible(NULL))
    }
    
    wf <- input$working_folder
    if (!nzchar(wf) || !dir.exists(wf)) {
      showNotification("Working folder does not exist or is not accessible.", type = "error")
      return(invisible(NULL))
    }
    
    withProgress(message = "Generating EML...", value = 0.3, {
      result <- run_generation(
        working_folder = wf,
        high_level_state = high_level(),
        people_state = people(),
        tables_state = tables(),
        fields_state = fields(),
        geo_state = geography(),
        taxonomy_state = taxonomy()
      )
      incProgress(0.7)
      
      output$generation_result <- renderUI({
        if (isTRUE(result$success)) {
          tagList(
            tags$div(class = "text-success mt-2", result$message),
            tags$div(class = "text-muted", paste0("Written to: ", result$xml_path))
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
  })
}

shinyApp(ui, server)