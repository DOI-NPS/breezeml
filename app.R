library(shiny)
library(NPSdataverse)
library(bslib)
library(shinyBS)

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
              highLevelInput("high_level")  # call the UI module for high level metadata
    ),
    # --- End Tab 1 ---
    
    ## ---- Tab 2: People ----
    nav_panel("2. People",
            peopleInput("people")
    ),
    # --- End tab 2 ---
    
    ## ---- Tab 3: Data tables ----
    nav_panel("3. Data tables",
              tableMetadataUI("table_metadata"),
              verbatimTextOutput("module_test")  # for testing/demo
    ),
    # --- End tab 3 ---
    
    ## ---- Tab 4: Field metadata ----
    nav_menu("4. Fields",
             nav_panel("Example data table")
    )
    # --- End tab 4 ---
  )
  # --- End main section ---
)

# ---- Server ----
server <- function(input, output, session) {
  data <- highLevelServer("high_level")  # Call the server module that returns high-level metadata
  tables <- tableMetadataServer("table_metadata")
  output$module_test <- renderPrint(tables())  # for testing/demo
}

shinyApp(ui, server)