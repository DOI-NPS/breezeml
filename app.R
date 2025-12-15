library(shiny)
library(NPSdataverse)
library(bslib)

ui <- page_sidebar(
  
  theme = bs_theme(preset = "lumen"),  # bootstrap theme, lots to choose from
  
  title = "BreezEML",
  
  # ---- Sidebar ----
  sidebar = sidebar(
    width = "25%",
    title = "About this app",
    accordion(
      accordion_panel("About",
        helpText("bReezEML is a Tool for creating",
             "Ecological Metadata Language metadata. bReezEML servers as a ",
             "wrapper for the series of packages and functions in the",
             a("NPSdataverse R packages",
               href = "https://nationalparkservice.github.io/NPSdataverse/",
               target = "_blank"),
             ". bReezEML is maintained by the National Park Service and is ",
             "specifically designed to help create data packages to be ",
             "uploaded to the NPS designated science repository,",
             a("DataStore",
               href="https://irma.nps.gov/DataStore/",
               target = "_blank"),
             ".")
      ),
      accordion_panel("Cite bReezEML",
        helpText("Please cite the Journal of Open Source Software",
             a("publication",
               href = "https://doi.org/10.21105/joss.08066",
               target = "_blank"),
             " associated with the NPSdataverse: ",
             br(),
             br(),
             "Baker et al. (2025). NPSdataverse: a suite of R packages for ",
             "data processing, authoring Ecological Metadata Language ",
             "metadata, checking data-metadata congruence, and ",
             "accessing data. Journal of Open Source Software, 10(109),",
             " 8066, ",
             a("https://doi.org/10.21105/joss.08066",
               href = "https://doi.org/10.21105/joss.08066",
               target = "_blank")
             )
      ),
      accordion_panel("Help",
                      helpText("Contacts:",
                               br(),
                               "Maintainers: ",
                               a("sarah_wright@nps.gov",
                                 href = "mailto:sarah_wright@nps.gov"),
                               br(),
                               a("robert_baker@nps.gov",
                                 href = "mailto:robert_baker@nps.gov")
                               )
                      )
      ),
      accordion_panel("Issues",
                    helpText("Please use github for all ",
                             a("issues",
                               href = "https://github.com/nationalparkservice/shinyEML/issues"),
                             "."
                             )
                    ),
      accordion_panel("Source Code",
                    helpText("Source code can be found on ",
                             a("GitHub.com",
                               href = "https://github.com/nationalparkservice/shinyEML"),
                             ".")
    )
  ),
  # --- End sidebar ---
  
  # ---- Main section ----
  navset_underline(
    id = "main_panel",
    
    ## ---- Tab 1: High-level info ----
    nav_panel("1. High-level info",
              highLevelInput("high_level"),  # call the UI module for high level metadata
              verbatimTextOutput("module_test")  # for testing/demo
    ),
    # --- End Tab 1 ---
    
    ## ---- Tab 2: People ----
    nav_panel("2. People"),
    # --- End tab 2 ---
    
    ## ---- Tab 3: Data tables ----
    nav_panel("3. Data tables",
              tableMetadataUI("table_metadata")
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