library(shiny)
library(NPSdataverse)
library(bslib)

ui <- page_sidebar(
  
  theme = bs_theme(preset = "lumen"),  # bootstrap theme, lots to choose from
  
  title = "BreezEML",
  
  # ---- Sidebar ----
  sidebar = sidebar(
    width = "25%",
    title = "About this app"
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
    nav_panel("2. People", 
              id = "tab_people"),
    # --- End tab 2 ---
    
    ## ---- Tab 3: Data tables ----
    nav_panel("3. Data tables", 
              id = "tab_data_tables"),
    # --- End tab 3 ---
    
    ## ---- Tab 4: Field metadata ----
    nav_menu("Fields",
             
    )
    # --- End tab 4 ---
  )
  # --- End main section ---
)

# ---- Server ----
server <- function(input, output, session) {
  data <- highLevelServer("high_level")  # Call the server module that returns high-level metadata
  output$module_test <- renderPrint(data()$title)  # for testing/demo
}

shinyApp(ui, server)