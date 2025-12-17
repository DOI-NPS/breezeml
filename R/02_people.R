peopleInput <- function(id) {
  layout_columns(
    card(
      card_header("Authors"),
      textInput(NS(id, "authors"), label = NULL, width = "100%", updateOn = "blur"),
      helpText("Authors must be individuals (not organizations) and must ",
               "have ORCIDs. See NPS IMD guidance on ",
               a("best practices for authorship",
               href = "https://doimspp.sharepoint.com/sites/nps-nrss-imdiv/data-publication/SitePages/Data-package-authorship.aspx?csf=1&web=1&e=ll809Q",
               target = "_blank"),
               " for additional information."),
      actionButton("author_help", "Show/Hide Help", class = "btn-info btn-sm"),
      
      div(
        id = "author_help_section",
        class = "collapse mt-2",
        helpText("Debating show/hide help functionality. I like the cleaner, ",
                 "simpler look of hidden help text. But I also want people to ",
                 HTML("<b>freaking see</b>"),
                 " the help text!")
      )
    ),
    
    # Small script to toggle collapse
    tags$script(HTML("
    $('#author_help').on('click', function() {
      $('#author_help_section').collapse('toggle');
    });
  ")),
    
    card(
      card_header("Contacts"),
      textInput(NS(id, "contacts"), label = NULL, width = "100%", updateOn = "blur"),
      helpText("Contacts must be NPS employees or partners and should be ",
               "familiar with all aspects of the data package. Contacts are ",
               "almost always one or more authors. Consider `contacts` ",
               "similar to `corresponding author` in journal publications.")
    ),
    col_widths = c(-2, 8, -2), fill = FALSE
  )
}

peopleServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      list(title = input$title)
    })
  })
}    