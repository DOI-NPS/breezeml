peopleInput <- function(id) {
  layout_columns(
    card(
      card_header("Authors (email)"),
      textInput(NS(id, "authors"),
                placeholder = "Enter an email address ending in nps.gov",
                label = NULL,
                width = "100%",
                updateOn = "blur"),
      helpText("Authors must be individuals (not organizations) and must ",
               "have ORCIDs. See NPS IMD guidance on ",
               a("best practices for authorship",
               href = paste0("https://doimspp.sharepoint.com/sites/nps-nrss",
                             "-imdiv/data-publication/SitePages/Data-package",
                             "-authorship.aspx?csf=1&web=1&e=ll809Q"),
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
      ),
      textOutput("validated_authors"),
    ),
    # Small script to toggle collapse
    tags$script(HTML("
    $('#author_help').on('click', function() {
      $('#author_help_section').collapse('toggle');
    });
  ")),
    card(
      card_header("Contacts"),
      textInput(NS(id, "contacts"),
                label = NULL,
                width = "100%",
                updateOn = "blur"),
      helpText('Contacts must be NPS employees or partners and should be ',
               'familiar with all aspects of the data package. Contacts are ',
               'almost always one or more of the authors. Consider "contacts"',
               ' similar to "corresponding author" in journal publications.')
    ),
    card(
      card_header("Contributers"),
      textInput(NS(id, "contacts"), 
                label = NULL, 
                width = "100%",
                updateOn = "blur"),
      helpText("Contributors are personell who did not rise to the level of ",
               "authorship but should still be acknowledged. Each contributor ",
               "requires a role. Roles are open format and can be anything ",
               "you choose (e.g. Field Assistant).")
    ),
    card(
      card_header("Editors"),
      textInput(NS(id, "contacts"),
                label = NULL,
                width = "100%",
                updateOn = "blur"),
      helpText("Editors will have the ability to make reasonable updates to ",
               "the DataStore reference such as fixing typos or updating ",
               "permissions. Only editors can access a reference when it is ",
               "in draft status so include potential data package revieres ",
               "as editors (they can be removed later). Editors must be NPS ",
               "employees or partners.")
    ),
    col_widths = c(-2, 8, -2), fill = FALSE
  )
}

peopleServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      output$validated_authors <- renderText({
        #require input before proceeding with output:
        req(input$authors)
        
        #test metadata filename for special characters
        if (grepl("[]:/?#@\\!\\$&'()*+,;=%[]", input$metadata_name)) {
          validate(paste0("The metadata file name cannot contain special ",
                          "characters other than \"_\""))
        }
        #generate tentative metadata file name output
        paste0("Your metadata filename will be: ",
               input$metadata_name, 
               "_metadata.xml")})
    })
  })
}    