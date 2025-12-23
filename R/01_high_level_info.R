highLevelInput <- function(id) {
  layout_columns(
    card(
      card_header("Title"),
      textInput(NS(id, "title"), 
                label = NULL, 
                width = "100%", 
                updateOn = "blur"),
      helpText("A good title should address ",
               HTML("<b>What</b>"),
               " the data are, ",
               HTML("<b>Where</b>"),
               " they are from and ",
               HTML("<b>When</b>"),
               " they were collected. Avoid acronyms: spell out park and ",
               "network units."),
      actionButton("toggle_help", "Show/Hide Help", class = "btn-info btn-sm"),
      
      div(
        id = "help_section",
        class = "collapse mt-2",
        helpText("Debating show/hide help functionality. I like the cleaner, ",
                 "simpler look of hidden help text. But I also want people to ",
                 HTML("<b>freaking see</b>"),
                 " the help text!")
      )
    ),
    
    # Small script to toggle collapse
    tags$script(HTML("
    $('#toggle_help').on('click', function() {
      $('#help_section').collapse('toggle');
    });
  ")),
    
    card(
      card_header("Abstract"),
      textInput(NS(id, "abstract"),
                label = NULL,
                width = "100%",
                updateOn = "blur"),
      helpText("An abstract should allow a non-expert to understand ",
      HTML("<b>Why</b>"),
      " the study was conducted as well as ",
      HTML("<b>How</b>"),
      ", ",
      HTML("<b>Where</b>"),
      " and ",
      HTML("<b>When</b>"),
      " it was conducted as well as ",
      HTML("<b>What</b>"),
      " data were collected. An abstract is typically about 250 words.")
    ),
    card(
      card_header("Methods"),
      textInput(NS(id, "methods"),
                label = NULL,
                width = "100%",
                updateOn = "blur"),
      helpText("Methods should contain sufficient detail that an expert in ",
               "the field could repeat the study. The Methods likely require ",
               "citing references such as SOPs and Protocols; however, ",
               HTML("<b>only citing SOPs or Protocols is insufficient</b>"),
               ". Methods should include not only experimental design and ",
               "data collection protocols but also data quality assurance ",
               "and control methods as well as any deviations from the cited ",
               "resources.")
    ),
    card(
      card_header("Additional notes"),
      textInput(NS(id, "additional_notes"),
                label = NULL,
                width = "100%",
                updateOn = "blur"),
      helpText("Additional Notes includes any information that may be useful ",
               "to a data user that is not included elsewhere. If there are ",
               "citations in the Methods, Additional Notes is a great place ",
               "to include the full citations and URLs to cited resources.")
    ),
    col_widths = c(-2, 8, -2), fill = FALSE
  )
}

highLevelServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      list(title = input$title,
           abstract = input$abstract,
           methods = input$methods,
           additional_notes = input$addtional_notes)
    })
  })
}