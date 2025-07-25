library(shiny)
library(NPSdataverse)
library(rhandsontable)

ui <- fluidPage(
  titlePanel("bReezEML: metadata authoring made easy"),
  fileInput("upload",
           NULL,
           buttonLabel = "Add your .csv files... (max 30Mb)",
           multiple = TRUE,
           accept = (".csv")),
  DT::DTOutput("files"),
  tableOutput("selected_file_table"),
  
  downloadButton("download1"),
  downloadLink("download2"),
  
  #collect metadata file name text
  textAreaInput("metadata_name", 
                label = "Metadata file name", 
                value = "",
                width = "100%",
                rows = 1, 
                placeholder = paste0("Provide a filename for your metadata",
                                     " file. \"_metadata.xml\" will",
                                     " automatically be appended." )),
  #output metadata file name text
  textOutput("metadata_filename"),
  
  #textbox for abstract input:
  textAreaInput("abstract_input",
                label = "Abstract",
                value = "",
                width = "100%",
                rows = 10,
                placeholder = paste0("Please provide an abstract of about ",
                                     "250 words describing the ")),
  
  #write abstract to a temp .txt file:
  
  html_blocks <- c(
    paste0("Your abstract will be forwarded to data.gov, DataCite, google ",
           "dataset search, etc. so it is worth some time to carefully ",
           "consider what is relevant and important information for an ",
           "abstract. Abstracts must be greater than 20 words. Good abstracts ",
           "tend to be 250 words or less. You may consider including the ",
           "following information: The premise for the data collection ",
           "(why was it done?), why is it important, a brief overview of ",
           "relevant methods, and a brief explanation of what data are ",
           "included such as the period of time, location(s), and type of ",
           "data collected. Keep in mind that if you have lengthy ",
           "descriptions of methods, provenance, data QA/QC, etc it may be ",
           "better to expand upon these topics in a the methods section or ",
           "by providing links to a Data Release Report or similar document ",
           "uploaded separately to DataStore.")),
  
  #add in a line or so of whitespace:
  HTML(r"(<br></br>)"),
  
  textAreaInput("methods_input",
                label = "Methods",
                value = "",
                width = "100%",
                rows = 15,
                placeholder = "Please provide a brief synopsis of the methods"),
  
  html_blocks <- 
    paste0("Please provide a description of the methods used to generate the ",
           "published data. These should include experimental design, data ",
           "collection, and quality assurance and control procedures. These do ",
           "not need to be detailed enough to replicate the study but should ",
           "be sufficiently detailed that a knowledgeable person can ",
           "understand and use the data. You may wish to refer to an existing ",
           "protocol, SOP, script, or other publication."),
  
  HTML(r"(<br></br>)"),
  
  textAreaInput("notes_input",
                label = "Additional Information",
                value = "",
                width = "100%",
                rows = 1,
                placeholder = paste0("This optional text will become the ",
                                     "\"notes\" section on DataStore.")),
  
  textAreaInput("keywords_input",
                label = "Keywords",
                value = "",
                width = "100%",
                rows = 1,
                placeholder = "keywords"),
  
  radioButtons("rb", "Choose a License",
               choiceNames = list(
                 HTML("<a href = 'https://creativecommons.org/public-domain/cc0/'>CC0 (a great default choice!)</a>"),
                 HTML("<a href = 'https://creativecommons.org/public-domain/pdm/'>Public Domain (no license)</a>"),
                 "Restricted (no license)"),
               choiceValues = list(
                 "CC0", "pubdomain", "restricted"),
               selected = "CC0",
               inline = TRUE),
  
  html_blocks <- 
    paste0("Only references containing Confidential Unclassified Information (CUI) can use the \"restricted\" option. All other references must be CC0 or Public Domain)"),
  
  
  
  # radioButtons( 
  #   inputId = "license", 
  #   label = "Choose a License", 
  #   choiceNames = list( 
  #     "CC0 - Most people should select this option" = 1, 
  #     "CC-BY-4.0" = 2, 
  #     "CC-NY-NC 4.0" = 3,
  #     "Public Domain" = 4,
  #     "Unlicensed - Not for public dissemination" = 5,
  #     HTML("Shiny Website <a href ='https://www.rstudio.com/products/shiny/'>Link</a>") = 6),
  #   choiceValues = list("CC0",
  #                       "CC-BY-4.0",
  #                       "CC_NY_NC 4.0",
  #                       "Public Domain",
  #                       "Shiny website"),
  #   selected = "CC0"),
      
  textOutput("txt")
  
  #DF = data.frame(integer = 1:10,
  #                numeric = rnorm(10),
  #                logical = rep(TRUE, 10), 
  #                character = LETTERS[1:10],
  #                factor = factor(letters[1:10], levels = letters[10:1], 
  #                                ordered = TRUE),
  #                factor_allow = factor(letters[1:10], levels = letters[10:1], 
  #                                      ordered = TRUE),
  #                date = seq(from = Sys.Date(), by = "days", length.out = 10),
  #                stringsAsFactors = FALSE),
  
  #rhandsontable(DF, width = 600, height = 300) %>%
  #  hot_col("factor_allow", allowInvalid = TRUE)

)
  

server <- function(input, output, session) {
  # increase maximum file size to 30 MB
  options(shiny.maxRequestSize=30*1024^2)

  # check that files actually are .csv:
  data <- reactive({
    req(input$upload)
  
    ext <- tools::file_ext(input$upload$name)
    switch(ext,
           csv = vroom::vroom(input$upload$datapath, delim = ","),
           validate("Invalid file; Please upload a .csv or .tsv file")
    )
    
   
  })
    
  output$files <- DT::renderDataTable({
    DT::datatable(input$upload,
                  selection = c("single"),
                  colnames = c("File name",
                               "Size (bytes)",
                               "Type", "Path"))
  })
  
  #read all uploaded files
  all_files <- reactive({
    req(input$upload)
    purrr::map(input$upload$datapath, read_csv) %>%
      purrr::set_names(input$upload$name)
  })
    
  
  
  output$metadata_filename <- renderText({
    #require input before proceeding with output:
    req(input$metadata_name)
    
    #test metadata filename for special characters
    if (grepl("[]:/?#@\\!\\$&'()*+,;=%[]", input$metadata_name)) {
      validate(paste0("The metadata file name cannot contain special ",
                      "characters other than \"_\""))
    }
    #generate tentative metadata file name output
    paste0("Your metadata filename will be: ",
           input$metadata_name, 
           "_metadata.xml")})
  
  #write abstract text to temp .txt file:
  output$abstract_text <- renderPrint({
    # A temp file to save the output.
    
    abstract <- tempfile(fileext='.txt')
    
    # Generate the PNG
    txt(outfile, width=400, height=300)
    hist(rnorm(input$obs), main="Generated in renderImage()")
    dev.off()
  })
  
  
  
}

shinyApp(ui, server)



#,names(files) <- c("File name", "File size (MB)", "File type"))