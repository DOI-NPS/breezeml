# app_ui.R v1
#
# The top-level Shiny UI, converted from app.R's top-level `ui <- ...`
# script variable into a function - required for packaging, since a
# package's R/ files must contain only function/object definitions
# evaluated at load time, not code that builds UI immediately (building
# the UI at package-load time, rather than at app-launch time via
# run_breezeml(), would be wrong - e.g. getwd() at load time would not
# reflect the user's actual working directory when they later launch the
# app).

#' Build the bReezEML application UI
#' @keywords internal
app_ui <- function() {
  bslib::page_sidebar(

    theme = bslib::bs_theme(preset = "lumen"),

    title = "bReezEML",

    # ---- Sidebar ----
    sidebar = bslib::sidebar(
      width = "25%",
      title = "About this app",
      bslib::accordion(open = FALSE,
                       bslib::accordion_panel("Overview",
                                              shiny::helpText("bReezEML is a grahical interface tool for creating",
                                                              shiny::a("Ecological Metadata Language",
                                                                       href = "https://eml.ecoinformatics.org/",
                                                                       target = "_blank"),
                                                              " metadata using the",
                                                              shiny::a("NPSdataverse R packages",
                                                                       href = "https://nationalparkservice.github.io/NPSdataverse/",
                                                                       target = "_blank"),
                                                              ". bReezEML is maintained by the ",
                                                              shiny::a("National Park Service",
                                                                       href = "",
                                                                       target = "_blank"),
                                                              " and is designed to help create data packages to ",
                                                              "uploaded to the NPS designated science repository,",
                                                              shiny::a("DataStore",
                                                                       href="https://irma.nps.gov/DataStore/",
                                                                       target = "_blank"),
                                                              ". For detailed information on NPS data package specifications ",
                                                              "and construction, see",
                                                              shiny::a("the NPS data publication best practices SharePoint site",
                                                                       href = paste0("https://doimspp.sharepoint.com/sites/nps-",
                                                                                     "nrss-imdiv/data-publication"),
                                                                       target = "_blank"),
                                                              ".")
                       ),
                       bslib::accordion_panel("Help",
                                              shiny::helpText("For additional information on constructing ",
                                                              "data packages, see the NPS best practices for ",
                                                              "data publication ",
                                                              shiny::a("SharePoint site",
                                                                       href = paste0("https://doimspp.sharepoint.",
                                                                                     "com/sites/nps-nrss-imdiv/data-publication"),
                                                                       target = "_blank"),
                                                              ".",
                                                              shiny::br(),
                                                              shiny::br(),
                                                              "Maintainers: ",
                                                              shiny::br(),
                                                              shiny::a("sarah_wright@nps.gov",
                                                                       href = "mailto:sarah_wright@nps.gov"),
                                                              shiny::br(),
                                                              shiny::a("robert_baker@nps.gov",
                                                                       href = "mailto:robert_baker@nps.gov")
                                              )
                       ),
                       bslib::accordion_panel("Cite bReezEML",
                                              shiny::helpText("Baker et al. (2025). NPSdataverse: a suite of R packages for",
                                                              " data processing, authoring Ecological Metadata Language ",
                                                              "metadata, checking data-metadata congruence, and ",
                                                              "accessing data. Journal of Open Source Software, 10(109),",
                                                              " 8066, ",
                                                              shiny::a("https://doi.org/10.21105/joss.08066",
                                                                       href = "https://doi.org/10.21105/joss.08066",
                                                                       target = "_blank")
                                              )
                       ),
                       bslib::accordion_panel("Issues",
                                              shiny::helpText("Please use github for all ",
                                                              shiny::a("issues",
                                                                       href = paste0("https://github.com/nationalparkservice/",
                                                                                     "shinyEML/issues"),
                                                                       target = "_blank"),
                                                              "."
                                              )
                       ),
                       bslib::accordion_panel("Source Code",
                                              shiny::helpText("Source available on ",
                                                              shiny::a("GitHub.com",
                                                                       href = "https://github.com/nationalparkservice/shinyEML",
                                                                       target = "_blank"),
                                                              ".")
                       ),
                       bslib::accordion_panel("License",
                                              shiny::helpText("bReezEML is released under a ",
                                                              shiny::a("CC0",
                                                                       href = "https://creativecommons.org/public-domain/cc0/",
                                                                       target = "_blank"),
                                                              "license with no rights reserved.")
                       )
      )
    ),
    # --- End sidebar ---

    # ---- Main section ----
    bslib::navset_underline(
      id = "main_panel",

      ## ---- Tab 1: High-level info ----
      bslib::nav_panel("1. High-level info",
                       highLevelInput("high_level")
      ),
      # --- End Tab 1 ---

      ## ---- Tab 2: People ----
      bslib::nav_panel("2. People",
                       peopleInput("people")
      ),
      # --- End tab 2 ---

      ## ---- Tab 3: Data tables ----
      bslib::nav_panel("3. Data tables",
                       tableMetadataUI("table_metadata")
      ),
      # --- End tab 3 ---

      ## ---- Tab 4: Fields ----
      bslib::nav_panel("4. Fields",
                       fieldsUI("fields")
      ),
      # --- End tab 4 ---

      ## ---- Tab 5: Geography ----
      bslib::nav_panel("5. Geography",
                       geographyUI("geography")
      ),
      # --- End tab 5 ---

      ## ---- Tab 6: Taxonomy ----
      bslib::nav_panel("6. Taxonomy",
                       taxonomyUI("taxonomy")
      ),
      # --- End tab 6 ---

      ## ---- Tab 7: Permissions ----
      bslib::nav_panel("7. Permissions & Rights",
                       permissionsUI("permissions")
      ),
      # --- End tab 7 ---

      ## ---- Tab 8: Org context ----
      bslib::nav_panel("8. Units & Project",
                       org_context_ui("org_context")
      ),
      # --- End tab 8 ---

      ## ---- Tab 9: Generate ----
      bslib::nav_panel("9. Generate",
                       bslib::layout_columns(
                         bslib::card(
                           bslib::card_header("Generate EML"),
                           shiny::uiOutput("generate_status"),
                           shiny::textInput("working_folder", "Parent output folder (existing, writable directory)",
                                            value = getwd(), width = "100%"),
                           shiny::helpText("A subfolder named after the metadata filename (Tab 1) will be ",
                                           "created here, containing:"),
                           shiny::tags$ul(
                             shiny::tags$li(shiny::HTML("<code>data_package/</code> - data files + the final .xml (ready for DataStore)")),
                             shiny::tags$li(shiny::HTML("<code>data_package_creation/</code> - the generation script and all .txt templates"))
                           ),
                           bslib::layout_columns(
                             shiny::actionButton("preview_script", "Preview script", class = "btn-outline-secondary", width = "100%"),
                             shiny::actionButton("generate_script", "Generate EML", class = "btn-success", width = "100%"),
                             col_widths = c(6, 6)
                           ),
                           shiny::uiOutput("generation_result")
                         ),
                         col_widths = c(-2, 8, -2), fill = FALSE
                       )
      )
      # --- End tab 9 ---
    )
    # --- End main section ---
  )
}
