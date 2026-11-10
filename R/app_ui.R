# app_ui.R v6
#
# v6: added Phase 1 National Park Service visual styling:
#   - approved, unmodified NPS arrowhead artwork in the application header
#   - NPS-inspired brown, green, black, sandstone, and warm-neutral palette
#   - modern card, tab, button, form, sidebar, and focus styling
#   - responsive branded header and improved visual hierarchy
#   - packaged browser assets loaded from inst/app/www
#
# Also retains the unified Import / Restore Session card:
#   - JSON session restore
#   - revise an existing draft from EML XML
#   - create a new package from EML XML
#   - Save Progress remains available in every startup mode
#   - the pending Tab 3 upload banner is shared by JSON and XML workflows
#
# v5: added Tab 11 "Upload and Publish" (uploadPublishUI("upload_publish"))
# after the Congruence checks tab. This is the final step: upload the
# generated data package to its DataStore draft reference, draft a
# reviewer-invite email, and (self-certified) activate/publish the
# reference. See 11_upload_publish.R and app_server.R (uploadPublishServer
# wiring - passed the last generated package directory, doi_state, and the
# package title). Placed last because it can only run AFTER Generate
# (needs the finalized data_package/ directory) and, logically, after the
# Congruence checks have been reviewed.
#
# v4: added Tab 10 "Congruence checks" (congruenceUI("congruence")) after
# the Generate tab. This tab runs DPchecker's congruence checks against
# the data_package/ directory a successful Generate produces - it is
# purely informational and placed last because it can only run AFTER
# Generate (it needs the finalized .xml). See 10_congruence.R and
# app_server.R v7 (last_package_dir + congruenceServer wiring).
#
# The top-level Shiny UI, converted from app.R's top-level `ui <- ...`
# script variable into a function - required for packaging, since a
# package's R/ files must contain only function/object definitions
# evaluated at load time, not code that builds UI immediately (building
# the UI at package-load time, rather than at app-launch time via
# run_breezeml(), would be wrong - e.g. getwd() at load time would not
# reflect the user's actual working directory when they later launch the
# app).
#
# v2: added a "Session" card at the top of the sidebar (Save Progress /
# Load Progress) - see app_server.R for the corresponding
# downloadHandler()/observeEvent() and save_load.R for the underlying
# save_app_state()/load_app_state() logic. Placed above the existing
# "About this app" accordion so it's the first thing visible in the
# sidebar, and reachable from any tab without navigating away.
#
# v3: removed the sidebar's "About this app" title (no longer accurate
# now that the sidebar also holds Session save/load, not just app info).
# fileInput's placeholder text changed from generic "No file selected"
# to "*.json" so users know what file type is expected before browsing.

#' Build the breezeml application UI
#' @keywords internal

app_ui <- function() {
  # Locate packaged browser assets in both installed-package and
  # devtools::load_all() development contexts.
  asset_dir <- system.file("app/www", package = "breezeml")

  if (!nzchar(asset_dir) || !dir.exists(asset_dir)) {
    development_asset_dir <- file.path("inst", "app", "www")

    if (dir.exists(development_asset_dir)) {
      asset_dir <- normalizePath(
        development_asset_dir,
        winslash = "/",
        mustWork = TRUE
      )
    }
  }

  if (!nzchar(asset_dir) || !dir.exists(asset_dir)) {
    stop(
      paste0(
        "Could not locate breezeml web assets. Expected ",
        "inst/app/www in the source tree or app/www in the ",
        "installed package."
      )
    )
  }

  css_file <- file.path(asset_dir, "breezeml.css")
  arrowhead_file <- file.path(asset_dir, "nps-arrowhead.png")

  if (!file.exists(css_file)) {
    stop(
      paste0(
        "Could not locate the breezeml stylesheet: ",
        css_file
      )
    )
  }

  if (!file.exists(arrowhead_file)) {
    stop(
      paste0(
        "Could not locate the NPS arrowhead artwork: ",
        arrowhead_file
      )
    )
  }

  shiny::addResourcePath(
    "breezeml-assets",
    asset_dir
  )

  branded_title <- shiny::tagList(
    # Embed the stylesheet in the generated page rather than relying on
    # a separate link in page_sidebar(header = ...). This works reliably
    # in both installed-package and devtools::load_all() launches.
    shiny::includeCSS(css_file),

    shiny::tags$div(
      class = "nps-brand",

      shiny::tags$img(
        class = "nps-brand__arrowhead",
        src = "breezeml-assets/nps-arrowhead.png",
        alt = "National Park Service arrowhead",

        # Critical fallback dimensions are inline. Even if custom CSS
        # later fails to load, the full-resolution artwork can never
        # expand to cover or displace the application.
        style = paste0(
          "display: block;",
          "height: 68px;",
          "width: auto;",
          "max-width: 58px;",
          "object-fit: contain;",
          "flex: 0 0 auto;"
        )
      ),

      shiny::tags$div(
        class = "nps-brand__text",

        shiny::tags$div(
          class = "nps-brand__agency",
          "National Park Service"
        ),

        shiny::tags$div(
          class = "nps-brand__product",
          "breezeml"
        ),

        shiny::tags$div(
          class = "nps-brand__tagline",
          "Ecological metadata authoring for DataStore"
        )
      )
    )
  )

  bslib::page_sidebar(
    theme = bslib::bs_theme(
      version = 5,
      bg = "#f5f1e8",
      fg = "#252219",
      primary = "#4b3621",
      secondary = "#4f633c",
      success = "#466437",
      warning = "#d8a638",
      danger = "#8b3f2f",
      info = "#4c6f7a"
    ),

    title = branded_title,

    # ---- Sidebar ----
    sidebar = bslib::sidebar(
      width = "25%",

      # ---- Start / Restore Session ----
      bslib::card(
        class = "nps-session-card",
        bslib::card_header("Import / Restore Session"),

        # Saving is always available once a user is working in the app,
        # regardless of whether they started from JSON, imported XML, or
        # entered metadata manually.
        shiny::downloadButton(
          "save_session",
          "Save Progress",
          class = "btn-outline-primary btn-sm w-100"
        ),

        shiny::tags$div(style = "height: 12px;"),

        shiny::radioButtons(
          "startup_mode",
          "How would you like to begin?",
          choices = c(
            "Load progress from a breezeml session (.json)" =
              "load_session",
            "Revise an existing draft data package (reuse imported DOI/reference)" =
              "revise_draft",
            "Create a new data package from existing metadata and data (create a new DOI/reference)" =
              "new_package"
          ),
          selected = "load_session"
        ),

        # ---------- JSON restore ----------
        shiny::conditionalPanel(
          condition = "input.startup_mode == 'load_session'",
          shiny::fileInput(
            "load_session",
            "Load Progress",
            accept = ".json",
            width = "100%",
            buttonLabel = "Browse...",
            placeholder = "*.json"
          )
        ),

        # ---------- XML import ----------
        shiny::conditionalPanel(
          condition = "input.startup_mode != 'load_session'",
          shiny::fileInput(
            "import_eml",
            "Import Existing EML",
            accept = ".xml",
            width = "100%",
            buttonLabel = "Browse...",
            placeholder = "*.xml"
          ),
          shiny::uiOutput("import_eml_status"),

          # Replace Shiny's built-in wording so the widget matches the
          # action the user is performing.
          shiny::tags$script(
            shiny::HTML(
              "
              (function() {
                function relabelImportComplete() {
                  var progress = document.getElementById('import_eml_progress');
                  if (!progress) return;
                  var nodes = progress.querySelectorAll('*');
                  nodes.forEach(function(node) {
                    if (node.textContent && node.textContent.trim() === 'Upload complete') {
                      node.textContent = 'Import complete';
                    }
                  });
                  if (progress.textContent && progress.textContent.trim() === 'Upload complete') {
                    progress.textContent = 'Import complete';
                  }
                }

                var observer = new MutationObserver(relabelImportComplete);

                function attachObserver() {
                  var progress = document.getElementById('import_eml_progress');
                  if (!progress) {
                    window.setTimeout(attachObserver, 100);
                    return;
                  }
                  observer.observe(progress, {
                    childList: true,
                    subtree: true,
                    characterData: true
                  });
                  relabelImportComplete();
                }

                document.addEventListener('DOMContentLoaded', attachObserver);
                $(document).on('shiny:connected', attachObserver);
              })();
              "
            )
          )
        ),

        # Shared banner for BOTH JSON restores and XML imports.
        shiny::uiOutput("load_session_status")
      ),
      # --- End Start / Restore Session ---


      bslib::accordion(
        class = "nps-info-accordion",
        open = FALSE,

        bslib::accordion_panel(
          title = "Overview",
          icon = shiny::icon("map"),
          value = "overview",

          shiny::helpText(
            "breezeml is a graphical interface tool for creating",
            shiny::a(
              "Ecological Metadata Language",
              href = "https://eml.ecoinformatics.org/",
              target = "_blank"
            ),
            " metadata using the",
            shiny::a(
              "NPSdataverse R packages",
              href = "https://doi-nps.github.io/NPSdataverse/",
              target = "_blank"
            ),
            ". breezeml is maintained by the ",
            shiny::a(
              "National Park Service",
              href = "",
              target = "_blank"
            ),
            " and is designed to help create data packages to ",
            "upload to the NPS designated science repository,",
            shiny::a(
              "DataStore",
              href = "https://irma.nps.gov/DataStore/",
              target = "_blank"
            ),
            ". The breezeml application runs locally and does not require ",
            "uploading data to any non-FedRAMP-certified servers. It is ",
            "therefore suitable for working with sensitive data. For detailed ",
            "information on NPS data package specifications ",
            "and construction, see",
            shiny::a(
              "the NPS data publication best practices SharePoint site",
              href = paste0(
                "https://doimspp.sharepoint.com/sites/nps-",
                "nrss-imdiv/data-publication"
              ),
              target = "_blank"
            ),
            "."
          )
        ),

        bslib::accordion_panel(
          title = "Help",
          icon = shiny::icon("circle-question"),
          value = "help",

          shiny::helpText(
            "For additional information on constructing ",
            "data packages, see the NPS best practices for ",
            "data publication ",
            shiny::a(
              "SharePoint site",
              href = paste0(
                "https://doimspp.sharepoint.",
                "com/sites/nps-nrss-imdiv/data-publication"
              ),
              target = "_blank"
            ),
            ".",
            shiny::br(),
            shiny::br(),
            "Maintainers: ",
            shiny::br(),
            shiny::a(
              "robert_baker@nps.gov",
              href = "mailto:robert_baker@nps.gov"
            )
          )
        ),

        bslib::accordion_panel(
          title = "Cite breezeml",
          icon = shiny::icon("quote-left"),
          value = "cite-breezeml",

          shiny::helpText(
            "Baker et al. (2025). NPSdataverse: a suite of R packages for",
            " data processing, authoring Ecological Metadata Language ",
            "metadata, checking data-metadata congruence, and ",
            "accessing data. Journal of Open Source Software, 10(109),",
            " 8066, ",
            shiny::a(
              "https://doi.org/10.21105/joss.08066",
              href = "https://doi.org/10.21105/joss.08066",
              target = "_blank"
            )
          )
        ),

        bslib::accordion_panel(
          title = "Issues",
          icon = shiny::icon("circle-exclamation"),
          value = "issues",

          shiny::helpText(
            "Please use GitHub for all ",
            shiny::a(
              "issues",
              href = paste0(
                "https://github.com/doi-nps/",
                "breezeml/issues"
              ),
              target = "_blank"
            ),
            "."
          )
        ),

        bslib::accordion_panel(
          title = "Source Code",
          icon = shiny::icon("github"),
          value = "source-code",

          shiny::helpText(
            "Source available on ",
            shiny::a(
              "GitHub.com",
              href = "https://github.com/doi-nps/breezeml",
              target = "_blank"
            ),
            "."
          )
        ),

        bslib::accordion_panel(
          title = "License",
          icon = shiny::icon("scale-balanced"),
          value = "license",

          shiny::helpText(
            "breezeml is released under a ",
            shiny::a(
              "CC0",
              href = "https://creativecommons.org/public-domain/cc0/",
              target = "_blank"
            ),
            " license with no rights reserved."
          )
        )
      )
      ),

  #),
  # --- End sidebar ---

    # ---- Main section ----
    bslib::navset_underline(
      id = "main_panel",

      ## ---- Tab 1: High-level info ----
      bslib::nav_panel(
        "1. High-level info",
        highLevelInput("high_level")
      ),

      ## ---- Tab 2: People ----
      bslib::nav_panel(
        "2. People",
        peopleInput("people")
      ),

      ## ---- Tab 3: Data tables ----
      bslib::nav_panel(
        "3. Data tables",
        tableMetadataUI("table_metadata")
      ),

      ## ---- Tab 4: Fields ----
      bslib::nav_panel(
        "4. Fields",
        fieldsUI("fields")
      ),

      ## ---- Tab 5: Geography ----
      bslib::nav_panel(
        "5. Geography",
        geographyUI("geography")
      ),

      ## ---- Tab 6: Taxonomy ----
      bslib::nav_panel(
        "6. Taxonomy",
        taxonomyUI("taxonomy")
      ),

      ## ---- Tab 7: Permissions ----
      bslib::nav_panel(
        "7. Permissions & Rights",
        permissionsUI("permissions")
      ),

      ## ---- Tab 8: Org context ----
      bslib::nav_panel(
        "8. Units & Project",
        org_context_ui("org_context")
      ),

      ## ---- Tab 9: Generate ----
      bslib::nav_panel(
        "9. Generate",
        bslib::layout_columns(
          bslib::card(
            bslib::card_header("Generate EML"),
            shiny::uiOutput("generate_status"),
            shiny::textInput(
              "working_folder",
              "Parent output folder (existing, writable directory)",
              value = getwd(),
              width = "100%"
            ),
            shiny::helpText(
              "A subfolder named after the metadata filename (Tab 1) will be ",
              "created here, containing:"
            ),
            shiny::tags$ul(
              shiny::tags$li(
                shiny::HTML(
                  "<code>data_package/</code> - data files + the final .xml (ready for DataStore)"
                )
              ),
              shiny::tags$li(
                shiny::HTML(
                  "<code>data_package_creation/</code> - the generation script and all .txt templates"
                )
              )
            ),
            bslib::layout_columns(
              shiny::actionButton(
                "preview_script",
                "Preview script",
                class = "btn-outline-secondary",
                width = "100%"
              ),
              shiny::actionButton(
                "generate_script",
                "Generate EML",
                class = "btn-success",
                width = "100%"
              ),
              col_widths = c(6, 6)
            ),
            shiny::uiOutput("generation_result")
          ),
          col_widths = c(-2, 8, -2),
          fill = FALSE
        )
      ),

      ## ---- Tab 10: Congruence checks ----
      bslib::nav_panel(
        "10. Congruence checks",
        congruenceUI("congruence")
      ),

      ## ---- Tab 11: Upload and Publish ----
      bslib::nav_panel(
        "11. Upload and Publish",
        uploadPublishUI("upload_publish")
      )
    )
    # --- End main section ---
  )
}
