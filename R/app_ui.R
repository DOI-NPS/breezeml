# app_ui.R v15
#
# v15: comments only, on the sidebar's initial state. open = "closed"
# below is the START SCREEN's default, which is where the app opens.
# From v21 of app_server.R the default on every other screen is "open",
# and a choice the user makes with the About control overrides both and
# follows them from screen to screen. All of that is applied at runtime
# by go_to_screen(); this declaration only sets what the first painted
# screen looks like.
#
# v14: comments only. The blank "About" slide-out was a CSS fault, not a
# markup fault - a rule in breezeml.css pinned `transform` on bslib's
# collapse chevron for the start screen, and bslib only un-hides the
# sidebar's content from a transitionend listener on that chevron. The
# accordion below, including open = "overview", was correct throughout;
# it is left exactly as it was.
#
# v13: checks that inst/app/www/wind-mark.svg is present alongside the
# stylesheet and the arrowhead, since the start screen's wordmark mark is
# now served from that file (start_screen.R v9) rather than drawn inline.
# A missing asset stops the app with a clear message instead of showing a
# broken image.
#
# v12: pressing "About" now shows readable text. The accordion's Overview
# panel is EXPANDED in the markup (open = "overview") instead of every
# panel starting collapsed (open = FALSE). The sliding sidebar therefore
# carries visible text with it the moment it opens, with no server round
# trip and no dependence on bslib reporting the sidebar's state - which
# is why the v11 approach (an observer on input$app_sidebar calling
# accordion_panel_open()) did not work here.
#
# v11: the sidebar accordion has an id ("app_about") so that a panel can
# be opened from the server, and the sidebar itself has an id
# ("app_sidebar") and starts closed, so the app opens on the start screen
# with the sidebar tucked away. about_toggle_script() is included once; it
# labels bslib's sidebar collapse control "About".
#
# (The v10 and v11 changes were made in earlier steps but this header was
# not bumped at the time - it still read v9. Corrected here; the file's
# contents were current.)
#
# v9: added the "revise_package" screen to the app_screen navset
# (revise_screen_panel() in start_screen.R v4). Start-screen option c,
# "Revise an existing data package", now opens that screen - a package
# folder selected on this computer, or a draft downloaded from DataStore -
# instead of the single-.xml import screen, which now serves only option
# d. No other change.
#
# v8: the orange "Re-load on Tab 3 to finish restoring: ..." banner is
# removed from the sidebar (uiOutput "load_session_status"). The pending
# file names now appear in the green result message on the .json and .xml
# screens (app_server.R v13), with a link to the data-table tab. Also
# includes relabel_progress_script() once, so Shiny's file-box progress
# text reads "Import complete" instead of "Upload complete" everywhere.
#
# v7: the sidebar "Import / Restore Session" card (JSON restore, XML
# import, and the startup_mode radio) is replaced by a START SCREEN.
#   - The main area is now one hidden navset (id "app_screen") with four
#     screens: "start" (six options), "load_json" (.json upload),
#     "import_xml" (.xml upload), and "workflow" (the existing numbered
#     tabs, still the navset with id "main_panel"). The three new
#     screens are built by start_screen_panel(), load_session_panel() and
#     import_eml_panel() in start_screen.R.
#   - The sidebar and the branded header are unchanged and show on every
#     screen, including the start screen. The sidebar still holds the
#     information accordion. Its top card is now a slim "Session" card
#     (Save Progress and Return to start screen) that appears only once the
#     user is in the numbered tabs. (The orange "Re-load on Tab 3" banner
#     that v7 kept in the sidebar was removed in v8.)
#   - The numbered tabs are unchanged. Their code is NOT re-indented
#     inside the new wrapper, so this file's diff stays small.
#   - The input ids startup_mode, and the two sidebar file boxes'
#     placement, are gone; load_session and import_eml are still the file
#     input ids, now created by the server (app_server.R v12).
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
  wind_mark_file <- file.path(asset_dir, "wind-mark.svg")

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

  # The start screen's wind-and-leaf mark (start_screen.R).
  if (!file.exists(wind_mark_file)) {
    stop(
      paste0(
        "Could not locate the breezeml wind-and-leaf mark: ",
        wind_mark_file
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

    # "Upload complete" -> "Import complete" on every file box (v8).
    relabel_progress_script(),

    # Labels bslib's sidebar collapse control "About" instead of a bare
    # chevron, and records which screen is showing so the stylesheet can
    # place that control at the lower left of the start screen (v10).
    about_toggle_script(),

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

      # id: required so the server can open and close this sidebar
      # (app_server.R, set_sidebar()).
      #
      # open = "closed": the app opens on the start screen, and the
      # start screen's default is a closed sidebar, so the hero is
      # uncluttered and the About pill invites a first press. This is
      # the FIRST PAINT only.
      #
      # Thereafter app_server.R's go_to_screen() decides, and its rule
      # is different: the default is "open" on every screen except the
      # start screen, and a choice the user has made with the About
      # control beats the default on every screen from then on. So this
      # value must not be read as "the sidebar is closed on the start
      # screen" - returning to the start screen with the sidebar open
      # leaves it open.
      #
      # The collapse control bslib draws for a closed sidebar is a bare
      # chevron. breezeml.css gives it the visible label "About" and
      # about_toggle_script() gives it a matching tooltip, so the
      # accordion below is reached by a named control rather than an
      # arrow. There is no second button: this is that control. From
      # v15 it is drawn as the same pill at the lower left on every
      # screen, so it does not move as the user navigates.
      id = "app_sidebar",
      open = "closed",

      # ---- Session (only once the user is in the numbered tabs) ----
      #
      # The start screen replaced the old Import / Restore card. Save
      # Progress is still available from every numbered tab, and the
      # button below returns to the start screen (nothing the user has
      # entered is cleared by going back).
      shiny::conditionalPanel(
        condition = "input.app_screen === 'workflow'",
        bslib::card(
          class = "nps-session-card",
          bslib::card_header("Session"),

          shiny::downloadButton(
            "save_session",
            "Save Progress",
            class = "btn-outline-primary btn-sm w-100"
          ),

          shiny::tags$div(style = "height: 8px;"),

          shiny::actionButton(
            "back_to_start",
            "Return to start screen",
            icon = shiny::icon("house"),
            class = "btn-outline-secondary btn-sm w-100"
          )
        )
      ),

      # --- End Session ---


      bslib::accordion(
        class = "nps-info-accordion",

        # id: lets the server open a panel in this accordion
        # (app_server.R). Kept, but it is no longer what makes About
        # work; see `open` below.
        id = "app_about",

        # open = "overview": the Overview panel is expanded in the markup
        # that is sent to the browser. This is what makes the "About"
        # control useful - the sidebar is closed at startup, so opening it
        # slides out a panel that already has text in it.
        #
        # This is correct and was not the reason the slide-out appeared
        # empty in v12 and v13. That was a stylesheet rule pinning the
        # transform of bslib's collapse chevron on the start screen,
        # which stopped bslib from ever clearing the .transitioning class
        # that hides .sidebar-content. See the note in breezeml.css.
        #
        # This was `open = FALSE` in v11, which left every panel
        # collapsed: the sidebar slid out showing six headings and
        # nothing to read. An observer was added in app_server.R v19 to
        # open this panel when the sidebar opened, but it depends on
        # bslib reporting the sidebar's state through
        # input$app_sidebar, which did not happen here. Expanding the
        # panel in the markup needs no round trip and no such input, so
        # it works regardless of the installed bslib version.
        #
        # multiple defaults to TRUE, so the user can still open Help or
        # any other panel without this one closing, and can collapse
        # this one by hand.
        open = "overview",

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
    # One hidden navset switches between the start screen, the two file
    # screens, and "workflow", which holds the numbered tabs (the
    # navset_underline below). The numbered tabs are intentionally not
    # re-indented, to keep the diff reviewable.
    bslib::navset_hidden(
      id = "app_screen",
      selected = "start",
      start_screen_panel(),
      load_session_panel(),
      revise_screen_panel(),
      import_eml_panel(),
      bslib::nav_panel_hidden(
        "workflow",
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
      ) # end "workflow" screen
    ) # end app_screen navset
    # --- End main section ---
  )
}
