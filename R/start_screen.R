# start_screen.R v11
#
# v11: the About control is now identical on every screen - same pill
# button, same place at the lower left (breezeml.css, no longer scoped
# to the start screen). What differs by screen is only the sidebar's
# DEFAULT state: closed on the start screen, open everywhere else.
#
# A user's own choice overrides that default and follows them from
# screen to screen. The choice can only be observed here, in the
# browser, because it is made by clicking bslib's control rather than a
# Shiny input: onAboutClick() reports it to the server as
# input$about_user_pref, and app_server.R's go_to_screen() prefers it
# over the per-screen default. See the long comment on onAboutClick().
#
# v10: the blank "About" slide-out is fixed. The cause was in
# breezeml.css, not here: a rule pinned `transform` on bslib's
# .collapse-icon for the start screen only, which is why the sidebar
# behaved on every other screen. bslib removes its .transitioning class -
# and that class is what sets `.sidebar-content { display: none }` -
# only from a transitionend listener on that icon, so an icon whose
# transform could not change never released the content. The rule is
# gone; see the long note in breezeml.css where it was.
#
# about_toggle_script() gains a safety net for the same failure: if the
# layout is still .transitioning 900 ms after an About click, the class
# is removed and aria-expanded corrected by hand. Nothing in the package
# needs it now, but the failure it guards against is silent and total,
# and a future CSS change could reintroduce it.
#
# v9: the wind-and-leaf mark is now the traced reference artwork, served
# as a file (inst/app/www/wind-mark.svg) instead of an inline SVG drawn
# by hand. Its outlines are the reference design's own silhouettes -
# three air strokes, two of them curling closed, and a leaf lifted clear
# at the upper right - reduced to cubic curves, with the gradients
# sampled from the same artwork. The hand-drawn versions in v1-v8 were
# approximations and did not look like it.
#
# v8: two wordmark corrections, both to match the reference design.
#   - The colour split is now "breez" (dark green) + "eml" (light green),
#     so the product name reads as "breez" + EML, the metadata standard.
#     v1-v7 split it "breeze" + "ml", which obscured that.
#   - The wind-and-leaf mark is redrawn: three tapering slate strokes
#     with curled ends and a single pointed leaf lifting off the top
#     stroke. The old mark was two flat lines and a round blob. Colours
#     are sampled from the reference: #8fb0bf / #7f9fb0 / #9dbccb for the
#     strokes and #84a266 for the leaf.
#
# v7: about_toggle_script() also opens the sidebar's Overview panel in
# the browser when the About control is pressed. It clicks the panel's
# own accordion button, which is what the user would click, so no Shiny
# message and no bslib version check is involved. Together with
# accordion(open = "overview") in app_ui.R v12 this means pressing About
# shows readable text whether or not the panel was collapsed by hand
# earlier in the session.
#
# v6: the six options are simplified to match the reference design
# (image-1.png): a round coloured icon, one word in capitals, and a short
# line under it. They are no longer button-shaped - each is a plain row
# separated from the next by a thin rule - so the panel reads as a list
# rather than a stack of six blue blocks. The long sentences and the two
# section headings are gone, and so is the "Return to start screen" hint,
# which the Session card in the sidebar already says. Same six input ids
# and the same behaviour; only the label markup changed.
#
# v5: the sidebar accordion is reached from a NAMED control instead of a
# bare chevron. bslib already draws a collapse control for a closed
# sidebar, so no second button was added: about_toggle_script() gives that
# control the accessible name and tooltip "About", breezeml.css gives it
# the visible word "About", and on the start screen the stylesheet moves
# it to the lower left. The script also records the current screen on
# <body data-breezeml-screen>, which is what lets the stylesheet treat the
# start screen differently from the numbered tabs. The sidebar itself
# starts closed (app_ui.R v10) and the server opens it on entering the
# numbered tabs (app_server.R v18).
#
# v4: the "Revise an existing data package" screen no longer asks for a
# metadata .xml file. A data package is a FOLDER (*_metadata.xml plus the
# .csv data files), so the screen now offers two ways to get one:
#
#   A. Select a folder on this computer, using the same operating-system
#      folder window as the Congruence tab (choose_directory_native()).
#   B. Download a DRAFT data package from DataStore by reference id. The
#      app checks the reference first (exists, is a data package, is in
#      DRAFT, and the user is an editor), then warns where files will be
#      written before anything is downloaded, then calls
#      NPSutils::get_data_package(secure = TRUE, force = TRUE). The checks
#      and the download live in datastore_download.R.
#
# Either way, breezeml reads the folder's *_metadata.xml with the existing
# importer and loads the folder's .csv files as data tables, so nothing
# has to be re-selected by hand. revise_screen_panel() builds this screen
# (value "revise_package").
#
# The OTHER .xml screen is unchanged and still single-file: option d,
# "Create a new data package from existing metadata", legitimately starts
# from one .xml with no data files, so import_eml_panel() stays as it was
# and now serves only that option.
#
# v3: wording. The word "upload" is no longer used for local files: the
# screens say "import" / "re-imported". The .json screen's help text now
# reads "...need your .csv files to be re-imported." and the XML screen's
# help text no longer points to Tab 3. The progress-bar relabelling script
# ("Upload complete" -> "Import complete") moved out of the XML screen into
# relabel_progress_script(), which app_ui.R includes once for the whole
# app, so it now also covers the .json screen and the data-table file
# boxes. (The start screen's last option, which sends a package to
# DataStore, still says "Upload and publish": that is a real upload to a
# remote server, not a local file import.)
#
# v2: the start screen is now a landscape "hero" (photograph background,
# breezeml wordmark and tagline, and a translucent panel holding the six
# options) instead of a plain card. Same six option ids and the same
# behavior as v1; only the layout, text and styling changed. The
# photograph is inst/app/www/start-landscape.jpg and the layout/styles are
# in the "Start screen" section of breezeml.css. The two upload screens
# are unchanged.
#
# v1: initial version.
#
# UI for the breezeml START SCREEN and the two file-upload screens that
# follow from it. These replace the old "Import / Restore Session" card
# that used to sit at the top of the sidebar (app_ui.R v6 and earlier).
#
# app_ui.R places these inside one hidden navset (id "app_screen"):
#
#   "start"        start_screen_panel()   the six options
#   "load_json"    load_session_panel()   .json upload (option b)
#   "import_xml"   import_eml_panel()     .xml upload (options c and d)
#   "workflow"     the existing numbered tabs (id "main_panel")
#
# The server side (the six option observers, the file boxes, and the
# status messages) is in app_server.R v12, in the block headed "Start
# screen and navigation". The option buttons are:
#
#   start_new           a) start a new EML metadata file   -> Tab 1
#   start_load_json     b) load a breezeml session (.json) -> load_json screen
#   start_revise        c) revise an existing data package  -> import_xml
#                          screen, keeping the DOI/reference id
#   start_new_from_xml  d) new data package from existing   -> import_xml
#                          metadata, creating a new DOI/reference
#   start_review        e) review or check a data package   -> Tab 10
#   start_upload        f) upload and publish               -> Tab 11
#
# The file boxes (load_session, import_eml) are created by the server with
# renderUI so each visit to a screen starts with an empty box: the uiOutput
# placeholders below are load_session_input and import_eml_input. The ids
# of the file inputs themselves are unchanged from the sidebar version.

#' One option on the start screen. A round coloured icon, one word in
#' capitals, and a short line under it, in the manner of the reference
#' design: a plain row rather than a button-shaped block. Styled by
#' .nps-start-option in breezeml.css.
#'
#' @param id      input id (unchanged from v1)
#' @param icon_name Font Awesome name
#' @param word    the action, one word, shown in capitals
#' @param line    one short line; no trailing sentences
#' @param tone    "forest", "slate" or "sage" - the icon's colour, which
#'                follows the reference design's alternating circles
#' @noRd
start_option_button <- function(id, icon_name, word, line,
                                tone = "forest") {
  shiny::actionButton(
    id,
    label = shiny::tags$span(
      class = "nps-start-option__text",
      shiny::tags$span(class = "nps-start-option__word", word),
      shiny::tags$span(class = "nps-start-option__line", line)
    ),
    icon = shiny::icon(icon_name),
    class = paste0("nps-start-option nps-start-option--", tone)
  )
}

#' The start screen: a landscape "hero" with the breezeml wordmark and
#' tagline on the left and a translucent panel of the six options on the
#' right (layout and styling: "Start screen" section of breezeml.css). The
#' photograph is a CSS background (inst/app/www/start-landscape.jpg) and is
#' decorative; if it is missing the hero falls back to a plain pale
#' gradient and everything still works.
#' @noRd
start_screen_panel <- function() {
  # Decorative wind-and-leaf mark next to the wordmark: three strokes of
  # moving air with a leaf lifted clear of them. This is the reference
  # design's own artwork, traced to cubic curves and served from
  # inst/app/www/wind-mark.svg (see app_ui.R, which registers that
  # directory as "breezeml-assets" and checks the file is present).
  #
  # An <img> rather than inline SVG, so the markup stays readable and the
  # browser caches the artwork. alt = "" with aria-hidden, because the
  # wordmark beside it already names the application, so a screen reader
  # announcing the mark would only repeat it.
  wind_mark <- shiny::tags$img(
    class = "nps-hero__mark",
    src = "breezeml-assets/wind-mark.svg",
    alt = "",
    `aria-hidden` = "true",

    # Fallback dimensions, in case the stylesheet does not load. The
    # artwork's viewBox is 220 x 170.
    width = "220",
    height = "170"
  )

  bslib::nav_panel_hidden(
    value = "start",
    shiny::tags$div(
      class = "nps-hero",

      # ---- Left: wordmark and tagline over the sky ----
      shiny::tags$div(
        class = "nps-hero__intro",
        shiny::tags$h1(
          class = "nps-hero__wordmark",
          # "breez" + "eml": the light-green half is the metadata
          # standard the app writes, so the split falls after the z.
          shiny::tags$span(class = "nps-hero__wordmark-a", "breez"),
          shiny::tags$span(class = "nps-hero__wordmark-b", "eml"),
          wind_mark
        ),
        shiny::tags$p(
          class = "nps-hero__tagline",
          "Create, revise, check, and publish Ecological Metadata Language ",
          "(.xml) files and data packages to DataStore."
        )
      ),

      # ---- Right: the six options ----
      shiny::tags$div(
        class = "nps-hero__panel",
        role = "group",
        `aria-label` = "What would you like to do?",
        shiny::tags$h2(class = "nps-hero__panel-title",
                       "What would you like to do?"),

        start_option_button(
          "start_new", "file-code",
          "CREATE", "Build a new EML .xml file.",
          tone = "forest"
        ),
        start_option_button(
          "start_load_json", "clock-rotate-left",
          "RESUME", "Continue a saved breezeml session.",
          tone = "slate"
        ),
        start_option_button(
          "start_revise", "pencil",
          "REVISE", "Edit an existing data package.",
          tone = "sage"
        ),
        start_option_button(
          "start_new_from_xml", "copy",
          "REUSE", "Start from existing metadata.",
          tone = "slate"
        ),
        start_option_button(
          "start_review", "circle-check",
          "CHECK", "Validate a data package.",
          tone = "sage"
        ),
        start_option_button(
          "start_upload", "cloud-arrow-up",
          "PUBLISH", "Publish a data package to DataStore.",
          tone = "forest"
        )
      )
    )
  )
}

#' Screen for option b: choose a saved session (.json).
#' @noRd
load_session_panel <- function() {
  bslib::nav_panel_hidden(
    value = "load_json",
    bslib::layout_columns(
      bslib::card(
        bslib::card_header("Load progress from a previous breezeml session"),
        shiny::helpText(
          "Choose a .json file that you saved earlier with ",
          shiny::tags$strong("Save Progress"),
          ". Your entries are restored. Data tables, fields, geography and ",
          "taxonomy also need your .csv files to be re-imported."
        ),
        shiny::uiOutput("load_session_input"),
        shiny::uiOutput("load_session_result"),
        shiny::tags$div(
          class = "d-flex flex-wrap gap-2 mt-2",
          shiny::actionButton("back_from_json", "Back",
                              class = "btn-outline-secondary",
                              icon = shiny::icon("arrow-left")),
          shiny::uiOutput("load_session_continue", inline = TRUE)
        )
      ),
      col_widths = c(-2, 8, -2), fill = FALSE
    )
  )
}

#' Screen for option c: revise an existing data package. The user either
#' selects the package folder on this computer or downloads a draft
#' package from DataStore. Most of this screen is server-rendered, because
#' which half is shown depends on the choice made here
#' (revise_source) and on what the checks found - see the "Revise an
#' existing data package" block in app_server.R.
#' @noRd
revise_screen_panel <- function() {
  bslib::nav_panel_hidden(
    value = "revise_package",
    bslib::layout_columns(
      bslib::card(
        bslib::card_header("Revise an existing data package"),
        shiny::helpText(
          "Choose the folder that holds the data package you want to revise: ",
          "its metadata .xml and its .csv data files. Its DataStore DOI and ",
          "reference id are kept and reused, so generating updates that same ",
          "draft reference."
        ),

        shiny::radioButtons(
          "revise_source",
          "Where is the data package?",
          choices = c(
            "On this computer" = "local",
            "On DataStore (download a draft)" = "datastore"
          ),
          selected = "local"
        ),

        # ---- A. folder on this computer ----
        shiny::conditionalPanel(
          condition = "input.revise_source == 'local'",
          shiny::uiOutput("revise_folder_ui")
        ),

        # ---- B. download from DataStore ----
        shiny::conditionalPanel(
          condition = "input.revise_source == 'datastore'",
          shiny::uiOutput("revise_datastore_ui")
        ),

        # Shared: what was found in the folder, and the outcome.
        shiny::uiOutput("revise_package_summary"),
        shiny::uiOutput("revise_result"),

        shiny::tags$div(
          class = "d-flex flex-wrap gap-2 mt-2",
          shiny::actionButton("back_from_revise", "Back",
                              class = "btn-outline-secondary",
                              icon = shiny::icon("arrow-left")),
          shiny::uiOutput("revise_load_button", inline = TRUE),
          shiny::uiOutput("revise_continue", inline = TRUE)
        )
      ),
      col_widths = c(-2, 8, -2), fill = FALSE
    )
  )
}

#' Screen for option d: "Create a new data package from existing
#' metadata" - start from one metadata .xml, with no data files and no
#' DOI reuse. (Option c has its own screen: revise_screen_panel() above.)
#' The heading and explanation are still server-rendered
#' (xml_screen_title, xml_screen_intro).
#' @noRd
import_eml_panel <- function() {
  bslib::nav_panel_hidden(
    value = "import_xml",
    bslib::layout_columns(
      bslib::card(
        bslib::card_header(shiny::textOutput("xml_screen_title", inline = TRUE)),
        shiny::uiOutput("xml_screen_intro"),
        shiny::uiOutput("import_eml_input"),
        shiny::uiOutput("import_eml_status"),

        shiny::tags$div(
          class = "d-flex flex-wrap gap-2 mt-2",
          shiny::actionButton("back_from_xml", "Back",
                              class = "btn-outline-secondary",
                              icon = shiny::icon("arrow-left")),
          shiny::uiOutput("import_continue", inline = TRUE)
        )
      ),
      col_widths = c(-2, 8, -2), fill = FALSE
    )
  )
}

#' Page-wide script that changes Shiny's built-in file-box progress text
#' "Upload complete" to "Import complete". breezeml only ever reads files
#' from the user's own computer, so "upload" is the wrong word for them.
#' Included once by app_ui.R. It watches the whole page, so it covers every
#' file box, including ones the server re-creates (the .json and .xml
#' screens) and the data-table file box on Tab 3.
#' @noRd
relabel_progress_script <- function() {
  shiny::tags$script(
    shiny::HTML(
      "
      (function() {
        function relabelImportComplete() {
          document.querySelectorAll('.shiny-file-input-progress .progress-bar')
            .forEach(function(bar) {
              if (bar.textContent && bar.textContent.trim() === 'Upload complete') {
                bar.textContent = 'Import complete';
              }
            });
        }
        new MutationObserver(relabelImportComplete).observe(
          document.documentElement,
          { childList: true, subtree: true, characterData: true }
        );
      })();
      "
    )
  )
}


#' Labels bslib's sidebar collapse control "About", opens the sidebar's
#' Overview panel when it is pressed, and records the current screen on
#' the <body> element.
#'
#' The sidebar accordion (Overview, Help, Cite breezeml, Issues, Source
#' Code, License) is the app's About material. bslib already draws a
#' control that opens the closed sidebar, but it is an unlabelled chevron
#' with the tooltip "Toggle sidebar", which does not say what is behind
#' it. Rather than add a second button that would do the same thing, this
#' script renames the control bslib already provides:
#'
#'   * title      -> "About breezeml" (the tooltip)
#'   * aria-label -> "About breezeml" (what a screen reader announces)
#'
#' The visible word "About" is added by breezeml.css, which also moves the
#' control to the lower left while the start screen is showing.
#'
#' SHOWING THE TEXT. The Overview panel is expanded in the markup
#' (app_ui.R: accordion(open = "overview")), so the sidebar carries
#' visible text with it when it opens. As a second measure, pressing the
#' About control here also expands Overview if it happens to be
#' collapsed, by clicking that panel's own accordion button - exactly
#' what a user would click. That uses Bootstrap's own collapse
#' behaviour, so it needs no Shiny message and does not depend on the
#' installed bslib reporting the sidebar's state. The click is skipped
#' when the sidebar is already open, so pressing About to CLOSE the
#' sidebar does not expand anything.
#'
#' The script also mirrors the active screen of the hidden navset onto
#' <body data-breezeml-screen>, because CSS cannot otherwise tell which
#' screen is showing. A MutationObserver keeps the labels and that
#' attribute up to date, since bslib rewrites the control's attributes
#' when the sidebar opens or closes, and Shiny swaps the navset's panels
#' after the page loads.
#' @noRd
about_toggle_script <- function() {
  shiny::tags$script(
    shiny::HTML(
      "
      (function() {
        var ABOUT = 'About breezeml';

        function labelToggles() {
          document.querySelectorAll('.collapse-toggle').forEach(
            function(btn) {
              if (btn.getAttribute('title') !== ABOUT) {
                btn.setAttribute('title', ABOUT);
              }
              if (btn.getAttribute('aria-label') !== ABOUT) {
                btn.setAttribute('aria-label', ABOUT);
              }
              if (!btn.dataset.breezemlAbout) {
                btn.dataset.breezemlAbout = '1';
                btn.addEventListener('click', onAboutClick);
              }
            }
          );
        }

        // Pressing About while the sidebar is CLOSED is a request to see
        // the About material, so make sure the Overview panel is
        // expanded. aria-expanded is read before bslib handles the
        // click, so 'false' means the sidebar is about to open.
        function onAboutClick(ev) {
          // Runs whichever way the sidebar is moving: a stuck
          // .transitioning class breaks closing as well as opening.
          setTimeout(releaseStuckSidebar, 900);

          var opening = ev.currentTarget.getAttribute(
            'aria-expanded') === 'false';

          reportPreference(opening);

          if (!opening) { return; }
          setTimeout(openOverview, 0);
        }

        // Tell the server which state the user chose, so it carries to
        // the next screen.
        //
        // Why this is reported from the browser. The sidebar's default
        // state is per-screen (closed on the start screen, open
        // elsewhere) and is applied by the server, which calls
        // bslib::toggle_sidebar() as it switches screens. But the user
        // changes that state by clicking bslib's own control, which is
        // not a Shiny input, and the state input bslib offers for a
        // sidebar is not arriving in this app - that was the dead end in
        // app_server.R v19. So the click is observed here and sent up
        // explicitly.
        //
        // priority 'event' matters: without it Shiny drops a repeat of
        // the same value, and the user may well choose 'open' twice in a
        // row with a server-applied default in between.
        //
        // Only a real click reaches this function. The server's own
        // toggle_sidebar() calls do not, so applying a default never
        // looks like a user preference.
        function reportPreference(isOpening) {
          if (typeof Shiny === 'undefined' || !Shiny.setInputValue) {
            return;
          }
          Shiny.setInputValue(
            'about_user_pref',
            isOpening ? 'open' : 'closed',
            { priority: 'event' }
          );
        }

        // Expand the Overview panel by clicking its own accordion
        // button, which is what a user would do. Bootstrap handles the
        // animation. A button that is already expanded is left alone, so
        // this never collapses an open panel.
        function openOverview() {
          var acc = document.getElementById('app_about');
          if (!acc) { return; }
          var item = acc.querySelector('.accordion-item[data-value=\"overview\"]');
          if (!item) { item = acc.querySelector('.accordion-item'); }
          if (!item) { return; }
          var btn = item.querySelector('.accordion-button');
          if (btn && btn.getAttribute('aria-expanded') !== 'true') {
            btn.click();
          }
        }

        // Mirror the hidden navset's active screen onto the body as
        // data-breezeml-screen.
        //
        // NOTE: as of v11 no rule in breezeml.css reads this attribute.
        // It existed so the stylesheet could place the About control
        // differently on the start screen, and that difference is gone -
        // the control is the same pill in the same place everywhere. The
        // attribute is kept because it is the only way a stylesheet can
        // tell which screen of the hidden navset is showing, and it
        // costs nothing; if a future rule needs to style one screen, the
        // hook is already here. Remove this function and its calls if
        // that never happens.
        function markScreen() {
          var panes = document.querySelectorAll('.tab-pane.active');
          var screen = '';
          panes.forEach(function(pane) {
            var id = pane.getAttribute('data-value') || pane.id || '';
            if (id.indexOf('start') >= 0) { screen = 'start'; }
            else if (id.indexOf('load_json') >= 0 && !screen) {
              screen = 'load_json';
            }
            else if (id.indexOf('revise_package') >= 0 && !screen) {
              screen = 'revise_package';
            }
            else if (id.indexOf('import_xml') >= 0 && !screen) {
              screen = 'import_xml';
            }
            else if (id.indexOf('workflow') >= 0 && !screen) {
              screen = 'workflow';
            }
          });
          if (screen && document.body.getAttribute(
                'data-breezeml-screen') !== screen) {
            document.body.setAttribute('data-breezeml-screen', screen);
          }
        }

        // Safety net for a silent bslib failure mode. bslib hides
        // .sidebar-content while the layout carries .transitioning, and
        // clears that class only from a 'transitionend' listener on the
        // collapse chevron. If the chevron's transform cannot change -
        // for instance because some stylesheet pins it - the event never
        // fires, the class sticks, and the sidebar opens empty. That was
        // the 'About shows nothing' bug (breezeml.css, start-screen
        // .collapse-icon rule, removed). Nothing in the package pins it
        // now, so this should never run; it costs one timer per click.
        function releaseStuckSidebar() {
          document.querySelectorAll(
            '.bslib-sidebar-layout.transitioning').forEach(
            function(layout) {
              layout.classList.remove('transitioning');
              var sb = layout.querySelector(':scope > .sidebar');
              var tg = layout.querySelector(':scope > .collapse-toggle');
              var closed = layout.classList.contains('sidebar-collapsed');
              if (sb) { sb.hidden = closed; }
              if (tg) {
                tg.setAttribute('aria-expanded', closed ? 'false' : 'true');
              }
            }
          );
        }

        function update() { labelToggles(); markScreen(); }

        if (document.readyState === 'loading') {
          document.addEventListener('DOMContentLoaded', update);
        } else {
          update();
        }

        new MutationObserver(update).observe(
          document.documentElement,
          { childList: true, subtree: true, attributes: true,
            attributeFilter: ['class', 'title', 'aria-label', 'hidden'] }
        );
      })();
      "
    )
  )
}
