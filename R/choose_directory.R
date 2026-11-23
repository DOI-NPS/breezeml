# choose_directory.R v6
#
# v6 - THE FOLDER WINDOW IS NOW RAISED IN FRONT OF THE BROWSER
#
# Users reported that the folder window often opens BEHIND the browser
# and other windows, so a click on "Browse" looks like it did nothing
# until the window is found on the taskbar. v5's own header already
# recorded the symptom as known and unaddressed: "On Windows the window
# can open behind the browser."
#
# WHY IT HAPPENS. A native folder dialog is raised relative to its OWNER
# (Windows) or PARENT (Tk's own term) window. Given none, it is owned by
# nothing, and the window manager is free to leave it wherever the
# stacking order puts it - which, for a dialog opened by a background R
# process while the user is looking at a browser, is usually behind the
# browser. The R process that opens it is not the foreground application,
# so nothing claims the front for it.
#
# Tk's documentation for tk_chooseDirectory states the remedy directly:
#
#   -parent window
#       Makes window the logical parent of the dialog. The dialog
#       is displayed on top of its parent window. On macOS, this
#       turns the file dialog into a sheet attached to the parent
#       window.
#
# WHAT IS FIXED, AND WHAT CANNOT BE. Only some of the methods in the
# chain accept any instruction about stacking:
#
#   tcltk       FIXED. A tiny always-on-top Tk window is created, the
#               dialog is given it as -parent, and it is destroyed
#               afterwards. See .tk_front_window() below. This is the
#               method that matters most in practice, because v5 made it
#               FIRST on Windows under RStudio, which is how most users
#               run breezeml.
#   osascript   FIXED, with a fallback. The AppleScript now runs inside
#               a tell block that activates System Events first, so the
#               dialog belongs to an application that has just come to
#               the front. If that call fails for any reason other than
#               the user cancelling - most plausibly because sending
#               Apple events to System Events has not been authorised on
#               that Mac - the plain unactivated script is run once as a
#               fallback, exactly as v5 ran it. The dialog never
#               appeared in that case, so the fallback cannot produce a
#               second window.
#   choose.dir  NOT FIXABLE HERE. utils::choose.dir() takes only
#               `default` and `caption`; it exposes no owner, parent or
#               topmost argument, so there is no supported way to raise
#               it from R. It is hosted by the Rgui front end, and under
#               Rgui that front end is normally the window the user just
#               clicked in. Left alone.
#   zenity      NOT CHANGED. zenity has no option to attach a dialog to
#               a window it was not given, and kdialog's --attach wants
#               an X11 window id for the browser, which an R process has
#               no reliable way to discover. Raising these would mean
#               guessing at a window id, which is exactly the kind of
#               fragile trick that does not belong in a package. GNOME
#               and KDE normally present a dialog from a running
#               application in front in any case.
#   rstudioapi  NOT CHANGED. RStudio draws this dialog itself, as part
#               of the IDE window, so the IDE's own stacking applies.
#
# NOTHING IN THE CHAIN'S LOGIC CHANGED. The method order, the
# unreliable-cancel rule, the NO_WINDOW_SECONDS heuristic, the returned
# list and $attempts are all exactly as in v5, and no caller needs
# editing: 10_congruence.R, 11_upload_publish.R and app_server.R all
# call choose_directory_native() with the same two arguments and read
# the same fields.
#
# EVERY NEW CALL IS GUARDED AND REVERSIBLE. The helper window is built
# inside tryCatch(); if any part of it fails, the dialog is opened
# exactly as v5 opened it, unparented. A user therefore never loses the
# ability to choose a folder because of this change - at worst the
# window appears where it did before.
#
# v5 - THE ORDER NOW DEPENDS ON R'S FRONT END, AND choose.dir() IS LAST
#      UNDER RSTUDIO
#
# Testing on Windows 11 / R 4.6.1 / RStudio: utils::choose.dir() drew no
# window and returned NA, but took LONGER than NO_WINDOW_SECONDS to do
# it, so v4's timing heuristic believed the cancel, stopped the chain,
# and never reached Tcl/Tk - even though capabilities("tcltk") was TRUE
# and tk_choose.dir() would have worked. The button therefore still did
# nothing. The diagnostic that showed this:
#
#   $gui "RStudio"   $methods_in_order choose.dir, tcltk, rstudioapi
#   $attempts  choose.dir = "cancelled"   (chain stopped here)
#
# choose.dir() returning NA with no window inside the RStudio IDE, while
# working in Rgui on the same R installation, is a known and reported
# behaviour: the dialog needs the Rgui front end to host it. R's own
# documentation for choose.dir() says NA means Cancel, so a caller cannot
# tell "cancelled" from "never drew anything" by its return value alone.
#
# Two changes, so the chain no longer depends on timing:
#
#   1. THE METHOD ORDER IS CHOSEN BY FRONT END. On Windows under RStudio
#      the order is tcltk, rstudioapi, choose.dir - Tcl/Tk first because
#      it draws its own window and does not need a particular front end.
#      Under Rgui, choose.dir() stays first, since there it is the real
#      native dialog and works properly.
#
#   2. A CANCEL FROM A METHOD KNOWN TO BE UNRELIABLE IN THIS FRONT END IS
#      NEVER BELIEVED. .unreliable_cancel_methods() names them
#      (choose.dir under RStudio), and their cancel is recorded as
#      "no_window" whatever it timed, so the chain continues. The timing
#      heuristic is kept as a backstop for everything else.
#
# v4: DESCRIPTION now lists rstudioapi under Imports, so it is installed
# with breezeml and its folder dialog is available to every user rather
# than only to those who happened to have it already. tcltk stays under
# Suggests because it CANNOT be installed: it is a base package, built
# into R itself when R was compiled with Tcl/Tk support, and there is no
# version of it on CRAN to install. Listing it in Imports would therefore
# not give it to a user whose R lacks it - it would only make breezeml
# fail to install there. The requireNamespace() guards below stay for
# that reason, and remain the right pattern for a base package that may
# be absent.
#
# Opens the operating system's own "select a folder" window so a user can
# pick a directory with the mouse instead of typing a path. Used by the
# Congruence tab (10_congruence.R), the Upload and Publish tab
# (11_upload_publish.R) and the revise-a-data-package screen
# (app_server.R).
#
# v3 - NO SHELL SCRIPTS ON WINDOWS
#
# v2 reached for a PowerShell script when utils::choose.dir() drew no
# window. That is withdrawn. Generating and running a PowerShell command
# from an R package is hard to review, is the kind of thing endpoint
# security tools are built to flag, and buys nothing on macOS or Linux,
# where PowerShell is normally absent. v3 uses only folder dialogs that
# are part of R itself or part of the desktop the user already runs.
#
# METHODS BY PLATFORM, TRIED IN ORDER
#
#   Windows  choose.dir   utils::choose.dir(), the Windows tree-style
#                         dialog. Needs the Rgui front end to host it:
#                         inside RStudio it returns NA without drawing
#                         anything, which is what testing found. It is
#                         therefore FIRST under Rgui and LAST under
#                         RStudio.
#            tcltk        tcltk::tk_choose.dir(). Part of R on Windows
#                         (R for Windows ships with Tcl/Tk), opens its own
#                         window, and does not need R's GUI front end. It
#                         is therefore the one most likely to work where
#                         choose.dir() does not.
#            rstudioapi   rstudioapi::selectDirectory(), when the app was
#                         started from RStudio. RStudio draws the native
#                         dialog itself. Installed with breezeml (it is
#                         under Imports), but still only usable when the
#                         session really is inside RStudio, which
#                         rstudioapi::isAvailable() reports. Last in the
#                         chain because an IDE call from inside a running
#                         Shiny app is the least predictable of the
#                         three.
#   macOS    osascript    the Finder "choose folder" dialog. Part of
#                         macOS; no extra package.
#            tcltk, rstudioapi as above. (R's Tcl/Tk on macOS may need
#                         XQuartz, which is why osascript comes first.)
#   Linux    zenity       GTK folder dialog, usual on GNOME.
#            kdialog      KDE folder dialog.
#            tcltk, rstudioapi as above.
#
# Nothing here writes a script to disk, and nothing is downloaded. The
# zenity and kdialog calls pass a fixed list of arguments to a program
# already installed on the desktop, with every value quoted by shQuote().
#
# AN INSTANT "CANCEL" IS TREATED AS "NO WINDOW APPEARED". A person cannot
# find and close a window in a fraction of a second, so a method that
# reports a cancel in under NO_WINDOW_SECONDS almost certainly never drew
# one. That is recorded as "no_window" and the next method is tried. A
# cancel that takes longer is believed: the chain stops and nothing is
# selected. This is a heuristic. A user who cancels exceptionally fast may
# see a second window appear; the alternative, believing every instant
# cancel, is what made the button look broken.
#
# IT REPORTS WHAT IT TRIED. The returned list carries $method (the one
# that produced the answer) and $attempts (every method with its
# outcome), so a failure can be explained instead of being silent.
#
# WHERE THE WINDOW OPENS. breezeml runs locally, so the R process and the
# user's screen are on the same computer. If breezeml were hosted on a
# server (Shiny Server, Posit Connect, RStudio Server), the window would
# open on the SERVER. Callers must therefore only call this for a local
# session (congruence_is_local_session() in 10_congruence.R) and otherwise
# offer a typed-path box.
#
# While a window is open the R process waits for it, so the page will not
# respond until it is answered.

# A cancel reported faster than this many seconds is taken to mean no
# window was ever drawn. See the note above.
NO_WINDOW_SECONDS <- 0.75

#' Build the result list returned by the per-method helpers.
#'
#' @param status one of "ok", "cancelled", "unavailable", "no_window"
#'   ("no_window" is internal: a method that returned without drawing
#'   anything. choose_directory_native() never returns it.)
#' @noRd
.dir_result <- function(status, path = NA_character_) {
  list(status = status, path = path)
}

#' The folder-window methods to try, in order, on this computer.
#' @return character vector of method names
#' @noRd
choose_directory_methods <- function(sysname = Sys.info()[["sysname"]],
                                     gui = .Platform$GUI) {
  rstudio <- identical(gui, "RStudio")

  switch(sysname,
         Windows = if (rstudio) {
           # choose.dir() draws nothing under RStudio: try it last, so a
           # future RStudio that fixes it still gets the native dialog.
           c("tcltk", "rstudioapi", "choose.dir")
         } else {
           c("choose.dir", "tcltk", "rstudioapi")
         },
         Darwin  = c("osascript", "tcltk", "rstudioapi"),
         c("zenity", "kdialog", "tcltk", "rstudioapi"))
}

#' Methods whose "cancelled" must not be believed in this front end,
#' because they report a cancel when they never drew a window. Their
#' cancel is recorded as "no_window" so the chain carries on.
#' @noRd
.unreliable_cancel_methods <- function(gui = .Platform$GUI) {
  if (identical(gui, "RStudio")) "choose.dir" else character(0)
}

#' Run one named method.
#' @noRd
.choose_dir_method <- function(method, caption, start) {
  tryCatch(
    switch(method,
           choose.dir = .choose_dir_windows(caption, start),
           osascript  = .choose_dir_macos(caption, start),
           zenity     = .choose_dir_zenity(caption, start),
           kdialog    = .choose_dir_kdialog(caption, start),
           tcltk      = .choose_dir_tcltk(caption, start),
           rstudioapi = .choose_dir_rstudio(caption, start),
           .dir_result("unavailable")),
    error = function(e) .dir_result("unavailable")
  )
}

#' Open the operating system's folder-selection window.
#'
#' Tries each method for this platform in turn until one selects a folder
#' or one reports a cancel that looks real (see the file header).
#'
#' @param caption text shown in the window's title bar or prompt
#' @param default folder the window should start in (ignored if it does
#'   not exist)
#' @param methods override the method order; mainly for testing
#' @return list(status, path, method, attempts).
#'   status  "ok" (path holds the chosen folder, forward slashes, no
#'           trailing slash), "cancelled" (a window was answered without
#'           choosing), or "unavailable" (no method could open a window;
#'           path is NA).
#'   method  the method that produced the answer, or NA.
#'   attempts named character vector: outcome of every method tried,
#'           e.g. c(choose.dir = "no_window", tcltk = "ok").
#' @noRd
choose_directory_native <- function(caption = "Select a folder",
                                    default = NULL,
                                    methods = NULL) {
  start <- ""
  if (!is.null(default) && length(default) == 1 && !is.na(default) &&
      nzchar(default) && dir.exists(default)) {
    start <- normalizePath(default, winslash = "/", mustWork = FALSE)
  }

  if (is.null(methods)) methods <- choose_directory_methods()
  unreliable <- .unreliable_cancel_methods()
  attempts <- character(0)

  for (m in methods) {
    began <- Sys.time()
    res <- .choose_dir_method(m, caption, start)
    elapsed <- as.numeric(difftime(Sys.time(), began, units = "secs"))

    status <- res$status

    # A cancel from a method known not to draw a window in this front end
    # is never believed, however long it took (v5).
    if (identical(status, "cancelled") && m %in% unreliable) {
      status <- "no_window"
    }

    # Otherwise, an instant cancel means no window was drawn: a person
    # cannot find and close a window that fast. Try the next method.
    if (identical(status, "cancelled") && elapsed < NO_WINDOW_SECONDS) {
      status <- "no_window"
    }

    attempts[[m]] <- status

    if (identical(status, "ok")) {
      p <- normalizePath(res$path, winslash = "/", mustWork = FALSE)
      # Drop a trailing slash (but keep a bare root such as "C:/" or "/").
      if (nchar(p) > 1 && !grepl("^[A-Za-z]:/$", p)) p <- sub("/+$", "", p)
      return(list(status = "ok", path = p, method = m, attempts = attempts))
    }

    # A cancel that took long enough to be a real one is respected.
    if (identical(status, "cancelled")) {
      return(list(status = "cancelled", path = NA_character_,
                  method = m, attempts = attempts))
    }
    # "unavailable" or "no_window": keep going.
  }

  list(status = "unavailable", path = NA_character_,
       method = NA_character_, attempts = attempts)
}

#' A one-line summary of what was tried, for showing a user why no window
#' appeared. Empty string when nothing was attempted.
#' @noRd
choose_directory_attempts_text <- function(res) {
  att <- res$attempts
  if (is.null(att) || length(att) == 0) return("")
  paste(paste0(names(att), ": ", unname(att)), collapse = "; ")
}

#' What folder-window methods exist on this computer, and what each one
#' reports without opening anything. For troubleshooting a button that
#' appears to do nothing: run breezeml:::choose_directory_diagnose() in
#' the R session that is running the app and read the result.
#' @noRd
choose_directory_diagnose <- function() {
  sysname <- Sys.info()[["sysname"]]
  list(
    sysname = sysname,
    r_version = paste(R.version$major, R.version$minor, sep = "."),
    gui = .Platform$GUI,
    interactive = interactive(),
    methods_in_order = choose_directory_methods(sysname),
    unreliable_cancel = .unreliable_cancel_methods(),
    choose_dir_exists = !is.null(get0("choose.dir", envir = asNamespace("utils"),
                                      mode = "function")),
    tcltk_capable = isTRUE(capabilities("tcltk")),
    tcltk_installed = requireNamespace("tcltk", quietly = TRUE),
    rstudioapi_installed = requireNamespace("rstudioapi", quietly = TRUE),
    rstudio_available = .rstudio_available(),
    osascript = unname(Sys.which("osascript")),
    zenity = unname(Sys.which("zenity")),
    kdialog = unname(Sys.which("kdialog")),
    display = Sys.getenv("DISPLAY")
  )
}

# ---- Windows -----------------------------------------------------------

#' Windows tree-style folder dialog. utils::choose.dir() exists only on
#' Windows, so it is looked up by name rather than called with `::`.
#'
#' This dialog cannot be raised from R (v6): choose.dir() accepts only
#' `default` and `caption`, with no owner, parent or topmost argument.
#' Under Rgui, which is the only front end that hosts it, the window the
#' user clicked in is normally Rgui itself, so it is usually in front
#' anyway.
#' @noRd
.choose_dir_windows <- function(caption, start) {
  choose_dir <- get0("choose.dir", envir = asNamespace("utils"),
                     mode = "function")
  if (is.null(choose_dir)) return(.dir_result("unavailable"))

  # choose.dir() expects backslashes, and "" (not NA) for "no default".
  win_start <- if (nzchar(start)) gsub("/", "\\", start, fixed = TRUE) else ""
  path <- choose_dir(default = win_start, caption = caption)

  if (length(path) != 1 || is.na(path) || !nzchar(path)) {
    return(.dir_result("cancelled"))
  }
  .dir_result("ok", path)
}

# ---- macOS -------------------------------------------------------------

#' Run one AppleScript through osascript and classify the outcome.
#'
#' @param lines character vector; each element becomes one -e argument,
#'   which osascript treats as one line of a single script.
#' @return list(result = "ok"/"cancelled"/"unavailable", path, out)
#' @noRd
.osascript_run <- function(lines) {
  args <- as.vector(rbind("-e", shQuote(lines)))
  out <- suppressWarnings(
    system2("osascript", args, stdout = TRUE, stderr = TRUE)
  )
  status <- attr(out, "status")

  if (!is.null(status) && status != 0) {
    if (any(grepl("User canceled|-128", out))) {
      return(list(result = "cancelled", path = NA_character_, out = out))
    }
    return(list(result = "unavailable", path = NA_character_, out = out))
  }
  if (length(out) < 1 || !nzchar(out[1])) {
    return(list(result = "unavailable", path = NA_character_, out = out))
  }
  list(result = "ok", path = out[1], out = out)
}

#' macOS Finder folder dialog through osascript. Cancelling makes
#' osascript exit with an error containing "User canceled" (-128); that is
#' reported as "cancelled", not as a failure.
#'
#' The dialog is raised by running inside `tell application "System
#' Events"` with `activate`, so it belongs to an application that has
#' just come to the front rather than to a background process (v6).
#' Should that fail for any reason other than the user cancelling - most
#' plausibly because this Mac has not authorised sending Apple events to
#' System Events - the plain script v5 used is run once instead. No
#' window appeared in that case, so the fallback cannot produce a second
#' one.
#' @noRd
.choose_dir_macos <- function(caption, start) {
  if (!nzchar(Sys.which("osascript"))) return(.dir_result("unavailable"))

  # Escape backslashes and double quotes for an AppleScript string literal.
  esc <- function(x) gsub('"', '\\\\"', gsub("\\\\", "\\\\\\\\", x))

  choose_stmt <- paste0(
    'POSIX path of (choose folder with prompt "', esc(caption), '"',
    if (nzchar(start)) paste0(' default location POSIX file "', esc(start), '"') else "",
    ")"
  )

  activated <- .osascript_run(c(
    'tell application "System Events"',
    "activate",
    choose_stmt,
    "end tell"
  ))

  if (identical(activated$result, "ok")) {
    return(.dir_result("ok", activated$path))
  }
  # A real cancel is a real answer: do not reopen the dialog.
  if (identical(activated$result, "cancelled")) {
    return(.dir_result("cancelled"))
  }

  # The activated form could not run at all. Fall back to v5's script.
  plain <- .osascript_run(choose_stmt)
  if (identical(plain$result, "ok")) return(.dir_result("ok", plain$path))
  if (identical(plain$result, "cancelled")) return(.dir_result("cancelled"))
  .dir_result("unavailable")
}

# ---- Linux and other Unix ---------------------------------------------

#' Shared handling for the zenity and kdialog desktop dialogs: exit code 0
#' with a path means chosen, exit code 1 means cancelled. Both are desktop
#' programs that ship with GNOME and KDE; neither is a script, and every
#' argument is quoted by the caller.
#'
#' Neither is raised explicitly (v6). zenity has no option to attach a
#' dialog to a window it was not given, and kdialog's --attach needs the
#' browser's X11 window id, which an R process has no reliable way to
#' discover; guessing at one does not belong in a package. Both desktops
#' normally present a running application's dialog in front.
#' @noRd
.choose_dir_cli <- function(exe, args) {
  if (!nzchar(Sys.which(exe))) return(.dir_result("unavailable"))
  if (!nzchar(Sys.getenv("DISPLAY")) && !nzchar(Sys.getenv("WAYLAND_DISPLAY"))) {
    return(.dir_result("unavailable"))
  }
  out <- suppressWarnings(system2(exe, args, stdout = TRUE, stderr = TRUE))
  code <- attr(out, "status")
  if (is.null(code)) code <- 0L
  if (identical(as.integer(code), 1L)) return(.dir_result("cancelled"))
  if (!identical(as.integer(code), 0L)) return(.dir_result("unavailable"))
  path <- trimws(paste(out, collapse = ""))
  if (!nzchar(path)) return(.dir_result("cancelled"))
  .dir_result("ok", path)
}

#' GTK folder dialog (GNOME and similar).
#' @noRd
.choose_dir_zenity <- function(caption, start) {
  args <- c("--file-selection", "--directory",
            paste0("--title=", shQuote(caption)))
  if (nzchar(start)) args <- c(args, paste0("--filename=", shQuote(paste0(start, "/"))))
  .choose_dir_cli("zenity", args)
}

#' KDE folder dialog.
#' @noRd
.choose_dir_kdialog <- function(caption, start) {
  args <- c("--title", shQuote(caption), "--getexistingdirectory",
            if (nzchar(start)) shQuote(start) else shQuote("."))
  .choose_dir_cli("kdialog", args)
}

# ---- Any platform with Tcl/Tk -----------------------------------------

#' A tiny always-on-top Tk window, to be handed to the folder dialog as
#' its -parent so the dialog is raised in front of the browser (v6).
#'
#' WHY A HELPER WINDOW IS NEEDED AT ALL. Tk's own root window "." would
#' be the obvious parent, but R withdraws it as soon as Tk starts - from
#' R's src/library/tcltk/src/tcltk.c:
#'
#'   code = Tcl_Eval(RTcl_interp, "wm withdraw .");  /* Hide window */
#'
#' A withdrawn window is not on screen and cannot raise anything, so a
#' parent has to be made.
#'
#' WHY IT IS INVISIBLE IN PRACTICE. It is 1x1 pixels at the top-left
#' corner, has no title bar or taskbar entry (overrideredirect), and is
#' asked to be fully transparent. Tk's wm documentation notes that where
#' -alpha is not supported "the -alpha value remains at 1.0", which is
#' why the window is also made 1x1 rather than relying on transparency:
#' at worst a single pixel is drawn for as long as the dialog is open.
#'
#' -topmost is documented by Tk as supported on all platforms
#' ("Specifies whether this is a topmost window (displays above all other
#' windows)"), with X11 caveats; each attribute is set inside its own
#' try() so a platform that refuses one still gets the others.
#'
#' tkwm.attributes() does not exist in the tcltk package - the wrappers
#' it ships cover wm withdraw, deiconify, transient and so on but not
#' attributes - so "wm attributes" is sent through tcltk::tcl(), which is
#' the documented general route and what every tkwm.* wrapper itself
#' calls.
#'
#' @return a tkwin, or NULL if one could not be made (the caller then
#'   opens the dialog unparented, exactly as v5 did)
#' @noRd
.tk_front_window <- function() {
  tryCatch({
    win <- tcltk::tktoplevel()

    # Out of the way, undecorated, and not in the taskbar.
    try(tcltk::tkwm.geometry(win, "1x1+0+0"), silent = TRUE)
    try(tcltk::tkwm.overrideredirect(win, TRUE), silent = TRUE)
    try(tcltk::tcl("wm", "attributes", win, "-alpha", 0), silent = TRUE)

    # The part that actually matters: above all other windows.
    try(tcltk::tcl("wm", "attributes", win, "-topmost", TRUE), silent = TRUE)

    # Map it and let Tk process the request, so it really exists on
    # screen before it is used as a parent.
    try(tcltk::tkwm.deiconify(win), silent = TRUE)
    try(tcltk::tcl("focus", "-force", win), silent = TRUE)
    try(tcltk::tcl("update"), silent = TRUE)

    win
  }, error = function(e) NULL)
}

#' Destroy the helper window, ignoring any error. Called from on.exit(),
#' so the window is cleaned up whether the dialog was answered,
#' cancelled, or failed.
#' @noRd
.tk_destroy_quietly <- function(win) {
  if (is.null(win)) return(invisible(NULL))
  try(tcltk::tkdestroy(win), silent = TRUE)
  invisible(NULL)
}

#' Tcl/Tk folder dialog. Part of R itself wherever R was built with
#' Tcl/Tk, which includes the standard R for Windows, so it is the
#' cross-platform answer and the main replacement for the PowerShell
#' dialog withdrawn in v3. It opens its own window and does not need R's
#' GUI front end.
#'
#' tcltk is a BASE package: it is not on CRAN and cannot be installed, so
#' it is declared under Suggests and guarded here instead of being
#' imported. capabilities("tcltk") is FALSE when R was built without
#' Tcl/Tk support, in which case no amount of installing would help and
#' the next method (or the typed-path box) is used.
#'
#' There is deliberately no interactive() guard: a Shiny app's R session
#' is often not interactive, and Tk does not require it.
#'
#' WHY tk_choose.dir() IS NO LONGER CALLED (v6). It cannot pass -parent.
#' Its whole body, from R's src/library/tcltk/R/utils.R, is:
#'
#'   tk_choose.dir <- function(default = '', caption = 'Select directory')
#'   {
#'       res <- tclvalue(tcl("tk_chooseDirectory", initialdir = default,
#'                           title = caption))
#'       if(nzchar(res)) res else NA_character_
#'   }
#'
#' The call below is that same one line with `parent` added, so the
#' dialog is raised in front (see .tk_front_window()). Tk's own
#' documentation for tk_chooseDirectory describes -parent as "Makes
#' window the logical parent of the dialog. The dialog is displayed on
#' top of its parent window."
#'
#' Behaviour is otherwise identical, including treating an empty result
#' as a cancel. If the parented call fails for any reason, the
#' unparented form is tried once - no window was drawn in that case, so
#' this cannot produce two.
#' @noRd
.choose_dir_tcltk <- function(caption, start) {
  if (!isTRUE(capabilities("tcltk")) ||
      !requireNamespace("tcltk", quietly = TRUE)) {
    return(.dir_result("unavailable"))
  }
  # Loading the namespace is what starts Tk. If that fails (no system
  # Tcl/Tk libraries, or no display), say "unavailable" rather than
  # letting an error reach the caller as a cancel.
  if (!isTRUE(tryCatch({
    loadNamespace("tcltk")
    TRUE
  }, error = function(e) FALSE))) {
    return(.dir_result("unavailable"))
  }

  parent <- .tk_front_window()
  on.exit(.tk_destroy_quietly(parent), add = TRUE)

  path <- NULL
  if (!is.null(parent)) {
    path <- tryCatch(
      tcltk::tclvalue(tcltk::tcl("tk_chooseDirectory",
                                 initialdir = start,
                                 title = caption,
                                 parent = parent)),
      error = function(e) NULL
    )
  }

  # No helper window, or the parented call could not run: ask exactly as
  # v5 did. The dialog never opened, so nothing is shown twice.
  if (is.null(path)) {
    path <- tryCatch(
      tcltk::tclvalue(tcltk::tcl("tk_chooseDirectory",
                                 initialdir = start,
                                 title = caption)),
      error = function(e) NULL
    )
  }

  if (is.null(path) || length(path) != 1 || is.na(path) || !nzchar(path)) {
    return(.dir_result("cancelled"))
  }
  .dir_result("ok", path)
}

# ---- RStudio -----------------------------------------------------------

#' Is RStudio there to ask? rstudioapi is installed with breezeml, but it
#' only works when the R session is actually running inside RStudio, so
#' isAvailable() is what decides. The requireNamespace() check is kept as
#' a cheap guard in case the package was removed by hand.
#' @noRd
.rstudio_available <- function() {
  if (!requireNamespace("rstudioapi", quietly = TRUE)) return(FALSE)
  isTRUE(tryCatch(rstudioapi::isAvailable(), error = function(e) FALSE))
}

#' RStudio's own folder dialog, drawn by the IDE. Returns NA when the user
#' cancels. Last in the chain because calling the IDE from inside a
#' running Shiny app is the least predictable of the methods here.
#'
#' Not raised explicitly (v6): the IDE draws this itself as part of its
#' own window, so RStudio's stacking applies and there is nothing for
#' this package to parent it to.
#' @noRd
.choose_dir_rstudio <- function(caption, start) {
  if (!.rstudio_available()) return(.dir_result("unavailable"))
  if (!isTRUE(tryCatch(rstudioapi::hasFun("selectDirectory"),
                       error = function(e) FALSE))) {
    return(.dir_result("unavailable"))
  }
  path <- rstudioapi::selectDirectory(
    caption = caption,
    path = if (nzchar(start)) start else NULL
  )
  if (length(path) != 1 || is.na(path) || !nzchar(path)) {
    return(.dir_result("cancelled"))
  }
  .dir_result("ok", path)
}
