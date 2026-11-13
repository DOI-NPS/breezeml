# choose_directory.R v1
#
# Opens the operating system's own "select a folder" window so a user can
# pick a directory with the mouse instead of typing a path.
#
#   Windows : the Windows folder-browser dialog (utils::choose.dir()).
#   macOS   : the Finder "choose folder" dialog, opened through osascript
#             (ships with macOS; no extra R package needed).
#   Other   : the Tcl/Tk folder dialog (tcltk::tk_choose.dir()) when R was
#             built with Tcl/Tk, otherwise reported as "unavailable".
#
# IMPORTANT - where the window opens. breezeml runs locally, so the R
# process and the user's screen are on the same computer and the window
# appears on the user's own desktop. If breezeml were ever hosted on a
# server (Shiny Server, Posit Connect, RStudio Server), the window would
# try to open on the SERVER, not on the user's computer. Callers should
# therefore only call this for a local session (see
# congruence_is_local_session() in 10_congruence.R) and otherwise offer a
# typed-path fallback.
#
# While the window is open, the R process waits for the user to finish.
# That is fine for a local single-user app. On some systems the window can
# open behind the browser; the user may need to look for it on the taskbar
# or Dock.
#
# This file is deliberately separate from the Congruence tab so that the
# Upload and Publish tab (11_upload_publish.R) can reuse it later.

#' Build the result list returned by choose_directory_native().
#' @noRd
.dir_result <- function(status, path = NA_character_) {
  list(status = status, path = path)
}

#' Open the operating system's folder-selection window.
#'
#' @param caption text shown in the window's title bar or prompt
#' @param default folder the window should start in (ignored if it does
#'   not exist)
#' @return list(status, path). status is "ok" (path holds the chosen
#'   folder, forward slashes, no trailing slash), "cancelled" (the user
#'   closed the window without choosing), or "unavailable" (no folder
#'   window could be opened on this computer; path is NA).
#' @noRd
choose_directory_native <- function(caption = "Select a folder", default = NULL) {
  start <- ""
  if (!is.null(default) && length(default) == 1 && !is.na(default) &&
      nzchar(default) && dir.exists(default)) {
    start <- normalizePath(default, winslash = "/", mustWork = FALSE)
  }

  sysname <- Sys.info()[["sysname"]]

  res <- tryCatch(
    switch(sysname,
           Windows = .choose_dir_windows(caption, start),
           Darwin  = .choose_dir_macos(caption, start),
           .choose_dir_tcltk(caption, start)),
    error = function(e) .dir_result("unavailable")
  )

  if (identical(res$status, "ok")) {
    p <- normalizePath(res$path, winslash = "/", mustWork = FALSE)
    # Drop a trailing slash (but keep a bare root such as "C:/" or "/").
    if (nchar(p) > 1 && !grepl("^[A-Za-z]:/$", p)) p <- sub("/+$", "", p)
    res$path <- p
  }
  res
}

#' Windows folder-browser dialog. utils::choose.dir() exists only on
#' Windows, so it is looked up by name rather than called with `::`.
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

#' macOS Finder folder dialog through osascript. Cancelling makes
#' osascript exit with an error containing "User canceled" (-128); that is
#' reported as "cancelled", not as a failure.
#' @noRd
.choose_dir_macos <- function(caption, start) {
  if (!nzchar(Sys.which("osascript"))) return(.dir_result("unavailable"))

  # Escape backslashes and double quotes for an AppleScript string literal.
  esc <- function(x) gsub('"', '\\\\"', gsub("\\\\", "\\\\\\\\", x))

  script <- paste0(
    'POSIX path of (choose folder with prompt "', esc(caption), '"',
    if (nzchar(start)) paste0(' default location POSIX file "', esc(start), '"') else "",
    ")"
  )

  out <- suppressWarnings(
    system2("osascript", c("-e", shQuote(script)), stdout = TRUE, stderr = TRUE)
  )
  status <- attr(out, "status")

  if (!is.null(status) && status != 0) {
    if (any(grepl("User canceled|-128", out))) return(.dir_result("cancelled"))
    return(.dir_result("unavailable"))
  }
  if (length(out) < 1 || !nzchar(out[1])) return(.dir_result("unavailable"))
  .dir_result("ok", out[1])
}

#' Tcl/Tk folder dialog for systems that are neither Windows nor macOS.
#' @noRd
.choose_dir_tcltk <- function(caption, start) {
  if (!interactive() || !isTRUE(capabilities("tcltk")) ||
      !requireNamespace("tcltk", quietly = TRUE)) {
    return(.dir_result("unavailable"))
  }
  path <- tcltk::tk_choose.dir(default = start, caption = caption)
  if (length(path) != 1 || is.na(path) || !nzchar(path)) {
    return(.dir_result("cancelled"))
  }
  .dir_result("ok", path)
}
