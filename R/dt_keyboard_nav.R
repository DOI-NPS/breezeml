# dt_keyboard_nav.R v1
#
# Shared keyboard navigation for the editable DT grids on Tab 3
# (03_data_tables.R) and Tab 4 (04_fields.R).
#
# WHY THIS EXISTS
#
# DT's cell editor (editable = list(target = "cell", ...)) binds exactly
# two keys, and neither of them moves between cells. From DT's own
# inst/htmlwidgets/datatables.js, the immediate (target = "cell") branch
# binds only:
#
#   if (immediate) $input.on('blur', function(e) { ...commit... })
#                        .on('keyup', function(e) {
#                          // hit Escape to cancel editing
#                          if (e.keyCode === 27) $input.trigger('blur');
#                        });
#
# Enter (keyCode 13) is bound ONLY in the bulk branch (target = "row",
# "column" or "all"), and there only as Ctrl+Enter. So in breezeml's
# grids, before this file existed:
#
#   * Enter did nothing at all.
#   * Tab committed the cell (because leaving the input blurs it) but
#     then moved focus to whatever came next in the DOM, because the
#     next cell has no <input> until it is double-clicked.
#
# Users filling in a wide table therefore had to double-click every
# single cell. This file adds the movement keys:
#
#   Enter        commit, move DOWN one row in the same column
#   Shift+Enter  commit, move UP one row in the same column
#   Tab          commit, move RIGHT to the next editable cell, wrapping
#                to the first editable column of the next row
#   Shift+Tab    commit, move LEFT, wrapping to the last editable column
#                of the previous row
#   Esc          cancel - DT's own binding, deliberately untouched
#
# HOW IT WORKS, AND WHY IT IS BUILT THIS WAY
#
# Committing is done by blurring the current input, which is DT's own
# commit path - this file never writes a cell value itself, so the R-side
# contract (input$<id>_cell_edit -> DT::editData()) is completely
# unchanged.
#
# Opening the next editor is done by triggering a 'dblclick' on the
# destination <td>, which is DT's own open-editor path. DT guards that
# handler with `if (e.target !== this) return;`, and a jQuery trigger on
# the td makes e.target the td itself, so the guard passes.
#
# The two steps are separated by setTimeout(..., 0) because DT's blur
# handler calls table.draw(false) in client-side mode, which rebuilds the
# <td> nodes. The destination is therefore looked up by (row, column)
# index AFTER the redraw rather than held as a stale node reference.
#
# Movement follows DISPLAY order, not the order of the underlying data
# frame, so navigation still behaves correctly after a user sorts a
# column. Rows come from
#   table.rows({order: 'applied', search: 'applied', page: 'current'})
# which is deliberately limited to the current page: navigation never
# silently pages the grid, so a user cannot be taken to a row they
# cannot see. At the first or last navigable cell, movement simply stops
# and the edit is still committed.
#
# Read-only columns are skipped. The caller passes the SAME 0-based
# column indices it passes to editable = list(disable = list(columns =
# ...)), so the two can never disagree. Columns hidden with
# columnDefs$visible = FALSE are skipped as well, since a hidden column
# has no <td> to put an editor in.
#
# Enter moving down the column is safe for both callers because in both
# of them `disable` is per COLUMN: if a cell is editable then the cell
# above and below it is editable too.
#
# Only <input> elements are handled. Neither caller configures the
# `area` (textarea) editor, and in a textarea Enter legitimately means
# "new line", so restricting the selector keeps that option open.
# Ctrl/Alt/Meta combinations are ignored and left to DT and the browser.
#
# TWO ENTRY POINTS
#
#   dt_keyboard_nav_body()     the JS statements, for a caller that
#                              already has its own callback body to
#                              combine with (Tab 4's attributes grid,
#                              which attaches a <datalist> to the class
#                              column's editor)
#   dt_keyboard_nav_callback() the same statements as a DT::JS() value
#                              ready to pass straight to datatable()'s
#                              `callback`, for a caller with nothing
#                              else to do
#
# NOTE ON WHAT `callback` EXPECTS. It is the BODY of the function, not a
# function. DT wraps it itself - from DT's datatable():
#
#   callback = if (!missing(callback)) JS('function(table) {', callback, '}')
#
# so dt_keyboard_nav_callback() must NOT add its own
# "function(table) {...}" wrapper, or the result is nested two deep and
# never runs. (DT's own default for the argument is the bare statement
# JS('return table;'), which shows the same thing.)
#
# Both entry points therefore produce statements that run with the
# DataTables API object in scope as `table`, and rely on jQuery being
# available as `$`. Both assumptions already hold in this app - see the
# existing rowCallbacks in 04_fields.R, which use $('td', row).
#
# Verified against DataTables 1.13.6 and the DT release bundling it; the
# APIs used (columns().indexes(), column().visible(), rows().indexes(),
# cell().index(), cell(row, col).node()) are all long-standing.

# JS statements implementing the navigation. Returns a single character
# string so it can be concatenated with other JS inside one callback.
#
# @param disabled_columns 0-based column indices that cannot be edited -
#   pass exactly what is passed to editable$disable$columns
dt_keyboard_nav_body <- function(disabled_columns = integer(0)) {
  paste(
    c(
      "  /* ---- breezeml keyboard navigation (dt_keyboard_nav.R) ---- */",
      sprintf(
        "  var bzDisabled = %s;",
        jsonlite::toJSON(as.integer(disabled_columns))
      ),
      "",
      "  /* Editable, visible columns in ascending index order. */",
      "  function bzEditableColumns() {",
      "    var all = table.columns().indexes().toArray();",
      "    var out = [];",
      "    for (var i = 0; i < all.length; i++) {",
      "      var col = all[i];",
      "      if (bzDisabled.indexOf(col) === -1 &&",
      "          table.column(col).visible()) {",
      "        out.push(col);",
      "      }",
      "    }",
      "    return out;",
      "  }",
      "",
      "  /* Internal row indices of the rows on screen, in display",
      "     order, so sorting is respected and paging is never jumped. */",
      "  function bzDisplayRows() {",
      "    return table",
      "      .rows({ order: 'applied', search: 'applied', page: 'current' })",
      "      .indexes()",
      "      .toArray();",
      "  }",
      "",
      "  /* Where a move from (fromRow, fromCol) lands, or null if it",
      "     would leave the navigable cells. */",
      "  function bzTarget(fromRow, fromCol, rowStep, colStep) {",
      "    var rows = bzDisplayRows();",
      "    var cols = bzEditableColumns();",
      "    var ri = rows.indexOf(fromRow);",
      "    var ci = cols.indexOf(fromCol);",
      "    if (ri === -1 || ci === -1) { return null; }",
      "    if (colStep !== 0) {",
      "      ci = ci + colStep;",
      "      if (ci >= cols.length) {",
      "        ci = 0;",
      "        ri = ri + 1;",
      "      } else if (ci < 0) {",
      "        ci = cols.length - 1;",
      "        ri = ri - 1;",
      "      }",
      "    } else {",
      "      ri = ri + rowStep;",
      "    }",
      "    if (ri < 0 || ri >= rows.length) { return null; }",
      "    return { row: rows[ri], column: cols[ci] };",
      "  }",
      "",
      "  /* Open DT's editor on a destination cell, bringing it into view",
      "     only if it is off screen, and selecting its text so typing",
      "     replaces the value. */",
      "  function bzOpenEditor(dest) {",
      "    var node = table.cell(dest.row, dest.column).node();",
      "    if (!node) { return; }",
      "    if (typeof node.scrollIntoView === 'function') {",
      "      node.scrollIntoView({ block: 'nearest', inline: 'nearest' });",
      "    }",
      "    $(node).trigger('dblclick');",
      "    var input = $(node).find('input').get(0);",
      "    if (input) {",
      "      input.focus();",
      "      if (typeof input.select === 'function') { input.select(); }",
      "    }",
      "  }",
      "",
      "  $(table.table().body()).on(",
      "    'keydown.bzNav', 'input', function(e) {",
      "      var isEnter = (e.key === 'Enter' || e.keyCode === 13);",
      "      var isTab = (e.key === 'Tab' || e.keyCode === 9);",
      "      if (!isEnter && !isTab) { return; }",
      "      /* Ctrl+Enter belongs to DT's bulk editor; leave modified",
      "         keypresses to DT and the browser. */",
      "      if (e.ctrlKey || e.altKey || e.metaKey) { return; }",
      "",
      "      var td = $(this).closest('td').get(0);",
      "      if (!td) { return; }",
      "      var idx = table.cell(td).index();",
      "      if (!idx) { return; }",
      "",
      "      e.preventDefault();",
      "      e.stopPropagation();",
      "",
      "      var dest = isTab",
      "        ? bzTarget(idx.row, idx.column, 0, e.shiftKey ? -1 : 1)",
      "        : bzTarget(idx.row, idx.column, e.shiftKey ? -1 : 1, 0);",
      "",
      "      /* Commit through DT's own blur handler. In client-side mode",
      "         that also calls table.draw(false), which rebuilds the",
      "         cells - so the destination is resolved afterwards. */",
      "      $(this).trigger('blur');",
      "",
      "      if (dest !== null) {",
      "        setTimeout(function() { bzOpenEditor(dest); }, 0);",
      "      }",
      "    }",
      "  );"
    ),
    collapse = "\n"
  )
}

# The same navigation ready to hand to datatable()'s `callback`. This is
# the statement BODY wrapped only in DT::JS() - DT supplies the
# surrounding function(table) { ... } itself, so adding one here would
# nest it twice and it would never run. See the note above.
#
# @param disabled_columns as for dt_keyboard_nav_body()
dt_keyboard_nav_callback <- function(disabled_columns = integer(0)) {
  DT::JS(dt_keyboard_nav_body(disabled_columns = disabled_columns))
}
