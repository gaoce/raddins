#' View Selected Object
#' View Selected Object
#'
#' @export
view_name = function() {
    context = rstudioapi::getSourceEditorContext()

    # Multiple selection, abort
    if (length(context$selection) > 1) return()

    pos_frt = context$selection[[1]]$range$start
    pos_end = context$selection[[1]]$range$end

    # Multiple-line selection, abort
    if (pos_frt[1] != pos_end[1]) return()

    line = context$contents[pos_frt[1]]

    # Get location of valid variable names
    locs = str_locate_all(line, '[a-zA-Z][a-zA-Z0-9_\\.]+')[[1]]

    pos = which(locs[, 1] <= pos_frt[2] & locs[, 1] <= pos_end[2] &
                locs[, 2] >= (pos_frt[2] - 1) & locs[, 2] >= (pos_end[2] - 1))

    # Selection cross multiple tokens, abort
    if (length(pos) == 0) return()

    pos = pos[1]
    obj_name = substr(line, locs[pos, 1], locs[pos, 2])
    expr_str = sprintf('View(%s)', obj_name)
    eval(parse(text = expr_str), envir = globalenv())
}