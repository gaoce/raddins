#' View Selected Object
#' View Selected Object
#'
#' @import stringr
#' @export
view_name = function() {
    # Get context from editor or console
    context = rstudioapi::getActiveDocumentContext()

    # Multiple selection, abort
    if (length(context$selection) > 1) return()

    # Start and end positions of selection[[1]]
    pos_frt = context$selection[[1]]$range$start
    pos_end = context$selection[[1]]$range$end

    # Multiple-line selection, abort
    if (pos_frt[1] != pos_end[1]) return()

    line = context$contents[pos_frt[1]]

    # Get location of valid variable names
    locs = stringr::str_locate_all(line, '[a-zA-Z][a-zA-Z0-9_\\.]*')[[1]]

    # Test relative position between cursor and tokens
    pos = which(locs[, 1] <= pos_frt[2] & locs[, 1] <= pos_end[2] &
                locs[, 2] >= (pos_frt[2] - 1) & locs[, 2] >= (pos_end[2] - 1))

    # Selection cross multiple tokens, abort
    if (length(pos) == 0) return()

    # Construct evaluation string
    pos = pos[1]
    obj_name = substr(line, locs[pos, 1], locs[pos, 2])
    expr_str = sprintf('View(%s)', obj_name)

    # Evaluation and capture the error
    # Note 1: We must view the variable directly, see "Using the Data Viewer"
    #   on RStudio Support
    # Note 2: RStudio hajack the utils::View and cat() the error instead of
    #   "throw" it, so we need to capture.output() to become fully silent
    capture.output(
        try(eval(parse(text = expr_str), envir = globalenv()), silent = T)
    )
    invisible()
}
