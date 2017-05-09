#' Open Selected Object in Viewer.
#'
#' @import stringr
#' @export
view_name = function() {
    # Get the object name near the cursor
    obj_name = get_name_near_cursor()

    if (is.null(obj_name)) return()

    expr_str = sprintf('View(%s)', obj_name)

    # Evaluation and capture the error
    # Note 1: We must view the variable directly, see "Using the Data Viewer"
    #   on RStudio Support
    # Note 2: RStudio hajack the utils::View and cat() the error instead of
    #   "throw" it, so we need to capture.output() to become fully silent
    capture.output(
        try(eval(parse(text = expr_str), envir = globalenv()),
            silent = T)
    )
    invisible()
}

#' Echo the Class of Object Under Cursor to Console.
#'
#' @export
echo_class = function() {
    # Get the object name near the cursor
    obj_name = get_name_near_cursor()

    if (is.null(obj_name) || (!exists(obj_name))) return()

    print(class(get(obj_name)))

    invisible()
}

#' Get the object name near the cursor.
#'
#' @keywords internal
#' @return The name of the object. \code{NULL} if none is under cursor.
#' @seealso \code{\link{echo_class}}
get_name_near_cursor <- function() {
    # Get context from editor or console
    context <- rstudioapi::getActiveDocumentContext()

    # Multiple selection, abort
    if (length(context$selection) > 1) return(NULL)

    # Front and end positions (incl. row & col num for each) of the range
    rng_frt = context$selection[[1]]$range$start
    rng_end = context$selection[[1]]$range$end

    # Multiple-line selection, abort
    if (rng_frt['row'] != rng_end['row']) return(NULL)

    # Get columns
    col_frt = rng_frt['column']
    col_end = rng_end['column']

    # Get the content of the line
    line = context$contents[rng_frt['row']]

    # Get location of all valid variable names in the line
    # Each line in word_locs is one word
    word_locs = stringr::str_locate_all(line, '[a-zA-Z][a-zA-Z0-9_\\.]*')[[1]]

    # Test relative column position between cursor and tokens: the range of
    # cursor selection range must be fully within the range of the word
    #   word_locs[1, 1]     word_locs[1, 2]
    #               |       |
    #               V       V
    #               XXXXXXXXX           <= word
    #                 ****              <= cursor selection
    #                 ^  ^
    #                 |  |
    #           col_frt  col_end
    word_hit = which(word_locs[, 1] <= col_frt &
                     word_locs[, 1] <= col_end &
                     word_locs[, 2] >= (col_frt - 1) &
                     word_locs[, 2] >= (col_end - 1))

    # Selection cross multiple tokens, abort
    if (length(word_hit) != 1) return(NULL)

    # Construct evaluation string
    obj_name = substr(line, word_locs[word_hit, 1], word_locs[word_hit, 2])

    return(obj_name)
}
