#' @title add_one_sel_top
#' @description extend selection one line up
#' @return new ranges for extended selection
#' @export
add_one_sel_top = function() extend_selection(-1)

#' @title add_one_sel_end
#' @description extend selection one line down
#' @return new ranges for extended selection
#' @export
add_one_sel_end = function() extend_selection(1)

#' @title remove_one_sel_top
#' @description remove top selection
#' @return new ranges for shrunken selection
#' @export
remove_one_sel_top = function() remove_selection(-1)

#' @title remove_one_sel_end
#' @description remove bottom selection
#' @return new ranges for shrunken selection
#' @export
remove_one_sel_end = function() remove_selection(1)

#' @title extend_range
#' @description Extend the current single-line range to lines above or below,
#'     spanning the same columns
#'
#' @param doc_range (single) document_range object
#' @param ext number of lines to be extend, positive downward
#'
#' @return a new list of extended range(s)
extend_range = function(doc_range, ext) {
    # Abort in case of multi-line selection
    if (doc_range$start[['row']] != doc_range$end[['row']]) return(list())

    if (ext > 0 ) {
        lines = 1:ext
    } else if (ext < 0) {
        lines = ext:-1
    } else {
        return(list())
    }

    new_ranges = lapply(lines, function(n) {
        rstudioapi::document_range(
            c(doc_range$start['row'] + n, doc_range$start['column']),
            c(doc_range$end[  'row'] + n, doc_range$end[  'column'])
        )
    })
    return(new_ranges)
}

#' @title extend_selection
#' @description extend current selection by `ext` number of lines
#' @param ext number of lines to be extend, positive means downward
#' @return new extened ranges (including the old ones)
extend_selection = function(ext) {
    # Context in editor
    context = rstudioapi::getSourceEditorContext()
    # Ranges for all selection
    ranges = lapply(context$selection, function(x) x$range)

    if (ext < 0) {
        # Top range
        top_range = ranges[[1]]
        # Create new extened range list
        ranges = c(extend_range(top_range, ext), ranges)
    } else if (ext > 0) {
        # Top range
        end_range = ranges[[length(ranges)]]
        # Create new extened range list
        ranges = c(ranges, extend_range(end_range, ext))
    }
    # Set new selection ranges
    rstudioapi::setSelectionRanges(ranges, id = context$id)
}

#' @title remove_range
#' @description remove ranges by ext
#' @param doc_ranges list of document_ranges objects
#' @param ext number of line
#' @return new ranges
remove_range = function(doc_ranges, ext) {
    if (ext < 0) {
        lines = ext:-1
    } else if (ext > 0) {
        lines = 1:(length(doc_ranges)-ext)
    } else {
        return(list())
    }
    # Create new extened range list
    doc_ranges = doc_ranges[lines]
    return(doc_ranges)
}

#' @title remove_selection
#' @description remove current selection by `ext` amount
#' @param ext number of lines to be removed, positive means from bottom
#' @return newly shrunken ranges
remove_selection = function(ext) {
    # Context in editor
    context = rstudioapi::getSourceEditorContext()
    # Ranges for all selection
    ranges = lapply(context$selection, function(x) x$range)
    # create new ranges
    ranges = remove_range(ranges, ext)
    # Set new selection ranges
    rstudioapi::setSelectionRanges(ranges, id = context$id)
}
