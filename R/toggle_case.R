#' Toggle case
#'
#' Toggle the case of selected text in editor
#'
#' @export
toggle_case = function() {
    context = rstudioapi::getSourceEditorContext()

    txt = sapply(context$selection, function(selection) {
        text = selection$text
        if (all(strsplit(text, '')[[1]] %in% LETTERS)){
            return(tolower(text))
        }
        if (all(strsplit(text, '')[[1]] %in% letters)){
            return(toupper(text))
        }
        return(text)
    })
    loc = lapply(context$selection, function(x) x$range)

    rstudioapi::modifyRange(loc, txt)
    rstudioapi::setSelectionRanges(loc)
}
