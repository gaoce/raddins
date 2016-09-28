#' View Selected Object
#' View Selected Object
#'
#' @export
view_name = function() {
    context = rstudioapi::getSourceEditorContext()
    if (length(context$selection) == 1 & context$selection[[1]]$text != '') {
        obj_name = context$selection[[1]]$text
        expr_str = sprintf('View(%s)', obj_name)
        eval(parse(text = expr_str), envir = globalenv())
    }
}
