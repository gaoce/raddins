# Project related functions

#' @title from_import imports specific objects from a package
#' @description It attempts to mimic python's `import ... from ...` construct
#'
#' @param pkg package name, a string
#' @param objs a vector of obj names
#' @param as aliases of the object in the current environment
#' @param .hidden logical, import the hidden obj or not
#'
#' @export
#'
#' @examples
#' from_import('dplyr', 'select', 'dselect')
#' from_import('AnnotationDbi', 'select', 'aselect')
from_import = function(pkg, objs, as = NULL, .hidden = F) {
    # If not given, use obj's name as aliases
    if (is.null(as)) as = objs
    # Import hidden object or not
    if (.hidden) sep = ':::' else sep = '::'

    # Get from_import's parent.frame
    pf = parent.frame()

    # Assignments
    mapply(function(obj, alias) {
        assign(alias, eval(parse(text = paste(pkg, obj, sep = sep))),
               envir = pf)
    }, objs, as)

    invisible()
}
