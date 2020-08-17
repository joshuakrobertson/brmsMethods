#' brmsMethod: as_one_logical
#' @export

as_one_logical <- function (x, allow_na = FALSE) {
    s <- substitute(x)
    x <- as.logical(x)
    if (length(x) != 1L || anyNA(x) && !allow_na) {
        s <- deparse_combine(s, max_char = 100L)
        stop2("Cannot coerce '", s, "' to a single logical value.")
    }
    x
}
