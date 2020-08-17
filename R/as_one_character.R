#' brmsMethod: as_one_character
#' @export

as_one_character <- function (x, allow_na = FALSE) {
    s <- substitute(x)
    x <- as.character(x)
    if (length(x) != 1L || anyNA(x) && !allow_na) {
        s <- deparse_combine(s, max_char = 100L)
        stop2("Cannot coerce '", s, "' to a single character value.")
    }
    x
}