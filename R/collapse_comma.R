#' brmsMethod: collapse_comma
#' @export

collapse_comma <- function (...) {
    paste0("'", ..., "'", collapse = ", ")
}