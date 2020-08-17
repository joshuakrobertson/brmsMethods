#' brmsMethod: deparse_combine
#' @export

deparse_combine <- function (x, max_char = NULL) {
    out <- collapse(deparse(x))
    if (isTRUE(max_char > 0)) {
        out <- substr(out, 1L, max_char)
    }
    out
}
