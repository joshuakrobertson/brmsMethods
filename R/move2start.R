#' brmsMethod: move2start
#' @export

move2start <- function (x, first) {
    x[c(first, setdiff(names(x), first))]
}