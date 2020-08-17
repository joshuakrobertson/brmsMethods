#' brmsMethod: eval2
#' @export

eval2 = function (expr, envir = parent.frame()) {
    if (is.character(expr)) {
        expr <- str2expression(expr)
    }
    eval(expr, envir)
}