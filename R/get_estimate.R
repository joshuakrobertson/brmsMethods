#' brmsMethod: get_estimate
#' @export

get_estimate <- function (coef, samples, margin = 2, ...) {
    dots <- list(...)
    args <- list(X = samples, MARGIN = margin, FUN = coef)
    fun_args <- names(formals(coef))
    if (!"..." %in% fun_args) {
        dots <- dots[names(dots) %in% fun_args]
    }
    x <- do_call(apply, c(args, dots))
    if (is.null(dim(x))) {
        x <- matrix(x, dimnames = list(NULL, coef))
    }
    else if (coef == "quantile") {
        x <- aperm(x, length(dim(x)):1)
    }
    x
}