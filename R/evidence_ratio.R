#' brmsMethod: evidence_ratio
#' @export

evidence_ratio <- function (x, cut = 0, wsign = c("equal", "less", "greater"), 
    prior_samples = NULL, ...) {
    wsign <- match.arg(wsign)
    if (wsign == "equal") {
        if (is.null(prior_samples)) {
            out <- NA
        }
        else {
            out <- density_ratio(x, prior_samples, point = cut, 
                ...)
        }
    }
    else if (wsign == "less") {
        out <- length(which(x < cut))
        out <- out/(length(x) - out)
    }
    else if (wsign == "greater") {
        out <- length(which(x > cut))
        out <- out/(length(x) - out)
    }
    out
}
