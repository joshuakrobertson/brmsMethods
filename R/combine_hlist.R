#' brmsMethod: combine_hlist
#' @export

combine_hlist <- function (hlist, class, alpha) {
    stopifnot(is.list(hlist))
    hs <- do_call(rbind, lapply(hlist, function(h) h$summary))
    rownames(hs) <- NULL
    samples <- lapply(hlist, function(h) h$samples)
    samples <- as.data.frame(do_call(cbind, samples))
    prior_samples <- lapply(hlist, function(h) h$prior_samples)
    prior_samples <- as.data.frame(do_call(cbind, prior_samples))
    names(samples) <- names(prior_samples) <- paste0("H", seq_along(hlist))
    class <- sub("_+$", "", class)
    out <- nlist(hypothesis = hs, samples, prior_samples, class, 
        alpha)
    structure(out, class = "brmshypothesis")
}
