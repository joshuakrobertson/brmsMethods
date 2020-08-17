#' brmsMethod: hypothesis_df
#'
#' Evaluate a non-linear hypothesis using Savage-Dickey method with data frame.
#' @param h A hypothesis to evaluate, in string form.
#' @param x A data frame containing necessary values for hypothesis to be evaluated. Note that this dataframe must contain both prior data and true data for evaluation to be effective.
#' @param class The class of predictors with which the hypothesis will evaluate. Default is b.
#' @param alpha Alpha values used for hypothesis test.
#' @import tidyverse, mgsub
#' @export

hypothesis_df <- function (h, x, class = "b", alpha, name = NULL) {
    require('tidyverse')
    require('mgsub')

    stopifnot(length(h) == 1L && is.character(h))
    pars <- parnames(x)[grepl(paste0("^", class), parnames(x))]
    h <- gsub("[ \t\r\n]", "", h)
    sign <- get_matches("=|<|>", h)
    lr <- get_matches("[^=<>]+", h)
    if (length(sign) != 1L || length(lr) != 2L) {
        stop2("Every hypothesis must be of the form 'left (= OR < OR >) right'.")
    }
    h <- paste0("(", lr[1], ")")
    h <- paste0(h, ifelse(lr[2] != "0", paste0("-(", lr[2], ")"), 
        ""))
    varsH <- find_vars(h)
    parsH <- paste0(class, "_", varsH)
    miss_pars <- setdiff(parsH, pars)
    if (length(miss_pars)) {
        miss_pars <- collapse_comma(miss_pars)
        stop("Some parameters cannot be found in the model: \n", 
            miss_pars)
    }
    h_renamed <- mgsub::mgsub(h, c(":", "\\[", "\\]", ","), c("___", ".", ".", ".."))
    samples <- x %>% dplyr::select(c(parsH))
    names(samples) <- mgsub::mgsub(names(samples), c("b_", ":", "\\[", "\\]", ","), 
        c("", "___", ".", ".", ".."))
    samples <- as.matrix(eval2(h_renamed, samples))
    prior_pull <- paste0("prior_", parsH)
    prior_samples <- x %>% dplyr::select(c(prior_pull))
    if (!is.null(prior_samples) && ncol(prior_samples) == length(varsH)) {
        names(prior_samples) <- mgsub::mgsub(names(prior_samples), c("prior_", "b_", ":", "\\[", "\\]", ","), 
        c("", "", "___", ".", ".", ".."))
        prior_samples <- as.matrix(eval2(h_renamed, prior_samples))
    }
    else {
        prior_samples <- NULL
    }
    wsign <- switch(sign, `=` = "equal", `<` = "less", `>` = "greater")
    probs <- switch(sign, `=` = c(alpha/2, 1 - alpha/2), `<` = c(alpha, 
        1 - alpha), `>` = c(alpha, 1 - alpha))
    sm <- lapply(c("mean", "sd", "quantile", "evidence_ratio"), 
        get_estimate, samples = samples, probs = probs, wsign = wsign, 
        prior_samples = prior_samples)
    sm <- as.data.frame(matrix(unlist(sm), nrow = 1))
    names(sm) <- c("Estimate", "Est.Error", "CI.Lower", "CI.Upper", 
        "Evid.Ratio")
    sm$Post.Prob <- sm$Evid.Ratio/(1 + sm$Evid.Ratio)
    if (is.infinite(sm$Evid.Ratio)) {
        sm$Post.Prob <- 1
    }
    if (sign == "=") {
        sm$Star <- str_if(!(sm$CI.Lower <= 0 && 0 <= sm$CI.Upper), 
            "*")
    } else {
        sm$Star <- str_if(sm$Post.Prob > 1 - alpha, "*")
    }
    name <- names(h)
    if (!length(name) || !nzchar(name)) {
        name <- paste(h, sign, "0")
    }
    sm$Hypothesis <- as_one_character(name)
    sm <- move2start(sm, "Hypothesis")
    if (is.null(prior_samples)) {
        prior_samples <- as.matrix(rep(NA, nrow(samples)))
    }
    out <- list(nlist(summary = sm, samples, prior_samples))
    out <- combine_hlist(out, class = class, alpha = alpha)
}
