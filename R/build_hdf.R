#' brmsMethod: build_hdf
#' Build a dataframe to load into the hypothesis_df function to evaluate a non-linear hypothesis using the Savage-Dickey method.
#' @param vars A list containing vectors of data to be used for evaluation of non-linear hypothesis test.
#' @param priors A list containing vectors, each of which contain prior samples for variables loaded in vars.
#' @param names A vector of names corresponding to variables loaded in vars.
#' @param class The class of predictors with which the hypothesis will evaluate.
#' @import tidyverse
#' @export

build_hdf <- function (vars, priors, names, class = "b"){
    require('tidyverse')
    new_names <- paste(class, names, sep = "_")
    prior_names <- paste("prior", new_names, sep = "_")
    out <- cbind(do.call(cbind.data.frame, vars), do.call(cbind.data.frame, priors))
    colnames(out) <- c(new_names, prior_names)
    return(out)
}