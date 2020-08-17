#' brmsMethod: find_vars
#' @export

find_vars <- function (x, dot = TRUE, brackets = TRUE) {
    x <- gsub("[[:space:]]", "", as_one_character(x))
    dot <- as_one_logical(dot)
    brackets <- as_one_logical(brackets)
    regex_all <- paste0("([^([:digit:]|[:punct:])]", if (dot) 
        "|\\.", ")", "[[:alnum:]_\\:", if (dot) 
        "\\.", "]*", if (brackets) 
        "(\\[[^],]+(,[^],]+)*\\])?")
    pos_all <- gregexpr(regex_all, x)[[1]]
    regex_fun <- paste0("([^([:digit:]|[:punct:])]", if (dot) 
        "|\\.", ")", "[[:alnum:]_", if (dot) 
        "\\.", "]*\\(")
    pos_fun <- gregexpr(regex_fun, x)[[1]]
    pos_decnum <- gregexpr("\\.[[:digit:]]+", x)[[1]]
    keep <- !pos_all %in% c(pos_fun, pos_decnum)
    pos_var <- pos_all[keep]
    attr(pos_var, "match.length") <- attributes(pos_all)$match.length[keep]
    if (length(pos_var)) {
        out <- unique(unlist(regmatches(x, list(pos_var))))
    }
    else {
        out <- character(0)
    }
    out
}