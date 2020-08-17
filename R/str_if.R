#' brmsMethod: str_if
#' @export

str_if <- function (cond, yes, no = "") {
    cond <- as_one_logical(cond)
    if (cond) 
        as.character(yes)
    else as.character(no)
}