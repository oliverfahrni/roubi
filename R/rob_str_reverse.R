#' str_reverse
#'
#' str_reverse reverses the order of a strings component
#'
#' @param x: input string
#' @return string with its component in reversed order
#' @import tidyverse
#' @export
#' @examples
#' rob_str_reverse("abc")
#' "cba"

rob_str_reverse <- function(x) {
  sapply(lapply(strsplit(x, NULL), rev), paste, collapse="")
}
