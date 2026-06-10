#' @importFrom rlang inform
.onAttach <- function(libname, pkgname) {
  # inspired by tidyverse, base R approach turns everything yellow...
  # the correct class is retained

  # we cannot use mere inform without the namespace, apparently
  rlang::inform(read_me(), use_cli_format = TRUE, class = "packageStartupMessage")
}
