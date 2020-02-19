check_pkg_deps <- function() {
  if(!require(dplyr)) {
    stop("the 'dplyr' package needs to be installed first")
  }
  if(!require(tidyr)) {
    stop("the 'tidyr' package needs to be installed first")
  }
  if(!require(stringr)) {
    stop("the 'stringr' package needs to be installed first")
  }
}
