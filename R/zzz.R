#' @noRd
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("persval: computing personal values scores.")
  packageStartupMessage("For more information on getting started, type ?persval.")
  packageStartupMessage("For suggestions or to report issues, please contact Giuseppe Corbelli at giuseppe.corbelli@uninettunouniversity.net or giuseppe.corbelli@uniroma1.it.")
}
