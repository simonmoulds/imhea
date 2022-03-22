.onLoad <- function(libname, pkgname) {
  options(digits.secs = 6)
}

.onAttach <- function(libname, pkgname) {
  packageStartUpMessage("Welcome to imhea!")
}
