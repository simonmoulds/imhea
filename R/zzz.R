.onLoad <- function(libname, pkgname) {
  options(digits.secs = 6)
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to imhea!")
}
