
switch_r <- function(wd) {
  if (!dir.exists(wd)) {
    stop("The specified directory does not exist.")
  }

  rm(list = ls(all.names = TRUE, envir = .GlobalEnv), envir = .GlobalEnv)

  setwd(wd)

  if (file.exists(".Renviron")) {
    readRenviron(".Renviron")
  }

  if (file.exists(".Rprofile")) {
    source(".Rprofile", local = .GlobalEnv)
  }

  if (file.exists(".RData")) {
    load(".RData", envir = .GlobalEnv)
  }
  if (interactive() && file.exists(".Rhistory")) {
    loadhistory(".Rhistory")
  }

  if (exists(".First", envir = .GlobalEnv, inherits = FALSE)) {
    do.call(".First", list(), envir = .GlobalEnv)
  }

  invisible(NULL)
}

