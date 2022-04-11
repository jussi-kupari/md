clear_libraries <- function() {
  invisible(
  lapply(paste("package:", names(sessionInfo()$otherPkgs), sep=""),
         detach, character.only = TRUE, unload = TRUE))

}
