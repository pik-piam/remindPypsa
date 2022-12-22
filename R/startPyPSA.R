#' Start PyPSA
#'
#' @param pyDir PyPSA folder
#' @param iter Current iteration
#'
#' @return None.
#' @export
#'
startPyPSA <- function(pyDir, iter) {
  # Get REMIND run name including time stamp
  runName <- stringr::str_extract(getwd(), "(?<=output/).*")
  # Copy required configuration files from inst/extdata into PyPSA directory
  pyFiles <- c(
    "config.REMIND.yaml",
    "Snakefile_REMIND_prepare",
    "Snakefile_REMIND_solve",
    "StartPyPSA.sh")
  dirPckg <- system.file("extdata", package = "remindPypsa")
  for (p in pyFiles) {
    # Copy if file doesn't exist
    if (!file.exists(file.path(pyDir, p))) {
      file.copy(
        from = file.path(dirPckg, p),
        to = pyDir
      )
    }
  }
  # Start PyPSA
  command <- paste(file.path(pyDir, "StartPyPSA.sh"), iter, runName)
  system(command)
}
