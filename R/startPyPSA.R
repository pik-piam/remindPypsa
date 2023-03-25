#' Start PyPSA
#'
#' @param pyDir PyPSA folder
#' @param iter Current iteration
#' @param copyConfig Logical indicating whether to copy PyPSA config files from inst/extdata
#'
#' @return None.
#' @export
#'
startPyPSA <- function(pyDir, iter, copyConfig = TRUE) {
  # Get REMIND run name including time stamp
  runName <- stringr::str_extract(getwd(), "(?<=output/).*")
  if (copyConfig) {
    # Copy required configuration files from inst/extdata into PyPSA directory
    # This could get moved to remind/scripts/start/prepare_and_run.R in the future
    pyFiles <- c(
      "config.remind.yaml",
      "Snakefile_remind",
      "StartPyPSA.sh")
    dirPckg <- system.file("extdata", package = "remindPypsa")
    # Copy and overwrite if file already exists
    file.copy(
      from = file.path(dirPckg, pyFiles),
      to = pyDir,
      overwrite = TRUE
    )
    # Copy SLURM configuration file into cluster_config directory
    dirCluster <- file.path(pyDir, "cluster_config")
    if (!dir.exists(dirCluster)) {
      dir.create(dirCluster)
    }
    file.copy(
      from = file.path(dirPckg, "config.yaml"),
      to = dirCluster,
      overwrite = TRUE
    )
    # Enable execution of StartPyPSA.sh for owner (group read+write, others read)
    Sys.chmod(paths = file.path(pyDir, "StartPyPSA.sh"),
              mode = "0764")
  }
  # Start PyPSA
  command <- paste(file.path(pyDir, "StartPyPSA.sh"), iter, runName)
  system(command)
}
