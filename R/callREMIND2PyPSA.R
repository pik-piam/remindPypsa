#' Wrapper function to exchange data from REMIND to PyPSA
#'
#' @param pyDir PyPSA folder
#' @param iter Current iteration
#'
#' @return Writes data automatically.
#' @export
#'
callREMIND2PyPSA <- function(pyDir, iter) {
  # Get REMIND run name including time stamp
  runName <- stringr::str_extract(getwd(), "(?<=output/).*")

  # Years to couple
  years <- c(seq(2025, 2060, 5), seq(2070, 2110, 10), 2130, 2150)

  # Scale up load time series for all years
  remindPypsa::calcLoad(
    rmFile = paste0("REMIND2PyPSA_i", iter, ".gdx"),
    pyLoad = file.path(pyDir, "resources", "RM_Py_default", "load.csv"),
    outDir = file.path(pyDir, "resources", runName),
    years = years
    )
}
