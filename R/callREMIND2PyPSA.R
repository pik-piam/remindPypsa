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

  # REMIND file
  rmFile <- paste0("REMIND2PyPSA_i", iter, ".gdx")

  # REMIND to PyPSA technology mapping (some technologies are aggregated)
  rm2pyTech <- c(
    "biochp" = "biomass",
    "bioigcc" = "biomass",
    "bioigccc" = "biomass",
    "ngcc" = "CCGT",
    "ngccc" = "CCGT",
    "gaschp" = "CCGT",
    "igcc" = "all_coal",
    "igccc" = "all_coal",
    "pc" = "all_coal",
    "coalchp" = "all_coal",
    "tnrs" = "nuclear",
    "fnrs" = "nuclear",
    "ngt" =  "OCGT",
    "windoff" = "all_offwind",
    "dot" = "oil",
    "wind" = "onwind",
    "hydro" = "ror",
    "spv" = "solar")

  # PyPSA technologies to aggregated PyPSA technologies
  py2aggTech <- c(
    "biomass" = "biomass",
    "CCGT" = "CCGT",
    "coal" = "all_coal",
    "lignite" = "all_coal",
    "nuclear" = "nuclear",
    "OCGT" = "OCGT",
    "offwind-ac" = "all_offwind",
    "offwind-dc" = "all_offwind",
    "oil" = "oil",
    "onwind" = "onwind",
    "ror" = "ror",
    "solar" = "solar")

  # Years to couple
  years <- c(seq(2025, 2060, 5), seq(2070, 2110, 10), 2130, 2150)

  # Scale up load time series for all years
  remindPypsa::calcLoad(
    rmFile = rmFile,
    pyLoad = file.path(pyDir, "resources", "RM_Py_default", "load.csv"),
    outDir = file.path(pyDir, "resources", runName),
    years = years
    )

  # Calculate costs for all years
  remindPypsa::calcCosts(
    rmFile = rmFile,
    outDir = file.path(pyDir, "resources", runName),
    years = years,
    rm2pyTech = rm2pyTech,
    py2aggTech = py2aggTech
    )

}
