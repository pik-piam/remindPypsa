#' Wrapper function to exchange data from REMIND to PyPSA
#'
#' @param pyDir PyPSA model directory
#' @param iter Current iteration
#'
#' @return Writes data automatically.
#' @export
#'
#' @examples
#' \dontrun{
#' callREMIND2PyPSA(pyDir = ".../pypsa-eur", iter = 5)
#' }
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
    "windoff" = "offwind",
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
    "offwind" = "offwind",
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
  costs <- remindPypsa::calcCosts(
    rmFile = rmFile,
    outDir = file.path(pyDir, "resources", runName),
    years = years,
    rm2pyTech = rm2pyTech,
    py2aggTech = py2aggTech
    )

  # Adjust powerplant database for all years
  remindPypsa::calcPowerplants(
    rmFile = rmFile,
    pyPowerplants = file.path(pyDir, "resources", "RM_Py_default", "powerplants.csv"),
    rm2pyTech = rm2pyTech,
    outDir = file.path(pyDir, "resources", runName),
    years = years
    )

  # Combine data
  remind2pypsa <- bind_rows(costs)

  # Save data for plotting
  remind2pypsaReport <- remind2pypsa %>%
    mutate(iter = iter)
  # If file exists, append data
  if (file.exists("remind2pypsa.rds")) {
    remind2pypsaTemp <- readRDS("remind2pypsa.rds")
    remind2pypsaReport <-
      bind_rows(remind2pypsaTemp, remind2pypsaReport)
  }
  # Save as R object
  saveRDS(remind2pypsaReport, file = "remind2pypsa.rds")
  # Save as CSV
  readr::write_csv(remind2pypsaReport, "remind2pypsa.csv")

}
