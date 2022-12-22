#' Wrapper function to exchange data from PyPSA to REMIND
#'
#' @param pyDir PyPSA folder
#' @param iter Current iteration
#'
#' @return Writes data automatically.
#' @export
#'
callPyPSA2REMIND <- function(pyDir, iter) {
  # Get REMIND run name including time stamp
  runName <- stringr::str_extract(getwd(), "(?<=output/).*")

  # PyPSA to REMIND technology mapping
  py2rmTech <- c(
    "biomass" = "biomass",
    "CCGT" = "CCGT",
    "coal" = "all_coal",  # Aggregate coal
    "lignite" = "all_coal",  # Aggregate coal
    "nuclear" = "nuclear",
    "OCGT" = "OCGT",
    "offwind-ac" = "all_offwind",  # Aggregate offshore wind
    "offwind-dc" = "all_offwind",  # Aggregate offshore wind
    "oil" = "oil",
    "onwind" = "onwind",
    "ror" = "ror",
    "solar" = "solar"
  )

  # PyPSA to REMIND region mapping
  py2rmRegi <- c(
    "DE0" = "DEU"
  )

  # Calculate capacity factor
  capfac <- remindPypsa::calcCapfac(
    pyDirRes = file.path(pyDir, "results", "networks", runName),
    py2rmTech = py2rmTech,
    py2rmRegi = py2rmRegi,
    iter = iter
  )

  # Combine data
  pypsa2remind <- dplyr::bind_rows(capfac)

  # Write GDX parameter
  gdxdt::writegdx.parameter(
    gdx = paste0("PyPSA2REMIND_i", iter, ".gdx"),
    dt = data.table::data.table(pypsa2remind),
    name = "PyPSA2REMIND",
    valcol = "value",
    uelcols = c("year", "region", "tech", "var"))
}
