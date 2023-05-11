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

  # Move results from PyPSA directory to REMIND directory
  # Create directory if it does not exist
  if (!dir.exists("pypsa")) {
    dir.create("pypsa")
  }
  pyDirRes <- file.path(pyDir, "results", runName)
  pyResFiles <- list.files(pyDirRes, full.names = TRUE)
  # Copy results
  file.copy(
    from = pyResFiles,
    to = "pypsa",
    recursive = TRUE)
  # Delete original results
  unlink(pyResFiles, recursive = TRUE)

  # Calculate capacity factor
  capfac <- remindPypsa::calcCapfac(
    pyDirRes = "pypsa",
    py2rmTech = py2rmTech,
    py2rmRegi = py2rmRegi,
    iter = iter
  )

  # Combine data
  pypsa2remind <- bind_rows(capfac)

  # Write GDX parameter for REMIND
  # F: Change this to gdx package
  gdxdt::writegdx.parameter(
    gdx = paste0("PyPSA2REMIND_i", iter, ".gdx"),
    dt = data.table::data.table(pypsa2remind),
    name = "PyPSA2REMIND",
    valcol = "value",
    uelcols = c("year", "region", "technology", "var"))

  # Save data for plotting
  pypsa2remindReport <- pypsa2remind %>%
    mutate(year = as.integer(.data$year),
           iter = as.integer(iter))

  # If file exists, append data
  if (file.exists("pypsa2remind.rds")) {
    pypsa2remindTemp <- readRDS("pypsa2remind.rds")
    pypsa2remindReport <- bind_rows(pypsa2remindTemp, pypsa2remindReport)
  }
  # Save as R object
  saveRDS(pypsa2remindReport, file = "pypsa2remind.rds")
  # Save as CSV
  readr::write_csv(pypsa2remindReport, "pypsa2remind.csv")

}
