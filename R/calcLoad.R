#' Calculate multiple load time series for different years (REMIND to PyPSA)
#'
#' @param rmFile REMIND output gdx file
#' @param pyLoad PyPSA load.csv file
#' @param outDir Output folder for load_yXXXX.csv files
#' @param years Years coupled from REMIND to PyPSA
#'
#' @return Writes one csv file for each year into outDir.
#' @export
#'
calcLoad <- function(rmFile, pyLoad, outDir, years) {
  # Create output folder
  if (!dir.exists(outDir)) dir.create(outDir)

  # Read in final energy production of electricity (stationary + transport)
  prodFe <- readGDXtibble(
    rmFile = rmFile,
    gdxVar = "vm_prodFe",
    restoreZeros = FALSE,
    columns = c("all_regi" = "region", "all_enty" = "seCarrier",
    "all_enty1" = "FeCarrier", "ttot" = "year", "value" = "prodFe"),
    colFilter = list("region" = "DEU", "seCarrier" = "seel", "FeCarrier" = c("feels", "feelt"), "year" = years),
    recalcUnit = 1E6 * 8760  # TWa to MWh
  ) %>%
  group_by(.data$region, .data$year) %>%
  summarise(prodFe = sum(.data$prodFe))

  # Import original load time series
  load <- readr::read_csv(pyLoad, col_types = c("c", "n"))

  # Loop over all years and export new load time series
  for (y in years){
    # Get sum of load in original time series
    prodFeY <- prodFe %>%
      filter(.data$year == y) %>%
      pull(.data$prodFe)
    # Scale up load time series
    loadScaled <- load %>%
      mutate(DE = prodFeY / sum(.data$DE) * .data$DE) %>%
      mutate(DE = round(.data$DE, 4))
    # Write csv into PyPSA folder
    readr::write_csv(loadScaled, file.path(outDir, paste0("load_y", y, ".csv")))
  }
}
