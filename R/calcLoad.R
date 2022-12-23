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

  # Read in secondary energy production
  seel <- gdxdt::readgdx(rmFile, "vm_prodSe") %>%
    tibble::as_tibble(.name_repair = "unique") %>%
    # Secondary electricity
    filter(.data$all_enty...4 == "seel",
           .data$all_regi == "DEU",
           .data$tall %in% years) %>%
    group_by(.data$tall, .data$all_regi) %>%
    # Summarise over technologies
    summarise(value = sum(.data$value)) %>%
    mutate(value = 1E6 * 8760 * .data$value) %>%   # TWa to MWh
    quitte::revalue.levels(all_regi = c("DEU" = "DE"))

  # Import original load time series
  load <- readr::read_csv(pyLoad, col_types = c("c", "n"))

  # Loop over all years and export new load time series
  for (y in years){
    # Get sum of load in original time series
    seelY <- seel %>%
      filter(.data$tall == y) %>%
      pull(.data$value)
    # Scale up load time series
    loadScaled <- load %>%
      mutate(DE = seelY / sum(.data$DE) * .data$DE) %>%
      mutate(DE = round(.data$DE, 4))
    # Write csv into PyPSA folder
    readr::write_csv(loadScaled, file.path(outDir, paste0("load_y", y, ".csv")))
  }
}
