#' Calculate database of powerplants for different years (REMIND to PyPSA)
#' This function scales the PyPSA powerplant database so that the sum
#' matches the pre-investment capacities of REMIND.
#'
#' @param rmFile REMIND output gdx file
#' @param pyPowerplants PyPSA powerplants.csv file
#' @param rm2pyTech REMIND to aggregated PyPSA technology mapping
#' @param outDir Output folder for powerplants_yXXXX.csv files
#' @param years Years coupled from REMIND to PyPSA
#'
#' @return Writes one csv file for each year into outDir.
#' @export
#'
calcPowerplants <- function(rmFile, pyPowerplants, rm2pyTech, outDir, years) {
  # Read in vm_cap
  vmCap <- readGDXtibble(
      rmFile = rmFile,
      gdxVar = "vm_cap",
      columns = c("all_regi" = "region", "tall" = "year", "all_te" = "technology", "rlf" = "rlf", "value" = "vmCap"),
      colFilter = list("region" = "DEU", "year" = years, "technology" = names(rm2pyTech), "rlf" = 1),
      restoreZeros = FALSE,
      recalcUnit = 1E6  # TW to MW
      ) %>%
    select(!"rlf")
  # Read in pm_dt
  pmDt <- readGDXtibble(
      rmFile = rmFile,
      gdxVar = "pm_dt",
      columns = c("tall" = "year", "value" = "pmDt"),
      colFilter = list("year" = years)
      )
  # Read in vm_deltaCap
  vmDeltaCap <- readGDXtibble(
      rmFile = rmFile,
      gdxVar = "vm_deltaCap",
      columns = c("all_regi" = "region", "tall" = "year",
                  "all_te" = "technology", "rlf" = "rlf", "value" = "vmDeltaCap"),
      colFilter = list("region" = "DEU", "year" = years, "technology" = names(rm2pyTech), "rlf" = 1),
      restoreZeros = FALSE,
      recalcUnit = 1E6  # TW to MW
      ) %>%
    select(!"rlf")
  # Read in vm_capEarlyReti
  vmCapEarlyReti <- readGDXtibble(
      rmFile = rmFile,
      gdxVar = "vm_capEarlyReti",
      columns = c("all_regi" = "region", "tall" = "year", "all_te" = "technology", "value" = "vmCapEarlyReti"),
      colFilter = list("region" = "DEU", "year" = years, "technology" = names(rm2pyTech)),
      restoreZeros = FALSE
      )

  # Calculate pre-investment capacities
  preInvCap <- vmCap %>%
    full_join(pmDt) %>%
    full_join(vmDeltaCap) %>%
    full_join(vmCapEarlyReti) %>%
    group_by(.data$region, .data$year, .data$technology) %>%
    mutate(preInvCap = vmCap - (pmDt / 2 * vmDeltaCap) * (1 - vmCapEarlyReti),
           preInvCap = max(0, preInvCap)) %>%
    # Aggregate to PyPSA technologies
    quitte::revalue.levels(technology = rm2pyTech) %>%
    summarise(preInvCap = sum(preInvCap))

  # Read powerplants.csv file
  powerplants <- readr::read_csv(pyPowerplants)

  # Mapping of PyPSA technologies to carriers in powerplants
  # Q: What about Waste, Other and Hydro/ror?
  # Q: What about bioenergy? None left after 2030, is this then excluded?
  # Q: What about renewable capacities?
  # F: Reintroduce waste and other? Which PyPSA techs are these mapped to?
  pplFuel2pyTech <- c(
    "Nuclear" = "nuclear",
    # Hydro ? ror
    "Lignite" = "all_coal",
    "Hard Coal" = "all_coal",
    "CCGT" = "CCGT",
    "Oil" = "oil",
    "OCGT" = "OCGT",
    "Bioenergy" = "biomass"
  )
  # Loop over all years and export new powerplant database
  for (y in years){
    # First, filter power plants that are supposed to be scraped by year y
    powerplantsY <- powerplants %>%
      filter(.data$DateOut > y,
             .data$Fueltype %in% names(pplFuel2pyTech))
    preInvCapY <- preInvCap %>%
      filter(.data$year == y) %>%
      dplyr::ungroup()
    # Scale up powerplants
    powerplantsScaled <- powerplantsY %>%
      mutate(pyTech = .data$Fueltype) %>%
      quitte::revalue.levels(pyTech = pplFuel2pyTech) %>%
      dplyr::left_join(preInvCapY %>%
                  select("technology", "preInvCap"),
                by = c("pyTech" = "technology")) %>%
      group_by(.data$pyTech) %>%
      mutate(Capacity = .data$Capacity * .data$preInvCap / sum(.data$Capacity)) %>%
      dplyr::ungroup() %>%
      select(!c("pyTech", "preInvCap"))
    # Write csv into PyPSA folder
    readr::write_csv(powerplantsScaled, file.path(outDir, paste0("powerplants_", y, ".csv")))
  }
}
