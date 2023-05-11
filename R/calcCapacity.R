#' Calculate pre-investment capacities (REMIND to PyPSA)
#' This function calculates capacities after
#' This function does two things:
#' 1. Scaling of the PyPSA power plant database for conventional generators and
#' output as powerplants_y***.csv files
#' 2. Output of
#'
#' (i) scales the PyPSA power plant database for conventional
#' generators and exports this as a csv file and (ii) exports another csv file
#' for the pre-investment capacities of renewable generators.
#'
#' @param rmFile REMIND output gdx file
#' @param pyPowerplants PyPSA powerplants.csv file
#' @param rm2pyTech REMIND to aggregated PyPSA technology mapping
#' @param outDir Output folder for csv files
#' @param years Years coupled from REMIND to PyPSA
#'
#' @return Writes one csv file for each year into outDir.
#' @export
#'
calcCapacity <- function(rmFile, pyPowerplants, rm2pyTech, outDir, years) {
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
    mutate(preInvCap = vmCap - (pmDt / 2 * vmDeltaCap) * (1 - vmCapEarlyReti)) %>%
    # Aggregate to PyPSA technologies
    quitte::revalue.levels(technology = rm2pyTech) %>%
    # Take sum with minimum capacity of 0.1 MW to avoid zero capacity in PyPSA
    summarise(preInvCap = max(0.1, sum(preInvCap)))

  # Read powerplants.csv file
  powerplants <- readr::read_csv(pyPowerplants)

  # Mapping of PyPSA technologies to carriers in powerplants
  # Q: Reintroduce waste and other? Which PyPSA techs are these mapped to?
  # Q: What about CHP? Exclude?
  # Q: Why is PyPSA's hydro capacity only 1/5 of REMIND's in 2025?
  pplFuel2pyTech <- c(
    "Nuclear" = "nuclear",
    "Hydro" = "ror",  # Hydro in REMIND is ror + reservoir, but not PHS
    "Lignite" = "all_coal",
    "Hard Coal" = "all_coal",
    "CCGT" = "CCGT",
    "Oil" = "oil",
    "OCGT" = "OCGT",
    "Bioenergy" = "biomass"
  )
  # Loop over all years and export:
  # 1: powerplants_y****.csv and
  # 2: renewables_capacities_y****.csv
  for (y in years){
    # 1: Powerplants database
    # Filter by fuel type, this removes Waste and Other
    powerplantsY <- powerplants %>%
      filter(.data$Fueltype %in% names(pplFuel2pyTech),
             .data$Technology != "Pumped Storage") %>%  # Exclude pumped storage
      # Set capacity to 0.1 MW if DateOut < y
      mutate(DateOut = tidyr::replace_na(.data$DateOut, 2200),  # If missing set to 2200
             Capacity = ifelse(.data$DateOut < y, 0.1, .data$Capacity))

    # Get pre-investment capacity in year y
    preInvCapY <- preInvCap %>%
      filter(.data$year == y) %>%
      dplyr::ungroup()

    # Scale up/down power capacity of power plants according to REMIND data
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

    # Attach pumped storage powerplants unchanged
    powerplantsPHS <- powerplants %>%
      filter(.data$Technology == "Pumped Storage") %>%
      # Set capacity to 0.1 MW if DateOut < y
      mutate(DateOut = tidyr::replace_na(.data$DateOut, 2200),  # If missing set to 2200
             Capacity = ifelse(.data$DateOut < y, 0.1, .data$Capacity))

    # Write csv into PyPSA folder
    readr::write_csv(bind_rows(powerplantsScaled,
                               powerplantsPHS),
                     file.path(outDir, paste0("powerplants_y", y, ".csv")))

    # 2: Renewable capacities
    preInvCapY <- preInvCap %>%
      dplyr::ungroup() %>%
      filter(.data$year == y,
             .data$technology %in% c("onwind", "offwind", "solar")) %>%
      quitte::revalue.levels(technology = c("onwind" = "Onshore",
                                            "offwind" = "Offshore",
                                            "solar" = "PV"),
                             region = c("DEU" = "DE")) %>%
      select("technology", "region", "preInvCap") %>%
      rename("Technology" = "technology", "Country" = "region", "value" = "preInvCap")

    # Write csv into PyPSA folder
    readr::write_csv(preInvCapY, file.path(outDir, paste0("renewable_capacities_y", y, ".csv")))
  }
  # Return for plotting
  preInvCapOut <- preInvCap %>%
    rename("tech" = "technology",
           "value" = "preInvCap") %>%
    mutate(variable = "preInvCap") %>%
    select("year", "region", "tech", "variable", "value")

  return(preInvCapOut)
}
