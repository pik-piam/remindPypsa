#' Calculate capital and marginal costs (REMIND to PyPSA)
#' This function is currently still very messy.
#'
#' @param rmFile REMIND output gdx file
#' @param outDir Output folder for costs_yXXXX.csv files
#' @param years Years coupled from REMIND to PyPSA
#' @param rm2pyTech REMIND to aggregated PyPSA technology mapping
#' @param py2aggTech PyPSA to aggregated PyPSA technology mapping
#'
#' @return Writes one csv file for each year into outDir.
#' @export
#'
calcCosts <- function(rmFile, outDir, years, rm2pyTech, py2aggTech) {
  # Create output folder
  if (!dir.exists(outDir)) dir.create(outDir)

  # Function to calculate annuity with discount rate r and lifetime n
  calcAnnuity <- function(r, n) {
    return(r / (1 - (1 + r)**(-n)))
  }

  # ---------------------------------------------------------------------------
  # Calculate annualised capital costs
  # ---------------------------------------------------------------------------

  # Read in capital costs
  capCostOv <- readGDXtibble(
    rmFile = rmFile,
    gdxVar = "vm_costTeCapital",
    columns = c("all_regi" = "region", "all_te" = "tech", "ttot" = "year", "value" = "capCostOv"),
    colFilter = list("region" = "DEU", "tech" = names(rm2pyTech), "year" = years),
    recalcUnit = 1E6  # T$/TW -> $/MW
  )

  # Read in discount rate
  disRate <- readGDXtibble(
    rmFile = rmFile,
    gdxVar = "p32_PyDisrate",
    columns = c("all_regi" = "region", "ttot" = "year", "value" = "disRate"),
    colFilter = list("region" = "DEU", "year" = years)
  ) %>%
  # Complete dataset with missing years and fill with value from 2130
  group_by(.data$region) %>%
  tidyr::complete(year = years) %>%
  tidyr::fill("disRate", .direction = "down")

  # Read in fixed O&M and lifetime
  pmData <- readGDXtibble(
    rmFile = rmFile,
    gdxVar = "pm_data",
    columns = c("all_regi" = "region", "all_te" = "tech", "char" = "char", "value" = "value"),
    colFilter = list("region" = "DEU", "tech" = names(rm2pyTech), "char" = c("omf", "lifetime"))
  ) %>%
  # Pivot data to wide format with columns for omf and lifetime
  tidyr::pivot_wider(names_from = "char", values_from = "value")

  # Calculate annualised capital costs
  capCostAn <- capCostOv %>%
    # Join discount rate, omf and lifetime
    dplyr::full_join(disRate) %>%
    dplyr::full_join(pmData) %>%
    group_by(.data$region, .data$tech, .data$year) %>%
    # Calculate annualised capital costs
    dplyr::transmute(capCostAn =  (calcAnnuity(.data$disRate, .data$lifetime)
                          + .data$omf
                        )
                        * .data$capCostOv
          )

  # Read in secondary energy electricity production
  # Q: Weighting instead by capacity?
  prodSe <- readGDXtibble(
    rmFile = rmFile,
    gdxVar = "vm_prodSe",
    restoreZeros = FALSE,
    columns = c("all_regi" = "region", "all_enty1" = "enty", "tall" = "year", "all_te" = "tech", "value" = "prodSe"),
    colFilter = list("region" = "DEU", "enty" = "seel", "year" = years, "tech" = names(rm2pyTech))
    ) %>%
    select(!"enty")

  # Calculate average of annualised capital costs weighted by prodSe by aggregated technology
  capCostAgg <- capCostAn %>%
    dplyr::right_join(prodSe) %>%
    mutate(prodSe = ifelse(is.na(.data$prodSe), 0, .data$prodSe)) %>%
    # Calculated average of capCost weighted by prodSe with technology mapping rm2pyTech
    quitte::revalue.levels(tech = rm2pyTech) %>%
    rename(techAgg = "tech") %>%
    group_by(.data$year, .data$region, .data$techAgg) %>%
    summarise(capCost = sum(.data$capCostAn * .data$prodSe) / sum(.data$prodSe)) %>%
    # Remove technologies without capCost (currently csp and geohdr)
    filter(!is.na(.data$capCost))

  # Create new tibble with all PyPSA technologies and copy values from capCostAgg
  # F: Do this for all countries
  capCost <- dplyr::tibble(
      "year" = numeric(),
      "region" = character(),
      "tech" = character()
    ) %>%
    tidyr::complete(year = years, region = "DEU", tech = names(py2aggTech)) %>%
    mutate(techAgg = .data$tech) %>%
    quitte::revalue.levels(techAgg = py2aggTech) %>%
    dplyr::right_join(capCostAgg) %>%
    select(!"techAgg")

  # ---------------------------------------------------------------------------
  # Calculate marginal costs
  # ---------------------------------------------------------------------------

  # Read in variable O&M
  vom <- readGDXtibble(
    rmFile = rmFile,
    gdxVar = "pm_data",
    columns = c("all_regi" = "region", "all_te" = "tech", "char" = "char", "value" = "vom"),
    colFilter = list("region" = "DEU", "tech" = names(rm2pyTech), "char" = "omv"),
    recalcUnit = 1E6 / 8760  # T$/TWa -> $/MWh
  ) %>%
  select(!"char")

  # Read in efficiencies for technologies without time-dependent eta (still converge to global values by 2050)
  # Q: What about nuclear efficiency?
  pmEtaConv <- readGDXtibble(
    rmFile = rmFile,
    gdxVar = "pm_eta_conv",
    columns = c("all_regi" = "region", "all_te" = "tech", "tall" = "year", "value" = "eta"),
    colFilter = list("region" = "DEU", "tech" = names(rm2pyTech), "year" = years)
  )

  # Read in regional efficiencies for technologies with time-dependent eta
  pmDataEta <- readGDXtibble(
    rmFile = rmFile,
    gdxVar = "pm_dataeta",
    columns = c("all_regi" = "region", "all_te" = "tech", "tall" = "year", "value" = "eta"),
    colFilter = list("region" = "DEU", "tech" = names(rm2pyTech), "year" = years)
  )

  # Read in se2fe efficiencies for transmission and distribution (tdels and tdelt)
  # F: Weighted average of tdels and tdelt. Currently not important as they are the same.
  tdEta <- readGDXtibble(
    rmFile = rmFile,
    gdxVar = "pm_eta_conv",
    columns = c("all_regi" = "region", "all_te" = "tech", "tall" = "year", "value" = "tdEta"),
    colFilter = list("region" = "DEU", "tech" = c("tdels", "tdelt"), "year" = years)
    ) %>%
    group_by(.data$region, .data$year) %>%
    summarise(tdEta = mean(.data$tdEta))

  # Combine efficiencies
  eta <- pmEtaConv %>%
    dplyr::bind_rows(pmDataEta) %>%
    # Multiply with tdEta because of se2fe efficiencies
    # This is necessary to represent the full pe2fe efficiency in PyPSA.
    # If prodSe instead of prodFe would be used to scale up demand this could be different.
    dplyr::full_join(tdEta) %>%
    mutate(eta = .data$eta * .data$tdEta) %>%
    select(!"tdEta")

  # Mapping of REMIND technologies to primary energy carriers
  # F: Read in from REMIND
  rm2pe <- c("bioigcc" = "pebiolc",
             "bioigccc" = "pebiolc",
             "biochp" = "pebiolc",
             "ngcc" = "pegas",
             "ngccc" = "pegas",
             "gaschp" = "pegas",
             "igcc" = "pecoal",
             "igccc" = "pecoal",
             "pc" = "pecoal",
             "coalchp" = "pecoal",
             "tnrs" = "peur",
             "fnrs" = "peur",
             "ngt" = "pegas",
             "dot" = "peoil")

  # Read in PE fuel prices in REMIND technologies
  fuelPricePe <- readGDXtibble(
    rmFile = rmFile,
    gdxVar = "pm_PEPrice",
    columns = c("all_regi" = "region", "all_enty" = "peCarrier", "ttot" = "year", "value" = "fuelPrice"),
    colFilter = list("region" = "DEU", "peCarrier" = rm2pe, "year" = years),
    recalcUnit = 1E6 / 8760  # T$/TWa -> $/MWh
  )

  # Map fuel price onto REMIND pe2se technologies
  fuelPrice <- dplyr::tibble(
      "year" = numeric(),
      "region" = character(),
      "tech" = character()
    ) %>%
    tidyr::complete(year = years, region = "DEU", tech = names(rm2pe)) %>%
    mutate(peCarrier = .data$tech) %>%
    quitte::revalue.levels(peCarrier = rm2pe) %>%
    dplyr::right_join(fuelPricePe) %>%
    select(!"peCarrier") %>%
    tidyr::complete(year = years, region = "DEU", tech = names(rm2pyTech), fill = list(fuelPrice = 0))

  # Read in carbon price
  carbonPrice <- readGDXtibble(
    rmFile = rmFile,
    gdxVar = "p_priceCO2",
    columns = c("all_regi" = "region", "tall" = "year", "value" = "carbonPrice"),
    colFilter = list("region" = "DEU", "year" = years),
    recalcUnit = 1 / 3.666666666667  # $/tC -> $/tCO2
    ) %>%
    # Fill in missing values with 0
    tidyr::complete(year = years, region = "DEU", fill = list(carbonPrice = 0))

  # Read in emission intensities
  # Q: biochp and bioigcc have no emission factors
  emiInt <- readGDXtibble(
    rmFile = rmFile,
    gdxVar = "fm_dataemiglob",
    restoreZeros = FALSE,
    columns = c("all_te" = "tech", "all_enty1" = "seCarrier", "all_enty2" = "emiType", "value" = "emiInt"),
    colFilter = list("tech" = names(rm2pyTech), "seCarrier" = "seel", "emiType" = "co2"),
    recalcUnit = 3.666666666667 * 1E3 / 8760  # GtC per TWa -> tCO2 per Mwh
    ) %>%
    select(c("tech", "emiInt")) %>%
    # Temporary workaround: Set other emi factors to zero
    tidyr::complete(tech = names(rm2pyTech), fill = list(emiInt = 0))

  # Calculate marginal costs for REMIND technologies
  margCostRe <- vom %>%
    dplyr::full_join(eta) %>%
    dplyr::full_join(fuelPrice) %>%
    dplyr::full_join(carbonPrice) %>%
    dplyr::full_join(emiInt) %>%
    # Replace na with default values
    group_by(.data$region, .data$tech, .data$year) %>%
    dplyr::transmute(margCost = .data$vom + .data$fuelPrice / .data$eta + .data$carbonPrice * .data$emiInt)

  # Calculate average of marginal costs weighted by prodSe by aggregated technology
  margCostAgg <- margCostRe %>%
    dplyr::right_join(prodSe) %>%
    mutate(prodSe = ifelse(is.na(.data$prodSe), 0, .data$prodSe)) %>%
    # Calculated average of margCost weighted by prodSe with technology mapping rm2pyTech
    quitte::revalue.levels(tech = rm2pyTech) %>%
    rename(techAgg = "tech") %>%
    group_by(.data$year, .data$region, .data$techAgg) %>%
    summarise(margCost = sum(.data$margCost * .data$prodSe) / sum(.data$prodSe)) %>%
    # Remove technologies without margCost (currently csp and geohdr)
    filter(!is.na(.data$margCost))

  # Create new tibble with all PyPSA technologies and copy values from margCostAgg
  margCost <- dplyr::tibble(
      "year" = numeric(),
      "region" = character(),
      "tech" = character()
    ) %>%
    tidyr::complete(year = years, region = "DEU", tech = names(py2aggTech)) %>%
    mutate(techAgg = .data$tech) %>%
    quitte::revalue.levels(techAgg = py2aggTech) %>%
    dplyr::right_join(margCostAgg) %>%
    select(!"techAgg")

  # ---------------------------------------------------------------------------
  # Write output
  # ---------------------------------------------------------------------------
  for (y in years) {
    # Write both capital costs and marginal costs
    capCostY <- capCost %>%
      filter(.data$year == y) %>%
      mutate(unit = "$/MW",
             parameter = "capital_cost") %>%
      rename(value = "capCost")
    margCostY <- margCost %>%
      filter(.data$year == y) %>%
      mutate(unit = "$/MWh",
             parameter = "marginal_cost") %>%
      rename(value = "margCost")
    # Combine costs
    costY <- dplyr::bind_rows(capCostY, margCostY) %>%
      select(!c("year", "region")) %>%
      dplyr::arrange(.data$tech) %>%
      rename(technology = .data$tech) %>%
      select(c("technology", "parameter", "value", "unit"))

    # Write to csv
    readr::write_csv(costY, file.path(outDir, paste0("costs_y", y, ".csv")))
  }
  # ---------------------------------------------------------------------------
  # Return for plotting
  # ---------------------------------------------------------------------------
  costs <- dplyr::full_join(capCost, margCost) %>%
    tidyr::pivot_longer(
      cols = c("capCost", "margCost"),
      names_to = "variable",
      values_to = "value")

  return(costs)
}
