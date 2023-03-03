#' Plot capacity factors
#'
#' @param resDir REMIND directory
#' @param years Years coupled for REMIND-PyPSA
#' @param rm2pyTech REMIND to aggregated PyPSA technology mapping
#'
#' @return ggplot2 object
#' @export
#'
plotCapfac <- function(resDir, years, rm2pyTech) {
  # Read in data passed from PyPSA to REMIND
  pypsa2remind <- readRDS(file.path(resDir, "pypsa2remind.rds")) %>%
    filter(.data$var == "capfac")
  # Get REMIND fulldata files
  rmFiles <- list.files(resDir, pattern = "fulldata\\_\\d{1,}.gdx", full.names = TRUE)
  vmCapFac <- NULL
  vmCapDistr <- NULL
  for (r in rmFiles) {
    # Get iteration number from file name
    iter <- stringr::str_extract(r, "(?<=_)\\d{1,}(?=.gdx)") %>%
      as.integer()
    # Read vm_capFac
    temp <- readGDXtibble(
        rmFile = r,
        gdxVar = "vm_capFac",
        columns = c("ttot" = "year", "all_regi" = "region", "all_te" = "technology", "value" = "vmCapFac"),
        colFilter = list("year" = years, "region" = "DEU", "technology" = names(rm2pyTech)
        )) %>%
      mutate(iter = iter)
    vmCapFac <- bind_rows(temp, vmCapFac)
    # Read vm_capDistr
    temp <- readGDXtibble(
      rmFile = r,
      gdxVar = "vm_capDistr",
      columns = c("tall" = "year", "all_regi" = "region",
                  "all_te" = "technology", "rlf" = "rlf", "value" = "vmCapDistr"),
      colFilter = list("year" = years, "region" = "DEU", "technology" = names(rm2pyTech)),
      restoreZeros = FALSE
    ) %>%
      mutate(iter = iter)
    vmCapDistr <- bind_rows(temp, vmCapDistr)
  }
  # Read in pm_dataren (nur), simply use the last remind file as it's unchanged
  pmDataRen <- readGDXtibble(
    rmFile = r,
    gdxVar = "pm_dataren",
    columns = c("all_regi" = "region", "all_te" = "technology", "char" = "char", "rlf" = "rlf", "value" = "pmDataRen"),
    colFilter = list("region" = "DEU", "char" = "nur", "technology" = names(rm2pyTech)),
    restoreZeros = FALSE
  )

  # Capacity factor for rlf technologies
  teReNoBio <- gdx::readGDX(r, "teReNoBio", restore_zeros = FALSE)
  capfacRlf <- vmCapFac %>%
    filter(.data$technology %in% teReNoBio) %>%
    full_join(vmCapDistr %>%
                filter(.data$technology %in% teReNoBio)) %>%
    full_join(pmDataRen %>%
                filter(.data$technology %in% teReNoBio)) %>%
    # Filter away rlfs that are not used
    filter(!is.na(pmDataRen)) %>%
    group_by(.data$year, .data$region, .data$technology, .data$iter) %>%
    summarise(capfac = sum(vmCapFac * vmCapDistr * pmDataRen) / sum(vmCapDistr))

  # Capacity factor for non-rlf technologies
  capfacNoRlf <- vmCapFac %>%
    filter(!(.data$technology %in% teReNoBio)) %>%
    group_by(.data$year, .data$region, .data$technology, .data$iter) %>%
    dplyr::transmute(capfac = vmCapFac)

  # Combine rlf and non-rlf capacity factors
  remind <- bind_rows(capfacRlf, capfacNoRlf) %>%
    # Downscale to PyPSA resolution
    quitte::revalue.levels(technology = rm2pyTech) %>%
    group_by(.data$year, .data$region, .data$technology, .data$iter) %>%
    mutate(dummy = as.integer(dplyr::n_distinct(.data$capfac) == 1))

  # Throw warning if some capacity factors don't match within group
  if (any(remind$dummy == 0)) {
    techProb <- as.character(unique(remind$technology[remind$dummy == 0]))
    warning(paste("Capacity factors for", techProb, "don't match."))
    }

  # Reduce dimensionality
  remind <- remind %>%
    summarise(capfac = dplyr::first(.data$capfac))

  # Plot over years
  p1 <- ggplot() +
    geom_line(data = remind,
              mapping = aes(x = .data$year, y = .data$capfac, color = .data$technology)) +
    geom_point(data = pypsa2remind,
               mapping = aes(x = .data$year, y = .data$value, color = .data$tech)) +
    facet_wrap(~.data$iter) +
    xlab("Year") +
    ylab("Capacity factor [1]") +
    theme_bw()

  # Plot over iterations
  p2 <- ggplot() +
    geom_line(data = remind,
              mapping = aes(x = .data$iter, y = .data$capfac, color = .data$technology)) +
    geom_point(data = pypsa2remind,
               mapping = aes(x = .data$iter, y = .data$value, color = .data$tech)) +
    facet_wrap(~.data$year) +
    xlab("Iteration") +
    ylab("Capacity factor [1]") +
    theme_bw()

  return(list(p1, p2))
}
