#' Calculate capacity factors (PyPSA to REMIND)
#'
#' @param pyDirRes PyPSA results folder with sub-folders for years
#' @param py2rmTech PyPSA to REMIND technology mapping
#' @param py2rmRegi PyPSA to REMIND region mapping
#' @param iter Current iteration
#'
#' @return Tibble with capacity factors for various years.
#' @export
#'
calcCapfac <- function(pyDirRes, py2rmTech, py2rmRegi, iter) {
  # Initialise output
  cf <- NULL

  # Get directories
  dirs <- list.dirs(path = pyDirRes,
                    recursive = FALSE) %>%
    # Only includes directories of current iteration
    stringr::str_subset(pattern = paste0("i", iter))

  # Print status
  cat(length(dirs), "PyPSA output directories found.")

  # Loop over yearly PyPSA output
  for (d in dirs) {
    # Get year
    y <- stringr::str_extract(d, "(?<=y)\\d{4}")

    # Get production
    gP <- readr::read_csv(file.path(d, "generators-p.csv"),
                    col_types = "n") %>%
      rename(hour = .data$...1) %>%
      tidyr::pivot_longer(
        cols = !.data$hour,
        names_to = c("region", "bus", "tech"),
        names_sep = " ",
        values_to = "p"
      ) %>%
      # Sum over year
      group_by(.data$region, .data$tech, .data$bus) %>%
      summarise(p_sum = sum(.data$p))

    # Get optimal nominal capacity
    g <- readr::read_csv(file.path(d, "generators.csv"),
                  show_col_types = FALSE) %>%
      select(.data$name, .data$p_nom_opt) %>%
      tidyr::separate(.data$name,
               into = c("region", "bus", "tech"),
               sep = " ")

    # Calculate capacity factor
    calc <- dplyr::full_join(g, gP) %>%
      # Aggregate technologies
      quitte::revalue.levels(tech = py2rmTech,
                     region = py2rmRegi) %>%
      group_by(.data$region, .data$tech, .data$bus) %>%
      summarise(p_sum = sum(.data$p_sum),
                p_nom_opt = sum(.data$p_nom_opt)) %>%
      # Calculate capacity factors over regions
      summarise(value = sum(.data$p_sum) / (sum(.data$p_nom_opt) * 8760)) %>%
      # Add year
      mutate(year = y,
             var = "capfac") %>%
      select(.data$year, .data$region, .data$tech, .data$var, .data$value)

    # Append to output
    cf <- dplyr::bind_rows(cf, calc)
  }
  return(cf)
}
