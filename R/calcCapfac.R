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
    genP <- readr::read_csv(file.path(d, "generators-p.csv"),
                    col_types = "n") %>%
      rename("hour" = "...1") %>%
      tidyr::pivot_longer(
        cols = !"hour",
        names_to = c("region", "bus", "technology"),
        names_sep = " ",
        values_to = "p"
      ) %>%
      # Sum over year
      group_by(.data$region, .data$technology, .data$bus) %>%
      summarise(p_sum = sum(.data$p))

    # Get optimal nominal capacity
    genPnom <- readr::read_csv(file.path(d, "generators.csv"),
                  show_col_types = FALSE) %>%
      select("name", "p_nom_opt") %>%
      tidyr::separate(.data$name,
               into = c("region", "bus", "technology"),
               sep = " ")

    # Calculate capacity factor
    calc <- full_join(genP, genPnom) %>%
      # Aggregate technologies
      quitte::revalue.levels(technology = py2rmTech,
                             region = py2rmRegi) %>%
      group_by(.data$region, .data$technology, .data$bus) %>%
      summarise(p_sum = sum(.data$p_sum),
                p_nom_opt = sum(.data$p_nom_opt)) %>%
      # Filter technologies that have less than 1 MW in total
      filter(sum(.data$p_nom_opt) > 1) %>%
      # Calculate capacity factors over regions
      summarise(value = sum(.data$p_sum) / (sum(.data$p_nom_opt) * 8760)) %>%
      # Add year
      mutate(year = y,
             var = "capfac") %>%
      select("year", "region", "technology", "var", "value")

    # Append to output
    cf <- bind_rows(cf, calc)
  }
  return(cf)
}
