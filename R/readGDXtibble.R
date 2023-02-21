#' Read data from gdx file and return as tibble with filtered columns.
#'
#' @param rmFile Path to gdx file.
#' @param gdxVar Variable name in gdx file.
#' @param columns Named vector of columns in gdx file to new column names.
#' @param colFilter Named list of new column names to filter values of that column.
#' @param recalcUnit Factor that values are multiplied with.
#'
#' @return Tibble with renamed and filtered columns.
#'
readGDXtibble <- function(rmFile, gdxVar, columns, colFilter, recalcUnit = 1) {
  data <-
    # Read data from gdx file
    gdxdt::readgdx(rmFile, gdxVar) %>%
    tibble::as_tibble(.name_repair = "unique") %>%
    # Select desired columns and rename
    dplyr::select_at(names(columns), ~ columns) %>%
    # Filter columns according to colFilter
    dplyr::filter_at(names(colFilter), dplyr::all_vars(. %in% colFilter$.)) %>%
    # Change units
    dplyr::mutate_at(dplyr::vars(columns["value"]), ~ . * recalcUnit) %>%
    # Transform year into factor (if it exists)
    dplyr::mutate_at(dplyr::vars(dplyr::one_of("year")), ~ as.numeric(.))
  return(data)
}
