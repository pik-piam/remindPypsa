#' Read data from gdx file and return as tibble with filtered columns.
#'
#' @param rmFile Path to gdx file.
#' @param gdxVar Variable name in gdx file.
#' @param columns Named vector of columns in gdx file to new column names.
#' @param colFilter Named list of new column names to filter values of that column.
#' @param restoreZeros Logical indicating whether to restore zeros in readGDX.
#' @param field Field of variable to read from gdx file.
#' @param recalcUnit Factor that values are multiplied with.
#'
#' @return Tibble with renamed and filtered columns.
#'
readGDXtibble <- function(rmFile, gdxVar, columns, colFilter,
                          restoreZeros = TRUE, field = "l", recalcUnit = 1) {
  data <-
    # Read data from gdx file
    gdx::readGDX(rmFile, gdxVar, restore_zeros = restoreZeros, field = field) %>%
    tibble::as_tibble(.name_repair = "unique") %>%
    # Select desired columns and rename
    dplyr::select_at(names(columns), ~ columns) %>%
    # Filter columns according to colFilter
    dplyr::filter_at(names(colFilter), dplyr::all_vars(. %in% colFilter$.)) %>%
    # Change units
    dplyr::mutate_at(dplyr::vars(columns["value"]), ~ . * recalcUnit)
  return(data)
}
