#' Create REMIND-PyPSA validation file
#' This function renders the /inst/rmd/REMIND-PyPSA_Validation.Rmd file.
#' It has to be called from the REMIND output directory.
#'
#' @return REMIND-PyPSA validation file (usually in PDF format)
#' @export
#'
createValidation <- function() {
    # RMarkdown file path
    rmdFile <- system.file("rmd", "REMIND-PyPSA_Validation.Rmd",
                           package = "remindPypsa")
    # Copy RMarkdown file to current directory
    file.copy(rmdFile, getwd())
    # Render RMarkdown file
    rmarkdown::render("REMIND-PyPSA_Validation.Rmd")
}
