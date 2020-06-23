#' run_app
#'
#' @author Greg Pilgrim \email{gpilgrim@@vergason.com}
#'
#' @export
#'
#'
run_app <- function() {
  app_directory <- system.file("CrV_App", package = "CrV")
  if (app_directory == "") {
    stop("Could not find app directory. Try re-installing `CrV Calculator`.", call. = FALSE)
  }

  shiny::runApp(app_directory, display.mode = "normal")
}
