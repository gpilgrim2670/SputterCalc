#' run_app
#'
#' @description used to launch shiny app
#'
#' @author Greg Pilgrim \email{gpilgrim@@vergason.com}
#'
#' @export
#'
#'
run_app <- function() {
  app_directory <- system.file("SputterCalc_App", package = "SputterCalc")
  if (app_directory == "") {
    stop("Could not find app directory. Try re-installing `SputterCalc`.", call. = FALSE)
  }

  shiny::runApp(app_directory, display.mode = "normal")
}
