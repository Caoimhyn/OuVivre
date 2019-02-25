#' The launcher App
#'
#' @return nothing but launch the app
#'
launcher <-
  function() {
    appDir <-
      system.file("app", package = "OuVivre")
    shiny::runApp(appDir, display.mode = "normal")
  }
