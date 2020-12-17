#' Adds the content of www to jqScrollBar/
#'
#' @importFrom shiny addResourcePath registerInputHandler
#'
#' @noRd
#'
.onLoad <- function(...) {
  shiny::addResourcePath(
    prefix = 'jqScrollBar',
    directoryPath = system.file('www', package='jqScrollBar')
  )
  try({ shiny::removeInputHandler("jqScrollBarBinding") })
  shiny::registerInputHandler(
    "jqScrollBarBinding",
    function(value, shinysession, inputId) {
      if(is.null(value) ) {
        return("NULL")
      } else {
        value<-fromJSON(value)$text
        return(value)
      }
    },
    force=FALSE
  )
}
