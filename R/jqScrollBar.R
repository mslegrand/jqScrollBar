# library(shiny)
# library(jsonlite)

# try({ shiny::removeInputHandler("jqScrollBarBinding") })



#' @title jqScrollBar
#' @name jqScrollBar
#'
#' @param inputId the id of this shiny input
#' @param value the initial value for this control
#'
#' @export
jqScrollBar<-function(inputId,  choices =choices, selected=null){
# note: use toJSON for non-trivial initializations
  txt=selected
  if(!is.null(selected) && class(selected)=="character"){
      txt=selected
  }else if(
        !is.null(selected) &&
        class(selected)=="numeric" &&
        selected>0 &&
        selected<length(choices)
  ){
      txt=names(choices)[[selected]]
  }
  fn<-function(n,txt){span(rel=n,txt)}
  value=toJSON(data.frame(text=txt, rel=choices[[txt]]))

  ll<-mapply(fn, choices, names(choices),SIMPLIFY = FALSE)
  print('ok')
  tagList(
    # singleton(tags$head(initResourcePaths())),
      #singleton(tags$head(tags$script(src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.0/jquery.min.js"))),
      singleton(tags$head(tags$script(src = "jqScrollBar/jquery.scrolltabs.js"))),
      singleton(tags$head(tags$script(src = "jqScrollBar/jquery.mousewheel.js"))),
      singleton(tags$head(tags$script(src = "jqScrollBar/jqScrollBar.js"))),
      tags$link(rel = "stylesheet", type = "text/css", href = "jqScrollBar/scrolltabs.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "jqScrollBar/ptRScrollTabs.css"),
      div( id=inputId, class="jqScrollBar style1",
                ll,
                `data-ini`=value
           )
  )
}

#' updateJqScrollBar
#' server to client update
#'
#' @param session the shiny session
#' @param inputId the control Id
#' @param value update with this value
#'
#' @export
updateJqScrollBar<-function(session, inputId, cmd,  value='bogus'){
                if(cmd=='add' && class(value)=='character'){
                  value<-mapply(function(i){paste0(i,"-rel")}, value, SIMPLIFY = F, USE.NAMES = T)
                }
            # 2. Form message
                mssg<-list(cmd=cmd, text=value)
            # 3. Send message to client
                session$sendInputMessage(inputId, mssg)
}



