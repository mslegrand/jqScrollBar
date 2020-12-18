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
  if(length(choices)>0){

    if(class(choices)=='character'){
      choices<-mapply(function(i){paste0(i,"-rel")}, choices, SIMPLIFY = F, USE.NAMES = T)
    }

    if(!is.null(selected) && class(selected)=="character"){
      selected=selected
    }else if(
      length(selected)==1 &&
      class(selected)=="numeric" &&
      selected>0 &&
      selected<length(choices)
    ){
      selected=names(choices)[[selected]]
    } else {
      selected=names(choices)[1]
    }

    value=toJSON(data.frame(text=selected, rel=choices[[selected]]))

    fn<-function(n,txt){span(rel=n,txt)}
    ll<-mapply(fn, choices, names(choices),SIMPLIFY = FALSE)
  } else {
    ll<-null
    selected=" "
  }

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



