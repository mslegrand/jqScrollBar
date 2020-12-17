library(shiny)

library(jsonlite)
library(jqScrollBar)

selected='dog'
inputId.1<-"scrollId"
choices=list(
    dog=1,
    cat=2,
    rabbit=3,
    bird=4,
    squirrel=5,
    pig=6,
    horse=7,
    elephant=8
)

ui<-fluidPage(
    h1('Test App'),
    jqScrollBar(inputId=inputId.1,  choices =choices, selected='elephant'),
    jqScrollBar(inputId='tabxx',  choices =choices, selected='elephant'),
    jqScrollBar(inputId='tabyyy',  choices =choices, selected='elephant'),
    h3('current Value'),
    textOutput('currentValue'),

    textInput(inputId='addTabValue','add Tab', ""),
    actionButton('addButton', label='press to add tab'),
    textInput(inputId='removeTabValue','remove Tab', ""),
    actionButton('removeButton', label='press to remove tab by name'),
    textInput(inputId='updateSelectedTab','selected Tab', ""),
    actionButton('updateButton', label='press to update selected tab by name'),
    actionButton('clearButton', label='press to clear'),
    textInput(inputId='renameTab','rename selectedInput Tab to renameInput name'),
    actionButton('renameButton', label='press to rename')

    # actionButton('removeButton', label='press to remove tab by position'),
    # actionButton('removeButton', label='press to update selected tab by position')

)

server<-function(input,output,session){



    output$currentValue<-renderText(input[[ inputId.1 ]])

    observeEvent(input$addButton,{
        value<-input$addTabValue
        value<-unlist(strsplit(value,' '))
        #value<-mapply(function(i){paste0(i,"-rel")}, value, SIMPLIFY = F, USE.NAMES = T)
        tryCatch({
        updateJqScrollBar(session, inputId.1, cmd='add',value=value )
       },
        error=function(e){
            # do nothing , record error
            print('add error')
        })
      },
      ignoreInit = TRUE
    )
  observeEvent(input$removeButton,{
        value<-input$'removeTabValue'
        tryCatch({
            updateJqScrollBar(session, inputId.1, cmd='remove',value=value )
        },
        error=function(e){
            # do nothing , record error
            print('remove error')
        })
      },
      ignoreInit = TRUE
    )

    observeEvent(input$updateButton,{
        value<-input$updateSelectedTab
        tryCatch({
            updateJqScrollBar(session, inputId.1, cmd='select',value=value )
        },
        error=function(e){
            # do nothing , record error
            print('update selection error')
        })
      },
      ignoreInit = TRUE
    )
    observeEvent(input$renameButton,{
        oldName<-input$updateSelectedTab
        newName<-input$renameTab
        value=list(oldName=oldName, newName=newName)
        tryCatch({
            updateJqScrollBar(session, inputId.1, cmd='rename',value=value )
        },
        error=function(e){
            # do nothing , record error
            print('update selection error')
        })
      },
      ignoreInit = TRUE
    )
    observeEvent(input$clearButton,{

        tryCatch({
            updateJqScrollBar(session, inputId.1, cmd='clear',value='value' )
         },
        error=function(e){
            # do nothing , record error
            print('update selection error')
        })
      },
      ignoreInit = TRUE
    )

}

shinyApp(ui=ui, server=server)
