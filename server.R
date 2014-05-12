# Server for Clicker App
require(RCurl)

#### Shared Variables

# Only one instance will be made of these across all sessions

currentAnswers <- NULL

# All the slide information, including HTML
slideInfo <- list() # read in later

# The HTML of the slide currently being displayed
slideContentsGlobal <- "<h3>No slide yet selected by leader.</h3>"

globalSlideStyle <- 'Alphabetic' # to set the mode of submissions

#### Instructor Variables
# The file (or URL) containing the list of slides
controlFileURL <- "ControlFile.csv"

# This will be read in from the control file
controlSlideTable <- NULL 

# Reactive variable to drive slide display update
# see output$slideMaterial below

globalSlideNumber <- 0
makeReactiveBinding("globalSlideNumber")

# Whatever the file says, turn the style into one of the supported styles
standardizeStyle <- function(slideStyle) {
  res <- 'Alphabetic'
  if (grepl('[A|a]lpha',slideStyle)) res <- 'Alphabetic'
  if (grepl('[S|s]hort',slideStyle)) res <- 'Short'
  if (grepl('[T|t]ext',slideStyle)) res <- 'Text'
  if (grepl('[M|m]ulti',slideStyle)) res <- 'Multi'
  return(res)
}
#### The server itself

clickerServer <- function(input,output,session){

  # Set up this session's state
  makeReactiveBinding("updateDisplay")
  updateDisplay <- 0 # local to this session

  
  makeReactiveBinding('answerDisplayTrigger')
  answerDisplayTrigger <- 0 # local to this session
  
  # Give each new user a random ID
  myRandomID <- paste('User',
                      round(runif(1,0,10000000)),sep='')
  updateTextInput(session,'userID',value=myRandomID)
  
  # respond to events
  # Display the Slide HTML and the correct answer mode
  output$slideMaterial <- renderText({
    
    updateDisplay # for the dependency
    
    # Change the tab for user responses
    answerTabToShow <- 'Login' 
    if( length(slideInfo)>0 & globalSlideNumber>0) {
      slideStyle <- slideInfo[[isolate(globalSlideNumber)]]$style
      # updateTextInput(session,'slideStyle',value=globalSlideStyle)
      globalSlideStyle <<- slideStyle
      # THIS NEXT LINE IS PROBABLY DEFUNCT ...
      answerTabToShow <- switch(slideStyle, Short='Short',
                                Text='Text',
                                Alphabetic='Alphabetic')
      # output$answerInput <- renderUI(HTML(answerTabToShow))
      output$answerInput <- renderUI({
        switch(slideStyle,
               Short= textInput(inputId="shortAnswer2",
                  label="Your Answer:",
                  value=""),
               Alphabetic=list(
                 tags$head(tags$style(type="text/css",
                                      "label.radio { display: inline-block; padding-right:15px; margin-left=0px; }",
                                      ".radio input[type=\"radio\"] { float: none; }"
                 )),
                 radioButtons(inputId="letterAnswer2",
                                           label="Choice:",
                                           choices=c('A'="A", 'B'="B", 'C'="C", 'D'="D",
                                                     'E'="E", 'F'="F", 'G'="G", 'H'="H",
                                                     "don't know"='?',"none"),         
                                           selected = "none")
                 ),
               Multi=list(
                 tags$head(tags$style(type="text/css",
                                      "label.checkbox { display: inline-block; padding-right:15px; margin-left=0px; }",
                                      ".checkbox input[type=\"checkbox\"] { float: none; }"
                 )),
                 checkboxGroupInput('multiAnswer2',
                                    label="Choices",
                                    choices=as.list(LETTERS[1:10])
                 )
               ),
               Text=HTML('<textarea cols=80 rows=10 id="textAnswer2" placeholder="You can make this window bigger, as needed"></textarea>'
)
               )
       
      } )
    } else {
      output$answerInput <- renderUI({})
    }
    # Change the tab being displayed.  For leaders, leave it alone
    if( input$userID != 'leader' )
      updateTabsetPanel(session,'userInputs',selected='Alphabetic') #answerTabToShow)
    
    # return the slide contents for display
    HTML(isolate({slideContentsGlobal}))
  })
  
  # Set up a reactive file reader.  This is a dummy file.  Just used
  # for generating an event every second.
  fileData <- reactiveFileReader(
    1000, session, 'newSlide.csv', read.csv)
  
  # Update the display
  observe({
    fileData() # for the dependency
    # to signal other observers when this changes
    isolate({updateDisplay <- 1+updateDisplay}) 
  })
  
  # regularly trigger the answer display
  observe({
    invalidateLater(5000, session)
    isolate({
      answerDisplayTrigger <<- answerDisplayTrigger+1
    })
  })
  
  output$tallyPlot <- renderPlot({
    answerDisplayTrigger # for the dependency
    if (globalSlideNumber > 0){
      submissions <- slideInfo[[globalSlideNumber]]$responses
      slideStyle <- slideInfo[[globalSlideNumber]]$style
      if( slideStyle %in% c('Short','Alphabetic','Multi')) {
        if (slideStyle=='Alphabetic')
          submissions <- factor(submissions, levels=c(LETTERS[1:7],"?"))
        if (slideStyle=='Multi') {
          submissions <- unlist(strsplit(submissions,' '))
          submissions <- factor(submissions, levels=LETTERS[1:12])
        }
        
        return(barplot(table(submissions)))
      }
    }
  })  
  output$showTextAnswers <- renderText({
    answerDisplayTrigger # for the dependency
    ansStr <- if( globalSlideNumber > 0) {
      slideStyle <- slideInfo[[globalSlideNumber]]$style
      collapseString <- switch(slideStyle,
                                         Text="\n--------\n",
                                         Short=", ",
                                         Alphabetic="")
      paste(
        slideInfo[[globalSlideNumber]]$responses,
        sep=' ',collapse=collapseString)
    }
    else { "No answers yet!" }
    return(ansStr)
  })
  
  # respond to an Alphabet input
  observe({
    input$letterAnswer2 # for the dependency
    slide <- isolate(globalSlideNumber)
    myID <- isolate(input$userID)
    if (slide > 0 ) {
      # deal with some startup issues 
      # since the radiobutton gets triggered at startup
      if (!is.null(input$letterAnswer2) && 
            (input$letterAnswer2!='none' || myID!="starting ID") ){
        slideInfo[[slide]]$responses[isolate(myID)] <<- 
           input$letterAnswer2
      }
    }
    })
  # respond to a Text input
  observe({
    input$textAnswer2 # for the dependency
    myID <- isolate(input$userID)
    slide <- isolate(globalSlideNumber)
    if (slide > 0 & myID!="starting ID" )
      slideInfo[[slide]]$responses[myID] <<- input$textAnswer2
  })
  # respond to a multi answer
  observe({
    input$multiAnswer2 # for the dependency
    myID <- isolate(input$userID)
    slide <- isolate(globalSlideNumber)
    if (slide > 0 & myID!="starting ID" ) {
      slideInfo[[slide]]$responses[myID] <<- 
           paste( input$multiAnswer2,collapse=' ')
    }
  })
  # respond to a short answer
  observe({
    input$shortAnswer2 # for the dependency
    myID <- isolate(input$userID)
    slide <- isolate(globalSlideNumber)
    if (slide > 0 & myID!="starting ID")
      slideInfo[[slide]]$responses[myID] <<- input$shortAnswer2
  })
  
  observe({
    input$shortAnswer2
    input$textAnswer2
    input$letterAnswer2
    slide <- isolate(globalSlideNumber)
    if (slide > 0)
      cat(paste(paste(slideInfo[[slide]]$responses,collapse=" : "),'\n'),file=stderr())
  })
  

  # Other server responders.
  
  # Display the slide list table for the leader
  output$slideListTable <- renderTable({
    if (input$userID == 'leader'){
      controlName <- input$slideListURL
      if (grepl("^http",controlName)){
        controlName <- sub("https:","http:",controlName)
        controlSlideTable <<- url(controlName)
      } 
      controlSlideTable <<- 
        read.csv( controlName,as.is=TRUE)
      # read in all the slide information, including the HTML
      for (k in 1:nrow(controlSlideTable)){
        slideInfo[[k]] <<- list(html=getURLContent(url=controlSlideTable$URL[k]),
                                name=controlSlideTable$Name[k],
                                style=standardizeStyle(controlSlideTable$Style[k]),
                                responses=c() #empty, at first
        )
      }
      # Think about putting radio buttons in the table itself:
      # a description of how to do this: http://stackoverflow.com/a/15977759
      
      # update the choices in the leader's selector
      slideNames <- 0:nrow(controlSlideTable)
      names(slideNames)=c("Blank",1:nrow(controlSlideTable))
      updateRadioButtons(session,'slideToDisplay',
                        choices=slideNames,
                        selected=0)
      # return the table to be displayed
      return(controlSlideTable)
    }
  })

  
  observe({
    # set up an update of the display
    isolate({updateDisplay <<- updateDisplay+1})
    # Get the slide contents
    if ( globalSlideNumber==0 ) {
      slideContentsGlobal <<- 
        "<h4><i class=\"fa fa-spinner fa-spin\"></i>&nbsp;Waiting for leader to post question ...</h4>"
      return()
    }
    if ( globalSlideNumber > nrow(controlSlideTable)){
      slideContentsGlobal <<- 
        "<h3> Selected Slide out of Range of Table</h3>"
      return()
    }
    
    # Changing <slideContentsGlobal> will eventually trigger the change in display
    # in all clients
    slideContentsGlobal <<- slideInfo[[globalSlideNumber]]$html
  })

 
# When the leader changes the slide number, this executes.
observe({
  if( input$userID == 'leader'){
    slideNum <- as.numeric(input$slideToDisplay) # was slideNumber
    write.csv(list(a=rnorm(1)), 'newSlide.csv', row.names=FALSE)
    isolate({globalSlideNumber <<- slideNum}) # triggers the change for other participants
  }
})


## For downloading the data

output$downloadData <- downloadHandler(
  filename = function() {
    paste('submissions-', Sys.Date(), '.Rda', sep='')
  },
  content = function(file) {
    save(slideInfo, file=file)
  }
)



}


# start up the server
shinyServer( clickerServer )
