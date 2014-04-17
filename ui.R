# User Interface for clicker app

## The Clicker UI



userID <- 
  textInput(inputId="userID",
            label="User ID",
            value='starting ID'
  )
keyword <-
  textInput(inputId="userKey",
            label="Password",
            value=""
  )

# CSS to make the radio buttons horizontal
# from Joe Cheng
# https://groups.google.com/forum/#!topic/shiny-discuss/XHGVBWeiiTQ

letterAnswer <- 
  div(
    tags$head(tags$style(type="text/css",
                         "label.radio { display: inline-block; padding-right:15px; margin-left=0px; }",
                         ".radio input[type=\"radio\"] { float: none; }"
    )),
    radioButtons(inputId="letterAnswer",
                 label="Choice:",
                 choices=c('A'="A", 'B'="B", 'C'="C", 'D'="D",
                           'E'="E", 'F'="F", 'G'="G", 'H'="H",
                           "don't know"='?',"none"),         
                 selected = "none"
    )
  )
shortAnswer <- 
  textInput(inputId="shortAnswer",
            label="Your Answer:",
            value=""
  )

slideListURL <- textInput('slideListURL',
                          "URL for slide list",
                          value="ControlFile.csv")

slideListTable <- tableOutput("slideListTable")

slideToDisplay <- selectizeInput('slideToDisplay','Choose slide for display:',
                           choices=c('BLANK'=0) )

textAnswer <- '<textarea cols=80 rows=10 id="textAnswer" placeholder="You can make this window bigger, as needed"></textarea>'

leaderControls <- conditionalPanel(
  condition='input.userID=="leader"',
  slideListURL,
  slideListTable,
  slideToDisplay,
  submitButton('Press to change slide.')
  )

answerInputs <- 
  tabsetPanel( 
    id='userInputs',
    tabPanel('View Question',
             submitButton(text="Send Answer"),# letterAnswer, 
             uiOutput('answerInput'),
             value='Alphabetic'),
#     tabPanel('Short',
#              shortAnswer, submitButton(text="Send Answer"),
#              value='Short'),
#     # Something is not right with textAnswer
#     tabPanel('Text',
#              HTML(textAnswer), submitButton(text="Send Answer"),
#              value='Text'),
    tabPanel('Login', 
              h3('Project MOSAIC Polling App'),
              userID, keyword,
             submitButton(text="Login"),
             uiOutput('slideChooser'),
             # A dummy, to hold the slide answer Style
             textInput('slideStyle',
                       "",
                       value="Short"),
             value='Login'),
    tabPanel('Answers',
             textOutput('showTextAnswers',container=pre),
             plotOutput('tallyPlot',width='40%'),
             value="Answers"),
    selected='Login'
  )


overallUI <-
  column(12,  
   leaderControls,    
   answerInputs, 
   htmlOutput("slideMaterial") 
  ) 


# Create the page 
shinyUI(fluidPage(overallUI)) #bootstrapPage(overallUI))
