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

letterAnswer <- 
  radioButtons(inputId="letterAnswer",
               label="Choice:",
               choices=c("A","B","C","D",
                         "E","F","G","don't know","none"),         
               selected = "none"
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
    tabPanel('Alphabetic',
             letterAnswer, submitButton(text="Send Answer"),
             value='Alphabetic'),
    tabPanel('Short',
             shortAnswer, submitButton(text="Send Answer"),
             value='Short'),
    # Something is not right with textAnswer
    tabPanel('Text',
             HTML(textAnswer), submitButton(text="Send Answer"),
             value='Text'),
    tabPanel('Login', 
              h3('Project MOSAIC Polling App'),
              userID, keyword,
             submitButton(text="Login"),
             uiOutput('slideChooser'),
             value='Login'),
    tabPanel('Answers',
             textOutput('showTextAnswers',container=pre),
             plotOutput('tallyPlot',width='40%'),
             value="Answers"),
    selected='Login'
  )


overallUI <-
  div(class='span12',
   leaderControls,
   div(class='span12',    
      answerInputs, 
      htmlOutput("slideMaterial") 
  ) 
)

# Create the page 
shinyUI(bootstrapPage(overallUI))
