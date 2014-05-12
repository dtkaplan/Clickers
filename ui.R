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

selectSlide <- 
  div(
    tags$head(tags$style(type="text/css",
                         "label.radio { display: inline-block; padding-right:15px; margin-left=0px; }",
                         ".radio input[type=\"radio\"] { float: none; }"
    )),
    radioButtons(inputId="slideToDisplay",
                 label="Which slide?",
                 choices=c(BLANK=0, 1:30),    
                 selected = 0
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

textAnswer <- '<textarea cols=80 rows=10 id="textAnswer" placeholder="You can make this window bigger, as needed"></textarea>'

leaderControls <- conditionalPanel(
  condition='input.userID=="leader"',
  downloadButton('downloadData', label = "Download Slide Data", class = NULL),
  
  slideListURL,
  selectSlide,
  slideListTable
)

answerInputs <- 
  tabsetPanel( position='right',
    id='userInputs',
    tabPanel('View Question',
             value='Alphabetic'),
    tabPanel('Answers',
             textOutput('showTextAnswers',container=pre),
             plotOutput('tallyPlot',width='40%'),
             value="Answers"),
    tabPanel('Login', 
              userID, # keyword,
             # submitButton(text="Login"),
             uiOutput('slideChooser'),
             leaderControls,
             # A dummy, to hold the slide answer Style
             value='Login'),

    selected='Alphabetic'
  )


overallUI <-
  column(12, 
   h3('Project MOSAIC', 'Polling App (v 0.2)'),
   p('...........'),
   htmlOutput("slideMaterial"),
   br(),
   uiOutput('answerInput'),
   submitButton(text="Send",icon = icon("refresh")),
   answerInputs

  ) 


# Create the page 
shinyUI(fluidPage(overallUI)) #bootstrapPage(overallUI))
