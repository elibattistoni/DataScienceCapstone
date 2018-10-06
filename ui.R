## UI Elisa Battistoni
library(shiny)
library(shinythemes) # https://rstudio.github.io/shinythemes/


shinyUI(fluidPage(
    theme = shinytheme("sandstone"),
    # title
    titlePanel("Next word prediction App"),
    br(),
    h4("Introduction:"),
    p("The purpose of this app is to predict the next word of a series of words."),
    p("Depending on the number of words given as input, n-gram models will predict the next word: if the input is one word, the app will use a bigram (2-words) model; if the input is two words, the app will use a trigram (3-words) model; if the input is three words, the app will use a quadrigram (4-words) model."),
    p("For example, with three words as input, the app will try to predict the next word with a quadrigram model. If this model is not able to return a prediction, the next word guess will be made using a trigram model; if the prediction is still not achieved, the app will use a bigram model (in which the next word is predicted based on only the last word in the input sentence). If a bigram model cannot predict the next word, the word 'the' will be returned as prediction."),
    p("These N-gram models were built using text data from news, twitter and blogs provided by SwiftKey."),
    br(),
    h4("Instructions:"),
    p("Enter at least one word and press the 'Go' button. After the first button press, the next word will be automatically predicted without pressing the 'Go' button again."),
    br(),
    
    # layout
    sidebarLayout(
        # sidebar panel
        sidebarPanel(textInput(inputId = "inputText", label = "Enter at least one word:", value = ""),
                     actionButton(inputId = "button", label = "Go")
                     ),
        
        # main panel
        mainPanel(
            h4("The predicted next word is: "),
            h3(strong(textOutput('word_prediction'))),
            br(),
            br(),
            br()
            )
        ),
    br(),
    br(),
    h4("Further predictions:"),
    br(),
    h5("Prediction 1: Top words predicted using the same n-gram model used for the prediction above:"),
    strong(textOutput('model_prediction')),
    br(),
    h5("Prediction 2: Top words predicted using a bigram (2-words) model (i.e., considering only the last word of the sentence):"),
    strong(textOutput('bigram_prediction')),
    br(),
    p("Note: if a bigram model is used in 'Prediction 1', the results of 'Prediction 1' and 'Prediction 2' will be the same."),
    br()
))
