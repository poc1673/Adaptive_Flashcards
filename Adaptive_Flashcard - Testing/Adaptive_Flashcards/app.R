rm(list = ls())
library(pacman)
library(data.table)
library(reshape2)
library(rmarkdown)
library(shiny)
library(knitr)
library(ggplot2)
library(RJSONIO)
library(stringdist)
# p_load(data.table,reshape2,rmarkdown,shiny,knitr,ggplot2,JSONIO)
source("data_mgt_functions.r")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
# Application title
 
    titlePanel("French Translation Flash Cards"),
    
    fluidRow(
        column(width = 12,
               wellPanel(
                  htmlOutput("placeholder"),
                   # textOutput("placeholder"),
                   br(),br(),
                   htmlOutput("Original_text") ))),    

    # htmlOutput("Original_text"),
        # fluidRow(
        # column(width = 12,
        #        wellPanel(
        #            textOutput("Original_text")
        #        ))),    
    
    fluidRow(
        column(width = 12,
               wellPanel(textInput("answer", label = "Translate the word here", value = "", width = NULL,
                             placeholder = NULL) ))),

    fluidRow(
        column(width = 12,
               wellPanel(textInput("sentence_answer", label = "Translate the sentence here", value = "", width = NULL,
                                   placeholder = NULL) ))),
    
    # fluidRow(
    #     column(width = 12,
    #            wellPanel(textInput("Translated_text", label = "Translate the phrase here.", value = "", width = NULL,
    #                                placeholder = NULL) ))),
    actionButton(inputId = "button_press", label = "Submit Answers"))

# Define server logic required to draw a histogram
server <- function(input, output){
   text_data <- fread("Ngram Frequencies.csv")
   json_data <- RJSONIO::fromJSON("Mapping Information.json")
   text_data <- text_data[which((text_data$V1 %in% names(json_data)))]
   text_data <- recalc_probs( text_data[order(Percent,decreasing = T)])

   current_value <- gen_random_word(text_data)
   processed_text <- identify_english_translation(json_data = json_data,original_value = current_value)
   output$placeholder <- renderText({ current_value })
   output$Original_text <- renderText({ highlight(text = processed_text$Original.Sentence,search = current_value)  })
   output$answer <- renderText({   processed_text$Translation })
   output$Translated_text <- renderText({   processed_text$Translated.Sentence })
   
   # Placeholders for the output from the observeEvent scope. These will be updated once the user starts clicking the buttons.
   prev_value <- current_value
   answer_vals <- processed_text$Translation
   sentence_vals <-  processed_text$Translated.Sentence
   
   observeEvent(input$button_press, 
                { sentence_dist <- stringdist(input$sentence_answer, sentence_vals)
                    ngram_dist <-   stringdist(input$answer, answer_vals)
                  print(ngram_dist)
                  print(sentence_dist)
                  
                  print(input$sentence_answer)
                  print(sentence_vals)  
                  
                  print(input$answer)
                  print(answer_vals)
                  
                if(answer_vals==input$answer){
                      text_data[V1==processed_text$Translation,Percent := Percent/2]
                      text_data <- recalc_probs( text_data)}else{  }       
                   
                   current_value_lookup <- gen_random_word(text_data)
                    processed_text <- identify_english_translation(json_data = json_data,original_value = current_value_lookup)
                    output$placeholder <- renderText({ current_value_lookup })
                    output$Original_text <- renderText({   highlight(text = processed_text$Original.Sentence,search = current_value_lookup)   })
                    output$answer <- renderText({   processed_text$Translation })
                    answer_for_eval <- processed_text$Translation 
                    output$Translated_text <- renderText({   processed_text$Translated.Sentence })
                    prev_value <<- current_value_lookup
                    answer_vals <<- processed_text$Translation
                    sentence_vals <<-  processed_text$Translated.Sentence
                } )
   
       # output$greeting <- renderText(text())

   }

# Run the application 
shinyApp(ui = ui, server = server)




