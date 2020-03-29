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
    tabsetPanel(type = "tabs",
                tabPanel("Create flash cards", value = 
                    titlePanel("French Translation Flash Cards"), 
                         fluidRow(column(width = 12,
                                    wellPanel(textInput("text_for_processing", label = "First step: Copy the text you want to use for flash cards into the box below.", value = "", width = NULL,
                                                        placeholder = NULL) ))),
                                    actionButton(inputId = "format_text", label = "Process text for flashcards"),
                    
                    fluidRow(
                        column(width = 12, 
                               wellPanel(
                                   htmlOutput("text_output_val"))))
                    ),
                tabPanel("Flash Cards", value =  
                             titlePanel("French Translation Flash Cards"), 
                         fluidRow(
                             column(width = 12, 
                                    wellPanel(
                                        htmlOutput("placeholder"),
                                        br(),br(),
                                        htmlOutput(  "Original_text"  ) ))),    
                         
                         fluidRow(
                             column(width = 12,
                                    wellPanel(textInput("answer", label = "Translate the word here", value = "", width = NULL,
                                                        placeholder = NULL) ))),
                         fluidRow(
                             column(width = 12,
                                    wellPanel(textInput("sentence_answer", label = "Translate the sentence here", value = "", width = NULL,
                                                        placeholder = NULL) ))),
                         
                         actionButton(inputId = "button_press", label = "Submit Answers")),
                tabPanel("Metrics and Performance" , 
                         DT::dataTableOutput("mytable") ) 
 
                ),
                tabPanel("Table", tableOutput("table")) )
 

# Define server logic required to draw a histogram
server <- function(input, output){
    output$text_output_val <- renderText({ "No input yet."  })

# Take the text input:
    observeEvent(input$format_text, 
                 {     output$text_output_val <- renderText({ "Input text added."  })
                     } )

# Repeatedly generate the flashcards:    
   text_data <- fread("Ngram Frequencies.csv")
   json_data <- RJSONIO::fromJSON("Mapping Information.json")
   text_data <- text_data[which((text_data$V1 %in% names(json_data)))]
   text_data <- recalc_probs( text_data[order(Percent,decreasing = T)])

   cur_table <- text_data
   current_value <- gen_random_word(text_data)
   processed_text <- identify_english_translation(json_data = json_data,original_value = current_value)
   output$placeholder <- renderText({ current_value })
   output$Original_text <- renderText({ text = processed_text$Original.Sentence  })
   # output$Original_text <- renderText({ highlight(text = processed_text$Original.Sentence,search = current_value)  })
   output$answer <- renderText({   processed_text$Translation })
   output$Translated_text <- renderText({   processed_text$Translated.Sentence })
   output$mytable = DT::renderDataTable({ text_data })

   # Placeholders for the output from the observeEvent scope. These will be updated once the user starts clicking the buttons.
   prev_value <- current_value
   answer_vals <- processed_text$Translation
   sentence_vals <-  processed_text$Translated.Sentence

   observeEvent(input$button_press, 
                { sentence_dist <- stringdist(input$sentence_answer, sentence_vals)
                  ngram_dist <-   stringdist(input$answer, answer_vals)
                  if(answer_vals==input$answer){
                      cur_table <<- cur_table
                      cur_table$Percent[cur_table==prev_value] <<- cur_table$Percent[cur_table==prev_value]/2
                      cur_table <<- recalc_probs( cur_table)
                      }else{  }
                   
                    current_value_lookup <- gen_random_word(text_data)
                    processed_text <- identify_english_translation(json_data = json_data,original_value = current_value_lookup)
                    output$placeholder <- renderText({ current_value_lookup })
                    output$Original_text <- renderText({ text = processed_text$Original.Sentence  })
                    output$answer <- renderText({   processed_text$Translation })
                    answer_for_eval <- processed_text$Translation 
                    output$Translated_text <- renderText({   processed_text$Translated.Sentence })
                    output$mytable <- DT::renderDataTable({ cur_table }) 
                    prev_value <<- current_value_lookup
                    answer_vals <<- processed_text$Translation
                    sentence_vals <<-  processed_text$Translated.Sentence  } ) }

# Run the application 
shinyApp(ui = ui, server = server)
