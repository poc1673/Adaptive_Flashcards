# Script to take a small subsegment of the text corpus for testing.
rm(list = ls())
library(pacman)
p_load(data.table,magrittr,stringr,readr,tokenizers)
number_of_vals <- 1000000
conv_to_latin <- function(text){
    Encoding(text) <- "latin1"
    return(text)}

rm_extra_white <- function(txt){
    return(gsub(x = txt,pattern = "\\s+",replacement = " "))}

rm_comm <- function(txt){
    return(gsub(x = txt,pattern = "\\(Comment \\?\\)\\.",replacement = ""))}

rm_bar <- function(txt){
    return(gsub(x = txt,pattern = "\\|",replacement = ""))}

rm_bracks <- function(txt){
    return(gsub(x = txt,pattern = "<#\\d+>",replacement = ""))}

text_formatting_pipeline <- function(txt){
    return( txt %>%rm_comm %>%  rm_bracks() %>% rm_bar()%>% rm_extra_white()  )}

. <- "../Data/wikipediaTXT.txt"
a <- readLines(con = .,n = number_of_vals) %>% conv_to_latin() %>% paste(collapse = " ") %>% tokenizers::tokenize_sentences()
formatted_text_data <- sapply(a[[1]], text_formatting_pipeline)
short_inds <- sapply(X = formatted_text_data,FUN = function(x){ character_numbers <- nchar(x) 
                                                return( (character_numbers >2)&(character_numbers <150)  )})
b <- formatted_text_data[short_inds]

write.csv(x = b[!is.na(b)],file = "Training Output.csv",row.names = F)


