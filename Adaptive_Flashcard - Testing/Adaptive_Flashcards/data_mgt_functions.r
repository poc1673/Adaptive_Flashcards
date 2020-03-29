get_val_from_list <- function(list_vals,to_match){
  return(list_vals[which(names(list_vals)==to_match ) ])}

# identify_english_translation <- function(json_data,original_value){
#   translate_list <- get_val_from_list(list_vals = json_data,to_match = original_value) 
#   return(translate_list[[1]]$English[[1]][2])}

gen_random_word <- function(text_data){return(sample(x = text_data$V1,prob =1- text_data$Percent,size = 1))}

identify_english_translation <- function(json_data,original_value){
    translate_list <- get_val_from_list(list_vals = json_data,to_match = original_value)[[1]]
    english_word <- translate_list$English[[1]][2]
    # print(english_word)bv
    french_result <- translate_list$`French Sentence`[1]
    english_result <- translate_list$`English Sentence`[[1]][2]
    return(list("Translation" = english_word,
              "Original.Sentence" = french_result,
              "Translated.Sentence" = english_result) )}


recalc_probs <- function(text_for_recalc){
  return_text <- text_for_recalc[,Percent := Percent/sum(Percent)] 
  return(return_text)}

highlight <- function(text, search){
  x <- unlist(strsplit(text, split = " ", fixed = T))
  x[tolower(x) == tolower(search)] <- paste0("<mark>", x[tolower(x) == tolower(search)], "</mark>")
  paste(x, collapse = " ")}

#Example:
# text_data_2 <- recalc_probs( text_data[order(Percent,decreasing = T)][c(1,2,3,4)])
# 
# library(pacman)
# library(data.table)
# library(reshape2)
# library(rmarkdown)
# library(shiny)
# library(knitr)
# library(ggplot2)
# library(RJSONIO)
# # p_load(data.table,reshape2,rmarkdown,shiny,knitr,ggplot2,RJSONIO)
# text_data <- fread("Adaptive_Flashcards/Ngram Frequencies.csv")
# json_data <- RJSONIO::fromJSON("Adaptive_Flashcards/Mapping Information.json")
# processed_text <- identify_english_translation(json_data = json_data,original_value = gen_random_word(  text_data  ))


