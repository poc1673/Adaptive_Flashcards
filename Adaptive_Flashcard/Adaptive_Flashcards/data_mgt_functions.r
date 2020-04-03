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



























# Script to take a small subsegment of the text corpus for testing.
# rm(list = ls())

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




format_text_box_information <- function(txt_data){
  require(data.table)
  require(magrittr)
  require(stringr)
  require(readr)
  require(tokenizers)
  s1 <- tokenizers::tokenize_sentences(conv_to_latin(txt_data))
  s2 <- sapply(s1, text_formatting_pipeline)
  return(s2)   }




. <- "À circonstances exceptionnelles, mesures exceptionnelles. Pour la première fois depuis 25 ans et la création des filières S, ES et L, les épreuves du baccalauréat font l’objet d’une adaptation sur mesure à la pandémie que traverse le pays.

» Calculez votre moyenne au bac à l’aide de notre simulateur de notes

Alors qu’elles devaient commencer le 17 juin prochain, toutes les épreuves seront remplacées par le contrôle continu, soit par les notes des trois trimestres de terminale, a précisé le ministre de l’Éducation nationale, Jean-Michel Blanquer. Les notes obtenues durant toute la durée du confinement ne seront cependant pas prises en compte.

En supprimant les épreuves du baccalauréat, Jean-Michel Blanquer fait le choix de la poursuite des programmes. L’ensemble des élèves auront en effet cours jusqu’au 4 juillet, et le contrôle de l’assiduité jusqu’à cette date sera pris en compte par les jurys d’examens.

«Les candidats ayant obtenu entre 8 et 10 pourront passer les oraux de rattrapage dans les conditions habituelles», a précisé le ministre de l’Éducation nationale. Pour les candidats ayant eu moins de 8 de moyenne, une session d’épreuves écrites sera organisée au mois de septembre afin de leur offrir une deuxième chance. Les candidats libres, qui ne peuvent être évalués via le contrôle continu, pourront également y prendre part.

L’oral du bac de français est maintenu
Pour les élèves de première, qui devaient également passer des épreuves, la note de l’épreuve écrite de français sera celle de la moyenne annuelle du candidat. En revanche, l’épreuve orale est maintenue, et le nombre de textes à connaître est réduit (15 en filière générale, 12 en filière technologique)

Pour les élèves de première, la deuxième session des épreuves de contrôle continu est en revanche supprimée. La note finale prise en compte au baccalauréat 2021 sera donc la moyenne des premières et troisièmes sessions de ces épreuves."


short_inds <- sapply(X = a,FUN = function(x){ character_numbers <- nchar(x) 
return( (character_numbers >2)&(character_numbers <150)  )})
b <- a[short_inds]

write.csv(x = b[!is.na(b)],file = "Training Output.csv",row.names = F)




