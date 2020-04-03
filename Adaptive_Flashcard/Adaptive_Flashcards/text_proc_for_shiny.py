# This script performs the following:
# 1. Load in a csv file containing the processed text data.
# 2. The ngrams are tokenized and turned into a dictionary.
# 3. The sentences are translated.
# 4. The frequence table is output as a csv file. This file will be used to score the user.
# 5. A JSON file is output for loading. This will be used to seed the flashcard.

import ngram_functions
import map_to_translate
import json
import pandas as pd
def text_proc_function(text_data):
  ngram_data2 = map_to_translate.Tokenize_Ngram(text_data,    limit = 10000)
  
  ngram_table = ngram_data2["Ngram"] 
  ngram_table = ngram_data2["Ngram"][ngram_table.index.str.len()>1]
  ngram_table = ngram_table[((ngram_table.index.str.find("?")<0))]
  ngram_table = ngram_table[((ngram_table.index.str.find("!")<0))]
  
  ngram_table = ngram_table[ngram_table.Count >20]
  top_fifty = ngram_table.sort_values("Percent",ascending=False)[1:50]
  extra_ten = ngram_table.sort_values("Percent",ascending=False)[51:].sample(n = 10)
  data_for_export = pd.concat([top_fifty,extra_ten])

    #[2]
  text_for_output = data_for_export.index
  text_for_output = text_for_output[(text_for_output!="l'améliorant")]
  text_for_output = text_for_output[(text_for_output!="//")]
  
  text_list = {x : ngram_data2["Sentences"][x][0] for x in text_for_output }
  
  ngram_frame = ngram_data2["Ngram"][ngram_data2["Ngram"].index.isin(text_for_output)]
  
  ngrams_for_processing = {"Ngram" : ngram_frame ,
                           "Sentences" : text_list}    
  transl_info = map_to_translate.gen_english_corpus(ngrams_for_processing )
  return_dict = {"Corpus":ngrams_for_processing
                ,"Phrases":transl_info    }
  return(return_dict)


