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
text_data = "Data/2020-02-23 Testing Data.csv"
ngram_data2 = map_to_translate.Tokenize_Ngram(text_data,    limit = 10000)

#s1 = pd.read_csv(text_data,encoding='latin-1')
#text_data = "Data/2019-11-24 Testing Data.csv"

# We only want to take a portion of the results for the ngram_data. We will only take two random sentences for each one..

#[1] Get subset of ngrams. We'll do the top fifty and ten randomly chosen ones.
#[2] We pick two sentences randomly for each:

#[1]
ngram_table = ngram_data2["Ngram"] 
ngram_table = ngram_data2["Ngram"][ngram_table.index.str.len()>1]
ngram_table = ngram_table[((ngram_table.index.str.find("?")<0))]
ngram_table = ngram_table[((ngram_table.index.str.find("!")<0))]

ngram_table = ngram_table[ngram_table.Count >20]
top_fifty = ngram_table.sort_values("Percent",ascending=False)[1:50]
extra_ten = ngram_table.sort_values("Percent",ascending=False)[51:].sample(n = 10)
data_for_export = pd.concat([top_fifty,extra_ten])
data_for_export

#[2]
text_for_output = data_for_export.index
text_for_output = text_for_output[(text_for_output!="l'améliorant")]
text_for_output = text_for_output[(text_for_output!="//")]

text_list = {x : ngram_data2["Sentences"][x][0] for x in text_for_output }

ngram_frame = ngram_data2["Ngram"][ngram_data2["Ngram"].index.isin(text_for_output)]

ngrams_for_processing = {"Ngram" : ngram_frame ,
                         "Sentences" : text_list}

transl_info = map_to_translate.gen_english_corpus(ngrams_for_processing )

with open("Pipeline Output/Mapping Information.json", 'w') as json_file:
    json.dump(transl_info, json_file)  

ngrams_for_processing["Ngram"].to_csv("Pipeline Output/Ngram Frequencies.csv")
