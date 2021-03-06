# -*- coding: utf-8 -*-
"""
Created on Thu Dec 19 19:57:07 2019
The purpose of this script is to take the data which was cleaned in the ngram_functions script, map the words and the sentences
to English using google translate.

The translated information is then added to a "flipped" version of the flash card.
@author: USER
"""

# -*- coding: utf-8 -*-
"""
Created on Sat Dec 14 12:42:27 2019

Contains functions for performing the following:
    1. Generate ngrams from a csv file.
    2. Generate sentences and phrases from a csv file.
    3. Take the output of function #2 and map them to the output of function #1.

@author: USER
"""

import pandas as pd
import os
import functools
import re
import numpy as np

import json
# Function using the functools package to concatenate an iterable into one string object.
def reduce_concat(x, sep=""):
    return functools.reduce(lambda x, y: str(x) + sep + str(y), x)

# Function to clean the text from characters and additional white spaces.
def clean_text(str_text):
    return_string = str_text.lower()
    return_string =     re.sub(' +', ' ', return_string)
    return_string = re.sub(pattern = "\t|\|",repl = "",string = return_string)
    return_string = re.sub(string = return_string, pattern = "\\'",repl = "'")
    return_string = re.sub(string = return_string, pattern = "\\'",repl = "'")
    return_string = re.sub(string = return_string, pattern = " \? ",repl = "")
    return_string = re.sub(string = return_string, pattern = "\[.*\]",repl = " ")
    return_string = re.sub(string = return_string, pattern = "\<.*\>",repl = " ")
    return_string = re.sub(string = return_string, pattern = " \! ",repl = " ")
    return_string = re.sub(string = return_string, pattern = "\{.*\}",repl = " ")
    return_string = re.sub(string = return_string, pattern = " : ",repl = " ")
    return_string = re.sub(r'[^\w\s]','',return_string )
    return_string = re.sub(r'u\d\d\d\d|t\d\d\d\d','',return_string )
    return_string = re.sub(r'\d','',return_string )
    return(return_string)


def rm_front_ws(str_text):
    return_string = re.sub(pattern = "^ *",repl = "",string = str_text         )
    return(return_string)
    
    
def rm_extra_ws(str_text):
    return_string = re.sub(pattern = "\s+",repl = " ",string = str_text         )
    return(return_string)

def rm_end_ws(str_text):
    return_string = re.sub(pattern = " *$",repl = "",string = str_text         )
    return(return_string)


def rm_slash_apost(str_text):
    return_string = re.sub(pattern = "\'",repl = "'",string = str_text)
    return(return_string)
    
    
def text_cleaning_meta_function(removal_list):
    sep = "|"
    removal_list_formatted = sep.join(removal_list)    
    def return_function(text_for_formatting):
        return(re.sub(pattern = removal_list_formatted ,repl = "",string = text_for_formatting))
    return(return_function)

# Create a function to remove garbage characters:
remove_most_formatting = text_cleaning_meta_function(["\?","\(","\)","\!","\d",",","\-","\.","\[","\]",":","\;","\"","ISBN","%","\«","\*","\+","\&"," \w ","="])

def rm_backslack(str_text):
    return(re.sub(pattern = "\\\\",repl="",string = str_text))


def clean_text(str_text):
    return(rm_front_ws(rm_end_ws(str_text)))





#  gen_ngram
# Description: Generate the ngrams for some specific number of spaces, n.
def gen_ngram(str_for_proc,n):
    split_str = [token for token in str_for_proc.split(" ") if token != ""]
    ngrams = zip(*[split_str[i:] for i in range(n)])
    return [" ".join(ngram) for ngram in ngrams]


def unique_count(list_obj):
    bool_vals = [list_txt == unique_results[1] for i in results[0]]

# The function below takes a list of the elements generated by the gen_ngram result.
# It then creates a unique version of the list and counts the number of results for each element
# and places them into a dictionary which is then converted to a pandas data.frame. Finally, the portion of the results.
# that makes up each entry is turned into a percentage.
def count_ngrams(to_check,ngrams_list):
    bool_vals = [list_txt == to_check for list_txt in ngrams_list]
    return(sum(bool_vals))

# Parse_sentences
# This function generates a list of sentences from a string object. This is based on the character ". ".
# Inputs:
#   [1] A str object to be split.

# Outputs:
#   [1] A list object containing the individual sentences from the string.
def Parse_sentences(string_for_proc):
    split_period = string_for_proc.split(". ")
    split_period = [x+"." for x in split_period]
    return(split_period)

def lookup_sentences(ngram,phrase_list):
    return([x for x in phrase_list if x.find( " " + ngram+" " ) >=0    ])

def sort_phrases(ngram_list,phrase_list):
    return_dict = {x :lookup_sentences(ngram = x,phrase_list = phrase_list) for x in ngram_list  }
    return(return_dict)

# Tokenize_Ngram
# Description:
# Input:
#   [1] csv file path containing text data. The function will take the values of the csv file, paste them all together.
#       and then tokenize them based on some range of numbers.
# Output:
#   [1] A dictionary of data frames for each set of ngrams. Each dataframe contains the ngram, its frequency, and its frequency for that specific ngram corpus.
def Tokenize_Ngram(csv_file_path,limit = "all",min_str_length = 20, max_str_length = 100):
    s1 = pd.read_csv(csv_file_path,encoding='latin-1')
    s1 = s1[(s1.x.str.len() > min_str_length)&(s1.x.str.len() < max_str_length    )]

    if limit == "all": 
        data_nrows = s1.shape[0]
    else: data_nrows = limit

    print("Running with " + str(limit) + " rows.")
    
    s1 = s1[1:data_nrows] 
    
    # Combine the sentences together into one corpus for review.
    s2 = reduce_concat(s1.x,sep = "  ")
#    s3 = clean_text(s2)
    
    # Generate individual sentences to be used for the flash-cards.
    sentences = Parse_sentences(s2)
    
    # Collect ngrams from the corpus of sentences.
    get_ngrams = gen_ngram(rm_backslack(rm_extra_ws(remove_most_formatting(s2))),  n = 1)
    unique_ngrams = np.unique(get_ngrams)
    # Map each sentence to each unique ngram.
    mapped_sentences = sort_phrases(ngram_list=unique_ngrams, phrase_list = sentences)
    result_count = [count_ngrams(word,get_ngrams) for word in unique_ngrams ]

    count_dict = {unique_ngrams[i]: result_count[i] for i in range(len(unique_ngrams)) }
    ngram_frame =  pd.Series(count_dict).to_frame("Count")
    ngram_frame["Percent"] = ngram_frame.Count/ngram_frame.Count.sum()
    return( {"Ngram" : ngram_frame ,
             "Sentences" : mapped_sentences} )




# Peter Caya
# A quick function which translates a list of text in English into French.
# This is intended to be a quick wrapper for the google translate function.

from googletrans import Translator
import time
def francais_anglais(french_text):
    transl = Translator()
    translated = transl.translate(text = french_text, dest = 'en',src = 'fr')
    res = [ [k.origin,k.text] for k in translated]
# Adding in time to avoid overloading google and getting shutdown.
    time.sleep(1)
    return(res)


def gen_english_corpus(french_corpus):
    keys = french_corpus["Sentences"].keys()
    english_corpus = { cur_key: {"English": francais_anglais([cur_key]),
                                 "French Sentence": french_corpus["Sentences"][cur_key],
                                 "English Sentence" : francais_anglais(  [french_corpus["Sentences"][cur_key] ]),} for cur_key in keys }
    return(english_corpus )

#ngrams_for_processing
#francais_anglais([ngrams_for_processing["Sentences"][cur_key] ])