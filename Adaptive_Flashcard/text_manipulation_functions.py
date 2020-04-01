# Peter Caya
# Function to take a corpus of words and output a list of sentences.

import pandas as pd
import re
#os.chdir("Dropbox/Projects/Adaptive Flash Cards/")
#a = pd.read_csv("Data/2019-11-24 Testing Data.csv",encoding = "ISO-8859-1",squeeze = True).x

def detect_periods(string):
    return(string.find(".")>=0)

# Function to apply filtering criteria as the first step of the data cleaning process. It omits "junk" rows which contain only a few characters
# and selects for cases where the string ends with a period.
def filter_corpus(string):
     inds = string.apply(lambda x: (detect_periods(x)&(len(x)>5 ) ))
     return(string[inds])
     
def rm_end_white(x):
    return( re.sub(string= x,repl = ".",pattern = "\..*$"))




def detect_ngram(text,ngram):



 
