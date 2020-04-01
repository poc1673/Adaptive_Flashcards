# Peter Caya
# A quick function which translates a list of text in English into French.
# This is intended to be a quick wrapper for the google translate function.

from googletrans import Translator
def francais_anglais(french_text):
    transl = Translator()
    translated = transl.translate(text = french_text, dest = 'en',src = 'fr')
    res = [ [k.origin,k.text] for k in translated]
    return(res)        

