---
title: "Next Word Prediction App"
author: "Elisa Battistoni"
date: "10/6/2018"
output:
  ioslides_presentation:
    keep_md: yes
    css: slide.css
    widescreen: yes
  slidy_presentation: default
---

## Next Word Prediction App - Introduction

<span style="font-weight:bold">
What is it purpose? How does the underlying algorithm work?
</span>

<font size="4">
The purpose of this app is to predict the next word of a series of words.

Depending on the number of words given as input, n-gram models will predict the next word: if the input is one word, the app will use a bigram (2-words) model; if the input is two words, the app will use a trigram (3-words) model; if the input is three words, the app will use a quadrigram (4-words) model.

For example, with three words as input, the app will try to predict the next word with a quadrigram model. If this model is not able to return a prediction, the next word guess will be made using a trigram model; if the prediction is still not achieved, the app will use a bigram model (in which the next word is predicted based on only the last word in the input sentence). If a bigram model cannot predict the next word, the word 'the' will be returned as prediction.

These N-gram models were built using text data from news, twitter and blogs provided by SwiftKey. 
</font>

## Next Word Prediction App - Introduction
<span style="font-weight:bold">
How to use this app?
</span>

<font size="5">
Enter at least one word and press the 'Go' button. After the first button press, the next word will be automatically predicted without pressing the 'Go' button again.
</font>

<div align="center">
<img src="screenshot_app2.jpg">
</div>

## Next Word Prediction App - a glimpse

<span style="font-weight:bold">
A screenshot of an example prediction:
</span>

<div align="center">
<img src="screenshot_app1.jpg">
</div>

## Next Word Prediction App - conclusions

- The app can be found [here](https://elisab.shinyapps.io/myCapstoneApp/)

- The code can be found [here](https://github.com/elibattistoni/DataScienceCapstone)





