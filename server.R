## SERVER Elisa Battistoni
library(shiny)
library(stringr)
library(wordcloud)

# Loading bigram, trigram and quadgram frequencies words matrix frequencies
bigram = readRDS("my_bigram.RData"); 
trigram = readRDS("my_trigram.RData"); 
quadrigram = readRDS("my_quadrigram.RData");

## predict based on bigram frequency table
# when only one word is the input
next_word_bigram = function(sentence) {
    sent_spl = strsplit(tolower(sentence), " ")
    sent_spl = sent_spl[[1]]
    next_word = (bigram[bigram$word1 == sent_spl[1], ])$word2
    freq_next_word = (bigram[bigram$word1 == sent_spl[1], ])$frequency
    
    if(identical(next_word,character(0))) {
    next_word = "the"
    freq_next_word = 0
  }
  next_word_df = data.frame(prediction = next_word, freq = freq_next_word, stringsAsFactors = FALSE)
  return(next_word_df)
}

## predict based on trigram frequency table
# when input has two words
next_word_trigram = function(sentence) {
    sent_spl = strsplit(tolower(sentence), " ")
    sent_spl = sent_spl[[1]]
    next_word = (trigram[trigram$word1 == sent_spl[1] & trigram$word2 == sent_spl[2], ])$word3
    freq_next_word = (trigram[trigram$word1 == sent_spl[1] & trigram$word2 == sent_spl[2], ])$frequency
    
    if(identical(next_word,character(0))) {
        # search based on the last word using bigram model
        next_word_df = next_word_bigram(sent_spl[2])
    } else {next_word_df = data.frame(prediction = next_word, freq = freq_next_word, stringsAsFactors = FALSE)}
    
    return(next_word_df)
}

## predict based on quadrigram frequency table
# when input has three words
next_word_quadrigram = function(sentence) {
    sent_spl = strsplit(tolower(sentence), " ")
    sent_spl = sent_spl[[1]]
    next_word = (quadrigram[quadrigram$word1 == sent_spl[1] & quadrigram$word2 == sent_spl[2] & quadrigram$word3 == sent_spl[3], ])$word4
    freq_next_word = (quadrigram[quadrigram$word1 == sent_spl[1] & quadrigram$word2 == sent_spl[2] & quadrigram$word3 == sent_spl[3], ])$frequency
    
    if(identical(next_word,character(0))) {
        # search based on the last word using trigram
        next_word_df = next_word_trigram(paste(sent_spl[2],sent_spl[3], sep = " "))
    } else {next_word_df = data.frame(prediction = next_word, freq = freq_next_word, stringsAsFactors = FALSE)}
    return(next_word_df)
}

next_word_pred = function(sentence) {
    removeNumPunct = function(x){str_replace_all(x, "[^[:alpha:][:space:]]*", "")}
    sentence = removeNumPunct(sentence)
    sent_spl = strsplit(tolower(sentence), " ")
    sent_spl = sent_spl[[1]]
    n_words = length(sent_spl)
    if (n_words == 0) {
        predicted_word = "Waiting..."
        next_word_df = data.frame(prediction = NA, freq = 0)
        next_word_df_top10 = data.frame(prediction = NA, freq = 0)
        next_word_df_bigr = data.frame(prediction = NA, freq = 0)
    }
    else if (n_words == 1) {
        # predict based on bigrams
        next_word_df = next_word_bigram(sentence)
        predicted_word = next_word_df$prediction[1]
        next_word_df_top10 = next_word_df[1:10,]
        next_word_df_bigr = next_word_df_top10
    } else if (n_words == 2) {
        # predict based on trigrams
        next_word_df = next_word_trigram(sentence)
        predicted_word = next_word_df$prediction[1]
        next_word_df_top10 = next_word_df[1:10,]
        # predict with bigrams based on last word
        next_word_df_bigr = next_word_bigram(sent_spl[n_words])
        next_word_df_bigr = next_word_df_bigr[1:10,]
    } else if (n_words == 3) {
        # predict based on quadrigrams
        next_word_df = next_word_quadrigram(sentence)
        predicted_word = next_word_df$prediction[1]
        next_word_df_top10 = next_word_df[1:10,]
        # predict with bigrams based on last word
        next_word_df_bigr = next_word_bigram(sent_spl[n_words])
        next_word_df_bigr = next_word_df_bigr[1:10,]
    } else if (n_words > 3) {
        # predict based on quadrigrams
        next_word_df = next_word_quadrigram(paste(sent_spl[(n_words-2):n_words], collapse = " "))
        predicted_word = next_word_df$prediction[1]
        next_word_df_top10 = next_word_df[1:10,]
        # predict with bigrams based on last word
        next_word_df_bigr = next_word_bigram(sent_spl[n_words])
        next_word_df_bigr = next_word_df_bigr[1:10,]
    }
    return(list(n_words, predicted_word, next_word_df, next_word_df_top10, next_word_df_bigr))
}


# [[1]] number of words
# [[2]] predicted word
# [[3]] all predicted words using model as in [[2]]
# [[4]] top 10 predicted words using same model as for [[2]]
# [[5]] top 10 predicted words with bigrams based on last word (if number of words == 1, then [[4]] and [[5]] are equal)

shinyServer(function(input, output) {
    
    observeEvent(input$button, {
        output$word_prediction = renderPrint({
            next_word_pred(input$inputText)[[2]] %>% cat()
        })
        
        output$model_prediction = renderPrint({
            result = next_word_pred(input$inputText)[[4]]
            result = result$prediction
            result = result[!is.na(result)]
            cat(result, sep = ", ")
        })
        
        output$bigram_prediction = renderPrint({
            result = next_word_pred(input$inputText)[[5]]
            result = result$prediction
            result = result[!is.na(result)]
            cat(result, sep = ", ")
        })
    })
})
