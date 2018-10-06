library(stringi)
library(stringr)
library(tm)
library(wordcloud)
library(wordcloud2)
library(ggplot2)
library(quanteda)
library(dplyr)

# set wd and file paths
setwd("~/Desktop/Data Science/Coursera - Data Science Specialization/Course 10 - Capstone Project")
path_twitter = paste(getwd(), "/dataset/en_US/en_US.twitter.txt", sep = "")
path_news = paste(getwd(), "/dataset/en_US/en_US.news.txt", sep = "")
path_blogs = paste(getwd(), "/dataset/en_US/en_US.blogs.txt", sep = "")
# read data
twitter = readLines(path_twitter, encoding = "UTF-8", skipNul = TRUE)
news = readLines(path_news, encoding = "UTF-8", skipNul = TRUE)
blogs = readLines(path_blogs, encoding = "UTF-8", skipNul = TRUE)

data.frame("File_Name" = c("en_US.twitter.txt", "en_US.news.txt", "en_US.blogs.txt"),
           "File_Size_MB" = sapply(list(path_twitter,path_news,path_blogs), function(x){file.size(x)/(2^20)}),
           "Tot_NumLines" = sapply(list(twitter,news,blogs), length),
           "Tot_NumWords" = sapply(sapply(list(twitter,news,blogs), stri_count_words), sum),
           "MeanChars_per_Line" = sapply(list(twitter,news,blogs), function(x){mean(nchar(x))}),
           "Chars_LongestLine" = sapply(list(twitter,news,blogs), function(x){max(nchar(x))}))

set.seed(2018-09-10) # for reproducibility
sample_size = 4000 # only 2000 samples from each full dataset will be used
datasample = c(sample(blogs, sample_size), sample(news, sample_size), sample(twitter, sample_size))

# remove the original data to get some workspace
rm(blogs, twitter, news, path_twitter, path_blogs, path_news)

# convert all characters to ASCII
datasample = iconv(datasample, to = "ASCII", sub = "")

# get profanity words
bad_words = readLines("http://www.cs.cmu.edu/~biglou/resources/bad-words.txt")
# more info on profanity words here https://www.cs.cmu.edu/~biglou/resources/

# create the corpus with the tm package
corpus = VCorpus(VectorSource(datasample))

# define a general function to transform symbols/punctuations etc. into a space
cleanToSpace = content_transformer(function(x, pattern) {return (str_replace_all(x, pattern, " "))})

# remove (i.e. replace with a space) colons and hyphens
corpus = tm_map(corpus, cleanToSpace, "-")
corpus = tm_map(corpus, cleanToSpace, ":")

# remove also non-standard punctuation marks
corpus = tm_map(corpus, cleanToSpace, "`")
corpus = tm_map(corpus, cleanToSpace, "´")
corpus = tm_map(corpus, cleanToSpace, " -")

# convert to lower case
corpus = tm_map(corpus, content_transformer(tolower))

# find and replace common contractions
cleanContractions = function(x){
    x = str_replace_all(x, " i'm ", " i am ")
    x = str_replace_all(x, " it's ", " it is ")
    x = str_replace_all(x, "n't ", " not ")
    x = str_replace_all(x, " dont ", " do not ")
    x = str_replace_all(x, "'d ", " would ")
    x = str_replace_all(x, "'ll ", " will ")
    x = str_replace_all(x, "'re ", " are ")
    x = str_replace_all(x, "'ve ", " have ")
    return(x)
}
corpus = tm_map(corpus, content_transformer(cleanContractions))

# remove urls
corpus = tm_map(corpus, cleanToSpace, "(ftp|http|https|www)[^[:space:]]+")

# remove email addresses
corpus = tm_map(corpus, cleanToSpace, "[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+")

# remove characters corresponding to twitter hashtags, e.g. "#tag"
corpus = tm_map(corpus, cleanToSpace, "(#[[:alnum:]_]*)")

# remove characters corresponding to twitter users, e.g. "@user"
corpus = tm_map(corpus, cleanToSpace, "(@[[:alnum:]_]*)")

# remove characters @, .@, RT @, MT @, etc.
corpus = tm_map(corpus, cleanToSpace, "^(.*)?@")

# remove anything other than English letters or space (just in case)
removeNumPunct = function(x){str_replace_all(x, "[^[:alpha:][:space:]]*", "")}
corpus = tm_map(corpus, content_transformer(removeNumPunct))

# remove remaining punctuation with the specific tm function
corpus = tm_map(corpus, removePunctuation)

# remove numbers
corpus = tm_map(corpus, removeNumbers)

# remove english stopwords (i.e., "the", "for", "is", "of", "to", ... very common and very frequent words, which do not convey semantic meaning to a text) and bad words (but create a different corpus leaving the stopwords in, because they'll be needed for predictions)
#corpus_without_stopwords = tm_map(corpus, removeWords, c(stopwords("english"), bad_words))
corpus_with_stopwords = tm_map(corpus, removeWords, bad_words)

rm(corpus)

# remove duplicate words
remove_duplicates = Vectorize(
    function(x){paste(unique(trimws(unlist(strsplit(x, split = " ", fixed = F, perl = T)))), 
                      collapse = " ")}, USE.NAMES = F)
#corpus_without_stopwords = tm_map(corpus_without_stopwords, content_transformer(remove_duplicates))
corpus_with_stopwords = tm_map(corpus_with_stopwords, content_transformer(remove_duplicates))

# remove extra white spaces
#corpus_without_stopwords = tm_map(corpus_without_stopwords, stripWhitespace)
corpus_with_stopwords = tm_map(corpus_with_stopwords, stripWhitespace)

################################################################################
## STOPWORDS EXCLUDED
################################################################################
corpus = corpus_without_stopwords

my_corpus = quanteda::corpus(corpus)
print("Glimpse of corpus")
my_corpus[1]

# just to give some examples on how n-grams look like
my_tokens = quanteda::tokens(my_corpus)
paste("Some tokens: ", paste(my_tokens[1]$text1[1:3], collapse = ", "))
my_unigram = quanteda::tokens_ngrams(my_tokens, n = 1L, skip = 0L, concatenator = "_")
paste("Some unigrams: ", paste(my_unigram[1]$text1[1:3], collapse = ", "))
my_bigram = quanteda::tokens_ngrams(my_tokens, n = 2L, skip = 0L, concatenator = "_")
paste("Some bigrams: ", paste(my_bigram[1]$text1[1:3], collapse = ", "))
my_trigram = quanteda::tokens_ngrams(my_tokens, n = 3L, skip = 0L, concatenator = "_")
paste("Some trigrams: ", paste(my_trigram[1]$text1[1:3], collapse = ", "))

# now remove them
rm(my_tokens, my_bigram, my_unigram, my_trigram) 

# function for creating ngrams through the document feature matrix with the quanteda package
create_ngrams = function(x_corpus, x_ngrams){
    # create document feature matrix
    ngram_dfm = quanteda::dfm(x_corpus, ngrams = x_ngrams, verbose = FALSE)
    
    # tabulate feature frequencies, sort by descending frequency order
    features_ngram_dfm = quanteda::textstat_frequency(ngram_dfm) %>%
        arrange(desc(frequency))
    
    return(features_ngram_dfm)
}

# create ngrams
unigram_dfm = create_ngrams(my_corpus, 1)
head(unigram_dfm) # get a glimpse of the unigram document frequency matrix
bigram_dfm = create_ngrams(my_corpus, 2)
head(bigram_dfm)
trigram_dfm = create_ngrams(my_corpus, 3)
#head(trigram_dfm)
quadrigram_dfm = create_ngrams(my_corpus, 4)
#head(quadrigram_dfm)

# function for plotting the frequencies
frequency_barplot = function(data, ngrams){
    ggplot(data[1:20,], aes(x = reorder(feature, -frequency), y = frequency)) +
        theme_minimal() +
        ggtitle(paste("Top 20 ",ngrams,"grams", sep = "")) + 
        xlab(paste(ngrams,"grams", sep = "")) + 
        ylab("Frequency") +
        theme(axis.text.x = element_text(angle = 45, size = 11, hjust = 1, color = "black"),
              plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
              axis.title.x = element_text(size = 12, face = "bold"),
              axis.title.y = element_text(size = 12, face = "bold")) +
        geom_bar(stat = "identity", fill = "steelblue", color = "black") +
        geom_text(aes(label = frequency) , vjust = 1.5, color = "white", 
                  size = 3.5, fontface = "bold")
}

# plot them
frequency_barplot(unigram_dfm, "Uni")
frequency_barplot(bigram_dfm, "Bi")
frequency_barplot(trigram_dfm, "Tri")
frequency_barplot(quadrigram_dfm, "Quadri")

# make some wordclouds
# for unigrams
wordcloud2(unigram_dfm, size = 1, color = "random-light", 
           backgroundColor = "black", shape = "circle")

# for bigrams
wordcloud(bigram_dfm$feature, bigram_dfm$frequency, min.freq = 10, max.words = 100, 
          scale = c(5, .1), rot.per = 0.2, colors = brewer.pal(8, "Pastel2"), 
          random.order = FALSE)

rm(corpus)


################################################################################
## STOPWORDS INCLUDED
################################################################################

corpus = corpus_with_stopwords

my_corpus = quanteda::corpus(corpus)

unigram_dfm = create_ngrams(my_corpus, 1)
bigram_dfm = create_ngrams(my_corpus, 2)
trigram_dfm = create_ngrams(my_corpus, 3)
quadrigram_dfm = create_ngrams(my_corpus, 4)
fivegram_dfm = create_ngrams(my_corpus, 5)

frequency_barplot(unigram_dfm, "Uni")
frequency_barplot(bigram_dfm, "Bi")
frequency_barplot(trigram_dfm, "Tri")
frequency_barplot(quadrigram_dfm, "Quadri")
frequency_barplot(fivegram_dfm, "Five")

# make some wordclouds
# for unigrams
wordcloud(unigram_dfm$feature, unigram_dfm$frequency, min.freq = 10, max.words = 100, 
          scale = c(5, .1), rot.per = 0.2, colors = brewer.pal(8, "Pastel2"), 
          random.order = FALSE)

# for bigrams
wordcloud(bigram_dfm$feature, bigram_dfm$frequency, min.freq = 10, max.words = 100, 
          scale = c(5, .1), rot.per = 0.2, colors = brewer.pal(8, "Pastel2"), 
          random.order = FALSE)

rm(corpus)



########## for prediction app
corpus = corpus_with_stopwords
my_corpus = quanteda::corpus(corpus)

### Bigrams
bigram_dfm = create_ngrams(my_corpus, 2)
bigram_dfm = select(bigram_dfm, feature, frequency)
bigram_dfm$feature = str_replace_all(bigram_dfm$feature, "_", " ")

for (i in seq_len(nrow(bigram_dfm))){
    spl = strsplit(bigram_dfm$feature[i], " ")
    bigram_dfm$word1[i] = spl[[1]][1]
    bigram_dfm$word2[i] = spl[[1]][2]
}

saveRDS(bigram_dfm, file = "~/Desktop/Data Science/Coursera - Data Science Specialization/Course 10 - Capstone Project/00-ElisaBattistoni-DSCapstone/my_bigram.RData")

### Trigrams
trigram_dfm = create_ngrams(my_corpus, 3)
trigram_dfm = select(trigram_dfm, feature, frequency)
trigram_dfm$feature = str_replace_all(trigram_dfm$feature, "_", " ")

for (i in seq_len(nrow(trigram_dfm))){
    spl = strsplit(trigram_dfm$feature[i], " ")
    trigram_dfm$word1[i] = spl[[1]][1]
    trigram_dfm$word2[i] = spl[[1]][2]
    trigram_dfm$word3[i] = spl[[1]][3]
}

saveRDS(trigram_dfm, file = "~/Desktop/Data Science/Coursera - Data Science Specialization/Course 10 - Capstone Project/00-ElisaBattistoni-DSCapstone/my_trigram.RData")


### Quadrigrams
quadrigram_dfm = create_ngrams(my_corpus, 4)
quadrigram_dfm = select(quadrigram_dfm, feature, frequency)
quadrigram_dfm$feature = str_replace_all(quadrigram_dfm$feature, "_", " ")

for (i in seq_len(nrow(quadrigram_dfm))){
    spl = strsplit(quadrigram_dfm$feature[i], " ")
    quadrigram_dfm$word1[i] = spl[[1]][1]
    quadrigram_dfm$word2[i] = spl[[1]][2]
    quadrigram_dfm$word3[i] = spl[[1]][3]
    quadrigram_dfm$word4[i] = spl[[1]][4]
}

saveRDS(quadrigram_dfm, file = "~/Desktop/Data Science/Coursera - Data Science Specialization/Course 10 - Capstone Project/00-ElisaBattistoni-DSCapstone/my_quadrigram.RData")