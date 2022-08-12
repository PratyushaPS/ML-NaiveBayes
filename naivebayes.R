#loading
sms_raw=read.csv("sms_spam.csv", stringsAsFactors = FALSE)
#to see data structure
str(sms_raw)
#change type feature to factors(ham and spam)
sms_raw$type=factor(sms_raw$type)
str(sms_raw$type)
table(sms_raw$type)
#TEXT PROCESSING
sms_corpus=VCorpus(VectorSource(sms_raw$text))
#summary of texts
inspect(sms_corpus[1:2])
as.character(sms_corpus[[1]])
lapply(sms_corpus[1:2], as.character)
#cleaning the corpus by transforming text to lowercase
sms_corpus_clean=tm_map(sms_corpus,content_transformer(tolower))
as.character(sms_corpus[[1]])
as.character(sms_corpus_clean[[1]])
library(tm)
#remove all numbers from corpus
sms_corpus_clean=tm_map(sms_corpus_clean, removeNumbers)
#remove most frequent words
sms_corpus_clean=tm_map(sms_corpus_clean,removeWords, stopwords())
#remove punctuation
sms_corpus_clean=tm_map(sms_corpus_clean, removePunctuation)
#stemming
sms_corpus_clean=tm_map(sms_corpus_clean, stemDocument)
#strip whitespace after all text cleaning
sms_corpus_clean=tm_map(sms_corpus_clean, stripWhitespace)
#DATA preparation--text docs to words-tokenization
sms_dtm=DocumentTermMatrix(sms_corpus_clean)
#training and testing data
sms_dtm_train=sms_dtm[1:4169, ]
sms_dtm_test=sms_dtm[4170:5559, ]
sms_train_labels=sms_raw[1:4169, ]$type
sms_test_labels=sms_raw[4170:5559, ]$type
prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))
#wordcloud
library(wordcloud)
wordcloud(sms_corpus_clean, min.freq = 50, random.order = FALSE)
spam=subset(sms_raw, type == "spam")
ham= subset(sms_raw, type == "ham")
wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))
#data prep for training
findFreqTerms(sms_dtm_train, 5)
sms_freq_words=findFreqTerms(sms_dtm_train, 5)
str(sms_freq_words)
sms_dtm_freq_train=sms_dtm_train[ , sms_freq_words]
sms_dtm_freq_test=sms_dtm_test[ , sms_freq_words]
convert_counts=function(x) {
  x=ifelse(x > 0, "Yes", "No")
}
sms_train=apply(sms_dtm_freq_train, MARGIN = 2,convert_counts)
sms_test=apply(sms_dtm_freq_test, MARGIN = 2,convert_counts)
#train model
library(e1071)
sms_classifier=naiveBayes(sms_train, sms_train_labels)
sms_test_pred=predict(sms_classifier, sms_test)
library(gmodels)
CrossTable(sms_test_pred, sms_test_labels,prop.chisq = FALSE, prop.t = FALSE,dnn = c('predicted', 'actual'))
#improve model
sms_classifier2=naiveBayes(sms_train, sms_train_labels,laplace =1)
sms_test_pred2=predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_test_labels,prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,dnn = c('actual', 'predicted'))

