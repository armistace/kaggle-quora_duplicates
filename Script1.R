`train` <- read.csv(file = "C:/Users/andre/AppData/Local/Temp/train.csv.utf8", header = TRUE, row.names = NULL, encoding = "UTF-8", sep = ",", dec = ".", quote = "\"", comment.char = "")
library("tm")
my_corpus <- Corpus(VectorSource(train$question1))
my_corpus <- tm_map(my_corpus, tolower)
my_corpus <- tm_map(my_corpus, removePunctuation)
my_corpus <- tm_map(my_corpus, removeNumbers)
myStopwords <- c(stopwords('english'), "available", "via")
idx <- which(myStopwords == "r")
myStopwords <- myStopwords[-idx]
my_corpus <- tm_map(my_corpus, removeWords, myStopwords)
dict_corpus <- my_corpus
my_corpus <- tm_map(my_corpus, stemDocument)
inspect(my_corpus[1:3])
