#########################################
#                                       #
#           Quora Dups Script           #
#                                       #
#########################################

#this variable is changeable and point to the path of the data to 
#be analysed. From here it assumes the table structure of
#ID    |    QID1    |   QID2    |   Question 1  |   Question 2  |

##########
data_path <- "train.csv"
percent_compare <- .85
file_path <- "results.csv"
##########

# From here is script which should not need updating
# basic idea
# remove superflous words from column question 1 and question 2
# compare important words in column 1 and column 2 of resulting data
# same words over percent_compare will be duplicate 

#lets reset and write the header to the output file so it's done
output_file <- file(file_path)
writeLines("test_id,is_duplicate", output_file)
close(output_file)

raw_data <- read.csv(file = data_path, header = TRUE, sep = ",")

library(tm)

question_1 <- raw_data$question1
question_2 <- raw_data$question2

#set up as corpus (collection of text documents)
corpus_1 <- Corpus(VectorSource(question_1))
corpus_2 <- Corpus(VectorSource(question_2))

#change to lower case
corpus_1 <- tm_map(corpus_1, tolower)
corpus_2 <- tm_map(corpus_2, tolower)

# remove punctuation
corpus_1 <- tm_map(corpus_1, removePunctuation)
corpus_2 <- tm_map(corpus_2, removePunctuation)

# remove numbers
corpus_1 <- tm_map(corpus_1, removeNumbers)
corpus_2 <- tm_map(corpus_2, removeNumbers)

#Lets get rid of english stop words
corpus_1 <- tm_map(corpus_1, removeWords, stopwords('english'))
corpus_2 <- tm_map(corpus_2, removeWords, stopwords('english'))

#stem words with snowball stoppers
corpus_1 <- tm_map(corpus_1, stemDocument)
corpus_2 <- tm_map(corpus_2, stemDocument)

for (line in 1:length(corpus_1)) {

    print(paste("Comparing Row", line - 1, sep = " "))
    ###from here for speed is testing of first line###
    #create dictionary
    dict_1 <- corpus_1[line]
    dict_2 <- corpus_2[line]

    #create single corpus
    single_corp_1 <- corpus_1[line]
    single_corp_2 <- corpus_2[line]

    #turn into data frame
    df1 <- data.frame(text = get("content", single_corp_1))
    df2 <- data.frame(text = get("content", single_corp_2))

    #turn into string
    as_string_1 <- sapply(df1, as.character)
    as_string_2 <- sapply(df2, as.character)

    #here we split into the indiviual words
    #access with [[x]]
    words_1 <- strsplit(as_string_1, " ")[[1]]
    words_2 <- strsplit(as_string_2, " ")[[1]]

    #create a logical
    compare <- words_1 %in% words_2

    true_count <- 0
    for (i in seq_len(length(compare))) {
        if (compare[[i]] == TRUE) {
            true_count <- true_count + 1
        }
    }
    if (length(compare) == 0) {
        percent = 0
    } else {
        percent <- true_count / length(compare)
    }

    #write results to file
    if (percent > percent_compare) {
        output <- paste(line - 1, 1, sep = ",")
        write(output, file = file_path, append = TRUE)
        print("Detected as duplicate question")
    } else {
        output <- paste(line - 1, 0, sep = ",")
        write(output, file = file_path, append = TRUE)
        print("Not detected as duplicate question")
    }
}

output_data <- read.csv(file = "results.csv", header = TRUE, sep = ",")

#This is going to sum up the results compared to the original
#first result: both duplicates
#second result: original is duplicate my check is not
#third result: my is duplicate original is not
#fourth result: both not dup
results_summary <- c(0, 0, 0, 0)
original <- raw_data$is_duplicate
my_run <- output_data$is_duplicate

#little Idea I had... lets group into the major question groups and see how it goes
#1: Who
#2: What
#3: Where
#4: When
#5: Why
#6: How
major_question_group <- c()

for (line in 1:length(original)) {
    if (original[line] == 1 && my_run[line] == 1) {
        results_summary[1] = results_summary[1] + 1
    }
    else if (original[line] == 1 && my_run[line] == 0) {
        results_summary[2] = results_summary[2] + 1
    }
    else if (original[line] == 0 && my_run[line] == 1) {
        results_summary[3] = results_summary[3] + 1
    }
    else if (original[line] == 0 && my_run[line] == 0) {
        results_summary[4] == results_summary[4] + 1
    }
    else {
        print (paste("Encountered error on line ", line))
    }

}

#lets have a loot at how we went
barplot(results_summary, names.arg = c("Both Duplicates", "Original Dup", "My Dup", "Neither Dup"))

