setwd("C:/Users/Patrick/Documents/Eigene Dokumente/Dokumente/Persoenliches/Lebenslauf/Coursera/01_R_Foundations/02_Codes/Capstone/R-coding/Katz-Back-Off")

#########################what this script does##################################

# This script 
# - uploads the data string data for the language model 
# - differences between training and test data
# - merges the data from different sources (news, blog and twitter) to one data set
# - provides some general statistics on the data


###############################packages#########################################

library(downloader)
library(dplyr)
library(tidytext)
library(ggplot2)
library(tictoc)
library(stringr)
library(data.table)

##############################functions#########################################

nrow0 <- function(x) dim(x)[1]
chunk2 <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE))
coverage_words <- function(percentage, count_tibble){
        i <- 1
        total <- sum(count_tibble$n)
        help <- 0
        a <- 0
        while(a < percentage){
                summe <- count_tibble$n[i] + help
                help <- summe
                i <- i + 1
                a <- summe/total
        }
        return(i)
}

##############################script############################################

# Script works "sequently": first blogs, then news and lastly twitter
# Idea: load the whole data set into R, split it up into chunks and after 
# processing is merge the data into one data set.

p <- 3
set.seed(123)
exploratory_analysis <- function(name, p){
        # upload the data
        directory <- paste0("C:/Users/Patrick/Documents/Eigene Dokumente/Dokumente/Persoenliches/Lebenslauf/Coursera/01_R_Foundations/02_Codes/Capstone/R-Coding/Katz-Back-Off/data/dataset/final/en_US/", name)
        # random sampling the data 
        data <- readLines(directory, skipNul = T, encoding = "UTF-8")
        data <- sample(data, size = length(data)/p, replace = F)
        
        # now we pre process the data
        # delete special characters and digits
        data <- gsub("[[:punct:]]|[[:digit:]]", " ", data)
        # we treat "they're" or similar as "they re"
        data <- gsub("'", " ", data)
        data <- tolower(data)
        # trim everything to the right. this is necessary for evaluation
        data <- str_trim(data, side = "right")
        
        # Here, we split the data into the training and test data set and do some explorative
        # analysis on the training data set
        
        index_training <- sample(1:length(data), size = round(length(data)*0.8), replace = F)
        training_data  <-  data[index_training]
        index_test     <- setdiff(x = 1:length(data), index_training)
        test_data     <- tibble(text = data[index_test])
        
        # Now, we analyse the training_data
        # Here, the data is split into chunks as the data set is really big and then merge it
        
        # create the chunks and store the chunks into a list
        x <- 1 : length(training_data)
        chunks <- chunk2(x, 100)
        
        training_data_list <- lapply(chunks, function(x) tibble(text = training_data[x]))
        training_list_tokens <- lapply(training_data_list, function(x) x %>% unnest_tokens(word, text))

        # Some words are more frequent than others - what are the distributions of
        # word frequencies?         
        unigram_overall <- do.call("rbind", training_list_tokens)
        unigram_counts <- unigram_overall %>% count(word, sort = T)
        unigram_top25 <- unigram_counts %>% top_n(25)
        
        number_lines <- sum(as.numeric(sapply(training_data_list, nrow0)))/1e6
        number_words <- sum(unigram_counts$n)/1e6
        
        p_unigram <- unigram_top25 %>% mutate(word = reorder(word, n)) %>%
                ggplot(aes(n, word)) +
                geom_col() +
                labs(y = NULL) +
                ggtitle(paste("Top 25 words in the", name ,"data-set", sep = " ")) +
                xlab("Number of words")
        
        # bigram = line that consists of two words phrases
        # trigram = line that consists of three words phrases
        bigrams <- lapply(training_data_list, function(x) x %>% unnest_tokens(bigram, text, token = "ngrams", n = 2))
        trigrams <- lapply(training_data_list, function(x) x %>% unnest_tokens(trigram, text, token = "ngrams", n = 3))
        #tetragrams <- lapply(training_data_list, function(x) x %>% unnest_tokens(tetragram, text, token = "ngrams", n = 4))
        
        # idea: merge them now into one big list and then count them
        bigrams_overall <- do.call("rbind", bigrams)
        bigrams_overall <- bigrams_overall %>% select(bigram)
        bigrams_counts <- bigrams_overall %>% count(bigram, sort = T)
        top_25_bigrams <- bigrams_counts %>% top_n(25)
        
        trigrams_overall <- do.call("rbind", trigrams)
        trigrams_overall <- trigrams_overall %>% select(trigram)
        trigrams_counts <- trigrams_overall %>% count(trigram, sort = T)
        top_25_trigrams <- trigrams_counts %>% top_n(25)
        
        p_bigram <- top_25_bigrams %>% mutate(bigram = reorder(bigram, n)) %>%
                ggplot(aes(n, bigram)) +
                geom_col() +
                labs(y = NULL) +
                ggtitle(paste("Frequency of bigrams in the", name, sep = " ")) +
                xlab("Frequency")
        
        p_trigram <- top_25_trigrams %>% mutate(trigram = reorder(trigram, n)) %>%
                ggplot(aes(n, trigram)) +
                geom_col() +
                labs(y = NULL) +
                ggtitle(paste("Frequency of trigams", name, sep = " ")) +
                xlab("Frequency")
        
        result <- list(training_data = training_data, test_data = test_data,
                       number_lines = number_lines, number_words= number_words,
                       unigram_grafic = p_unigram, bigram_grafic = p_bigram,
                       trigram_grafic = p_trigram,
                       unigram_counts = unigram_counts, bigram_counts = bigrams_counts,
                       trigram_counts = trigrams_counts)
        return(result)
}
gc()
name <- "en_US.blogs.txt"
blogs <- exploratory_analysis(name = name, p = p)

gc()
name <- "en_US.news.txt"
news <- exploratory_analysis(name = name, p = p)

gc()
name <- "en_US.twitter.txt"
twitter <- exploratory_analysis(name = name, p = p)

# overall analysis
# number of words
# number of lines

summary <- data.frame(source = c("blogs", "news", "twitter"), number_lines = NA, number_words = NA)
summary$number_lines <- c(blogs$number_lines, news$number_lines, twitter$number_lines)
summary$number_words <- c(blogs$number_words, news$number_words, twitter$number_words)

ggplot(summary, aes(x = reorder(source, - number_lines), y = number_lines)) + 
        geom_bar(stat="identity") + ggtitle("Number of lines in the data sets") + 
        ylab("number of lines") + xlab("source")

ggplot(summary, aes(x = reorder(source, - number_words), y = number_words)) + 
        geom_bar(stat="identity") + ggtitle("Number of words in the data sets") + 
        ylab("number of words") + xlab("source")

# summary statistics of blogs, twitter and news

blogs$unigram_grafic
blogs$bigram_grafic
blogs$trigram_grafic

news$unigram_grafic
news$bigram_grafic
news$trigram_grafic

twitter$unigram_grafic
twitter$bigram_grafic
twitter$trigram_grafic

# get the counts for unigrams, bigrams and trigrams
# from the training data

overall_unigrams <- rbind(blogs$unigram_counts, news$unigram_counts, twitter$unigram_counts) 
overall_unigrams <- overall_unigrams %>% group_by(word) %>% summarise(n = sum(n)) %>% arrange(desc(n))

overall_bigrams <- rbind(blogs$bigram_counts, news$bigram_counts, twitter$bigram_counts)
overall_bigrams <- overall_bigrams %>% group_by(bigram) %>% summarise(n = sum(n)) %>% arrange(desc(n))

overall_trigrams <- rbind(blogs$trigram_counts, news$trigram_counts, twitter$trigram_counts)
overall_trigrams <- overall_trigrams %>% group_by(trigram) %>% summarise(n = sum(n)) %>% arrange(desc(n))

# get the test data and merge it:

test_data_list <- list(test_data_blog = blogs$test_data, test_data_news = news$test_data, 
                       test_data_twitter = twitter$test_data)
test_data_overall <- do.call("rbind", test_data_list) %>% select(text)

############################# data processing #####################################

# now we download an english dictionary
url <- "C:/Users/Patrick/Documents/Eigene Dokumente/Dokumente/Persoenliches/Lebenslauf/Coursera/01_R_Foundations/02_Codes/Capstone/R-coding/Katz-Back-Off/data/english3/english3.txt"
dictionary <- read.table(url)
names(dictionary) <- "english_word"

# take only english words
is_english <- overall_unigrams$word %in% dictionary$english_word
overall_unigrams <- overall_unigrams[is_english,]

names(overall_unigrams) <- c("ngram", "freq")
names(overall_bigrams) <- c("ngram", "freq")
names(overall_trigrams) <- c("ngram", "freq")

# we delete the most rare word/ word combinations
overall_unigrams <- overall_unigrams %>% filter(freq>=5)
overall_bigrams <- overall_bigrams %>% filter(freq>=5)
overall_trigrams <- overall_trigrams %>% filter(freq>=5)

save(list = c("overall_bigrams", "overall_trigrams", "overall_unigrams", "test_data_overall"), file = "./save_data/data.Rdata")
