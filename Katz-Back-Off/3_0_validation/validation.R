# Evalualuation - here we measure how accurate the preidction algorithm is
# measure: How well does the model perform on the test data set?
# Form all possible trigrams out of the test_data_overall
# Get the first two words and predict the third word
# compare prediction with real last word
# calculate accuracy in %

# first we load data/ functions into the global environment
load("~/Eigene Dokumente/Dokumente/Persoenliches/Lebenslauf/Coursera/01_R_Foundations/02_Codes/Capstone/R-coding/Katz-Back-Off/save_data/data.RData")

# form the bigrams on the test data set
test_data_overall_trigrams <- test_data_overall %>% unnest_tokens(trigram, text, token = "ngrams", n = 3)
help_string <- str_split_fixed(test_data_overall_trigrams$trigram, " ",3)
help_bigrams <- paste(help_string[,1], help_string[,2], " ")
help_bigrams <- str_trim(help_bigrams, side = "right")
# bigrams will be the input for the prediction algorithm
test_data_overall_trigrams$bigrams <- help_bigrams
rm(help_bigrams)
# get the last word to be able to calculate accuracy
test_data_overall_trigrams$last_word <- str_split_fixed(test_data_overall_trigrams$trigram, " ",3)[, 3]

# now we apply the prediction algorithm for the first 1000 entries (using 
# all of them would be too time expensive)
gamma <- 0.5

result <- rep(0, times = 1000)
for(i in c(1:10000)){
        result[i] <- prediction_function(test_data_overall_trigrams$bigrams[i], gamma, overall_unigrams, overall_bigrams, overall_trigrams)
        print(i)
}

test_data_overall_trigrams_prediction <- test_data_overall_trigrams[1:1000,]
test_data_overall_trigrams_prediction$prediction <- result
test_data_overall_trigrams_prediction$correct <- test_data_overall_trigrams_prediction$prediction == test_data_overall_trigrams_prediction$last_word
validation <- sum(test_data_overall_trigrams_prediction$correct/1000)*100


# validation gamma = 0.9
gamma <- 0.9

result_2 <- rep(0, times = 1000)
for(i in c(1:1000)){
        result_2[i] <- prediction_function(test_data_overall_trigrams$bigrams[i], gamma, overall_unigrams, overall_bigrams, overall_trigrams)
        print(i)
}

test_data_overall_trigrams_prediction$prediction <- result_2
test_data_overall_trigrams_prediction$correct <- test_data_overall_trigrams_prediction$prediction == test_data_overall_trigrams_prediction$last_word
validation_2 <- sum(test_data_overall_trigrams_prediction$correct/1000)*100

# validation gamma = 0.1
gamma <- 0.1

result_3 <- rep(0, times = 1000)
for(i in c(1:1000)){
        result_3[i] <- prediction_function(test_data_overall_trigrams$bigrams[i], gamma, overall_unigrams, overall_bigrams, overall_trigrams)
        print(i)
}

test_data_overall_trigrams_prediction$prediction <- result_3
test_data_overall_trigrams_prediction$correct <- test_data_overall_trigrams_prediction$prediction == test_data_overall_trigrams_prediction$last_word
validation_3 <- sum(test_data_overall_trigrams_prediction$correct/1000)*100


gamma <- 0.001

result_4 <- rep(0, times = 1000)
for(i in c(1:1000)){
        result_4[i] <- prediction_function(test_data_overall_trigrams$bigrams[i], gamma, overall_unigrams, overall_bigrams, overall_trigrams)
        print(i)
}

test_data_overall_trigrams_prediction$prediction <- result_4
test_data_overall_trigrams_prediction$correct <- test_data_overall_trigrams_prediction$prediction == test_data_overall_trigrams_prediction$last_word
validation_4 <- sum(test_data_overall_trigrams_prediction$correct/1000)*100


# Why does a low gamma leads to sligthly higher correctness of predictions?
# the slower the gamma, the more probability mass lies on observed bigrams
# and trigrams

