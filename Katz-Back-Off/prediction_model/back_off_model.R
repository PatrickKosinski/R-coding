##############################################packages#######################################

library(tidytext)
library(stringr)
library(quanteda)
library(dplyr)
library(data.table)

#############################################################################################

# first get the data 

load("~/Eigene Dokumente/Dokumente/Persoenliches/Lebenslauf/Coursera/01_R_Foundations/02_Codes/Capstone/R-coding/Katz-Back-Off/save_data/data.Rdata")

# transform tibbles into data.tables as we use the data-table package 
# to ensure quicker string searches
overall_bigrams <- data.table(overall_bigrams)
overall_trigrams <- data.table(overall_trigrams)
overall_unigrams <- data.table(overall_unigrams)

# data prefixes:
# str = string
# chr = character
# dt = data table
# log = logical
# num = numeric
# int = integer

########################################observed trigrams#######################################

# input_string = w_{i-2}w_{i}
# aim: get the probabilities of observed w_{i-2}w_{i}w
# and unobserved w_{i-2}w_{i}w

# first get the probabilities of observed trigrams
# input input_string = w_{i-2}w_{i}
# function get_observed_trigrams returns all trigrams that start
# as input_string
# output the trigram and frequency

get_observed_trigrams <- function(chr_input_string, dt_overall_trigrams){
  str_search <- paste("^", chr_input_string, " ", sep="")
  log_index <- grepl(str_search, dt_overall_trigrams$ngram)
  # leave one whitespace to make sure that 
  # we get only trigrams starting with input_string
  dt_observed_trigrams <- dt_overall_trigrams[log_index,]
  return(dt_observed_trigrams)
}

# input input_string = w_{i-2}w_{i} and and dt_observed_trigrams
# this will return a data.table with the columns ngram and probability, 
# allocating observed trigrams to probabilities
# gamma is the discount probability that redistributes probability mass from observed to unobserved trigrams

get_observed_trigrams_probability <- function(chr_input_string, dt_observed_trigrams, dt_overall_bigrams, num_gamma){
  int_get_bigram_count <- dt_overall_bigrams[ngram == chr_input_string]$freq
  dt_observed_trigrams$freq <- (dt_observed_trigrams$freq - num_gamma)/int_get_bigram_count
  names(dt_observed_trigrams) <- c("ngram", "probability")
  return(dt_observed_trigrams)
}


########################################unobserved trigrams#####################################

# get_unobs_trigram_probability intends to calculate all probabilities of unobserved trigrams
# input: input_string = w_{i-2}w_{i}
# num_gamma (discount to redistribute probability mass from observed to unobserved trigrams)
# dt_observed_trigrams (obtained by the function get_observed_trigrams)
# dt_overall_unigrams which stores the vocabulary
# dt_overall_bigrams which stores the bigrams

# The idea is that we first calculate the set B (set of all word combinations that are unobserved)
# then we calculate all backed-off bigrams by deleting the first word of w_{i-2}w_{i}w which is w_{i-2} (w element of B)
# - remember: we are operating on the set B
# from all abovementioned backed-off bigrams, we check which of them are observed and which are unobserved
# after this, we calculate set A which is the set of all observed bigrams of the form w_{i}w (w not element of B)
# with the set_A, we calculate the observed/ unobserved backed-off bigram probabilities
# for details on the calculation, please see the szczepaniak-paper that is provided in the repository as well

# output is a data.table with bigrams and probabilities

get_unobs_trigram_probability <- function(chr_input_string, gamma, dt_observed_trigrams, dt_overall_unigrams, dt_overall_bigrams){
  # we get the set of all unobserved trigrams first
  chr_get_set_B <- get_set_B_trigram(dt_observed_trigrams, dt_overall_unigrams)
  int_alpha_w_i_2_w_i_1 <- 1 - sum(get_observed_trigrams_probability(chr_input_string, dt_observed_trigrams , dt_overall_bigrams, gamma)$probability)
  # we calculate all observed backed off bigrams
  dt_get_BO_bigrams <- data.table(ngram = paste(str_split_fixed(chr_input_string, " ", 2)[2], chr_get_set_B ,sep =" "))
  # from all backed off bigrams, we calculate the observed bigrams
  dt_observed_BO_bigrams <- get_observed_BO_bigram(dt_get_BO_bigrams, dt_overall_bigrams)
  # by setting the difference between all backed off bigrams and the observed ones,
  # we get the unobserved backed off bigrams
  dt_unobserved_BO_bigrams <- data.table(ngram = setdiff(dt_get_BO_bigrams$ngram, dt_observed_BO_bigrams$ngram))
  # now we get all observed bigrams starting with the first word of chr_input_string
  dt_get_set_A <- get_set_A_bigram(chr_input_string, dt_overall_bigrams)
  # we calculate all observed bigram probabilities
  dt_set_A_prob <- get_obs_bigram_probability(gamma, dt_get_set_A, dt_overall_unigrams)
  dt_observed_BO_bigrams_prob <- dt_set_A_prob[dt_set_A_prob$ngram %in% dt_observed_BO_bigrams$ngram]
  # get unobserved bigram probabilities
  dt_unobserved_BO_bigrams_prob <- get_unobs_bigram_probability(dt_unobserved_BO_bigrams, dt_set_A_prob, dt_overall_unigrams)
  dt_numerator_prob <- rbind(dt_observed_BO_bigrams_prob, dt_unobserved_BO_bigrams_prob)
  rm(dt_observed_BO_bigrams_prob)
  rm(dt_unobserved_BO_bigrams_prob)
  int_denominator <- sum(dt_numerator_prob$probability)
  dt_numerator_prob <- dt_numerator_prob %>% mutate(probability = int_alpha_w_i_2_w_i_1 * probability)
  chr_input_string_help <- unlist(str_split(chr_input_string, " ", 2))[1]
  dt_unobserved_trigram_prob <- data.table(ngram = paste(chr_input_string_help, dt_numerator_prob$ngram, sep = " "), probability = dt_numerator_prob$probability/int_denominator)
  result <- dt_unobserved_trigram_prob
  return(result)
}

##############################help functions: unobserved trigrams#####################################


# the following function determines the set B(w_{i-2}, w_{i-1}) for the input_string
# B = all unigrams that complete unobserved trigrams
# input dt_observed_trigrams and then take the set of differences between
# known trigram endings and the overall vocabularly (i.e. dt_overall_unigrams)


get_set_B_trigram <- function(dt_observed_trigrams, dt_overall_unigrams){
  chr_last_word_observed <- str_split_fixed(dt_observed_trigrams$ngram, " ",3)[, 3]
  # use str_split_fixed because we use a data.table of characters
  chr_set_B_trigram <- setdiff(dt_overall_unigrams$ngram, chr_last_word_observed)
  return(chr_set_B_trigram)
}


# Now, we have all possible back-off-bigrams w_{i-1}w
# We have to distinguish between observed back-off-bigrams w_{i-1}w
# and unobserved ones.
# Function "get_observed_BO_bigram" provides the observed 
# back-off-bigrams w_{i-1}w
# input: dt_BO_bigram which contains all observed bigrams
# get observed back-off bigrams by comparing all back_off bigrams
# with the dt_overall_bigrams data.table

get_observed_BO_bigram <- function(dt_BO_bigram, dt_overall_bigrams){
  log_index <- dt_BO_bigram$ngram %in% dt_overall_bigrams$ngram
  dt_observed_BO_bigram <- dt_BO_bigram[log_index]
  # let's add the frequencies to the character vector
  #dt_observed_BO_bigram <- data.table(ngram = chr_observed_BO_bigram)
  dt_observed_BO_bigram <- left_join(x = dt_observed_BO_bigram, y = dt_overall_bigrams, by = "ngram")
  return(dt_observed_BO_bigram)
}

# get_set_A_bigram calculates all observed bigrams of the form w_{i-1}w
# difference between get_observed_BO_bigram and get_set_A_bigram:
# we look at get_set_A_bigram into all observed_bigrams
# in get_observed_BO_bigram, we did a pre-selection when reducing
# unobserved trigrams to bigrams. These bigrams can be either observed
# or unobserved.

get_set_A_bigram <- function(chr_input_string, dt_overall_bigrams){
  chr_w_i <- str_split_fixed(chr_input_string, " ", 2)[1,2]
  str_search <- paste("^", chr_w_i, " ", sep="")
  log_index <- grepl(str_search, dt_overall_bigrams$ngram)
  dt_set_A_bigram <- dt_overall_bigrams[log_index,]
  return(dt_set_A_bigram)
}

get_obs_bigram_probability <- function(num_gamma, dt_observed_BO_bigram, dt_unigrams){
  # get w_{i-1}. As w_{i-1} is for every entry in the data.table the same
  # it is sufficient to take the first entry of the first column from
  # the str_split_fixed function
  
  # check whether dt_observed_BO_bigram is an empty data.table
  
  if(nrow(dt_observed_BO_bigram) == 0){
    dt_observed_BO_bigram <- dt_observed_BO_bigram[0,]
    names(dt_observed_BO_bigram) <- c("ngram", "probability")
    return(dt_observed_BO_bigram)
  }
  else{
    chr_w_i_1 <- str_split_fixed(dt_observed_BO_bigram$ngram, " ",2)[1,1]
    int_count_w_i_1 <- dt_unigrams[ngram == chr_w_i_1]$freq
    # what if we dont have a unigram here?
    # then we the length value of int_count_w_i_1 will be 0
    if(length(int_count_w_i_1) == 0){
      dt_observed_BO_bigram <- dt_observed_BO_bigram[0,]
      names(dt_observed_BO_bigram) <- c("ngram", "probability")
      return(dt_observed_BO_bigram)
    }else{
    dt_observed_BO_bigram$freq <- (dt_observed_BO_bigram$freq - num_gamma)/int_count_w_i_1
    names(dt_observed_BO_bigram) <- c("ngram", "probability")
    return(dt_observed_BO_bigram)}
  }}


# now, we calculate unobserved bigrams
# set B = second part of unobserved bigrams


get_unobs_bigram_probability <- function(dt_unobserved_BO_bigram, dt_observed_BO_bigram, dt_unigrams){
  # either a bigram is observed or it is not observed
  # per definition of alpha: alpha = 1 - sum of all observed bigrams
  int_alpha_w_i_1 <- 1 - sum(dt_observed_BO_bigram$probability)
  #int_sum_freq <- sum(dt_unigrams$freq)
  chr_get_w_i <- data.table(ngram = str_split_fixed(dt_unobserved_BO_bigram$ngram, " ", 2)[, 2])
  dt_get_freq_w_i <- left_join(x = chr_get_w_i, y = dt_unigrams, by = "ngram")
  denominator <- sum(dt_get_freq_w_i$freq)
  dt_unobserved_BO_bigram <- dt_unobserved_BO_bigram %>% mutate(probability = (int_alpha_w_i_1 * dt_get_freq_w_i$freq)/denominator)
  return(dt_unobserved_BO_bigram)
}


########################################prediction########################################

# now, we "put everything together"
# input: chr_input_string (our input string) 
# num_gamma (the discount which redistributes probability mass from observed to unobserved word combination)
# dt_overall_unigrams, dt_overall_bigrams, dt_overall_trigrams as always

# idea: We first calculate the probability of observed trigrams
# if more than 50% of the probability mass is distributed across the observed trigrams, then 
# we assume that most probably predictions are amongst observed trigrams as well
# if less than 50% of the probability mass is distributed across observed trigrams, then
# we call the get_unobs_trigram_probability in order to calculate 
# probabilities of unobserved trigrams.

prediction_function <- function(chr_input_string, num_gamma, dt_overall_unigrams, dt_overall_bigrams, dt_overall_trigrams){
  dt_observed_trigrams <- get_observed_trigrams(chr_input_string, dt_overall_trigrams)
  dt_get_observed_trigrams_probability <- get_observed_trigrams_probability(chr_input_string, dt_observed_trigrams, dt_overall_bigrams, num_gamma)
  if(sum(dt_get_observed_trigrams_probability$probability) >= 0.5 ){
      dt_get_observed_trigrams_probability <- dt_get_observed_trigrams_probability[order(-probability),]
      dt_get_observed_trigrams_probability <- dt_get_observed_trigrams_probability[1:3,]
      # get the last words
      dt_get_observed_trigrams_probability$ngram <-  sapply(dt_get_observed_trigrams_probability$ngram, function(x) tail(strsplit(x, split=" ")[[1]],1))
      return(dt_get_observed_trigrams_probability)
  }else{
    dt_get_unobserved_trigrams_probability <- get_unobs_trigram_probability(chr_input_string, num_gamma, dt_observed_trigrams , dt_overall_unigrams, dt_overall_bigrams)
    dt_get_trigrams_probability <- rbind(dt_get_observed_trigrams_probability, dt_get_unobserved_trigrams_probability)
    dt_get_trigrams_probability <- dt_get_trigrams_probability[order(-probability),]
    dt_get_trigrams_probability <- dt_get_trigrams_probability[1:3,]
    # get the last words
    dt_get_trigrams_probability$ngram <-  sapply(dt_get_trigrams_probability$ngram, function(x) tail(strsplit(x, split=" ")[[1]],1))
    return(dt_get_trigrams_probability)
  }
}

prediction_function("i love", 0.1, overall_unigrams, overall_bigrams, overall_trigrams)

save.image("~/Eigene Dokumente/Dokumente/Persoenliches/Lebenslauf/Coursera/01_R_Foundations/02_Codes/Capstone/R-coding/Katz-Back-Off/save_data/data.RData")
