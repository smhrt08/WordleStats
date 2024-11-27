pacman::p_load(words, data.table, tidyverse)

# set working directory
setwd("/Users/samhart/Downloads")
set.seed(8990)

# load words into df 
nrow(words::words)

WordsDt <- words::words

temp <- slice_sample(WordsDt, n = 20)
letters <- unlist(strsplit(temp$word[1], ''))
paste0(sort(letters), collapse =  '')


# Function to sort letters in a word
sort_letters <- function(word) {
  paste(sort(strsplit(word, "")[[1]]), collapse = "")
}

sort_letters <- function(word_input) {
  paste0(sort(unlist(strsplit(word_input, ''))), collapse = '')
}
sort_letters(temp$word[12])

Sorted_WordsDt <- WordsDt %>% 
  rowwise() %>% 
  mutate(
    word_letter_str = sort_letters(word)
  ) 

Sorted_WordsDt %>% 
  filter(word_length > 4) %>% 
  group_by(word_length, word_letter_str) %>% 
  summarise(total = n()) %>%  
  group_by(word_length) %>% 
  filter(total == 1) %>% 
  count()
