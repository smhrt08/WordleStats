#' SPELLING BEE SOLVER 
#' 
#' @param key_letter Required letter in all answers 
#' @param letter_str All eligible letters in answer

library(words)
library(tidyverse)
library(data.table)

spelling_bee_summary <- function(key_letter, letter_str) {
  
  letter_str <- paste(
    substr(letter_str, 1, 1), 
    substr(letter_str, 2, 2),
    substr(letter_str, 3, 3),
    substr(letter_str, 4, 4),
    substr(letter_str, 5, 5),
    substr(letter_str, 6, 6),
    substr(letter_str, 7, 7),
    sep = '|', collapse = ''
  )
  
  abc_str <- c('abcdefghijklmnopqrstuvwxyz')
  abc_filter_str <- gsub(letter_str, '', abc_str)
  
  abc_filter <- paste(
    substr(abc_filter_str, 1, 1), substr(abc_filter_str, 2, 2), substr(abc_filter_str, 3, 3),
    substr(abc_filter_str, 4, 4), substr(abc_filter_str, 5, 5), substr(abc_filter_str, 6, 6),
    substr(abc_filter_str, 7, 7), substr(abc_filter_str, 8, 8), substr(abc_filter_str, 9, 9),
    substr(abc_filter_str, 10, 10), substr(abc_filter_str, 11, 11), substr(abc_filter_str, 12, 12),
    substr(abc_filter_str, 13, 13), substr(abc_filter_str, 14, 14), substr(abc_filter_str, 15, 15),
    substr(abc_filter_str, 16, 16), substr(abc_filter_str, 17, 17), substr(abc_filter_str, 18, 18),
    substr(abc_filter_str, 19, 19),
    sep = '|', collapse = ''
  )
  
  word_list <- words::words %>% 
    filter(str_detect(word, key_letter)) %>% 
    filter(!str_detect(word, abc_filter)) %>% 
    filter(word_length >= 4) %>% 
    # mutate(word = ifelse(sum(!!str_count(word), letters) == 7, toupper(word), word)) %>% 
    as.data.table()
  
  word_list
}

today_list <- spelling_bee_summary('t', 'ilucont')
reactable::reactable(today_list, pagination = F)
