##### WORDLE SOLUTIONS TO DATE ##### 

setwd("/Users/sdhart/Downloads")

pacman::p_load(tidyverse, data.table, rvest, janitor)

# url for wordle answers 
wordleAnswersUrl <- 'https://www.stadafa.com/2021/09/every-wordle-world-so-far-excluding.html'

# initial web scrape 
wordleScrapeRaw <- 
  wordleAnswersUrl %>% 
  read_html() %>% 
  html_elements('p') %>% 
  html_text2() 

# extract wordle answers and clean it up to have just the wordle number, answer, and date
max_wordle_row <- 6 + round(as.numeric(difftime(Sys.Date(), '2021-06-19', units = 'days')))
wordleScrapeData <- sub("^([^-]*-[^-]*).*", "\\1", wordleScrapeRaw[7:max_wordle_row])
# wordleScrapeData

# extract the wordle number 
wordleNumberVector <-
  sapply(str_extract_all(wordleScrapeData, ".*\\."), paste, collapse = ' ') %>%
  str_remove(., '\\.')

# extract the correct wordle solution 
wordleAnswersVector <-
  sapply(str_extract_all(wordleScrapeData, "\\b[A-Z]+\\b"),
         paste,
         collapse = ' ')

# extract the date of the wordle 
wordleDateVector <- 
  sapply(str_extract_all(wordleScrapeData, "\\-\\s.*"),
         paste,
         collapse = ' ') %>%
  str_remove(., '\\-\\s') %>% 
  trimws()

# tidy the date, wordle number, and solution into a single df 
wordleAnswersByDateTidy <- tibble(
  wordle_nbr = as.numeric(wordleNumberVector),
  date = wordleDateVector,
  answer = wordleAnswersVector
) %>% 
  mutate(date = as_date(dmy(date))) %>% 
  arrange(desc(wordle_nbr))

##### END WORDLE SOLUTIONS ##### 



##### WORDLE DICTIONARY ##### 

# Start by getting all 5-letter word possibilities (not filtered by wordle-only words) 
validSolutionsDf <- fread('valid_solutions.csv', header = T) %>% clean_names() %>% mutate(type = 'valid_sol')
validGuessesDf <- fread('valid_guesses.csv', header = T) %>% clean_names() %>% mutate(type = 'valid_guess')
valid_dt <- bind_rows(validSolutionsDf, validGuessesDf)

uniquenessRaw <- valid_dt %>% 
  mutate(
    g1 = substr(word, 1, 1), 
    g2 = substr(word, 2, 2), 
    g3 = substr(word, 3, 3), 
    g4 = substr(word, 4, 4), 
    g5 = substr(word, 5, 5),
    g12 = substr(word, 1, 2), 
    g13 = paste0(substr(word, 1, 1), substr(word, 3, 3)), 
    g14 = paste0(substr(word, 1, 1), substr(word, 4, 4)), 
    g15 = paste0(substr(word, 1, 1), substr(word, 5, 5)), 
    g23 = substr(word, 2, 3),
    g24 = paste0(substr(word, 2, 2), substr(word, 4, 4)), 
    g25 = paste0(substr(word, 2, 2), substr(word, 5, 5)), 
    g34 = substr(word, 3, 4),
    g35 = paste0(substr(word, 3, 3), substr(word, 5, 5)), 
    g45 = substr(word, 4, 5),
    g123 = substr(word, 1, 3),
    g124 = paste0(substr(word, 1, 2), substr(word, 4, 4)), 
    g125 = paste0(substr(word, 1, 2), substr(word, 5, 5)), 
    g134 = paste0(substr(word, 1, 1), substr(word, 3, 4)), 
    g135 = paste0(substr(word, 1, 1), substr(word, 3, 3), substr(word, 5, 5)), 
    g145 = paste0(substr(word, 1, 1), substr(word, 4, 5)), 
    g234 = substr(word, 2, 4),
    g235 = paste0(substr(word, 2, 3), substr(word, 5, 5)), 
    g245 = paste0(substr(word, 2, 2), substr(word, 4, 5)), 
    g345 = substr(word, 3, 5),
    g1234 = substr(word, 1, 4),
    g1235 = paste0(substr(word, 1, 3), substr(word, 5, 5)), 
    g1245 = paste0(substr(word, 1, 2), substr(word, 4, 5)),
    g1345 = paste0(substr(word, 1, 1), substr(word, 3, 5)), 
    g2345 = substr(word, 2, 5)
  ) 

wordle_solver_dt <- uniquenessRaw %>% 
  left_join(valid_dt, by = c('word', 'type')) %>% 
  mutate(word = toupper(word)) %>%
  left_join(wordleAnswersByDateTidy, by = c('word' = 'answer'))


# word list 
wordListTable <- uniquenessRaw %>% 
  left_join(valid_dt, by = c('word', 'type')) %>% 
  mutate(word = toupper(word)) %>% 
  left_join(wordleAnswersByDateTidy, by = c('word' = 'answer')) %>% 
  filter(type == 'valid_sol' & is.na(date)) %>% 
  # pull(word)
  as.data.table()

wordListStr <- wordListTable %>% pull(word)

count_vowels <- function(word) {
  str_count(word, "[aeiouyAEIOUY]")
}

lowVowelWordListStr <- wordListTable %>%
  mutate(vowel_count = count_vowels(word)) %>%
  filter(vowel_count == 1) %>%
  pull(word)

##### END WORDLE DICTIONARY ##### 



### Wordle App Functions 

# 1. Function to calculate Wordle score for a guess against the target word
wordleScore <- function(guess, target) {
  sum(toupper(guess) == toupper(strsplit(target, '')[[1]]))
}

# 2. Function to find the best next word from a word list
findBestWord <- function(wordListStr) {
  bestScore <- 0
  bestWord <- ""
  
  for (i in seq_along(wordListStr)) {
    word <- wordListStr[i]
    totalScore <- 0
    
    for (j in seq_along(wordListStr)) {
      if (i != j) {
        target <- wordListStr[j]
        commonLetters <- intersect(strsplit(word, '')[[1]], strsplit(target, '')[[1]])
        totalScore <- totalScore + length(commonLetters)
      }
    }
    
    # Update the best word if the total score is higher
    if (totalScore > bestScore) {
      bestScore <- totalScore
      bestWord <- word
    }
  }
  
  return(list(word = bestWord, score = bestScore))
}

findBestWord <- function(wordListStr, top_n = 5) {
  wordScores <- data.frame(word = character(), score = numeric(), stringsAsFactors = FALSE)
  
  for (i in seq_along(wordListStr)) {
    word <- wordListStr[i]
    totalScore <- 0
    
    for (j in seq_along(wordListStr)) {
      if (i != j) {
        target <- wordListStr[j]
        commonLetters <- intersect(strsplit(word, '')[[1]], strsplit(target, '')[[1]])
        totalScore <- totalScore + length(commonLetters)
      }
    }
    
    # Store the word and its score
    wordScores <- rbind(wordScores, data.frame(word = word, score = totalScore, stringsAsFactors = FALSE))
  }
  
  # Sort by score in descending order and get the top N results
  topWords <- wordScores %>% arrange(desc(score)) %>% head(top_n)
  
  return(topWords)
}

# Find the top 5 starting words
# topWords <- findBestWord(wordListStr, top_n = 5)
# topConsonantWords <- findBestWord(lowVowelWordListStr, top_n = 5)

# Find the best starting word
# bestWord <- findBestWord(wordListStr)

# Print the result
# cat("Best Starting Word:", bestWord$word, "\n")
# cat("Score:", bestWord$score, "\n")


# 3. 
wordGuessFunction <- function(guess, yellows = NULL, greens = NULL, greys = NULL) {
  
  # test guess 
  # guess <- 'arise'
  
  # convert to uppercase 
  guess <- toupper(guess) 
  
  # test
  # greens <- NULL
  # greens <- c(5)
  # greens <- c(2, 3)
  
  # Initialize empty variables for greens
  green_1 <- NA
  green_2 <- NA
  green_3 <- NA
  green_4 <- NA
  green_5 <- NA
  
  # Handle greens input
  if (!is.null(greens)) {
    for (green in greens) {
      if (green %in% 1:5) {
        assign(paste0("green_", green), substr(x = guess, start = green, stop = green))
      }
    }
  }
  
  # test yellows 
  # yellows <- NULL
  # yellows <- c(1)
  # yellows <- c(1,4)
  
  # Initialize empty variables for yellows
  yellow_1 <- NA
  yellow_2 <- NA
  yellow_3 <- NA
  yellow_4 <- NA
  yellow_5 <- NA
  
  # Handle yellows input
  if (!is.null(yellows)) {
    for (yellow in yellows) {
      if (yellow %in% 1:5) {
        assign(paste0("yellow_", yellow), substr(x = guess, start = yellow, stop = yellow))
      }
    }
  }
  
  
  # test greys 
  # greys <- 'aris'
  # greys <- NULL
  
  # Handle greys input
  greys_filter <- paste(strsplit(toupper(greys), '')[[1]], collapse = '|')
  
  # Filter the wordlist table
  nextWordList <- wordListTable %>%
    
    # Green letters filter 
    filter(
      if (!is.na(green_1)) g1 == green_1 else TRUE,
      if (!is.na(green_2)) g2 == green_2 else TRUE,
      if (!is.na(green_3)) g3 == green_3 else TRUE,
      if (!is.na(green_4)) g4 == green_4 else TRUE,
      if (!is.na(green_5)) g5 == green_5 else TRUE
    ) %>%
    
    # Yellow letters filter 
    filter(
      if (!is.na(yellow_1)) str_detect(word, toupper(yellow_1)) & substr(word, 1, 1) != toupper(yellow_1) else TRUE, 
      if (!is.na(yellow_2)) str_detect(word, toupper(yellow_2)) & substr(word, 2, 2) != toupper(yellow_2) else TRUE,
      if (!is.na(yellow_3)) str_detect(word, toupper(yellow_3)) & substr(word, 3, 3) != toupper(yellow_3) else TRUE, 
      if (!is.na(yellow_4)) str_detect(word, toupper(yellow_4)) & substr(word, 4, 4) != toupper(yellow_4) else TRUE, 
      if (!is.na(yellow_5)) str_detect(word, toupper(yellow_5)) & substr(word, 5, 5) != toupper(yellow_5) else TRUE
    ) %>% 
    
    # Grey letters filter 
    filter(
      !str_detect(word, greys_filter)
    ) %>% 
    
    as.data.table()
  
  findBestWord(nextWordList$word)
  
}


wordGuessFunction('crash', yellows = c(2,3,4), greens = NULL, greys = 'ch')



wordListTable %>% 
  filter(g5 == 'E' & !str_detect(word, 'A|R|I|S'))
