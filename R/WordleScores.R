library(rtweet)
library(tidyverse)
library(lubridate)
library(data.table)

#' wordle stats script 
setwd("/Users/sam.hart/Downloads")

api_key <- "neGKT32lRO5fkcoGrEuVSsIkU"
api_secret_key <- "ovJuG1FJ8B20iRKYf7lwwKlB6cNW4467qJPb2fTIVMeSI4PsqW"
access_token <- "25075575-8yTbHKq3ZoTR29sVmBxByi0gY1gKHNbCApdTCIvUH"
access_token_secret <- "sqGpL0MDlEKSbWTBCPC8YfwLQ9AJrGqY4e1znYlsmGfUK"

## authenticate via web browser ------------------------------------------------
TwitterAuthToken <- create_token(
  # create_token(
  app = "SamuelFFC",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret, 
  set_renv = FALSE
)

wordleTweetStats <- rtweet::get_timeline(
  user = c('WordleStats'), 
  n = 150, 
  parse = T, 
  token = TwitterAuthToken, 
  q = c('-is:reply') # remove tweet replies from the api request 
)

CleanWordleStats <- wordleTweetStats %>%
  filter(str_detect(text, '#Wordle [:digit:]')) %>% 
  select(text) %>%
  plain_tweets() %>%
  separate(
    col = text,
    into = c('results', 'hard_mode', 'scores'),
    sep = '\\. '
  ) %>%
  separate(
    col = scores,
    into = c('pct1', 'pct2', 'pct3', 'pct4', 'pct5', 'pct6', 'pctX'),
    sep = '\\% ',
    extra = 'drop'
  ) %>%
  mutate(
    across(
      c('pct1', 'pct2', 'pct3', 'pct4', 'pct5', 'pct6'),
      ~ str_remove(., '[:digit:]: ')
    ),
    across(c('pctX'), ~ str_remove(., 'X: ')),
    results = str_remove(results, ' results found on Twitter'),
    results = str_remove(results, '#Wordle [:digit:]..\\s')
  ) %>%
  separate(
    col = results,
    into = c('wordle_date', 'tot_results'),
    sep = '\\s'
  ) %>%
  mutate(tot_results = str_remove(tot_results, '[:punct:]')) %>%
  mutate(across(everything(), type.convert, as.is = TRUE)) %>%
  mutate(
    n1 = round(1 * (pct1 / 100), 2),
    n2 = round(2 * (pct2 / 100), 2),
    n3 = round(3 * (pct3 / 100), 2),
    n4 = round(4 * (pct4 / 100), 2),
    n5 = round(5 * (pct5 / 100), 2),
    n6 = round(6 * (pct6 / 100), 2),
    nX = round(7 * (pctX / 100), 2)
  ) %>%
  mutate(weighted_daily_avg = rowSums(select(., starts_with('n'))),
         wordle_date = as_date(ymd(wordle_date))) %>%
  select(-hard_mode) %>%
  filter(wordle_date >= '2022-12-01' &
           wordle_date < '2023-01-01') %>%
  as.data.table()

fwrite(CleanWordleStats, '2022_12_WordleStats.csv')


# df <- 
#   list.files(path = "/Users/admin/apps/csv-courses/", pattern = "*.csv") %>% 
#   map_df(~fread(.))
# df

# combine all wordle CSVs into a single df 
hx_wordleStats <- list.files(pattern = '*.WordleStats.csv') %>%
  map_df( ~ fread(.)) %>% 
  mutate(wordle_date = as_date(ymd(wordle_date)))



#' Wordle Difficult Rating Per Day 
#' 
#' analyisis of how difficult the wordle is for a day given the following: 
#' 
#' /uniqueness -- how many 5-letter words have similar letter placement combinations (e.g. how many 5-letter words end with 'INE')
#' /familiarity -- how common does the word occur in regular speech 
#' /singularity -- number of distinct letters in the word / number of words with those letters in them 
#' 

# Start by getting all 5-letter word possibilities (not filtered by wordle-only words) 
validSolutionsDf <- fread('valid_solutions.csv') %>% clean_names()
validGuessesDf <- fread('valid_guesses.csv') %>% clean_names()


wordOfTheDay <- 'grimy'

# uniqueness -- how many 5-letter words have the same letter placement

# all substring combinations from the solutions 
uniquenessRaw <- validSolutionsDf %>% 
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
    g1235 = paste0(substr(word, 1, 2), substr(word, 3, 5)), 
    g1345 = paste0(substr(word, 1, 1), substr(word, 3, 5)), 
    g2345 = substr(word, 2, 5)
  ) 

# grouped matrix of all substring combinations 
uniquenessDf <- uniquenessRaw %>% 
  select(-word) %>% 
  pivot_longer(everything(), names_to = 'position', values_to = 'substr') %>% 
  group_by(substr, position) %>% 
  summarise(substr_freq = n())

# join with valid solutions 
joinUniqueDf <- uniquenessDf %>% pivot_wider(names_from = position, values_from = substr_freq)

# generate uniquenessScore 
finalUniquenessScore <- 
  uniquenessRaw %>% 
  mutate(word = toupper(word)) %>% 
  left_join(select(joinUniqueDf, substr, g1), by = c('g1' = 'substr')) %>% 
  left_join(select(joinUniqueDf, substr, g2), by = c('g2' = 'substr')) %>% 
  left_join(select(joinUniqueDf, substr, g3), by = c('g3' = 'substr')) %>% 
  left_join(select(joinUniqueDf, substr, g4), by = c('g4' = 'substr')) %>% 
  left_join(select(joinUniqueDf, substr, g5), by = c('g5' = 'substr')) %>% 
  left_join(select(joinUniqueDf, substr, g12), by = c('g12' = 'substr')) %>% 
  left_join(select(joinUniqueDf, substr, g13), by = c('g13' = 'substr')) %>% 
  left_join(select(joinUniqueDf, substr, g14), by = c('g14' = 'substr')) %>% 
  left_join(select(joinUniqueDf, substr, g15), by = c('g15' = 'substr')) %>% 
  left_join(select(joinUniqueDf, substr, g23), by = c('g23' = 'substr')) %>% 
  left_join(select(joinUniqueDf, substr, g24), by = c('g24' = 'substr')) %>% 
  left_join(select(joinUniqueDf, substr, g25), by = c('g25' = 'substr')) %>% 
  left_join(select(joinUniqueDf, substr, g34), by = c('g34' = 'substr')) %>% 
  left_join(select(joinUniqueDf, substr, g35), by = c('g35' = 'substr')) %>% 
  left_join(select(joinUniqueDf, substr, g45), by = c('g45' = 'substr')) %>% 
  left_join(select(joinUniqueDf, substr, g123), by = c('g123' = 'substr')) %>% 
  left_join(select(joinUniqueDf, substr, g124), by = c('g124' = 'substr')) %>% 
  left_join(select(joinUniqueDf, substr, g125), by = c('g125' = 'substr')) %>% 
  left_join(select(joinUniqueDf, substr, g134), by = c('g134' = 'substr')) %>% 
  left_join(select(joinUniqueDf, substr, g135), by = c('g135' = 'substr')) %>% 
  left_join(select(joinUniqueDf, substr, g145), by = c('g145' = 'substr')) %>% 
  left_join(select(joinUniqueDf, substr, g234), by = c('g234' = 'substr')) %>% 
  left_join(select(joinUniqueDf, substr, g235), by = c('g235' = 'substr')) %>% 
  left_join(select(joinUniqueDf, substr, g345), by = c('g345' = 'substr')) %>% 
  left_join(select(joinUniqueDf, substr, g1234), by = c('g1234' = 'substr')) %>% 
  left_join(select(joinUniqueDf, substr, g1235), by = c('g1235' = 'substr')) %>% 
  left_join(select(joinUniqueDf, substr, g1345), by = c('g1345' = 'substr')) %>% 
  left_join(select(joinUniqueDf, substr, g2345), by = c('g2345' = 'substr')) %>% 
  mutate(all_combinations = rowSums(select(., ends_with('.y'))) / 5) %>%  # divide by 5 for the 5 different letter slots 
  # group options by str length 
  mutate(
    l1_green = g1.y + g2.y + g3.y + g4.y + g5.y, 
    l2_green = g12.y + g13.y + g14.y + g15.y + g23.y + g24.y + g25.y + g34.y + g35.y + g45.y, 
    l3_green = g123.y + g124.y + g125.y + g134.y + g135.y + g145.y + g234.y + g235.y + g345.y,
    l4_green = g1234.y + g1235.y + g1345.y + g2345.y
  )

# peek to check uniqueness score 
finalUniquenessScore %>% filter(g345 == 'ime')

min_pct_diff_uniq <- min(finalUniquenessScore$all_combinations)
max_pct_diff_uniq <- max(finalUniquenessScore$all_combinations)

uniquenessScaled <- function(uniq) {
  round((uniq - min_pct_diff_uniq) / (max_pct_diff_uniq - min_pct_diff_uniq) * 1, 3)
}

finalUniquenessScore$uniquenessScore <- uniquenessScaled(finalUniquenessScore$all_combinations)

finalUniquenessScore

corMatrix <- hx_wordleStats %>%
  left_join(wordleAnswersMatrix, by = c('wordle_date' = 'date')) %>%
  left_join(finalUniquenessScore, by = c('answer' = 'word')) %>% 
  mutate(perAC = (l1_green + l2_green + l4_green) / all_combinations) %>% 
  # mutate(xFIP = (6 * 4.187374) + (l4_green + l2_green - l1_green) / all_combinations) %>% 
  mutate(xFIP = (l1_green * -0.444) + (l2_green * -0.278) / all_combinations) %>% 
  mutate(across(everything(), ~type.convert(., as.is = TRUE)))

# xFIP = ((13*(Fly balls * lgHR/FB%))+(3*(BB+HBP))-(2*K))/IP + constant
# FIP Constant = lgERA – (((13*lgHR)+(3*(lgBB+lgHBP))-(2*lgK))/lgIP)

ggplot(corMatrix, aes(x = xFIP, y = weighted_daily_avg)) + geom_point()
round(cor(corMatrix[,c(3:9, 17)], corMatrix[,c(80:87)]), 3)
  # cor(OffSummary$PF, OffSummary[c(4:63)])

# /familiarity -- how common does the word occur in regular speech
# downloaded the commonality of 5-letter words based on wikipedia articles from this site: https://en.lexipedia.org/
familiarityRaw <- fread('en_wikt_words_1_5-5.txt') 

names(familiarityRaw) <- c('word', 'word_len', 'word_freq', 'nbr_articles')

familiarityDf <- 
  validSolutionsDf %>% 
  left_join(familiarityRaw, by = 'word') %>% 
  select(-word_len) %>% 
  mutate(word = toupper(word)) %>% 
  mutate(across(where(is.numeric), ~replace_na(., 0)))

familiarityDf$wavg <- ifelse(
  familiarityDf$word_freq > 0,
  yes = familiarityDf$word_freq / (familiarityDf$word_freq + familiarityDf$nbr_articles),
  no = 0
)

quantile(familiarityDf$wavg, 0.001)
quantile(familiarityDf$wavg, 0.999)
min_pct_diff_fam <- quantile(familiarityDf$wavg, 0.001)
max_pct_diff_fam <- quantile(familiarityDf$wavg, 0.999)

familiarityScaled <- function(wavg) {
  round((wavg - min_pct_diff_fam) / (max_pct_diff_fam - min_pct_diff_fam) * 1, 3)
}

familiarityDf$familiarityScore <- familiarityScaled(familiarityDf$wavg)

familiarityDf %>% arrange(desc(familiarityScore))
  


# /singularity -- how many words have repeated letters 
validSolutionsDf[1]


# scrape webpage for wordle answers by date 
suppressPackageStartupMessages(library(rvest))

# url for wordle answers 
wordleAnswersUrl <- 'https://www.stadafa.com/2021/09/every-wordle-world-so-far-excluding.html'

# initial web scrape 
wordleScrapeRaw <- 
  wordleAnswersUrl %>% 
  read_html() %>% 
  html_elements('p') %>% 
  html_text2() 

# extract wordle answers and clean it up to have just the wordle number, answer, and date
wordleScrapeData <- sub("^([^-]*-[^-]*).*", "\\1", wordleScrapeRaw[9:578])

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
wordleAnswersByDateTidy <-
  tibble(wordle_nbr = as.numeric(wordleNumberVector),
             date = wordleDateVector,
             answer = wordleAnswersVector) %>% 
  mutate(date = as_date(dmy(date))) %>% 
  add_row(wordle_nbr = 541, date = ymd('2022-12-12'), answer = 'APPLY') %>% 
  arrange(desc(wordle_nbr))

wordleAnswersByDateTidy %>% filter(date == '2022-12-12')



wordleAnswersMatrix <- 
  wordleAnswersByDateTidy %>% 
  inner_join(select(finalUniquenessScore, word, uniquenessScore), by = c('answer' = 'word')) %>%  # join to get uniquenessScore 
  inner_join(select(familiarityDf, word, familiarityScore), by = c('answer' = 'word')) %>% 
  mutate(finalScore = round((uniquenessScore + familiarityScore) / 2, 3)) %>% 
  arrange(desc(date))


hx_wordleStats %>%
  summarise(mean(weighted_daily_avg))
  left_join(wordleAnswersMatrix, by = c('wordle_date' = 'date')) %>%
  filter(month(wordle_date) == 12) %>%
  arrange(desc(wordle_date))




