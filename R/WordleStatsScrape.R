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
  n = 100, 
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
  filter(wordle_date >= '2022-08-01' &
           wordle_date < '2022-09-01') %>%
  as.data.table()

fwrite(CleanWordleStats, '2022_08_WordleStats.csv')
