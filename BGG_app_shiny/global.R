library(shiny)
library(shinydashboard)
library(tidyverse)
source('helpers.R')

games = read.csv('data/games_cleaned.csv')

numbers_of_bins = 5
games = games%>%
  mutate(owned_bins = cut(owned, 
                          breaks = unique(quantile(owned,probs=seq.int(0,1, by=1/numbers_of_bins))), 
                          include.lowest=TRUE))
