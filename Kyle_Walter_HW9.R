library(tidyverse)
ChessDf <- read_csv("H:/Graduate School/IST687 - Into to Data Science/Project/games.csv")
str(ChessDf)
length(unique(ChessDf$id))
ChessDf[duplicated(ChessDf$id),]
ChessDups <- ChessDf[duplicated(ChessDf$id),]
ChessDups <- ChessDups[order(ChessDups$id),]
ChessDf <- distinct(ChessDf)
histplot(ChessDf$rated)
hist(ChessDf$rated)
library(impute)
library(inpute)
library(imputeTS)
library(imputeTS)
ChessDf$rated <- na_replace(ChessDf$rated)
hist(ChessDf$rated)
str(ChessDf$rated)
ChessDf$white_rating
hist(ChessDf$white_rating)
hist(ChessDf$black_rating)
ChessDf$winner
neuralnet::neuralnet()