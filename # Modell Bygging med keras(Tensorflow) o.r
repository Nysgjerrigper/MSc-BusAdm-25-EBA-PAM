# Modell Bygging med keras(Tensorflow) og R
rm(list = ls(all = TRUE))

library(keras)
library(tensorflow)
library(tidyverse)
# Henter data (lokalt)

data <- read_csv("Differensiert gw alle tre sesonger(22-24), heltall.csv")

