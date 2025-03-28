# Modell Bygging med keras(Tensorflow) og R
rm(list = ls())

library(reticulate)
library(keras)
# library(keras3)
library(tidyverse)
library(tensorflow)
tf$constant("Hello TensorFlow!")

# Install Keras3
install.packages("keras3")
keras::install_keras(backend = "tensorflow")

# Load data
sesong22 <- read_csv("Sesong 22 til 23.csv")
