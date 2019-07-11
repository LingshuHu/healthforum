## code to prepare `medical_words` dataset goes here

## CSV from so and so
medical_words <- readr::read_csv("data-raw/medical_words.csv")

## drop row names
medical_words <- medical_words[, -1]

## save medical_words as exported package data
usethis::use_data("medical_words")
