% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scrape_data.R
\name{count_medical_terms}
\alias{count_medical_terms}
\title{Count medical glossaries}
\usage{
count_medical_terms(text)
}
\arguments{
\item{text}{Input data. Should be character vector or data frame with character
variable of interest named "text".}
}
\value{
A data frame containing the number of medical words, the number of total words,
  and the ratio of medical words to total words.
}
\description{
Count the number and ratio of medical glossaries used in the text.
}
\details{
The medical glossary dictionary was obtained from Aristotelis P.
  <https://github.com/glutanimate/wordlist-medicalterms-en>.
  It is based on two prominent medical dictionary projects:
  OpenMedSpel by R. Robinson of e-MedTools (Version 2.0.0, released 2014-01-21)
  <http://www.e-medtools.com/openmedspel.html> and
  MTH-Med-Spel-Chek by Rajasekharan N. of MT-Herald (released 2014-04-02)
  <http://mtherald.com/free-medical-spell-checker-for-microsoft-word-custom-dictionary/>.
}
\examples{
## create a character vector
medical_text <- c(
"No, it isn't possible to predict anything before the result of your biopsy is received.",
"Thank you for the nice reply! Very thoughtful answer that did ease my fears!",
"Can't help regards the meds.  Just want to give support.")

## get the medical glossaries counts from a character vector
count_medical_terms(text = medical_text)

## creat a data frame with a character vector named "text"
df <- data.frame(
  id = c(1, 2, 3),
  text = c("No, it isn't possible to predict anything before the result
            of your biopsy is received.",
           "Thank you for the nice reply! Very thoughtful answer that
            did ease my fears!",
           "Can't help regards the meds.  Just want to give support."),
  stringsAsFactors = FALSE
)

## get the medical glossaries counts from a data frame with "text" variable
count_medical_terms(df)

}
