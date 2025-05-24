install.packages(c("tm", "tokenizers", "qdapRegex", "stringi", 
                   "dplyr", "stringr", "SnowballC", "textstem"))
library(tm)
library(tokenizers)
library(qdapRegex)
library(stringi)
library(dplyr)
library(stringr)
library(SnowballC)
library(textstem)

replace_contraction <- function(text) {
  contractions <- c("n't" = " not", "'re" = " are", "'s" = " is", 
                    "'d" = " would", "'ll" = " will", "'t" = " not", 
                    "'ve" = " have", "'m" = " am",'won’t'='will not','Trump’s' ='Trump',' Zelensky’s'=' Zelensky')
  for (pattern in names(contractions)) {
    text <- gsub(pattern, contractions[pattern], text)
  }
  return(text)
}

normalize_politics <- function(text) {
  text <- str_replace_all(text, "\\b(awamilig|al league|al\\b|awami\\b)\\b", "Awami League")
  text <- str_replace_all(text, "\\b(bnp|bangladesh nationalist party)\\b", "BNP")
  text <- str_replace_all(text, "\\b(jamaat|jamaat-e-islami)\\b", "Jamaat-e-Islami")
  text <- str_replace_all(text, "\\b(pm|prime minister)\\b", "Prime Minister")
  text <- str_replace_all(text, "\\b(mp|member of parliament)\\b", "Member of Parliament")
  text <- str_replace_all(text, "\\b(ec|election commission)\\b", "Election Commission")
  text <- str_replace_all(text, "\\b(govt|gov|government\\b)", "Government")
  return(text)
}

preprocess_text <- function(text) {
  if (is.na(text) || nchar(text) < 2) return("")
  
  # Step-by-step cleaning
  text <- replace_contraction(text)
  text <- normalize_politics(text)
  text <- tolower(text)
  text <- rm_url(text)
  text <- removePunctuation(text)
  text <- removeNumbers(text)
  text <- stripWhitespace(text)
  
  # Tokenize
  tokens <- unlist(tokenize_words(text))
  
  # Remove stopwords
  tokens <- tokens[!tokens %in% stopwords("en")]
  
  # Stemming
  stemmed <- wordStem(tokens)
  
  # Lemmatization
  lemmatized <- lemmatize_words(stemmed)
  
  # Return clean string
  return(paste(lemmatized, collapse = " "))
}

data <- read.csv("ids_final_project_group_11_news_raw.csv", stringsAsFactors = FALSE)

data$clean_title <- sapply(data$title, preprocess_text)
data$clean_content <- sapply(data$content, preprocess_text)

write.csv(data, "ids_final_project_group_11_news_clean.csv", row.names = FALSE)




