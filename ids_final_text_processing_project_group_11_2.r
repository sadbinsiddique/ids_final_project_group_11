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
  contractions <- c(
    "won't" = "will not", "can't" = "cannot", "shan't" = "shall not",
    "n't" = " not", "'re" = " are", "'s" = " is", "'d" = " would",
    "'ll" = " will", "'t" = " not", "'ve" = " have", "'m" = " am",
    
    # Domain-specific contractions/possessives
    "Trump's" = "Trump", "Zelensky's" = "Zelensky", "Zelensky’s" = "Zelensky",
    "country's" = "country", "Congo's" = "Congo", "Israel's" = "Israel",
    "Nat’l" = "Nat", "int’l" = "int", "govt's" = "government",
    
    "let's" = "let us", "ain't" = "is not", "y'all" = "you all",
    "ma'am" = "madam", "o'clock" = "of the clock", "ne'er" = "never",
    "gimme" = "give me", "gonna" = "going to", "gotta" = "got to",
    "lemme" = "let me", "wanna" = "want to", "whatcha" = "what are you",
    "could've" = "could have", "should've" = "should have",
    "would've" = "would have", "might've" = "might have", "must've" = "must have",
    
    "it's" = "it is", "that's" = "that is", "this's" = "this is",
    "there's" = "there is", "here's" = "here is", "who's" = "who is",
    "what's" = "what is", "when's" = "when is", "where's" = "where is",
    "why's" = "why is", "how's" = "how is",
    
    "they'd" = "they would", "they'll" = "they will", "they're" = "they are", "they've" = "they have",
    "we'd" = "we would", "we're" = "we are", "we'll" = "we will", "we've" = "we have",
    "i'd" = "i would", "i'll" = "i will", "i'm" = "i am", "i've" = "i have",
    "he'd" = "he would", "he'll" = "he will", "he's" = "he is",
    "she'd" = "she would", "she'll" = "she will", "she's" = "she is",
    "you'd" = "you would", "you'll" = "you will", "you're" = "you are", "you've" = "you have",
    "it'd" = "it would", "it'll" = "it will"
  )
  
  for (pattern in names(contractions)) {
    text <- gsub(pattern, contractions[[pattern]], text, ignore.case = TRUE)
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
  text <- str_replace_all(text, "\\b(US|USA\\b)", "United States")
  return(text)
}

preprocess_text_full <- function(text) {
  if (is.na(text) || nchar(text) < 2) {
    return(c(tokens = "", clean_text = ""))
  }
  
  text <- normalize_politics(text)
  text <- tolower(text)
  text <- replace_contraction(text)
  
  text <- gsub("\\[[^\\]]*\\]", "", text)
  text <- gsub("\\([^\\)]*\\)", "", text)
  text <- gsub("\\{[^\\}]*\\}", "", text)
  
  text <- rm_url(text)
  text <- removePunctuation(text)
  text <- stripWhitespace(text)
  text <- trimws(text)
  
  tokens <- unlist(tokenize_words(text))
  tokens <- tokens[!tokens %in% stopwords("en")]
  
  lemmatized <- lemmatize_words(tokens)
  stemmed <- wordStem(lemmatized)
  
  return(c(
    tokens = paste(tokens, collapse = ", "),
    clean_text = paste(stemmed, collapse = " ")
  ))
}

data <- read.csv("ids_final_project_group_11_news_raw.csv", stringsAsFactors = FALSE)

content_results <- lapply(data$content, preprocess_text_full)
content_df <- as.data.frame(do.call(rbind, content_results), stringsAsFactors = FALSE)
colnames(content_df) <- c("content_tokens", "clear_content")

title_results <- lapply(data$title, preprocess_text_full)
title_df <- as.data.frame(do.call(rbind, title_results), stringsAsFactors = FALSE)
colnames(title_df) <- c("title_tokens", "clear_title")

data <- cbind(data, title_df, content_df)

data <- data %>% select(-section, -url, -title, -content)

write.csv(data, "ids_final_project_group_11_news_clean.csv", row.names = FALSE)

cat("✅ Text preprocessing complete. Cleaned file saved as: ids_final_project_group_11_news_clean.csv\n")

