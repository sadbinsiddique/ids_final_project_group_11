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
  text <- gsub("[‘’´`ʹʻʼʽʾʿˈˊˋ˵]", "'", text)
  text <- gsub("[“”]", "\"", text)
  
  contractions <- c(
    "won't" = "will not",
    "can't" = "cannot",
    "shan't" = "shall not",
    "n't" = " not",
    "'re" = " are",
    "'s" = " is",
    "'d" = " would",
    "'ll" = " will",
    "'t" = " not",
    "'ve" = " have",
    "'m" = " am",
    
    "Trump's" = "Trump",
    "Zelensky's" = "Zelensky",
    "Zelensky’s" = "Zelensky",
    "country's" = "country",
    "Congo's" = "Congo",
    "Israel's" = "Israel",
    "Station's" = "Station",
    "Nat’l" = "Nat",
    "Power's" = "Power",
    "Sudan's" = "Sudan",
    "India's" = "India",
    "World's" = "World",
    "int’l" = "int",
    "govt's" = "govt",
    "Adviser's" = "Adviser",
    "Pakistan's" = "Pakistan",
    "Cox's" = "Cox",
    "KUET's" = "KUET",
    "advisor's" = "advisor",
    "Hamid's" = "Hamid",
    "Azhar's" = "Azhar",
    "Commissioner's" = "Commissioner",
    "Pope's" = "Pope",
    "Chattogram's" = "Chattogram",
    "VC's" = "VC",
    "pope's" = "pope",
    "Shakib's" = "Shakib",
    "Ahsan's" = "Ahsan",
    "Google's" = "Google",
    "Minister's" = "Minister",
    "Abedin's" = "Abedin",
    "Messi's" = "Messi",
    "martyr's" = "martyr",
    "CA's" = "CA",
    "akh's" = "akh",
    "people's" = "people",
    "Canada's" = "Canada",
    "Rehana's" = "Rehana",
    "BNP's" = "BNP",
    "Women's" = "Women",
    "League's" = "League",
    "Rahman's" = "Rahman",
    "Hefazat-e-Islam's" = "Hefazat-e-Islam",
    "capital's" = "capital",
    "MoFA's" = "MoFA",
    "Ullah's" = "Ullah",
    "Putin's" = "Putin",
    "Hasnat's" = "Hasnat",
    "HWPL's" = "HWPL",
    "Modi's" = "Modi",
    "don't" = "do not",
    "Harun's" = "Harun",
    "ASP's" = "ASP",
    "Army's" = "Army",
    "AL's" = "AL",
    "Council's" = "Council",
    "unit's" = "unit",
    "Hasina's" = "Hasina",
    "NBR's" = "NBR",
    "Kushtia's" = "Kushtia",
    "Egypt's" = "Egypt",
    "China's" = "China",
    "Mymensingh's" = "Mymensingh",
    
    "let's" = "let us",
    "ain't" = "is not",
    "y'all" = "you all",
    "ma'am" = "madam",
    "o'clock" = "of the clock",
    "ne'er" = "never",
    "gimme" = "give me",
    "gonna" = "going to",
    "gotta" = "got to",
    "lemme" = "let me",
    "more'n" = "more than",
    "wanna" = "want to",
    "whatcha" = "what are you",
    "could've" = "could have",
    "should've" = "should have",
    "would've" = "would have",
    "might've" = "might have",
    "must've" = "must have",
    "how's" = "how is",
    "here's" = "here is",
    "there's" = "there is",
    "who's" = "who is",
    "what's" = "what is",
    "when's" = "when is",
    "where's" = "where is",
    "why's" = "why is",
    "it's" = "it is",
    "that's" = "that is",
    "this's" = "this is",
    "they'd" = "they would",
    "they'll" = "they will",
    "they're" = "they are",
    "they've" = "they have",
    "we'd" = "we would",
    "we're" = "we are",
    "we'll" = "we will",
    "we've" = "we have",
    "i'd" = "i would",
    "i'll" = "i will",
    "i'm" = "i am",
    "i've" = "i have",
    "he'd" = "he would",
    "he'll" = "he will",
    "he's" = "he is",
    "she'd" = "she would",
    "she'll" = "she will",
    "she's" = "she is",
    "you'd" = "you would",
    "you'll" = "you will",
    "you're" = "you are",
    "you've" = "you have",
    "it'd" = "it would",
    "it'll" = "it will"
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

preprocess_text <- function(text) {
  if (is.na(text) || nchar(text) < 2) return("")
  
  text <- normalize_politics(text)
  text <- replace_contraction(text)
  
  text <- gsub("\\[[^\\]]*\\]", "", text)  # square brackets
  text <- gsub("\\([^\\)]*\\)", "", text)  # round brackets
  text <- gsub("\\{[^\\}]*\\}", "", text)  # curly brackets
  
  text <- tolower(text)
  text <- rm_url(text)
  text <- removePunctuation(text)
  #text <- removeNumbers(text)
  text <- stripWhitespace(text)
  
  tokens <- unlist(tokenize_words(text))
  tokens <- tokens[!tokens %in% stopwords("en")]
  
  stemmed <- wordStem(tokens)
  lemmatized <- lemmatize_words(stemmed)
  
  return(paste(lemmatized, collapse = " "))
}

data <- read.csv("ids_final_project_group_11_news_raw.csv", stringsAsFactors = FALSE)

data$clean_title <- sapply(data$title, preprocess_text)
data$clean_content <- sapply(data$content, preprocess_text)

write.csv(data, "ids_final_project_group_11_news_clean.csv", row.names = FALSE)

