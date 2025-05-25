install.packages("remotes")
install.packages("tm")
install.packages("ggplot2")
install.packages("topicmodels")
install.packages("tidytext")
install.packages("ggforce")
install.packages("LDAvis")
install.packages("servr")
install.packages("wordcloud")
remotes::install_github("nikita-moor/ldatuning")

library(ldatuning)
library(tm)
library(topicmodels)
library(tidytext)
library(ggplot2)
library(dplyr)
library(ggforce)
library(LDAvis)
library(wordcloud)
library(RColorBrewer)

data <- read.csv("ids_final_project_group_11_news_clean.csv", stringsAsFactors = FALSE)

corpus <- VCorpus(VectorSource(data$clear_content))

dtm <- DocumentTermMatrix(corpus, control = list(
  wordLengths = c(3, Inf),
  removeNumbers = TRUE,
  removePunctuation = TRUE,
  stopwords = TRUE
))

dtm <- removeSparseTerms(dtm, 0.98)

result <- FindTopicsNumber(
  dtm,
  topics = seq(2, 10, by = 1),  
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 1234),
  mc.cores = 2L,
  verbose = TRUE
)

FindTopicsNumber_plot(result)

k <- result$topics[which.max(result$Griffiths2004)]


lda <- LDA(dtm, k = k, method = "Gibbs", control = list(seed = 1234))

lda_terms <- tidy(lda, matrix = "beta")

top_terms <- lda_terms %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


top_terms <- top_terms %>%
  mutate(term_label = paste0(term, "_", topic))

ggplot(top_terms, aes(x = reorder(term_label, beta), y = beta, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_discrete(labels = ~ gsub("_\\d+$", "", .)) +
  labs(title = "Top 10 Terms per Topic", x = NULL, y = "Word Probability")


phi <- posterior(lda)$terms
theta <- posterior(lda)$topics
vocab <- colnames(phi)
doc_length <- rowSums(as.matrix(dtm))
term_frequency <- colSums(as.matrix(dtm))

json_lda <- createJSON(
  phi = phi,
  theta = theta,
  vocab = vocab,
  doc.length = doc_length,
  term.frequency = term_frequency
)

json_lda <- createJSON(
  phi = phi,
  theta = theta,
  vocab = vocab,
  doc.length = doc_length,
  term.frequency = term_frequency
)

serVis(json_lda, open.browser = TRUE)
serVis(json_lda, out.dir = 'lda_vis', open.browser = FALSE)

terms_matrix <- posterior(lda)$terms

for (i in 1:k) {
  wordcloud(
    words = colnames(terms_matrix),
    freq = terms_matrix[i, ],
    min.freq = 0.001,
    max.words = 70,
    random.order = FALSE,
    colors = brewer.pal(8, "Dark2"),
    main = paste("Topic", i)
  )
}

top_terms <- tidy(lda, matrix = "beta") %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  summarise(terms = paste(term, collapse = ", "))

print(top_terms)


doc_topics <- tidy(lda, matrix = "gamma")

doc_dominant <- doc_topics %>%
  group_by(document) %>%
  slice_max(gamma, n = 1) %>%
  ungroup() %>%
  mutate(document = as.integer(document))

data <- data %>%
  mutate(document = row_number())

data_with_topics <- left_join(data, doc_dominant, by = "document")

gamma_matrix <- posterior(lda)$topics
gamma_df <- as.data.frame(gamma_matrix)
gamma_df$document <- 1:nrow(gamma_df)

data_with_all_topics <- data %>%
  mutate(document = row_number()) %>%
  left_join(gamma_df, by = "document")


write.csv(data_with_all_topics, "news_with_all_topic_probabilities.csv", row.names = FALSE)

library(tidyr)

gamma_long <- gamma_df %>%
  pivot_longer(cols = -document, names_to = "topic", values_to = "probability") %>%
  mutate(topic = as.integer(gsub("V", "", topic)))

top_docs_per_topic <- gamma_long %>%
  left_join(data, by = c("document")) %>%
  group_by(topic) %>%
  arrange(desc(probability)) %>%
  slice_head(n = 5) %>%  # Top 5 per topic
  ungroup()

write.csv(top_docs_per_topic, "top_documents_per_topic.csv", row.names = FALSE)
