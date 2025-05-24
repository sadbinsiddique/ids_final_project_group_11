packages <- c("readr", "dplyr", "tm", "SnowballC", "topicmodels", "ggplot2",
              "tidyr", "fmsb", "Rtsne", "slam", "scales")
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) install.packages(packages[!installed])
lapply(packages, library, character.only = TRUE)

df <- read_csv("ids_final_project_group_11_news_clean.csv")
df <- df %>% filter(!is.na(clean_content))
df$text <- df$clean_content

texts <- tolower(df$text)
texts <- gsub("[^a-z\s]", "", texts)
texts <- removeWords(texts, stopwords("en"))
texts <- stripWhitespace(texts)
texts <- wordStem(texts)

corpus <- VCorpus(VectorSource(texts))
dtm <- DocumentTermMatrix(corpus)
dtm <- removeSparseTerms(dtm, 0.99)

dtm_rowsums <- slam::row_sums(dtm)
nonempty_idx <- dtm_rowsums > 0
dtm <- dtm[nonempty_idx, ]
df <- df[nonempty_idx, ]

find_best_k <- function(dtm, k_seq = 2:10) {
  perps <- numeric(length(k_seq))
  
  for (i in seq_along(k_seq)) {
    cat("Fitting model with", k_seq[i], "topics...\n")
    lda <- LDA(dtm, k = k_seq[i], method = "Gibbs", control = list(seed = 1234, burnin = 500, iter = 1000))
    perps[i] <- perplexity(lda, dtm)
  }
  
  df_metrics <- data.frame(k = k_seq, perplexity = perps)
  
  print(
    ggplot(df_metrics, aes(x = k, y = perplexity)) +
      geom_line(color = "red") +
      geom_point(color = "red") +
      labs(title = "Model Selection: Perplexity vs. Number of Topics",
           y = "Perplexity", x = "Number of Topics (k)") +
      theme_minimal()
  )
  
  best_k <- k_seq[which.min(perps)]
  cat("Best k based on min perplexity:", best_k, "\n")
  return(best_k)
}

k <- find_best_k(dtm)

lda_model <- LDA(dtm, k = k, method = "Gibbs", control = list(seed = 1234, burnin = 500, iter = 1000))

top_terms <- terms(lda_model, 10)
topic_proportions <- posterior(lda_model)$topics
df$dominant_topic <- apply(topic_proportions, 1, which.max)
topic_labels <- apply(top_terms, 2, function(x) paste(x[1:4], collapse = "_"))
df$topic_label <- topic_labels[df$dominant_topic]


set.seed(123)
tsne <- Rtsne(topic_proportions)
tsne_df <- data.frame(X1 = tsne$Y[,1], X2 = tsne$Y[,2], Topic = factor(df$dominant_topic))
print(
  ggplot(tsne_df, aes(x = X1, y = X2, color = Topic)) +
    geom_point(alpha = 0.7) +
    labs(title = "t-SNE Scatterplot of Topics")
)

topic_df <- as.data.frame(topic_proportions)
colnames(topic_df) <- paste0("Topic", 1:ncol(topic_df))
topic_df$doc <- 1:nrow(topic_df)
long_topic_df <- pivot_longer(topic_df, cols = starts_with("Topic"), names_to = "Topic", values_to = "Proportion")
print(
  ggplot(long_topic_df, aes(x = Topic, y = Proportion, fill = Topic)) +
    geom_violin() +
    labs(title = "Violin Plot of Topic Proportions") +
    theme_minimal()
)

avg_topic <- colMeans(topic_proportions)
radar_data <- as.data.frame(t(avg_topic))
radar_data <- rbind(rep(1, k), rep(0, k), radar_data)
colnames(radar_data) <- paste("Topic", 1:k)
radarchart(radar_data, axistype = 1, pcol = "blue", pfcol = scales::alpha("skyblue", 0.5), plwd = 2)

df$published_date <- as.Date(df$published_date, format = "%d/%m/%Y")
topic_props_df <- as.data.frame(topic_proportions)
colnames(topic_props_df) <- paste0("Topic", 1:ncol(topic_props_df))
df_with_topics <- bind_cols(df %>% select(-starts_with("Topic")), topic_props_df)
long_df <- df_with_topics %>%
  select(published_date, starts_with("Topic")) %>%
  pivot_longer(cols = starts_with("Topic"), names_to = "Topic", values_to = "Proportion") %>%
  group_by(published_date, Topic) %>%
  summarise(avg_prop = mean(Proportion, na.rm = TRUE), .groups = "drop")

print(
  ggplot(long_df, aes(x = published_date, y = avg_prop, color = Topic)) +
    geom_line() +
    labs(title = "Topic Trends Over Time")
)

print(
  ggplot(df, aes(x = factor(dominant_topic))) +
    geom_bar(fill = "steelblue") +
    labs(title = "Histogram: Articles per Topic", x = "Topic", y = "Count")
)

avg_df <- data.frame(Topic = colnames(topic_proportions),
                     Avg = colMeans(topic_proportions))
print(
  ggplot(avg_df, aes(x = reorder(Topic, -Avg), y = Avg, fill = Topic)) +
    geom_bar(stat = "identity") +
    labs(title = "Bar Graph: Average Topic Proportion", x = "Topic", y = "Average Proportion")
)

print(
  ggplot(long_topic_df, aes(x = Topic, y = Proportion, fill = Topic)) +
    geom_boxplot() +
    labs(title = "Box Plot: Distribution of Topic Proportions") +
    theme_minimal()
)

final_df <- bind_cols(df, as.data.frame(topic_proportions))
write_csv(final_df, "ids_final_topic_modeling_project_group_11.csv")

