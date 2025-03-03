library("dplyr") 
library("rvest") 
library(tidyverse)
library("tidytext") 
library("textstem") 
library("SnowballC") 
library("textclean")  
library("emoji") 
library("hunspell") 
library(tm)
library("topicmodels")
library(stringdist)
library(reshape2)


url <- "https://www.thedailystar.net/city/dhaka-traffic-jam-congestion-eats-32-million-working-hours-everyday-world-bank-1435630"
webpage <- read_html(url)
text_data1 <- html_nodes(webpage, ".pb-20.clearfix p , .row.rsi-scroller-content .article-title") %>% html_text()

url <- "https://www.thedailystar.net/city/heavy-traffic-jam-in-dhaka-city-bangladesh-road-1432555"
webpage <- read_html(url)
text_data2 <- html_nodes(webpage, ".pb-20.clearfix p , .row.rsi-scroller-content .article-title") %>% html_text()

url <- "https://www.dhakatribune.com/bangladesh/dhaka/364419/traffic-chaos-continues-in-dhaka-despite-absence"
webpage <- read_html(url)
text_data3 <- html_nodes(webpage, ".jw_article_body p , p:nth-child(1) , .mb10") %>% html_text() 


i <- 2
text_data <- text_data1[1]
while(i <= length(text_data1)){
  text_data <- paste(text_data, text_data1[i], sep = " ")
  i <- i+1
}

text_data1 <- text_data


i <- 2
text_data <- text_data2[1]
while(i <= length(text_data2)){
  text_data <- paste(text_data, text_data2[i], sep = " ")
  i <- i+1
}

text_data2 <- text_data


i <- 2
text_data <- text_data3[1]
while(i <= length(text_data3)){
  text_data <- paste(text_data, text_data3[i], sep = " ")
  i <- i+1
}

text_data3 <- text_data


text_data1 <- tolower(text_data1)
text_data1 <- gsub("[[:punct:]]", "",text_data1)
text_data1 <- emoji::emoji_replace_name(text_data1)
text_data1 <- replace_contraction(text_data1)
text_data1 <- gsub("[0-9]+\\.?[0-9]*", "", text_data1)
text_data1 <- gsub("[!@#?$%^&*()_+=-]", "", text_data1)
text_data1 <- gsub("<.*?>", "", text_data1)
text_data1 <- removeWords(text_data1, stopwords("en"))
text_data1 <- wordStem(text_data1)
text_data1 <- lemmatize_words(text_data1)
text_data1 <- trimws(text_data1)

text_data2 <- tolower(text_data2)
text_data2 <- gsub("[[:punct:]]", "",text_data2)
text_data2 <- emoji::emoji_replace_name(text_data2)
text_data2 <- replace_contraction(text_data2)
text_data2 <- gsub("[0-9]+\\.?[0-9]*", "", text_data2)
text_data2 <- gsub("[!@#?$%^&*()_+=-]", "", text_data2)
text_data2 <- gsub("<.*?>", "", text_data2)
text_data2 <- removeWords(text_data2, stopwords("en"))
text_data2 <- wordStem(text_data2)
text_data2 <- lemmatize_words(text_data2)
text_data2 <- trimws(text_data2)

text_data3 <- tolower(text_data3)
text_data3 <- gsub("[[:punct:]]", "",text_data3)
text_data3 <- emoji::emoji_replace_name(text_data3)
text_data3 <- replace_contraction(text_data3)
text_data3 <- gsub("[0-9]+\\.?[0-9]*", "", text_data3)
text_data3 <- gsub("[!@#?$%^&*()_+=-]", "", text_data3)
text_data3 <- gsub("<.*?>", "", text_data3)
text_data3 <- removeWords(text_data3, stopwords("en"))
text_data3 <- wordStem(text_data3)
text_data3 <- lemmatize_words(text_data3)
text_data3 <- trimws(text_data3)

url <- "https://raw.githubusercontent.com/dwyl/
        english-words/master/words.txt"

word_list <- readLines(url)  
word_list <- tolower(word_list)

get_closest_word <- function(word, dictionary) {
  distances <- stringdist::stringdist(word, dictionary, method = "lv")  
  closest_word <- dictionary[which.min(distances)]  
  return(closest_word)
}

correct_spelling_in_sentence <- function(sentence, dictionary) {
  words <- unlist(strsplit(sentence, " ")) 
  corrected_words <- sapply(words, get_closest_word, dictionary = dictionary) 
  corrected_sentence <- paste(corrected_words, collapse = " ") 
  return(corrected_sentence)
}

text_df <- data.frame(Articles = c(text_data1,text_data2, text_data3))

text_df[1,1] <- sapply(text_df[1,1], correct_spelling_in_sentence, dictionary = word_list)
text_df[2,1] <- sapply(text_df[2,1], correct_spelling_in_sentence, dictionary = word_list)
text_df[3,1] <- sapply(text_df[3,1], correct_spelling_in_sentence, dictionary = word_list)


text_df[1,1] <- gsub("[0-9]+\\.?[0-9]*", "", text_df[1,1])
text_df[2,1] <- gsub("[0-9]+\\.?[0-9]*", "", text_df[2,1])
text_df[3,1] <- gsub("[0-9]+\\.?[0-9]*", "", text_df[3,1])


articles <- Corpus(VectorSource(text_df$Articles))

DTM <- DocumentTermMatrix(articles)
inspect(DTM)
tfidf <- weightTfIdf(DTM)
inspect(tfidf)

tfidf_matrix <- as.matrix(tfidf)
mean_tfidf <- rowMeans(tfidf_matrix)
tfidf_data <- data.frame(term = names(mean_tfidf), tfidf = mean_tfidf)
tfidf_data <- tfidf_data[order(tfidf_data$tfidf, decreasing = TRUE), ]


ggplot(tfidf_data, aes(x = reorder(term, tfidf), y = tfidf)) +
  geom_bar(stat = "identity", fill = "#2E86C1") +
  labs(title = "TF-IDF Scores for All Articles",
       x = "Term", y = "TF-IDF Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"))


k_values <- 2:10
perplexity_scores <- sapply(k_values, function(k) {
  model <- LDA(DTM, k = k, control = list(seed = 1234))
  perplexity(model)
})

best_k <- k_values[which.min(perplexity_scores)]

Model_lda <- LDA(DTM, k=best_k, control = list(seed = 1234))
top_terms <- terms(Model_lda, 10) 
top_terms


topic_proportions <- posterior(Model_lda)$topics
topic_proportions


topic_proportions_long <- melt(topic_proportions)
colnames(topic_proportions_long) <- c("Document", "Topic", "Proportion")


ggplot(topic_proportions_long, aes(x = factor(Document), y = factor(Topic), fill = Proportion)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(x = "Document", y = "Topic", title = "Heatmap of Topic Proportions by Document") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  


beta_topics <- tidy(Model_lda, matrix = "beta")

beta_top_terms <- beta_top_terms %>%
  mutate(topic_label = recode(as.character(topic), !!!topic_labels))

topic_labels <- c(
  "1" = "City Development Planning",
  "2" = "Science Lab and Shahbaag Traffic Congestion",
  "3" = "Stuck many Hours in Traffic"
)

beta_top_terms <- beta_topics %>%
  group_by(topic) %>%
  slice_max(beta, n=8) %>%
  ungroup() %>%
  arrange(topic, -beta)

beta_top_terms %>%
  mutate(term = reorder_within(term, beta, topic_label)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic_label, scales = "free") +  # Using labeled topics
  scale_y_reordered()


write.csv(top_terms,"C:\\Users\\Moinul\\Desktop\\top_terms_by_topic.csv")
write.csv(text_df,"C:\\Users\\Moinul\\Desktop\\text_dataframe.csv")
