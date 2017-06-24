# Text Mining for Capstone      #
# Reviews Tibble                #
# Steven Simecek                #
# Create Date: 03JUN2017        #
# Update Date: 03JUN2017        #

# Clear environment
# rm(list = ls())


# Helpful book: http://tidytextmining.com/tfidf.html
# - Silge, Julia, and David Robinson. 2016. “tidytext: Text Mining and Analysis Using Tidy Data Principles in R.” JOSS 1 (3). The Open Journal. doi:10.21105/joss.00037.

library(plyr); library(dplyr)
library(janeaustenr)
library(tidytext)
library(tidyverse)      # data manipulation & plotting
library(stringr)        # text cleaning and regular expressions
library(ggplot2)
library(arm)
library(e1071)
library(caret)
library(tm)
library(class)

# try splitting by 10 most common origins

# Prepare tibble
# Pull in tibble
text_mining_tibble <- new_reviews_tibble

table(text_mining_tibble$ovrl_score_nb)

# Group reviews by pos, neg, neutral
text_mining_tibble$ovrl_score_group <- as.ordered(
  cut(as.numeric(text_mining_tibble$ovrl_score_nb)
      ,breaks = c(0,1,4,5)
      ,label = c("Negative","Neutral","Positive")))

# Take sample 
# text_mining_tibble <- (ddply(text_mining_tibble,.(ovrl_score_group),function(x) x[sample(nrow(x),1000),]))

xtabs(~ovrl_score_group+ovrl_score_group,text_mining_tibble)

# Specify which variable to analyze
text_mining_tibble$text_var_analyze <- text_mining_tibble$ovrl_score_nb

# Add row number by airline
text_mining_tibble$review_nb <- ave(text_mining_tibble$obs_nb, text_mining_tibble$text_var_analyze, FUN = seq_along)

text_mining_tibble$review_tx <- as.character(text_mining_tibble$review_tx)

# Only keep desired columns
long_tibble <-  select(text_mining_tibble, text_var_analyze, review_nb, review_tx) %>%
  unnest_tokens(word, review_tx, token = "words")
# long_tibble

long_tibble <- subset(long_tibble, !is.na(long_tibble$text_var_analyze))


grouped_words <- long_tibble %>%
  count(text_var_analyze, word, sort = TRUE) %>%
  ungroup()

total_words <- grouped_words %>% 
  group_by(text_var_analyze) %>% 
  summarize(total = sum(n))

grouped_words <- left_join(grouped_words, total_words)

# grouped_words

ggplot(grouped_words, aes(n/total, fill = text_var_analyze)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~text_var_analyze, ncol = 2, scales = "free_y")



freq_by_rank <- grouped_words %>% 
  group_by(text_var_analyze) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total)

freq_by_rank

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = text_var_analyze)) + 
  geom_line(size = 1.2, alpha = 0.8) + 
  scale_x_log10() +
  scale_y_log10()


rank_subset <- freq_by_rank %>% 
  filter(rank < 500,
         rank > 10)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = text_var_analyze)) + 
  geom_abline(intercept = -0.62, slope = -1.1, color = "gray50", linetype = 2) +
  geom_line(size = 1.2, alpha = 0.8) + 
  scale_x_log10() +
  scale_y_log10()


grouped_words <- grouped_words %>%
  bind_tf_idf(word, text_var_analyze, n)
grouped_words

grouped_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))


plot_reviews <- grouped_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word))))

plot_reviews %>% 
  top_n(20) %>%
  ggplot(aes(word, tf_idf, fill = text_var_analyze)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()



plot_reviews %>% 
  group_by(text_var_analyze) %>% 
  top_n(15) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = text_var_analyze)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~text_var_analyze, ncol = 2, scales = "free") +
  coord_flip()

# Look
look <- filter(text_mining_tibble,str_detect(review_tx," y'all "))


# Classification





# install.packages("e1071")

# Training data.
data <- c('Cats like to chase mice.', 'Dogs like to eat big bones.')
class(data)
corpus <- VCorpus(VectorSource(data))

# Create a document term matrix.
tdm <- DocumentTermMatrix(corpus, list(removePunctuation = TRUE, stopwords = TRUE, stemming = TRUE, removeNumbers = TRUE))

# Convert to a data.frame for training and assign a classification (factor) to each document.
train <- as.matrix(tdm)
train <- cbind(train, c(0, 1))
colnames(train)[ncol(train)] <- 'y'
train <- as.data.frame(train)
train$y <- as.factor(train$y)

# Train.
fit <- train(y ~ ., data = train, method = 'bayesglm')

# Check accuracy on training.
predict(fit, newdata = train)

# Test data.
data2 <- c('Bats eat bugs.')
corpus <- VCorpus(VectorSource(text_mining_tibble$review_tx))
summary(corpus)
tdm <- DocumentTermMatrix(corpus, control = list(dictionary = Terms(tdm), removePunctuation = TRUE, stopwords = TRUE, stemming = TRUE, removeNumbers = TRUE))
test <- as.matrix(tdm)

inspect(corpus[[2]])

# Check accuracy on test.
predict(fit, newdata = test)



all_1s <- 

corpus_1 <- Corpus(VectorSource(subset(text_mining_tibble, ovrl_score_nb == 1)[,"review_tx"]))
corpus_1 <- tm_map(corpus_1,PlainTextDocument)
corpus_1 <- tm_map(corpus_1,tolower)
corpus_1 <- tm_map(corpus_1,removePunctuation)
corpus_1 <- tm_map(corpus_1,removeWords, stopwords("english"))
corpus_1 <- tm_map(corpus_1,stemDocument) # removes the last few letters of similar words such as get, getting, gets

dtm = TermDocumentMatrix(corpus_1) #turns the corpus into a document term matrix
notSparse = removeSparseTerms(dtm, 0.7) # extracts frequently occuring words
dtm
notSparse

finalWords=as.data.frame(as.matrix(notSparse)) # most frequent words remain in a dataframe, with one column per word





# Begin working
# Sample tibble

sample <- sample_n(text_mining_tibble,1000)


# All 5's
all_5s <- filter(sample, ovrl_score_nb == 5)
print(paste("Nrow 5:",nrow(all_5s)))
all_5s <- all_5s$review_tx
write.csv(all_5s, file = "C:/Users/steve/Documents/Capstone_Local_Drive/tm/all_5s.csv")

# All 4's
all_4s <- filter(sample, ovrl_score_nb == 4)
print(paste("Nrow 4:",nrow(all_4s)))
all_4s <- all_4s$review_tx
write.csv(all_4s, file = "C:/Users/steve/Documents/Capstone_Local_Drive/tm/all_4s.csv")

# All 3's
all_3s <- filter(sample, ovrl_score_nb == 3)
print(paste("Nrow 3:",nrow(all_3s)))
all_3s <- all_3s$review_tx
write.csv(all_3s, file = "C:/Users/steve/Documents/Capstone_Local_Drive/tm/all_3s.csv")

# All 2's
all_2s <- filter(sample, ovrl_score_nb == 2)
print(paste("Nrow 2:",nrow(all_2s)))
all_2s <- all_2s$review_tx
write.csv(all_2s, file = "C:/Users/steve/Documents/Capstone_Local_Drive/tm/all_2s.csv")

# All 1's
all_1s <- filter(sample, ovrl_score_nb == 1)
print(paste("Nrow 1:",nrow(all_1s)))
all_1s <- all_1s$review_tx
write.csv(all_1s, file = "C:/Users/steve/Documents/Capstone_Local_Drive/tm/all_1s.csv")

corpus_all <- VCorpus(DirSource(directory = "C:/Users/steve/Documents/Capstone_Local_Drive/tm"))
# corpus_all

# Cleaning
corpus_all <- tm_map(corpus_all, content_transformer(tolower))
corpus_all <- tm_map(corpus_all, removePunctuation)
corpus_all <- tm_map(corpus_all, stripWhitespace)
corpus_all <- tm_map(corpus_all, removeWords, stopwords('english'))
# List words to remove
corpus_all <- tm_map(corpus_all, removeWords, c("flight","time"))

# Create document/term matrix
dtm <- DocumentTermMatrix(corpus_all)
removeSparseTerms(dtm, 0.4)
dtm2 <- as.matrix(dtm)

# Find frequency
word_frequencies <- colSums(dtm2)
word_frequencies <- sort(word_frequencies, decreasing = T)

head(word_frequencies)

# install.packages("wordcloud")

library(wordcloud)

word_names <- names(word_frequencies)

wordcloud(word_names[1:100],word_frequencies[1:100])


# corpus[[1]]$meta
# corpus[[2]]$meta
# corpus[[3]]$meta
# corpus[[4]]$meta
# corpus[[5]]$meta

# Create a document term matrix.
tdm <- DocumentTermMatrix(corpus, list(removePunctuation = TRUE, stopwords = TRUE, stemming = TRUE, stripWhitespace = TRUE))

# Convert to a data.frame for training and assign a classification (factor) to each document.
train <- as.matrix(tdm)
train <- cbind(train, c(1,2,3,4,5))
colnames(train)[ncol(train)] <- 'y'
train <- as.data.frame(train)
train$y <- as.factor(train$y)

# Train.
fit <- train(y ~ ., data = train, method = 'bayesglm')

# Check accuracy on training.
predict(fit, newdata = train)

# Test data.
data2 <- c('Bats eat bugs.')
corpus <- VCorpus(VectorSource(text_mining_tibble$review_tx))
summary(corpus)
tdm <- DocumentTermMatrix(corpus, control = list(dictionary = Terms(tdm), removePunctuation = TRUE, stopwords = TRUE, stemming = TRUE, removeNumbers = TRUE))
test <- as.matrix(tdm)
