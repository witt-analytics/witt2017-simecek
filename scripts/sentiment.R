library(syuzhet)

load('data/init_reviews.Rdata')

steve_data <- init_reviews[1:10000,]

sentence1 <- get_sentences(steve_data$review_tx[1])

sentiments <- rbind(
get_sentiment(sentence1, method="syuzhet"),
get_sentiment(sentence1, method="bing"),
get_sentiment(sentence1, method="afinn"),
get_sentiment(sentence1, method="nrc")
)

sentiments2 <- rbind(
sign(get_sentiment(sentence1, method="syuzhet")),
sign(get_sentiment(sentence1, method="bing")),
sign(get_sentiment(sentence1, method="afinn")),
sign(get_sentiment(sentence1, method="nrc"))
)

nrc <- sentence1 %>%
  filter(lexicon == "nrc") %>%
  dplyr::select(word, sentiment)
