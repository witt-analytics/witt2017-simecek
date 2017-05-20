# Text Analysis Trip Advisor Data

# http://varianceexplained.org/r/trump-tweets/
nrc <- sentiments %>%
  filter(lexicon == "nrc") %>%
  dplyr::select(word, sentiment)

nrc


# FOR SENTIMENT
# syuzhet package
# coreNLP package

# do the vignette for syuzhet

browseURL()

# Import reviews to tibble
#install.packages("tidyverse")

# Clear everything
#rm(list = ls())

install.packages("tidyverse")
install.packages("dplyr")

# Load libraries
library(tidyverse)      # data manipulation & plotting
library(stringr)        # text cleaning and regular expressions
library(tidytext)       # text mining functions
library(dplyr)

# Read in excel file
reviews_tibble <- tibble()
reviews_tibble <- readxl::read_excel('C:/Users/steve/Documents/Capstone_Local_Drive/init_reviews.xlsx')

# How did you get my data into this Rdata format?
#load('C:/Users/steve/Documents/Capstone_Local_Drive/witt-analytics/witt2017-simecek/data/init_reviews.Rdata')

class(reviews_tibble)
#class(init_reviews)

######################################################################
# Format reviews_tibble for Analysis
######################################################################
# Add row number by airline
reviews_tibble$review_nb <- ave(reviews_tibble$airline_nm, reviews_tibble$airline_nm, FUN = seq_along)

# Only keep desired columns
long_tibble <-  select(reviews_tibble, airline_nm, review_nb, review_tx) %>%
                unnest_tokens(word, review_tx)
long_tibble

######################################################################
# Text Analysis
######################################################################
all_airlines <- unique(reviews_tibble$airline_nm)
all_airlines

# Remove stop words
long_tibble <- long_tibble %>%
  anti_join(stop_words)
long_tibble
class(stop_words)

# Possibly include step to remove airline names with a c() followed by an anti_join
airline_words <- tibble(word=c('airline'
                          ,'airlines'
                          ,'air'
                          ,'air lines'
                          ,'american'
                          ,'delta'
                          ,'southwest'
                          ,'united'
                          ,'jet'
                          ,'blue'
                          ,'jetblue'
                          ,'alaska'
                          ,'spirit'
                          ,'frontier'
                          ,'allegiant'
                          ,'hawaiian'
                        ))

long_tibble <- long_tibble %>%
  anti_join(airline_words)
long_tibble

# Count most common words
long_tibble %>%
  count(word, sort = TRUE)

# Most Common words for each airline
long_tibble %>%
  group_by(airline_nm) %>%
  count(word, sort = TRUE) %>%
  top_n(10)

# Visualize top ten words by airline
long_tibble %>%
  group_by(airline_nm) %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(airline_nm = factor(airline_nm, levels = all_airlines),
         text_order = nrow(.):1) %>%
  ggplot(aes(reorder(word, text_order), n, fill = airline_nm)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ airline_nm, scales = "free_y") +
  labs(x = "NULL", y = "Frequency") +
  coord_flip() +
  theme(legend.position="none")

# calculate percent of word use across all airlines
airline_pct <- long_tibble %>%
  count(word) %>%
  transmute(word, all_words = n / sum(n))

# calculate percent of word use within each airline
frequency <- long_tibble %>%
  count(airline_nm, word) %>%
  mutate(airline_words = n / sum(n)) %>%
  left_join(airline_pct) %>%
  arrange(desc(airline_words)) %>%
  ungroup()

frequency


ggplot(frequency, aes(x = airline_words, y = all_words, color = abs(all_words - airline_words))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = scales::percent_format()) +
  scale_y_log10(labels = scales::percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~ airline_nm, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "All Airlines", x = NULL)

frequency %>%
  group_by(airline_nm) %>%
  summarize(correlation = cor(airline_words, all_words),
            p_value = cor.test(airline_words, all_words)$p.value)









