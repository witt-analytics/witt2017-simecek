# Sentiment Analysis Trip Advisor Data  #
# Steven Simecek                        #
# Create Date: 23APR2017                #
# Update Date: 24MAY2017                #


#install.packages("syuzhet")

library(syuzhet)
library(dplyr)


# Read in excel file
reviews_tibble <- tibble()
reviews_tibble <- readxl::read_excel('C:/Users/steve/Documents/Capstone_Local_Drive/init_reviews.xlsx')

#vignette("syuzhet-vignette")

# Below is for testing
#reviews_tibble <- reviews_tibble[1:1000,]

review_rows <- nrow(reviews_tibble)

# plot(reviews_tibble$ovrl_score_nb ~ reviews_tibble$avg_sentiment)

avg_sentiment <- vector()
std_dev_sentiment <- vector()

length(avg_sentiment)
length(std_dev_sentiment)

# Store start time
Start_Time <- Sys.time()
print(paste("Start Time: ", Start_Time, sep = ''))
# Start the clock
ptm <- proc.time()

for (i in 1:review_rows) {
  
  if (i %% 100 == 0) {
    print(paste("Calculating sentiment for ",review_rows," rows. | Percent complete: ",round(100*(i/review_rows),2),"%",sep = ""))
  }
  
  # Combine review and title
  current_review <- c(reviews_tibble$review_tx[i],reviews_tibble$review_title[i])
  #print(current_review)
  
  # Get sentences
  current_sentences <- get_sentences(current_review)
  #print(current_sentences)
  
  sentiment_matrix <- rbind(
    sign(get_sentiment(current_sentences, method = "syuzhet")),
    sign(get_sentiment(current_sentences, method = "bing")),
    sign(get_sentiment(current_sentences, method = "afinn")),
    sign(get_sentiment(current_sentences, method = "nrc")))
  
  #print(sentiment_matrix)
  
    # Calcualate mean sentiment
  current_avg_sentiment <- mean(
    sentiment_matrix
  )
  
  # Calcualate std dev sentiment
  current_std_dev_sentiment <- sd(
    sentiment_matrix
  )
  
  # print(paste(current_avg_sentiment, current_std_dev_sentiment, sep = "|"))
  
  avg_sentiment <- c(avg_sentiment,current_avg_sentiment)
  std_dev_sentiment <- c(std_dev_sentiment,current_std_dev_sentiment)
  
}
# Store end time
End_Time <- Sys.time()
print(paste("Start Time: ", Start_Time, sep = ''))
print(paste("End Time: ", End_Time, sep = ''))
# Stop the clock
proc.time() - ptm

reviews_tibble <- cbind(reviews_tibble,avg_sentiment,std_dev_sentiment)
 
write.csv(reviews_tibble,"C:/Users/steve/Documents/Capstone_Local_Drive/init_reviews_sentiment.csv")
