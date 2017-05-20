######################################################################
# Split Reviews by Airline
######################################################################
all_airlines <- unique(reviews_tibble$airline_nm)
all_airlines

# Below is for testing
all_airlines <- c("Alaska Airlines")
all_airlines

for (airline in all_airlines) {
  assign(
    paste(gsub(" ","_",airline), sep=""),
    reviews_tibble[which(reviews_tibble$airline_nm == airline),"review_tx"]
  )
}
#Alaska_Airlines

# Add row number
Alaska_Airlines$review_nb <- 1:nrow(Alaska_Airlines)
Alaska_Airlines <- select(Alaska_Airlines, review_nb, review_tx)


# Unnest the tokens
Alaska_Airlines %>% unnest_tokens(word, review_tx)
