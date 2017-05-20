# Linear Modeling for Capstone Findings     #
#                                           #
# Steven Simecek                            #
# Wittenberg University                     #
# May 18, 2017                              #
#                                           #


install.packages('lubridate')

library(lubridate)


# Strategy: explore what could be most interesting
# literature and reviews of how the busses are operating
# what do the airlines want to know?  align to the desires of the airlines
# find interesting anomolies, insteresting contradictions and paradoxes
# get comfortable with the regression.

# Helpful sites:  http://data.princeton.edu/R/linearModels.html
#                 http://tutorials.iq.harvard.edu/R/Rstatistics/Rstatistics.html

# View all arguments in the linear modeling function - lm()
args(lm)

# Information on lm() function
?lm()

# Is sentiment score a predictor for overall score
scr_by_smt <- lm(ovrl_score_nb ~ avg_sentiment, data = reviews_tibble)

summary(scr_by_smt)

# Which of the six sub-categories is the best predictor for overall score?  Sentiment?
# Do subsetting where subcat is not null
# Modeling with a subset (only American Airlines)
scr_by_seat <- lm(ovrl_score_nb ~ seat_score_nb, data = reviews_tibble, subset = seat_score_nb != 'NA')
summary(scr_by_seat)

scr_by_cust_srvc <- lm(ovrl_score_nb ~ c_srvc_score_nb, data = reviews_tibble, subset = c_srvc_score_nb != 'NA')
summary(scr_by_cust_srvc)

scr_by_clean <- lm(ovrl_score_nb ~ clean_score_nb, data = reviews_tibble, subset = clean_score_nb != 'NA')
summary(scr_by_clean)

scr_by_food_bvrg <- lm(ovrl_score_nb ~ food_score_nb, data = reviews_tibble, subset = food_score_nb != 'NA')
summary(scr_by_food_bvrg)

scr_by_lgrm <- lm(ovrl_score_nb ~ lgrm_score_nb, data = reviews_tibble, subset = lgrm_score_nb != 'NA')
summary(scr_by_lgrm)

scr_by_entrtn <- lm(ovrl_score_nb ~ entn_score_nb, data = reviews_tibble, subset = entn_score_nb != 'NA')
summary(scr_by_entrtn)

scr_by_value <- lm(ovrl_score_nb ~ value_score_nb, data = reviews_tibble, subset = value_score_nb != 'NA')
summary(scr_by_value)

scr_by_chk_in <- lm(ovrl_score_nb ~ chk_in_score_nb, data = reviews_tibble, subset = chk_in_score_nb != 'NA')
summary(scr_by_chk_in)


all_subs <- lm(ovrl_score_nb  ~ seat_score_nb 
                              + c_srvc_score_nb
                              + clean_score_nb
                              + food_score_nb
                              + lgrm_score_nb
                              + entn_score_nb
                              + value_score_nb
                              + chk_in_score_nb
               ,data = reviews_tibble
)

summary(all_subs)

# since value and cust service are largest coeffs
# how do these differ by airline, does frontier have the best value scores?



# Is season of year a predictor of overall score
# Append season
  # Find month



# Is domestic / international a precidtor of score?
# also do anova for stuff like this (is the mean different across more than within?)
# Based on Tableau, pick only "Domestic", "International", "Carribbean", "USA", and "Mexico"


# Gender a predictor of overall score?  Use something + gender?
# Limit to just ones where we can get gender
# Also go ahead and run it with the U's too, to see if non-identifiable names do anything


# Is the class flown a predictor of overall score?
# Remove the ones that say "NA"


# Text subject analysis (most common words) on the 1's and the 5's?

# Is the number of review count a predictor?

# Is the number of times this particular person has reviewed (using username) a predictor

# Is the number of helpful votes a predictor?

# what themes do people talk about from different regions?

# Are the sub-scores different by airline?


# Start doing classification modeling (like trees)




