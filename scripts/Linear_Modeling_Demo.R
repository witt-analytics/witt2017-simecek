# Linear Modeling for Capstone      #
# Reviews Tibble                    #
# Steven Simecek                    #
# Create Date: 13MAY2017            #
# Update Date: 14MAY2017            #

# Helpful site: http://data.princeton.edu/R/linearModels.html
# Another helpful site: http://tutorials.iq.harvard.edu/R/Rstatistics/Rstatistics.html

# Install dplyr
#install.packages('dplyr')

# Load dplyr
library(dplyr)

# Use the following two lines for regression splines
#install.packages("splines")
#library(splines)


# Read in excel file (specify location of excel file)
# ***Must use forward slashes rather than backslashes in windows
reviews_tibble <- tibble()
reviews_tibble <- readxl::read_excel('C:/Users/steve/Desktop/SAMPLE_init_reviews.xlsx')


# View all arguments in the linear modeling function - lm()
args(lm)

# Information on lm() function
?lm()


# We use the data set "reviews_tibble"
# "ovrl_score_nb" is the response variable
# "avg_sentiment" is the explanatory variable
# Store the model in "new_mod"
new_mod <- lm(data=reviews_tibble, ovrl_score_nb ~ avg_sentiment)

# View summary of new model
summary(new_mod)

# Get an anova table of new mode
anova(new_mod)


# Specify the following for alll 4 plots at once
#par(mfrow=c(2,2))
# Plots back to normal
#par(mfrow=c(1,1))


# Plot the model (press ENTER in console to view plots)
plot(new_mod)


# View fitted values 
fitted(new_mod)

# View residuals
residuals(new_mod)

# View coefficients
coef(new_mod)

# Store actual, fitted, and residuals in a tibble
fitted_vs_predicted <- tibble()
fitted_vs_predicted <- cbind(ovrl_score = reviews_tibble$ovrl_score_nb
                            ,fitted_score = fitted(new_mod)
                            ,residual = residuals(new_mod))
# View tibble
fitted_vs_predicted

# View model formula
new_mod

# We can see that average sentiment is strongly positively correlated with overall score
# P-value is sufficiently small, coefficient is positive

# +	to combine elementary terms, as in A+B
# :	for interactions, as in A:B
# *	for both main effects and interactions, so A*B = A+B+A:B

new_mod <- lm(data=reviews_tibble, ovrl_score_nb ~ avg_sentiment + rvw_hpfl_ct)


# Plot relationship between review score and sentiment
plot(reviews_tibble$ovrl_score_nb ~ reviews_tibble$avg_sentiment)

# Plot relationship between review score and helpful count
plot(reviews_tibble$ovrl_score_nb ~ reviews_tibble$rvw_hpfl_ct)

summary(new_mod)


# See everything the model produces
names(new_mod)
#new_mod$coefficients
#new_mod$residuals
#new_mod$effects
#new_mod$rank
#new_mod$fitted.values
#new_mod$assign       
#new_mod$qr
#new_mod$df.residual
#new_mod$xlevels
#new_mod$call  
#new_mod$terms
#new_mod$model


# Modeling with a subset (only American Airlines)
new_mod <- lm(data=reviews_tibble, ovrl_score_nb ~ avg_sentiment, subset = airline_nm == 'American Airlines')

summary(new_mod)


# Work with categorical variables

# Cut function creates categorical variables (a.k.a. factors)
?cut()

ovrl_review_cat <- cut( reviews_tibble$ovrl_score_nb
# 0 (non-inclusive) thorough 3 (inclusive) go into Neg and 3 (non) through 5 (in) go into Pos
    ,breaks = c(0,3,5)
    ,label = c("Negative","Positive")
    )

# Predict sentiment based review score category
cat_mod <- lm(dat=reviews_tibble, formula = avg_sentiment ~ ovrl_review_cat)

summary(cat_mod)

plot(reviews_tibble$avg_sentiment ~ ovrl_review_cat)
