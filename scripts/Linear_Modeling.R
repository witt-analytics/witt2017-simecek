# Linear Modeling for Capstone      #
# Reviews Tibble                    #
# Steven Simecek                    #
# Create Date: 13MAY2017            #
# Update Date: 13MAY2017            #

# Helpful site: http://data.princeton.edu/R/linearModels.html
# Another helpful site: http://tutorials.iq.harvard.edu/R/Rstatistics/Rstatistics.html

# Install dplyr (must run this first time)
#install.packages('dplyr')

# Load dplyr
library(dplyr)

# Use the following two lines for regression splines
#install.packages("splines")
#library(splines)


# Read in excel file
reviews_tibble <- tibble()
reviews_tibble <- readxl::read_excel('C:/Users/steve/Documents/Capstone_Local_Drive/SAMPLE_init_reviews.xlsx')


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

# Plot relationship between review score and sentiment
plot(reviews_tibble$ovrl_score_nb ~ reviews_tibble$rvw_hpfl_ct)

summary(new_mod)

# Standard 


# See everything the model produces
names(new_mod)
# new_mod$coefficients
# new_mod$residuals
# new_mod$effects
# new_mod$rank
# new_mod$fitted.values
# new_mod$assign       
# new_mod$qr
# new_mod$df.residual
# new_mod$xlevels
# new_mod$call  
# new_mod$terms
# new_mod$model


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

cat_mod <- lm(dat=reviews_tibble, formula = ovrl_score_nb ~ ovrl_review_cat)

summary(cat_mod)

plot(reviews_tibble$ovrl_score_nb ~ ovrl_review_cat)


# Harvard
# summary of expense and csat columns, all rows
reviews.ovr.avg <- subset(reviews_tibble, select = c("ovrl_score_nb", "avg_sentiment"))
summary(reviews.ovr.avg)
# correlation between expense and csat
cor(reviews.ovr.avg)


plot(reviews.ovr.avg)



airline_mod <- lm( data=reviews_tibble
                  ,ovrl_score_nb ~ airline_nm
                  ,subset = (airline_nm == "American Airlines" | airline_nm == "Delta Air Lines"))


summary(airline_mod)
anova(airline_mod)

plot(airline_mod)



# Make sure airline name is not numeric
str(reviews_tibble$airline_nm)

# Store as factor
reviews_tibble$airline_factor <- factor(reviews_tibble$airline_nm)
str(reviews_tibble$airline_factor)

fac_mod <- lm(ovrl_score_nb ~ airline_factor,
                 data=reviews_tibble)

summary(fac_mod)
coef(fac_mod)
anova(fac_mod)

plot(reviews_tibble$ovrl_score_nb ~ reviews_tibble$avg_sentiment)

aggregate(reviews_tibble[,"avg_sentiment"], list(reviews_tibble$ovrl_score_nb), mean)

aggregate(reviews_tibble[,"std_dev_sentiment"], list(reviews_tibble$ovrl_score_nb), mean)


sentiment_mod <- lm(ovrl_score_nb ~ avg_sentiment, data = reviews_tibble)

# airline_mod <- glm(reviews_tibble$airline_nm ~ reviews_tibble$ovrl_score_nb, 
#                   data=reviews_tibble)

summary(sentiment_mod)

plot(sentiment_mod)
