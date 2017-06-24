# Classification Modeling #
# Steven Simecek          #
# Create Date: 21MAY2017  #
# Update Date: 24MAY2017  #

# Things to try: decision tree, random forest, 
# logistic regression, multinomial logistic regression
 # neural networks

# install.packages("mlbench")
# instal.packages("party")
# install.packages("MASS")

# Load packages
# library(mlbench)
library(party)
library(MASS)

#should use
# review length
# review count
# helpful votes 
# via mobile
# user level

# Pull in tibble
classification_tibble <- new_reviews_tibble

# Choose which variables are needed

classification_tibble <- classification_tibble[,c("review_id"
                                                 ,"ovrl_score_nb"
                                                 ,"review_length"
                                                 ,"airline_nm"
                                                 ,"user_rvw_ct"
                                                 ,"rvw_hpfl_ct"
                                                 ,"user_lvl"
                                                 ,"via_mobile_in")]

classification_tibble <- classification_tibble[complete.cases(classification_tibble),]

# Log transform user_rvw_ct
classification_tibble$log_user_rvw_ct <- log10(classification_tibble$user_rvw_ct)
# plot(classification_tibble$user_rvw_ct ~ classification_tibble$ovrl_score_nb)
# plot(classification_tibble$log_user_rvw_ct ~ classification_tibble$ovrl_score_group)

# Log transform rvw_hpfl_ct
classification_tibble[classification_tibble == 0] <- 1
classification_tibble$log_rvw_hpfl_ct <- log10(classification_tibble$rvw_hpfl_ct)
# plot(classification_tibble$rvw_hpfl_ct ~ classification_tibble$ovrl_score_nb)
# plot(classification_tibble$log_rvw_hpfl_ct ~ classification_tibble$ovrl_score_group)

# Group reviews by pos, neg, neutral
classification_tibble$ovrl_score_group <- as.ordered(
  cut(as.numeric(classification_tibble$ovrl_score_nb)
      ,breaks = c(0,1,4,5)
      ,label = c("Negative","Neutral","Positive")))
xtabs(~ovrl_score_nb+ovrl_score_group,classification_tibble)

# Group reviews by neg, pos
classification_tibble$ovrl_score_pos_in <- as.ordered(
  cut(as.numeric(classification_tibble$ovrl_score_nb)
      ,breaks = c(0,3,5)
      ,label = c("0","1")))
str(classification_tibble$ovrl_score_pos_in)

xtabs(~ovrl_score_nb+ovrl_score_pos_in,classification_tibble)

# Convert to ordinal variable
classification_tibble$ovrl_score_nb <- as.ordered(classification_tibble$ovrl_score_nb)


# Remove outliers
ul_user_rvw_ct <- mean(classification_tibble$user_rvw_ct) + (3*sd(classification_tibble$user_rvw_ct))
print(paste("Upper limit of user_rvw_ct: ", ul_user_rvw_ct, sep = ''))

ul_rvw_hpfl_ct <- mean(classification_tibble$rvw_hpfl_ct) + (3*sd(classification_tibble$rvw_hpfl_ct))
print(paste("Upper limit of rvw_hpfl_ct: ", ul_rvw_hpfl_ct, sep = ''))

# 50649
# 50093

# (50649-50093)/50649

classification_tibble <- subset(classification_tibble, user_rvw_ct <= ul_user_rvw_ct)
classification_tibble <- subset(classification_tibble, rvw_hpfl_ct <= ul_rvw_hpfl_ct)


# Append multi-review indicator?

# Separate to training and test
training_percent <- .5
training_tibble <- classification_tibble[sample(1:nrow(classification_tibble)
                                        ,nrow(classification_tibble)*training_percent),]

test_tibble <- classification_tibble[!(classification_tibble$review_id 
                                       %in% training_tibble$review_id),]


#######################################################
# Decision Tree
#######################################################
train_tree <- subset(training_tibble,select=-c(review_id,ovrl_score_pos_in,ovrl_score_nb,user_rvw_ct,rvw_hpfl_ct))
new_tree <- ctree(ovrl_score_group ~ .,data = train_tree)
plot(new_tree)


#######################################################
# Ordinal Logistic Regression
#######################################################
train_olr <- subset(training_tibble,select=-c(review_id,ovrl_score_pos_in,ovrl_score_nb,user_rvw_ct,rvw_hpfl_ct))
olr_model <- polr(ovrl_score_group ~ ., data = train_olr, Hess = T)

summary(olr_model)

options(scipen = 100)

# Calculate p-values
coefs <- coef(summary(olr_model))
p_values <- pnorm(abs(coefs[, "t value"]), lower.tail = F) * 2
(coefs <- cbind(coefs, "p value" = round(p_values,6)))

# Predict on training data set
olr_predictions <- predict(olr_model, train_olr)
# print(predictions)

# Show confusion matrix (training data)
(conf_matrix <- table(olr_predictions, train_olr$ovrl_score_group))
print(paste("Ordinal Logit Regression training accuracy rate: "
            ,round((sum(diag(conf_matrix))/sum(conf_matrix)*100),3)
            ,"%"
            ,sep = ''))


# Predict on test data set
olr_predictions <- predict(olr_model, test_tibble)
# print(predictions)

# Show confusion matrix (test data)
(conf_matrix <- table(olr_predictions, test_tibble$ovrl_score_group))
print(paste("Ordinal Logit Regression test accuracy rate: "
            ,round((sum(diag(conf_matrix))/sum(conf_matrix)*100),3)
            ,"%"
            ,sep = ''))


#######################################################
# Logistic Regression
#######################################################
train_logit <- subset(training_tibble,select=-c(review_id,ovrl_score_group,ovrl_score_nb,user_rvw_ct,rvw_hpfl_ct))
logit_model <- glm(ovrl_score_pos_in ~ ., data = train_logit, family = binomial)
summary(logit_model)
anova(logit_model, test="Chisq")


# Run predictions on training data set
logit_predictions <- predict(logit_model,newdata=train_logit,type='response')
logit_predictions <- ifelse(logit_predictions > 0.5,1,0)
misClasificError <- mean(logit_predictions != train_logit$ovrl_score_pos_in)
print(paste('Accuracy',1-misClasificError))

# Run predictions on test data set
logit_predictions <- predict(logit_model,newdata=test_tibble,type='response')
logit_predictions <- ifelse(logit_predictions > 0.5,1,0)
misClasificError <- mean(logit_predictions != test_tibble$ovrl_score_pos_in)
print(paste('Accuracy',1-misClasificError))


