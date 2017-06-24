# Feature Selection       #
# Steven Simecek          #
# Create Date: 25MAY2017  #
# Update Date: 25MAY2017  #

# Things to try: decision tree, 

# install.packages("Boruta")

# Load packages
library(Boruta)

# Load in data set
library(rprojroot) 
root <- find_root(is_git_root)
new_reviews_tibble <- read.csv(paste0(root,"/data","/reviews_dists.csv"), stringsAsFactors = F)[,2:40]


# Append season
get_season <- function(dates) {
    month <- substr(dates,6,7)
    ifelse(month == "12" | month == "01" | month == "02", "Winter",
    ifelse(month == "03" | month == "04" | month == "05", "Spring",
    ifelse(month == "06" | month == "07" | month == "08", "Summer","Fall")))
}

new_reviews_tibble$flight_season <- get_season(new_reviews_tibble$aprx_flight_dt)

# try cutting user rvw ct to 1 and more than 1

# Convert the following variables to factors
convert <- c( "ovrl_score_nb" 
             ,"seat_score_nb" 
             ,"c_srvc_score_nb"
             ,"clean_score_nb"
             ,"food_score_nb"
             ,"lgrm_score_nb"
             ,"entn_score_nb"
             ,"value_score_nb"
             ,"chk_in_score_nb"
             ,"user_lvl"
             ,"flight_class"
             ,"flight_dm_itl" 
             ,"user_gender"
             ,"via_mobile_in"
             ,"airline_nm"
             ,"flight_season"
)
# convert
new_reviews_tibble[,convert] <- data.frame(apply(new_reviews_tibble[convert], 2, as.factor))

class(new_reviews_tibble$review_tx)

# Select variables to keep for feature subsetting
features <- new_reviews_tibble[,c("airline_nm"   
                                 ,"review_dt"
                                 ,"aprx_flight_dt"
                                 ,"ovrl_score_nb"
                                 # ,"seat_score_nb"
                                 # ,"c_srvc_score_nb"
                                 # ,"clean_score_nb"
                                 # ,"food_score_nb"
                                 # ,"lgrm_score_nb"
                                 # ,"entn_score_nb"
                                 # ,"value_score_nb"
                                 # ,"chk_in_score_nb"
                                 ,"flight_class"
                                 ,"flight_dm_itl"
                                 ,"user_gender"
                                 ,"user_lvl"
                                 ,"user_rvw_ct"
                                 ,"rvw_hpfl_ct"
                                 ,"via_mobile_in"
                                 ,"review_length"
                                 
                                 # Leave these two out (not use sentiment to predict score)
                                 # ,"avg_sentiment"
                                 # ,"std_dev_sentiment"
                                 
                                 ,"flight_dist"
                                 ,"flight_season"
                                  )]

# Remove observations with any missing (NA) values
features <- features[complete.cases(features),]

# Take a random subset
features <- features[sample(1:nrow(features),20000),]

print(paste("number of rows to evaluate: ",nrow(features),sep=''))

# Summary of data frame
summary(features)

# Make results repeatable
set.seed(6)

boruta.train <- Boruta(ovrl_score_nb ~ ., data = features, doTrace = 2, maxRuns = 50)

print(boruta.train)

plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)



plot(new_reviews_tibble$review_length ~ new_reviews_tibble$ovrl_score_nb)
plot(new_reviews_tibble$ovrl_score_nb ~ new_reviews_tibble$airline_nm)
plot(new_reviews_tibble$flight_dist ~ new_reviews_tibble$ovrl_score_nb)
plot(new_reviews_tibble$user_rvw_ct ~ new_reviews_tibble$ovrl_score_nb)
plot(new_reviews_tibble$ovrl_score_nb ~ new_reviews_tibble$flight_dm_itl)
plot(new_reviews_tibble$rvw_hpfl_ct ~ new_reviews_tibble$ovrl_score_nb)
plot(new_reviews_tibble$ovrl_score_nb ~ new_reviews_tibble$user_gender)
plot(new_reviews_tibble$ovrl_score_nb ~ new_reviews_tibble$flight_class)
plot(new_reviews_tibble$ovrl_score_nb ~ new_reviews_tibble$flight_season)





# subone <- subset(new_reviews_tibble, user_gender == 'M')
# 
# plot(subone$ovrl_score_nb ~ subone$flight_season)




# plot(new_reviews_tibble$ovrl_score_nb ~ new_reviews_tibble$seat_score_nb)
# plot(new_reviews_tibble$ovrl_score_nb ~ new_reviews_tibble$c_srvc_score_nb)
# plot(new_reviews_tibble$ovrl_score_nb ~ new_reviews_tibble$clean_score_nb)
# plot(new_reviews_tibble$ovrl_score_nb ~ new_reviews_tibble$food_score_nb)
# plot(new_reviews_tibble$ovrl_score_nb ~ new_reviews_tibble$lgrm_score_nb)
# plot(new_reviews_tibble$ovrl_score_nb ~ new_reviews_tibble$entn_score_nb)
# plot(new_reviews_tibble$ovrl_score_nb ~ new_reviews_tibble$value_score_nb)
# plot(new_reviews_tibble$ovrl_score_nb ~ new_reviews_tibble$chk_in_score_nb)





# look <- subset(new_reviews_tibble,user_rvw_ct > 4000)
# summary(data.frame(look$user_full_nm))

# install.packages("gmodels")
# library(gmodels)

# look <- look[1:20,]

# CrossTable(look$user_full_nm,look$airline_nm)


