


# summary(new_reviews_tibble)
str(new_reviews_tibble$ovrl_score_nb)
str(new_reviews_tibble$value_score_nb)
str(new_reviews_tibble$user_lvl)
# Convert the following variables to factors

convert <- c("ovrl_score_nb" 
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
)
# convert
# new_reviews_tibble[,convert] <- data.frame(apply(new_reviews_tibble[convert], 2, as.factor))

str(new_reviews_tibble$ovrl_score_nb)
str(new_reviews_tibble$value_score_nb)
str(new_reviews_tibble$user_lvl)

new_reviews_tibble <- new_reviews_tibble[1:100,c("ovrl_score_nb" 
                                                 ,"flight_class"
                                                 ,"flight_dm_itl" 
                                                 ,"user_gender"
                                                 ,"avg_sentiment"
                                                 ,"airline_nm")]

summary(new_reviews_tibble)


# Ensure the results are repeatable
set.seed(6)

# load the data
# data(PimaIndiansDiabetes)
# calculate correlation matrix
# correlationMatrix <- cor(new_reviews_tibble[,c("ovrl_score_nb","avg_sentiment")])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)



set.seed(6)

control <- trainControl(method="repeatdcv", number=10)

model <- train(ovrl_score_nb ~ avg_sentiment, data=new_reviews_tibble, method = "lvq", preProcess="scale", trControl=control)

importance <- varImp(model, scale=F)




raw.orig <- read.csv(file = file.choose(), 
                     header = T, 
                     sep = "\t")
View(raw.orig)

raw = subset(raw.orig, 
             select = c("Metal","OTW","AirDecay","Koc"))

row.names(raw) = raw.orig$CASNumber
raw = na.omit(raw);

frmla = Metal ~ OTW + AirDecay + Koc

# Metal: Core Metal (CM); Metal (M); Non-Metal (NM); Core Non-Metal (CNM)

fit = rpart(frmla, method="class", data=raw)

printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results
summary(fit) # detailed summary of splits

plot(fit, uniform=TRUE, main="Classification Tree for Chemicals")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# tabulate some of the data
table(subset(raw, Koc>=190.5)$Metal)



library(maptree)
library(cluster)
draw.tree( clip.rpart (rpart ( raw), best=7),
           nodeinfo=TRUE, units="species",
           cases="cells", digits=0)
a = agnes ( raw[2:4], method="ward" )
names(a)
a$diss
b = kgs (a, a$diss, maxclust=20)

plot(names(b), b, xlab="# clusters", ylab="penalty", type="n")
xloc = names(b)[b==min(b)]
yloc = min(b)
ngon(c(xloc,yloc+.75,10, "dark green"), angle=180, n=3)
apply(cbind(names(b), b, 3, 'blue'), 1, ngon, 4) # cbind(x,y,size,color)


## Trip Advisor Data

Datas <- load('data/with_distances.Rdata')

Datas$ovrl_score_nb <- as.factor(Datas$ovrl_score_nb)
Datas$flight_class <- as.factor(Datas$flight_class)
Datas$user_gender  <- as.factor(Datas$user_gender)
Datas$user_lvl <- as.factor(Datas$user_lvl)


Data_sub <- Datas[sample(1:nrow(Datas), size = 1000),]

frmla <- ovrl_score_nb ~ flight_distance + flight_class + user_gender + user_lvl

ev.raw = evtree(frmla, 
                data = Data_sub)
plot(ev.raw)
table(predict(ev.raw), raw$Metal)
1-mean(predict(ev.raw) == raw$Metal)




"ovrl_score_nb" 
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











