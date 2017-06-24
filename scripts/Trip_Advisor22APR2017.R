# Web Scraping for Capstone Project #
# Tripadvisor.com                   #
# Steven Simecek                    #
# Create Date: 17JAN2017            #
# Update Date: 22APR2017            #


# To speed up, create the empty shells of the vecotrs first, with the correct lengths.

# Clear everything
#rm(list = ls())

# Store start time
Start_Time <- Sys.time()
print(paste("Start Time: ", Start_Time, sep = ''))
# Start the clock
ptm <- proc.time()

# Install rvest package
# install.packages("rvest")
# install.packages("gender")
# install.packages("devtools")
# install.packages("genderdata", type = "source",
#                  repos = "http://packages.ropensci.org")

# Load packages
library(xml2)
library(rvest)
library(stringr)
library(gender)
library(genderdata)

## Initialize data frames
#rm(all_review_ids)
#rm(full_reviews)
#rm(full_reviews_final)
all_review_ids <- data.frame
full_reviews <- data.frame
full_reviews_final <- data.frame
current_gender <- data.frame

# Initialize vectors
Page_Number_final <- vector()
Review_IDs_final <- vector()
User_Name_final <- vector()
User_Gender_final <- vector()
Review_Destinations_final <- vector()
Review_Classes_final <- vector()
Review_Dmstc_Intl_final <- vector()
Review_IDs_final_df <- vector()
Review_URL_final <- vector()
Review_Title_final <- vector()
Full_Review_final <- vector()
Via_Mobile_final <- vector()
Airline_Name_final <- vector()
Review_Date_final <- vector()
Overall_Review_final <- vector()
User_Level_final <- vector()
User_Location_final <- vector()
Flight_Date_final <- vector()
User_Review_Ct_final <- vector()
Helpful_Vote_Ct_final <- vector()

Seat_Comfort_final <- vector()
Customer_Service_final <- vector()
Cleanliness_final <- vector()
Food_Beverage_final <- vector()

Legroom_final <- vector()
Entertainment_final <- vector()
Value_final <- vector()
Check_In_final <- vector()


####################################################################################################
# Run Web Scraping from Tripadvisor.com
####################################################################################################

######################################################################
# Pull Initial Airline Information for:
######################################################################
# 1. American
# 2. Delta
# 3. Southwest
# 4. United
# 5. JetBlue
# 6. Alaska
# 7. Spirit
# 8. Frontier
# 9. Allegiant
# 10. Hawaiian

# List all airlines for which data will be collected
# Uncomment airlines below to scrape reviews
Airlines <- c( 
  
  # Smaller airlines for testing
   "Airline_Review-d10533124-Reviews-Cheap-Flights-Air-Iceland"
  ,"Airline_Review-d8728874-Reviews-Cheap-Flights-Aeromar"
  ,"Airline_Review-d8729135-Reviews-Cheap-Flights-Regent-Airways"

              #  "Airline_Review-d8729020-Reviews-Cheap-Flights-American-Airlines"
              # ,"Airline_Review-d8729060-Reviews-Cheap-Flights-Delta-Air-Lines"
              # ,"Airline_Review-d8729156-Reviews-Cheap-Flights-Southwest-Airlines"
              # ,"Airline_Review-d8729177-Reviews-Cheap-Flights-United-Airlines"
              # ,"Airline_Review-d8729099-Reviews-Cheap-Flights-JetBlue-Airways"
              # ,"Airline_Review-d8729017-Reviews-Cheap-Flights-Alaska-Airlines"
              # ,"Airline_Review-d8729157-Reviews-Cheap-Flights-Spirit-Airlines"
              # ,"Airline_Review-d8729213-Reviews-Cheap-Flights-Frontier-Airlines"
              # ,"Airline_Review-d8729019-Reviews-Cheap-Flights-Allegiant-Air"
              # ,"Airline_Review-d8729086-Reviews-Cheap-Flights-Hawaiian-Airlines"
              )

# Loop through each airline
for (airline in Airlines) {

  URL_ROOT <- "https://www.tripadvisor.com/"
  URL_SUB <- airline
  
  TA_SESSION <- html_session(paste(URL_ROOT,URL_SUB,sep = ""))
  TA_HTML <- read_html(TA_SESSION)
  
  # Pull name of airline
  Airline_Name_nd <- html_node(TA_HTML,".heading_height")
  Airline_Name_tx <- stringr::str_trim(html_text(Airline_Name_nd))
  print(paste("Starting Airline: ",Airline_Name_tx,sep = ''))
  
  # Pull airline URL id number (for use in individual URL looping)
  Airline_URL_id <- as.numeric(stringr::str_extract_all(airline,"[:number:]{1,10}"))
  #print(Airline_URL_id)
  
  # Pull airline URL name (for use in individual URL looping)
  Airline_URL_nm <- gsub(" ","_",Airline_Name_tx)
  #print(Airline_URL_nm)
  
  # Pull number of ratings
  Ratings_Count_nd <- html_node(TA_HTML,"#taplc_airline_detail_review_results_description_0 form")
  Ratings_Count_nb <- html_text(Ratings_Count_nd)
  # Keep only numbers
  Ratings_Count_nb <- stringr::str_replace_all(Ratings_Count_nb,":\nEnglish\nreviews\nClear all","")
  Ratings_Count_nb <- stringr::str_replace_all(Ratings_Count_nb,"\nShowing","")
  Ratings_Count_nb <- stringr::str_replace_all(Ratings_Count_nb," ","")
  Ratings_Count_nb <- stringr::str_replace_all(Ratings_Count_nb,",","")
  Ratings_Count_nb <- as.numeric(Ratings_Count_nb)
  print(paste("Number of ratings: ",Ratings_Count_nb, sep = ''))
  # Calculate number of pages (10 per page)
  Pages_Count_nb <- ceiling(Ratings_Count_nb/10)
  #print(Pages_Count_nb)
  
  # Below is for testing
  #Pages_Count_nb <- 2


  ######################################################################
  # Iterate Through All Pages
  ######################################################################
  for(a in 1:Pages_Count_nb) {
    # List all review IDs
    Review_IDs <- html_attr(html_nodes(TA_SESSION,".quote a"),"id")
    #print(Review_IDs)
    Review_IDs_final <- c(Review_IDs_final,Review_IDs)
    
    for (rev_id in Review_IDs){
      # Record page number
      Page_Number_final <- c(Page_Number_final,(a-1)*10)
      
      rev_id <- gsub("rn","",rev_id)
      #print(paste("Number: ",rev_id,sep = ''))
      
      # Pull review destination and origin
      Review_Destinations <- html_text(html_nodes(TA_HTML,paste("#review_",rev_id," .categoryLabel:nth-child(3)",sep = '')))
      #print(Review_Destinations)
      if (!length(Review_Destinations)) Review_Destinations <- ""
      
      # Pull review classes
      Review_Classes <- html_text(html_nodes(TA_HTML,paste("#review_",rev_id," .categoryLabel:nth-child(2)",sep = '')))
      #print(Review_Classes)
        # Check for missing class
        if(length(grep("-",Review_Classes))!=0){
          Review_Destinations <- Review_Classes
          Review_Classes <- ""
        }
      if (!length(Review_Classes)) Review_Classes <- ""
      # Only assign review dest final after checking for missing class
      Review_Destinations_final <- c(Review_Destinations_final,Review_Destinations)
      Review_Classes_final <- c(Review_Classes_final,Review_Classes)
      
      # Pull review domestic/international
      Review_Dmstc_Intl <- html_text(html_nodes(TA_HTML,paste("#review_",rev_id," .categoryLabel:nth-child(1)",sep = '')))
      #print(Review_Dmstc_Intl)
      if (!length(Review_Dmstc_Intl)) Review_Dmstc_Intl <- ""
      Review_Dmstc_Intl_final <- c(Review_Dmstc_Intl_final,Review_Dmstc_Intl)
    }
    
    #Go to next page
    if (a != Pages_Count_nb) TA_SESSION <- follow_link(TA_SESSION, css=".next")
    TA_HTML <- read_html(TA_SESSION)
  }
  
  # List all reviews in data frame
  all_review_ids <- cbind(Review_IDs_final)
  
  ######################################################################
  # Loop through all IDs by individual URL
  ######################################################################
  id_num <- 0
  
  for (id in Review_IDs_final) {
    id_num <- id_num + 1
    # Go to individual review page
    Review_URL_tx <- paste("https://www.tripadvisor.com/ShowUserReviews-g1-d",Airline_URL_id,"-r",str_sub(id, 3),"-",Airline_URL_nm,"-World.html#REVIEWS",sep = '')
    TA_SESSION <- html_session(Review_URL_tx)
    TA_HTML <- read_html(TA_SESSION)
    
    # Print information to console
    #print(paste("Airline is: ",Airline_Name_tx,sep = ""))
    #print(paste("URL: ",Review_URL_tx, sep = ''))
    #print(paste("Now reading review #: ",id,sep = ''))
    
    # Store URLs
    Review_URL_final <- c(Review_URL_final,Review_URL_tx)
    
    # Pull full review
    Full_Review_nd <- html_node(TA_HTML,".first p")
    Full_Review_tx <- str_sub(str_sub(html_text(Full_Review_nd),2),1,-2)
    #print(Full_Review_tx)
    Full_Review_final <- c(Full_Review_final,Full_Review_tx)
    
    #Pull review title
    Review_Title_nd <- html_node(TA_HTML,".first .quote")
    Review_Title_tx <- str_sub(str_sub(html_text(Review_Title_nd),2),1,-2)
    #print(Review_Title_tx)
    Review_Title_final <- c(Review_Title_final,Review_Title_tx)
    
    #Pull review mobile indicator
    Via_Mobile_nd <- html_node(TA_HTML,".first .viaMobile")
    Via_Mobile_tx <- html_text(Via_Mobile_nd)
    if (is.na(Via_Mobile_tx)){
      Via_Mobile_tx <- "N"
    }else{
      Via_Mobile_tx <- "Y"
    }
    #print(Via_Mobile_tx)
    Via_Mobile_final <- c(Via_Mobile_final,Via_Mobile_tx)
    
    # Pull review date
    Review_Date_nd <- html_node(TA_HTML,".first .ratingDate")
    Review_Date_tx <- html_attr(Review_Date_nd, "content")
    #print(Review_Date_tx)
    Review_Date_final <- c(Review_Date_final,Review_Date_tx)
    
    # Pull overall rating
    Overall_Review_nd <- html_node(TA_HTML,".first .rating_s_fill")
    Overall_Review_tx <- gsub(" of ","/",gsub(" bubbles","",html_attr(Overall_Review_nd,"alt")))
    # print(Overall_Review_tx)
    Overall_Review_final <- c(Overall_Review_final,Overall_Review_tx)
    
    # Pull user name
    User_Name_nd <- html_node(TA_HTML,".first .memberOverlayLink")
    User_Name_tx <- str_sub(gsub("\n","",html_text(User_Name_nd)),2)
    #print(User_Name_tx)
    User_Name_final <- c(User_Name_final,User_Name_tx)
    
    # Find user first name
    frst_nm <-  word(User_Name_tx,1,sep=(" ")) %>%
                word(1,sep=("_")) %>%
                word(1,sep=("-"))
    # Remove numbers
    frst_nm <- gsub("[[:digit:]]","",frst_nm)
    #print(frst_nm)
    
    # Find gender of first name
    current_gender <- gender(frst_nm, method="ssa", years=c(1900,1999))
    
    # Store blank value if no gender found
    if(nrow(current_gender) == 0){
      User_Gender_final <- c(User_Gender_final,"")
    }else{
      User_Gender_final <- c(User_Gender_final,current_gender[4])
    }
    User_Gender_final <- unname(User_Gender_final)
    #print(User_Gender_final)
    
    # Pull user location
    User_Location_nd <- html_node(TA_HTML,".first .location")
    User_Location_tx <- gsub("\n","",html_text(User_Location_nd))
    #print(User_Location_tx)
    User_Location_final <- c(User_Location_final,User_Location_tx)
    
    # Pull user level
    User_Level_nd <- html_node(TA_HTML,".first .badge")
    User_Level_tx <- stringr::str_extract_all(html_attr(User_Level_nd,"class"),"[:number:]{1,10}")
    #print(User_Level_tx)
    User_Level_final <- c(User_Level_final,User_Level_tx)
    
    # Pull flight date
    Flight_Date_nd <- html_node(TA_HTML,".first .recommend-titleInline")
    Flight_Date_tx <- gsub("Traveled ","",html_text(Flight_Date_nd))
    #print(Flight_Date_tx)
    Flight_Date_final <- c(Flight_Date_final,Flight_Date_tx)
    
    # Pull user number of reviews
    User_Review_Ct_nd <- html_node(TA_HTML,".first .badgeText")
    User_Review_Ct_tx <- stringr::str_extract_all(gsub(",","",html_text(User_Review_Ct_nd)),"[:number:]{1,10}")
    #print(User_Review_Ct_tx)
    User_Review_Ct_final <- c(User_Review_Ct_final,User_Review_Ct_tx)
    
    # Pull number of helpful votes
    Helpful_Vote_Ct_nd <- html_node(TA_HTML,".first .badge.no_cpu .badgeText")
    Helpful_Vote_Ct_tx <- stringr::str_extract_all(gsub(",","",html_text(Helpful_Vote_Ct_nd)),"[:number:]{1,10}")
    #print(Helpful_Vote_Ct_tx)
    Helpful_Vote_Ct_final <- c(Helpful_Vote_Ct_final,Helpful_Vote_Ct_tx)
    
    
      ######################################################################
      # Pull Sub-Reviews
      ######################################################################
        Seat_Comfort_tx <- ""
        Customer_Service_tx <- ""
        Cleanliness_tx <- ""
        Food_Beverage_tx <- ""
        
        Legroom_tx <- ""
        Entertainment_tx <- ""
        Value_tx <- ""
        Check_In_tx <- ""
        
        
        # Pull sub-reviews
        # Iterate through first column
        for (sub_row in 0:3) {
          #print(sub_row)
          iterate_row <- ""
          if (sub_row != 0) for (i in 1:sub_row){
            iterate_row <- paste(iterate_row, ".recommend-answer+ .recommend-answer",sep = '')
          }
          #print (iterate_row)
          
          # Pull sub-rating name
          Sub_Rating_nd <- html_node(TA_HTML,paste(".first .first ",iterate_row," .recommend-description",sep = ''))
          Sub_Rating_nm <- html_text(Sub_Rating_nd)
          #print(paste("Rating Name: ",Sub_Rating_nm, sep = ''))
          # Pull sub-rating number
          Sub_Rating_nd <- html_node(TA_HTML,paste(".first .first ",iterate_row," .ui_bubble_rating", sep = ''))
          Sub_Rating_nb <- gsub(" of ","/",gsub(" bubbles","",html_attr(Sub_Rating_nd,"alt")))
          #print(paste("Rating number: ",Sub_Rating_nb, sep = ''))
          if (!is.na(Sub_Rating_nm)){
            if(Sub_Rating_nm == "Seat comfort") Seat_Comfort_tx <- Sub_Rating_nb
            if(Sub_Rating_nm == "Customer service (e.g. attitude, care, helpfulness)") Customer_Service_tx <- Sub_Rating_nb
            if(Sub_Rating_nm == "Cleanliness") Cleanliness_tx <- Sub_Rating_nb
            if(Sub_Rating_nm == "Food and Beverage") Food_Beverage_tx <- Sub_Rating_nb
          }
        }
        
        # Iterate through second column
        for (sub_row in 0:3) {
          #print(sub_row)
          iterate_row <- ""
          if (sub_row != 0) for (i in 1:sub_row){
            iterate_row <- paste(iterate_row, ".recommend-answer+ .recommend-answer",sep = '')
          }
          #print (iterate_row)
          
          # Pull sub-rating name
          Sub_Rating_nd <- html_node(TA_HTML,paste(".first .first+ .recommend-column ",iterate_row," .recommend-description",sep = ''))
          Sub_Rating_nm <- html_text(Sub_Rating_nd)
          #print(paste("Rating Name: ",Sub_Rating_nm, sep = ''))
          # Pull sub-rating number
          Sub_Rating_nd <- html_node(TA_HTML,paste(".first .first+ .recommend-column ",iterate_row," .ui_bubble_rating", sep = ''))
          Sub_Rating_nb <- gsub(" of ","/",gsub(" bubbles","",html_attr(Sub_Rating_nd,"alt")))
          #print(paste("Rating number: ",Sub_Rating_nb, sep = ''))
          
            if (!is.na(Sub_Rating_nm)){
              if(Sub_Rating_nm == "Legroom") Legroom_tx <- Sub_Rating_nb
              if(Sub_Rating_nm == "In-flight entertainment (WiFi, TV, movies)") Entertainment_tx <- Sub_Rating_nb
              if(Sub_Rating_nm == "Value for money") Value_tx <- Sub_Rating_nb
              if(Sub_Rating_nm == "Check-in and Boarding (e.g. efficiency, service at gate)") Check_In_tx <- Sub_Rating_nb
            }
        }         
        
        Seat_Comfort_final <- c(Seat_Comfort_final,Seat_Comfort_tx)
        Customer_Service_final <- c(Customer_Service_final,Customer_Service_tx)
        Cleanliness_final <- c(Cleanliness_final,Cleanliness_tx)
        Food_Beverage_final <- c(Food_Beverage_final,Food_Beverage_tx)
        
        Legroom_final <- c(Legroom_final,Legroom_tx)
        Entertainment_final <- c(Entertainment_final,Entertainment_tx)
        Value_final <- c(Value_final,Value_tx)
        Check_In_final <- c(Check_In_final,Check_In_tx)
    
    # List airline name
    Airline_Name_final <- c(Airline_Name_final,Airline_Name_tx)
    
    # Print percent complete every 10 iterations
    if (id_num%%10 == 0) print(paste(Airline_Name_tx," | Page: ",id_num/10," of ",Pages_Count_nb," | Percent complete: ",round(100*(id_num/Ratings_Count_nb),2),"%",sep = ''))
  }

Review_IDs_final_df <- c(Review_IDs_final_df,Review_IDs_final)

# Write airline reviews to data frame
full_reviews <- cbind( Page_Number_final
                      ,Review_IDs_final_df
                      ,Review_URL_final
                      ,Airline_Name_final
                      ,Overall_Review_final
                      ,Review_Date_final
                      ,Flight_Date_final
                      ,Review_Destinations_final
                      ,Review_Classes_final
                      ,Review_Dmstc_Intl_final
                      ,User_Name_final
                      ,User_Gender_final
                      ,User_Level_final
                      ,User_Location_final
                      ,User_Review_Ct_final
                      ,Helpful_Vote_Ct_final
                      ,Via_Mobile_final
                      ,Review_Title_final
                      ,Full_Review_final
                      ,Seat_Comfort_final
                      ,Customer_Service_final
                      ,Cleanliness_final
                      ,Food_Beverage_final
                      ,Legroom_final
                      ,Entertainment_final
                      ,Value_final
                      ,Check_In_final)

# Keep only current airline
full_reviews <- subset(full_reviews,Airline_Name_final == Airline_Name_tx)

# Write airline reviews to text file
write.table(full_reviews,paste("C:/Users/steve/Documents/Capstone_Local_Drive/Text_files/",Airline_Name_tx,".txt",sep = ''),sep = "|")

# Clear the review ids and data frame in preparation for the next airline
rm(Review_IDs_final)
rm(full_reviews)
Review_IDs_final <- vector()
}

# Write final data
full_reviews_final <- cbind( Page_Number_final
                            ,Review_IDs_final_df
                            ,Review_URL_final
                            ,Airline_Name_final
                            ,Overall_Review_final
                            ,Review_Date_final
                            ,Flight_Date_final
                            ,Review_Destinations_final
                            ,Review_Classes_final
                            ,Review_Dmstc_Intl_final
                            ,User_Name_final
                            ,User_Gender_final
                            ,User_Level_final
                            ,User_Location_final
                            ,User_Review_Ct_final
                            ,Helpful_Vote_Ct_final
                            ,Via_Mobile_final
                            ,Review_Title_final
                            ,Full_Review_final
                            ,Seat_Comfort_final
                            ,Customer_Service_final
                            ,Cleanliness_final
                            ,Food_Beverage_final
                            ,Legroom_final
                            ,Entertainment_final
                            ,Value_final
                            ,Check_In_final)

# Write data to text file
write.table(full_reviews_final,paste("C:/Users/steve/Documents/Capstone_Local_Drive/Text_files/All_Airlines_",toupper(format(Sys.Date(),"%d%b%Y")),".txt",sep = ''),sep = "||")

# Store end time
End_Time <- Sys.time()
print(paste("Start Time: ", Start_Time, sep = ''))
print(paste("End Time: ", End_Time, sep = ''))
# Stop the clock
proc.time() - ptm
