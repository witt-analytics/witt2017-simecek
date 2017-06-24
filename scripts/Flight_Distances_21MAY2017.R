# Append Flight Distances #
# Steven Simecek          #
# Create Date: 20MAY2017  #
# Update Date: 24MAY2017  #


#install.packages("ggmap")
#install.packages("geosphere")
library(ggmap)
library(geosphere)

unique_places <- unique(c(reviews_tibble$flight_orgn,reviews_tibble$flight_dest))
unique_places <- unique_places[!is.na(unique_places)]
length(unique_places)

ggmap::geocodeQueryCheck()
lon_lats <- ggmap::geocode(unique_places)
ggmap::geocodeQueryCheck()

lon_lat_lookup <- data.frame(unique_places,lon_lats)

look <- subset(lon_lat_lookup, is.na(lon) | is.na(lat))
nrow(look)
look2 <- as.character(c('Montgomery','Jackson','Monroe','Regina'))

more_lon_lats <- ggmap::geocode(c( "Montgomery, Alabama"
                                  ,"Jackson, Mississippi"
                                  ,"Monroe, Louisiana"
                                  ,"Regina, Saskatchewan"), source = "dsk" )
more_lon_lats <- data.frame(unique_places = look2,more_lon_lats)

lon_lat_lookup_final <- merge( lon_lat_lookup
                              ,more_lon_lats
                              ,by.x = 'unique_places'
                              ,by.y = 'unique_places'
                              ,all = TRUE)

lon_lat_lookup_final$lon <- ifelse(!is.na(lon_lat_lookup_final$lon.x)
                                   ,lon_lat_lookup_final$lon.x
                                   ,lon_lat_lookup_final$lon.y)
lon_lat_lookup_final$lat <- ifelse(!is.na(lon_lat_lookup_final$lat.x)
                                   ,lon_lat_lookup_final$lat.x
                                   ,lon_lat_lookup_final$lat.y)

lon_lat_lookup_final <- lon_lat_lookup_final[,c('unique_places','lon','lat')]


# Merge data frames
# Merge destination lat lon
reviews_merged <- merge( reviews_tibble
                        ,lon_lat_lookup_final
                        ,by.x = "flight_orgn"
                        ,by.y = "unique_places"
                        ,all.x = TRUE)
reviews_merged <- reviews_merged[,c(names(reviews_tibble),'lon','lat')]
names(reviews_merged)[names(reviews_merged)=="lon"] <- "orgn_lon_nb"
names(reviews_merged)[names(reviews_merged)=="lat"] <- "orgn_lat_nb"

# Merge origin lat lon
reviews_merged <- merge( reviews_merged
                         ,lon_lat_lookup_final
                         ,by.x = "flight_dest"
                         ,by.y = "unique_places"
                         ,all.x = TRUE)
reviews_merged <- reviews_merged[,c(names(reviews_tibble),'orgn_lon_nb','orgn_lat_nb','lon','lat')]
names(reviews_merged)[names(reviews_merged)=="lon"] <- "dest_lon_nb"
names(reviews_merged)[names(reviews_merged)=="lat"] <- "dest_lat_nb"

reviews_merged <- reviews_merged[order(reviews_merged$obs_nb),]


orgns <- cbind(reviews_merged$orgn_lon_nb
           ,reviews_merged$orgn_lat_nb)

# str(orgns)

dests <- c(reviews_merged$dest_lon_nb
           ,reviews_merged$dest_lat_nb)

dists <- distGeo(cbind(reviews_merged$orgn_lon_nb
                      ,reviews_merged$orgn_lat_nb)
                ,cbind(reviews_merged$dest_lon_nb
                      ,reviews_merged$dest_lat_nb))

reviews_dists <- cbind(reviews_merged,flight_dist = dists * .000621371)

look <- subset(reviews_dists, is.na(flight_dist))
nrow(look)

write.csv(reviews_dists,"C:/Users/steve/Documents/Capstone_Local_Drive/reviews_dists.csv")
