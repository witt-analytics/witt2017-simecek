form <- ovrl_score_nb ~
  
origins <- unique(Datas$flight_orgn)
origin.locs <- ggmap::geocode(origins[1:100])

destins <- unique(Datas$flight_dest)
destin.locs <- ggmap::geocode(destins[1:100])

uniques <- unique(c(origins, destins))

locs <- ggmap::geocode(uniques, key = Sys.getenv('gmapsAPI'))

distances <- data.frame(origin = Datas$flight_orgn,
                        org.lon = rep(NA, nrow(Datas)),
                        org.lat = rep(NA, nrow(Datas)),
                        destin = Datas$flight_dest,
                        des.lon = rep(NA, nrow(Datas)),
                        des.lat = rep(NA, nrow(Datas)))

places <- data.frame(uniques, locs)

rows <- sapply(seq_along(Datas$flight_orgn),
       FUN = function(x) { 
         col1 <- which(Datas$flight_orgn[x]==places[,1])
         col2 <- which(Datas$flight_dest[x]==places[,1])
         return(data.frame(col1, col2))
         })

rows <- matrix(unlist(rows), ncol = 2, byrow = T)

distances$org.lon <- places[rows[,1],2]
distances$org.lat <- places[rows[,1],3]
distances$des.lon <- places[rows[,2],2]
distances$des.lat <- places[rows[,2],3]

flight_distances <- 
  fields::rdist.earth.vec(x1 = as.matrix(distances[,2:3]),
                          x2 = as.matrix(distances[,5:6]))
