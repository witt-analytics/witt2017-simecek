data_file <- file.choose()

library(readr)

init_reviews <- read_csv(data_file)

save.name <- file.path(dirname(data_file),'init_reviews.Rdata')

save(init_reviews, file = save.name, compression_level = 9)
