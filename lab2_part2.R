#***** PART 2: Data Processing *****#

# Use library
library(dplyr)
library(tidyverse)

##### WAC #####

# Input WAC dataset and clean up table
wac <- read.csv("wa_wac_S000_JT00_2015.csv", stringsAsFactors = FALSE, fileEncoding = "latin1")
wac$w_geocode <- as.character(wac$w_geocode)
wac$w_geocode[which(length(wac$w_geocode) == 14)] <- paste0("0",wac$w_geocode[which(length(wac$w_geocode) == 14)])
wac$createdate <- NULL

# Convert specific columns to numeric
wacid <- 2:52
wac[wacid] <-  data.matrix(wac[wacid])
sapply(wac, class)

# Modify column for tract code
wac$w_geocode <- substr(wac$w_geocode, 1, 11)

# Aggregate by the tract_geocode column
wac_tract_group <- aggregate(. ~ wac$w_geocode, data = wac[wacid], FUN = sum)

# Create a new csv file for tracts
write.csv(wac_tract_group, "wa_wac_S000_JT00_2015_tract.csv")

##### RAC #####

# Input RAC dataset and clean up table
rac <- read.csv("wa_rac_S000_JT00_2015.csv", stringsAsFactors = FALSE, fileEncoding = "latin1")
rac$h_geocode <- as.character(rac$h_geocode)
rac$h_geocode[which(length(rac$h_geocode) == 14)] <- paste0("0",rac$h_geocode[which(length(rac$h_geocode) == 14)])
rac$createdate <- NULL

# Convert specific columns to numeric
racid <- 2:42
rac[racid] <-  data.matrix(rac[racid])
sapply(rac, class)

# Modify column for tract code
rac$h_geocode <- substr(rac$h_geocode, 1, 11)

# Aggregate by the tract_geocode column
rac_tract_group <- aggregate(. ~ rac$h_geocode, data = rac[racid], FUN = sum)

# Create a new csv file for tracts
write.csv(rac_tract_group, "wa_rac_S000_JT00_2015_tract.csv")

# Part 3.2

# Store the wac data into a data frame for visualizing
wacData <- read.csv("wa_wac_S000_JT00_2015_tract.csv")

ggplot(wacData, aes(x=CR02, y=CE03)) + geom_point() + scale_x_continuous(breaks = seq(0, 4000, 500))+scale_y_continuous(breaks = seq(0, 50000, 5000))+labs(title = "Jobs making over $3333/month over Number of Jobs for African Americans", x = "Number of Jobs for African Americans", y = "Number of Jobs Making Over $3333/month")

ggplot(wacData, aes(x=CD04, y=CE03))+geom_point()+scale_x_continuous(breaks = seq(0, 40000, 2500))+scale_y_continuous(breaks = seq(0, 50000, 5000))+labs(title = "Jobs making over $3333/month over Number of Jobs for College Graduates", x = "Number of Jobs for College Graduates", y = "Number of Jobs Making over $3333/month")

