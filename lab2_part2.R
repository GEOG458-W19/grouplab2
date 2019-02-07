#***** PART 2: Data Processing *****#

# Use library
library(dplyr)
library(reshape2)
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

##### OD #####

# Input OD dataset and clean up table
od <- read.csv("wa_od_main_JT00_2015.csv")
od$w_geocode <- as.character(od$w_geocode)
od$h_geocode <- as.character(od$h_geocode)
od$createdate <- NULL

# Convert specific columns to numeric
odid <- 3:12
od[odid] <-  data.matrix(od[odid])
sapply(od, class)

# Modify column for tract code
od$w_geocode <- substr(od$w_geocode, 1, 11)
od$h_geocode <- substr(od$h_geocode, 1, 11)

# Cast into matrix
od_u <- unique(od[,1:2])

# Total number of jobs
od_S000_matrix <- acast(od, w_geocode ~ h_geocode, value.var = "S000", fun.aggregate = sum)
od_S000_matrix[is.na(od_S000_matrix)] = 0

# Number of jobs workers age 29 or younger
od_SA01_matrix <- acast(od, w_geocode ~ h_geocode, value.var = "SA01", fun.aggregate = sum)
od_SA01_matrix[is.na(od_SA01_matrix)] = 0

# Number of jobs workers age 30-54
od_SA02_matrix <- acast(od, w_geocode ~ h_geocode, value.var = "SA02", fun.aggregate = sum)
od_SA02_matrix[is.na(od_SA02_matrix)] = 0

# Number of jobs workers age 55 or older 
od_SA03_matrix <- acast(od, w_geocode ~ h_geocode, value.var = "SA03", fun.aggregate = sum)
od_SA03_matrix[is.na(od_SA03_matrix)] = 0

# Number of jobs with earnings $1250/month or less
od_SE01_matrix <- acast(od, w_geocode ~ h_geocode, value.var = "SE01", fun.aggregate = sum)
od_SE01_matrix[is.na(od_SE01_matrix)] = 0

# Number of jobs with earnings $1251/month - $3333/month
od_SE02_matrix <- acast(od, w_geocode ~ h_geocode, value.var = "SE02", fun.aggregate = sum)
od_SE02_matrix[is.na(od_SE02_matrix)] = 0

# Number of jobs with earnings greater than $3333/month
od_SE03_matrix <- acast(od, w_geocode ~ h_geocode, value.var = "SE03", fun.aggregate = sum)
od_SE03_matrix[is.na(od_SE03_matrix)] = 0

# Number of jobs in Goods Producing industry sectors 
od_SI01_matrix <- acast(od, w_geocode ~ h_geocode, value.var = "SI01", fun.aggregate = sum)
od_SI01_matrix[is.na(od_SI01_matrix)] = 0

# Number of jobs in Trade, Transportation, and Utilities industry sectors
od_SI02_matrix <- acast(od, w_geocode ~ h_geocode, value.var = "SI02", fun.aggregate = sum)
od_SI02_matrix[is.na(od_SI02_matrix)] = 0

# Number of jobs in all other service industry sectors 
od_SI03_matrix <- acast(od, w_geocode ~ h_geocode, value.var = "SI03", fun.aggregate = sum)
od_SI03_matrix[is.na(od_SI03_matrix)] = 0

#part3.2

#store the wac data into a data frame for visualizing
wacData <- read.csv("wa_wac_S000_JT00_2015_tract.csv")

ggplot(wacData, aes(x=CR02, y=CE03))+geom_point()+scale_x_continuous(breaks = seq(0, 4000, 500))+scale_y_continuous(breaks = seq(0, 50000, 5000))+labs(title = "Jobs making over $3333/month over Number of Jobs for African Americans", x = "Number of Jobs for African Americans", y = "Number of Jobs Making Over $3333/month")

ggplot(wacData, aes(x=CD04, y=CE03))+geom_point()+scale_x_continuous(breaks = seq(0, 40000, 2500))+scale_y_continuous(breaks = seq(0, 50000, 5000))+labs(title = "Jobs making over $3333/month over Number of Jobs for College Graduates", x = "Number of Jobs for College Graduates", y = "Number of Jobs Making over $3333/month")

