#####
# MLnoise.R:
# Script to download and train a model on noise complaint data
# Martin Schiff 2/24/2019


#####
# Check for required libraries
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(doParallel)) install.packages("doParallel", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(RSocrata)) install.packages("RSocrata", repos = "http://cran.us.r-project.org")
if(!require(Rborist)) install.packages("Rborist", repos = "http://cran.us.r-project.org")

#####
# Pull the public data sets

# SODA App token to avoid throttling
token <- "JNHB3MHkIFRbMn4r24dddAYAy"

# NYC 311 Service Requests from 2010 to Present API
url <- "https://data.cityofnewyork.us/resource/fhrw-4uyv.json"

# Query arguments - we only want noise complaints and don't need all columns
args <- c("where=complaint_type like '%Noise%'",
          "select=created_date, agency, complaint_type, descriptor, incident_zip, community_board, bbl, latitude, longitude, borough, open_data_channel_type",
          "order=created_date")
catargs <- paste(paste("$",args, sep=""), collapse="&")
caturl <- paste(url, catargs, sep="?")

# Download the 311 data if not already saved (takes a while)
if (file.exists("dfNoise.RData")) {
  load("dfNoise.RData")
} else {
  dfNoise <- read.socrata(caturl, app_token = token)
  save(dfNoise, file = "dfNoise.RData")
}

# Download 2010 population by community district
url <- "https://data.cityofnewyork.us/resource/5hae-yeks.json"
args <- "$select=cd_number, borough, _2010_population AS pop2010"
caturl <- paste(url, args, sep="?")
cdPop <- read.socrata(caturl, app_token=token) %>%
  mutate(cd = paste(str_pad(cd_number, 2, side="left", pad="0"), str_to_upper(borough), sep=" "), borough=toupper(borough)) %>%
  select(-cd_number) %>% select(cd, borough, pop2010) %>%
  mutate(pop2010 = as.numeric(pop2010))

# Download monthly median rents by neighborhood from Streeteasy
dl <- tempfile()
download.file("https://streeteasy-market-data-download.s3.amazonaws.com/rentals/All/medianAskingRent_All.zip", dl)
rents <- read_csv(unzip(dl)) %>%
  rename(borough = Borough)

# split rents into whole boro and neighborhood, and tidy with one row per month
boroRents <- rents %>% 
  filter(areaType == "borough") %>% 
  select(-areaName, -areaType) %>%
  mutate(borough = toupper(borough)) %>%
  gather(month, rent, -borough) %>%
  mutate(month = as_date(parse_date_time(month, orders = "ym", tz = "America/New_York"))) %>%
  spread(borough,rent)

hoodRents <- rents %>% 
  filter(areaType == "neighborhood") %>% 
  select(-areaType)

# crosswalk hood names to community districts
dl <- tempfile()
download.file("https://raw.githubusercontent.com/martensitic/311Noise/master/resource/hoodCD.csv", dl)
hoodCD <- read_csv(dl) %>%
  mutate(cd = str_c(str_pad(cd, 2, side="left", pad="0"), " ", toupper(borough))) %>%
  select(-borough)

# stop all the downloadin
rm(args, catargs, caturl, token, url, dl, rents)

#####
# Assemble a reasonable benchmark rent for each community district.
# We take the mean of all available neighborhood median rents in the district for that month.
# Staten island only has boro-wide data so we fill that in for its 3 districts
# In other boros we get NaN when there is no data for any component neighborhood: in those
# cases we substitute the boro-wide data for that month.
# This will still leave a few NA's for staten island which we can impute in the preprocessing

cdRents <- hoodRents %>%
  left_join(hoodCD, by = "areaName") %>%
  group_by(cd) %>%
  summarize_at(vars(starts_with("2")), list(~mean(., na.rm=TRUE))) %>%
  gather(month, rent, -cd) %>% 
  spread(cd,rent) %>%
  mutate(`01 STATEN ISLAND` = boroRents$`STATEN ISLAND`,
         `02 STATEN ISLAND` = boroRents$`STATEN ISLAND`,
         `03 STATEN ISLAND` = boroRents$`STATEN ISLAND`) %>%
  mutate_at(vars(ends_with("BRONX")), list(~if_else(is.nan(.), boroRents$BRONX, .))) %>%
  mutate_at(vars(ends_with("BROOKLYN")), list(~if_else(is.nan(.), boroRents$BROOKLYN, .))) %>%
  mutate_at(vars(ends_with("MANHATTAN")), list(~if_else(is.nan(.), boroRents$MANHATTAN, .))) %>%
  mutate_at(vars(ends_with("QUEENS")), list(~if_else(is.nan(.), boroRents$QUEENS, .))) %>%
  mutate(month = as_date(parse_date_time(month, orders = "ym", tz = "America/New_York"))) 

#####
# Let's look at monthly noise complaints in each district per 1000 residents
# Inner join to the clean population table discards nonsensical districts (e.g. typos, parks)
# First, lag the rent data by a month: so we are predicting
# based on last month's median rent, since a real prediction won't
# have real time rent data.  That means we also have to drop the Jan 2010 values

cdRentsLag <- cdRents %>%
  mutate(month = lead(month))

noiseCDMonth <- dfNoise %>% 
  group_by(month = as_date(floor_date(created_date, unit="month")), cd = community_board) %>% 
  summarize(complaints = n()) %>% 
  inner_join(cdPop, by="cd") %>%
  mutate(rate = complaints / (pop2010 / 1000)) %>%
  left_join(gather(cdRentsLag, cd, rent, -month), by=c("month","cd")) %>%
  mutate(calmonth = month(month)) %>%
  filter(month >= ymd("2010-02-01"))

# we'll train on the 2010 to 2017 data
# and test on every complete month since (no partial months)
trainSet <- noiseCDMonth %>%
  filter(year(month) <= 2017) %>% ungroup() %>%
  select(calmonth, cd, borough, rent, rate) %>%
  mutate(cd = factor(cd), borough = factor(borough))

testSet <- noiseCDMonth %>%
  filter(year(month) > 2017 & month < rollback(today())) %>% 
  ungroup() %>%
  select(calmonth, cd, borough, rent, rate) %>%
  mutate(cd = factor(cd), borough = factor(borough))

# preprocess sets to impute missing rent values, 
# which also scales/centers numerics
preProc <- preProcess(trainSet %>% select(-rate), method=c("knnImpute"))

# generate preprocessed train and test sets, detatching and re-attaching the
# rate outcome so it doesn't get scaled
ppTrainSet <- predict(preProc, newdata = trainSet %>% select(-rate)) %>%
  mutate(rate = trainSet$rate)

ppTestSet <- predict(preProc, newdata = testSet %>% select(-rate)) %>%
  mutate(rate = testSet$rate)

# random forest prediciton using Rborist
rfGrid <- expand.grid(predFixed = c(18,19,20,21,22), minNode = 7)

tc <- trainControl(method = "boot",
                   number = 25,
                   verboseIter = TRUE,
                   allowParallel = TRUE)

# start a parallel cluster to speed things up
cl <- makePSOCKcluster(2, outfile = "parallel.log.txt")
registerDoParallel(cl)

rfFit <- train(rate ~ ., 
            data = ppTrainSet,
            tuneGrid = rfGrid,
            trControl = tc,
            method = "Rborist")

stopCluster(cl)


# predict the test set
# making sure none are < 0
rfTest <- predict(rfFit, ppTestSet)
rfTest[rfTest < 0] <- 0
rfRMSE <- RMSE(rfTest, ppTestSet$rate)
rfMAE <- MAE(rfTest, ppTestSet$rate)
print(str_c("Test set RMSE is ", rfRMSE, " and MAE is ", rfMAE, "."))

# save some output for use in the report
save(rfFit, testSet, ppTestSet, rfTest, rfRMSE, rfMAE, file = "mlOutput.RData")
