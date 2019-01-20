if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(RSocrata)) install.packages("RSocrata", repos = "http://cran.us.r-project.org")

# My SODA App token to avoid throttling
token <- "JNHB3MHkIFRbMn4r24dddAYAy"

# NYC 311 Service Requests from 2010 to Present API
url <- "https://data.cityofnewyork.us/resource/fhrw-4uyv.json"

# Query arguments - we only want noise complaints and don't need all columns
args <- c("where=complaint_type like '%Noise%'",
          "select=created_date, agency, complaint_type, descriptor, incident_zip, community_board, bbl, latitude, longitude, borough, open_data_channel_type")

# Assemble the SODA URL
catargs <- paste(paste("$",args, sep=""), collapse="&")
caturl <- paste(url, catargs, sep="?")

# Download the 311 data (takes a while)
dfNoise <- read.socrata(caturl, app_token = token)

# download population by community district/PUMA
url <- "https://data.cityofnewyork.us/resource/5hae-yeks.json"
dfPop <- read.socrata(url, app_token=token)
dfPop <- dfPop %>% mutate(cd_full = paste(str_pad(cd_number, 2, side="left", pad="0"), str_to_upper(borough), sep=" "))

# Median rents from Streeteasy
dl <- tempfile()
download.file("https://streeteasy-market-data-download.s3.amazonaws.com/rentals/All/medianAskingRent_All.zip", dl)
rents <- read_csv(unzip(dl, "medianAskingRent_All.csv")) %>%
  rename(borough = Borough)

# split rents into two, whole boro and neighborhood, and tidy

boroughRents <- rents %>% 
  filter(areaType == "borough") %>% 
  select(-areaName, -areaType) %>%
  gather(date, rent, -borough) %>%
  mutate(borough = factor(borough)) %>%
  mutate(date = parse_date_time(date, orders = "ym", tz = "America/New_York"))

hoodRents <- rents %>% 
  filter(areaType == "neighborhood") %>% 
  select(-areaType) %>% 
  arrange(borough, areaName) %>%
  gather(date, rent, -areaName, -borough) %>% 
  mutate(borough = factor(borough)) %>%
  mutate(date = parse_date_time(date, orders = "ym", tz = "America/New_York"))

# TODO: crosswalk hood names to community districts
# and determine strategy for getting one median from several
# determine strategy for filling in missing values (use boro wide?)
