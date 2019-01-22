if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(RSocrata)) install.packages("RSocrata", repos = "http://cran.us.r-project.org")

##########################################
# Pull the public data sets
##########################################

# My SODA App token to avoid throttling
token <- "JNHB3MHkIFRbMn4r24dddAYAy"

# NYC 311 Service Requests from 2010 to Present API
url <- "https://data.cityofnewyork.us/resource/fhrw-4uyv.json"

# Query arguments - we only want noise complaints and don't need all columns
args <- c("where=complaint_type like '%Noise%'",
          "select=created_date, agency, complaint_type, descriptor, incident_zip, community_board, bbl, latitude, longitude, borough, open_data_channel_type")
catargs <- paste(paste("$",args, sep=""), collapse="&")
caturl <- paste(url, catargs, sep="?")

# Download the 311 data (takes a while)
dfNoise <- read.socrata(caturl, app_token = token)

# Download 2010 population by community district
url <- "https://data.cityofnewyork.us/resource/5hae-yeks.json"
args <- "$select=cd_number, borough, _2010_population AS pop2010"
caturl <- paste(url, args, sep="?")
cdPop <- read.socrata(caturl, app_token=token) %>%
  mutate(cd = paste(str_to_upper(borough), str_pad(cd_number, 2, side="left", pad="0"), sep=" ")) %>%
  select(-borough, -cd_number) %>% select(cd, pop2010)

# Download monthly median rents by neighborhood from Streeteasy
dl <- tempfile()
download.file("https://streeteasy-market-data-download.s3.amazonaws.com/rentals/All/medianAskingRent_All.zip", dl)
rents <- read_csv(unzip(dl, "medianAskingRent_All.csv")) %>%
  rename(borough = Borough)

# split rents into whole boro and neighborhood, and tidy with one row per month
boroRents <- rents %>% 
  filter(areaType == "borough") %>% 
  select(-areaName, -areaType) %>%
  mutate(borough = toupper(borough)) %>%
  gather(month, rent, -borough) %>%
  mutate(month = parse_date_time(month, orders = "ym", tz = "America/New_York")) %>%
  spread(borough,rent)

hoodRents <- rents %>% 
  filter(areaType == "neighborhood") %>% 
  select(-areaType)

rm(rents)

# crosswalk hood names to community districts
hoodCD <- read_csv("hoodCD.csv") %>%
  mutate(cd = str_c(toupper(borough), " ", str_pad(cd, 2, side="left", pad="0")))

# Assemble a reasonable benchmark rent for each district.
# We take the mean of all available neighborhood median rents in the district for that month.
# Staten island only has boro-wide data so we fill that in for its 3 districts
# In other boros we get NaN when there is no data for any component neighborhood: in those
# cases we substitute the boro-wide data for that month.

cdRents <- hoodRents %>%
  left_join(hoodCD, by = "areaName") %>%
  group_by(cd) %>%
  summarize_at(vars(starts_with("2")), funs(mean(., na.rm=TRUE))) %>%
  gather(month, rent, -cd) %>% 
  spread(cd,rent) %>%
  mutate(`STATEN ISLAND 01` = boroRents$`STATEN ISLAND`,
         `STATEN ISLAND 02` = boroRents$`STATEN ISLAND`,
         `STATEN ISLAND 03` = boroRents$`STATEN ISLAND`) %>%
  mutate_at(vars(starts_with("BRONX")), funs(if_else(is.nan(.), boroRents$BRONX, .))) %>%
  mutate_at(vars(starts_with("BROOK")), funs(if_else(is.nan(.), boroRents$BROOKLYN, .))) %>%
  mutate_at(vars(starts_with("MANHA")), funs(if_else(is.nan(.), boroRents$MANHATTAN, .))) %>%
  mutate_at(vars(starts_with("QUEEN")), funs(if_else(is.nan(.), boroRents$QUEENS, .))) %>%
  mutate(month = parse_date_time(month, orders = "ym", tz = "America/New_York")) 


  

