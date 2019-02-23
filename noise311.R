if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(RSocrata)) install.packages("RSocrata", repos = "http://cran.us.r-project.org")
if(!require(sf)) install.packages("sf", repos = "http://cran.us.r-project.org")
if(!require(ggmap)) install.packages("ggmap", repos = "http://cran.us.r-project.org")
if(!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org")


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

dfNoise <- dfNoise %>% mutate(latitude = as.numeric(latitude), longitude = as.numeric(longitude))

# Download 2010 population by community district
url <- "https://data.cityofnewyork.us/resource/5hae-yeks.json"
args <- "$select=cd_number, borough, _2010_population AS pop2010"
caturl <- paste(url, args, sep="?")
cdPop <- read.socrata(caturl, app_token=token) %>%
  mutate(cd = paste(str_pad(cd_number, 2, side="left", pad="0"), str_to_upper(borough), sep=" "), borough=toupper(borough)) %>%
  select(-cd_number) %>% select(cd, borough, pop2010) %>%
  mutate(pop2010 = as.numeric(pop2010))

boroPop <- cdPop %>% group_by(borough) %>% summarize(pop2010 = sum(pop2010))

cityPop <- sum(cdPop$pop2010)

# Download monthly median rents by neighborhood from Streeteasy
dl <- tempfile()
download.file("https://streeteasy-market-data-download.s3.amazonaws.com/rentals/All/medianAskingRent_All.zip", dl)
rents <- read_csv(unzip(dl, "medianAskingRent_All.csv")) %>%
  rename(borough = Borough)
rents <- read_csv("medianAskingRent_All.csv") %>%
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

cityRents <- rents %>%
  filter(areaName == "NYC") %>%
  select(-areaType, -borough)
  
# crosswalk hood names to community districts
download.file("https://raw.githubusercontent.com/martensitic/311Noise/master/hoodCD.csv", "hoodCD.csv")
hoodCD <- read_csv("hoodCD.csv") %>%
  mutate(cd = str_c(str_pad(cd, 2, side="left", pad="0"), " ", toupper(borough))) %>%
  select(-borough)

# CD boundary shapefiles from the city
dfCD <- read_sf("https://data.cityofnewyork.us/api/geospatial/yfnk-k7r4?method=export&format=GeoJSON")
boros <- c("MANHATTAN","BRONX","BROOKLYN","QUEENS","STATEN ISLAND")
dfCD <- dfCD %>% mutate(boro = factor(str_sub(boro_cd, end = 1), labels = boros),
                       cd = str_sub(boro_cd, start=2),
                       boro_cd = paste(cd, boro),
                       boro = NULL, cd = NULL)

# stop all the downloadin
rm(args, catargs, caturl, token, url, dl, boros)

#####
# Assemble a reasonable benchmark rent for each community district.
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
  mutate(`01 STATEN ISLAND` = boroRents$`STATEN ISLAND`,
         `02 STATEN ISLAND` = boroRents$`STATEN ISLAND`,
         `03 STATEN ISLAND` = boroRents$`STATEN ISLAND`) %>%
  mutate_at(vars(ends_with("BRONX")), funs(if_else(is.nan(.), boroRents$BRONX, .))) %>%
  mutate_at(vars(ends_with("BROOKLYN")), funs(if_else(is.nan(.), boroRents$BROOKLYN, .))) %>%
  mutate_at(vars(ends_with("MANHATTAN")), funs(if_else(is.nan(.), boroRents$MANHATTAN, .))) %>%
  mutate_at(vars(ends_with("QUEENS")), funs(if_else(is.nan(.), boroRents$QUEENS, .))) %>%
  mutate(month = as_date(parse_date_time(month, orders = "ym", tz = "America/New_York"))) 


## Let's look at monthly noise complaints in each district per 1000 residents
# Inner join to population table discards nonexistent districts (e.g. parks)
# FIlter out the latest partial month, only want whole months

noiseCDMonth <- dfNoise %>% 
  group_by(month = as_date(floor_date(created_date, unit="month")), cd = community_board) %>% 
  summarize(complaints = n()) %>% 
  inner_join(cdPop, by="cd") %>%
  mutate(rate = complaints / (pop2010 / 1000)) %>%
  select(-pop2010) %>% 
  left_join(gather(cdRents, cd, rent, -month), by=c("month","cd")) %>%
  filter(month < rollback(today())) %>%
  mutate(calmonth = month(month))

# Same for boros
noiseBoroMonth <- dfNoise %>%
  group_by(month = as_date(floor_date(created_date, unit="month")), borough) %>%
  summarize(complaints = n()) %>% filter(year(month) <= 2018) %>%
  inner_join(boroPop, by = "borough") %>%
  mutate(rate = complaints / (pop2010 / 1000)) %>%
  select(-pop2010) %>%
  left_join(gather(boroRents, borough, rent, -month), by=c("month","borough")) %>%
  mutate(calmonth = month(month))

# Same for city
cityRents <- gather(cityRents, month, rent)[-1:0,] %>%
  mutate(month = as_date(parse_date_time(month, orders = "ym", tz = "America/New_York")))
  
noiseNYCMonth <- dfNoise %>%
  group_by(month = as_date(floor_date(created_date, unit="month"))) %>%
  summarize(complaints = n()) %>% filter(year(month) <= 2018) %>%
  mutate(rate = complaints / (cityPop / 1000)) %>%
  left_join(cityRents, by = "month") %>%
  mutate(calmonth = month(month), rent = as.numeric(rent))
  
#####
# Some exploration plots

# monthly complaints all years stacked citywide
noiseNYCMonth %>% ggplot(aes(x = calmonth)) +
  geom_line(aes( y = rate, color = factor(year(month)))) +
  scale_x_continuous(breaks = 1:12) +
  xlab("Month") + ylab("Complaints per 1000 people") + labs(color="Year") +
  ggtitle("All NYC: Monthly Noise Complaints")

# five boros stacked, all years: noise complaints and then rents
noiseBoroMonth %>% ggplot(aes(x = month)) +
  geom_line(aes(y = rate, color = factor(borough))) +
  labs(x = "Year", y = "Complaints per 1000 people", color = "Borough") +
  ggtitle("Monthly Noise Complaints by Borough")

noiseBoroMonth %>% ggplot(aes(x = month)) +
  geom_line(aes(y = rent, color = factor(borough))) +
  labs(x = "Year", y = "Median Rent [USD]", color = "Borough") +
  ggtitle("Monthly Median Rent by Borough")


# my district time series, Brooklyn CB6
noiseCDMonth %>% filter(cd == "06 BROOKLYN") %>% ggplot(aes(x = calmonth)) +
  geom_line(aes( y = rate, color = factor(year(month)))) +
  scale_x_continuous(breaks = 1:12) +
  xlab("Month") + ylab("Complaints per 1000 people") + labs(color="Year") +
  ggtitle("Brooklyn CB6: Monthly Noise Complaints")

# scatter rate vs rent for all 5 boroughs stacked
noiseCDMonth %>% ggplot(aes(x = rent, y = rate, color = factor(cd))) + 
  geom_point() +
  facet_grid(rows = vars(factor(borough))) +
  xlab("Median Rent [USD]") + ylab("Complaints per 1000 people") + 
  theme(legend.position="none") +
  ggtitle("Monthly Complaints vs. Median Rent")

# Manhattan only
noiseCDMonth %>% filter(borough == "MANHATTAN") %>% 
  ggplot(aes(x = rent, y = rate, color = factor(cd))) + 
  geom_point() + xlab("Median Rent [USD]") + ylab("Complaints per 1000 people") + 
  labs(color="District") +
  ggtitle("Manhattan: Monthly Noise Complaints vs. Median Rent")

# 5 Boroughs
noiseBoroMonth %>% ggplot(aes(x=rent, y = rate, color=borough)) + geom_point() + 
  xlab("Median Rent [USD]") + ylab("Complaints per 1000 people") + 
  labs(color="Borough") +
  ggtitle("Monthly Noise Complaints vs. Median Rent")

# Look at distribution of agencies and complaints
agencies <- dfNoise %>% group_by(agency) %>% 
  summarize(num=n()) %>% arrange(desc(num)) %>% 
  mutate(frac = num / sum(num))

types <- dfNoise %>% group_by(complaint_type) %>% 
  summarize(num=n()) %>% arrange(desc(num)) %>% 
  mutate(frac = num / sum(num))

descs <- dfNoise %>% group_by(descriptor) %>% 
  summarize(num=n()) %>% arrange(desc(num)) %>% 
  mutate(frac = num / sum(num))

channels <- dfNoise  %>% mutate(channel = factor(open_data_channel_type)) %>%
  group_by(channel) %>% summarize(num = n()) %>% arrange(desc(num)) %>% 
  mutate(frac = num / sum(num))

# use of the channels over time
noiseChannelMonth <- dfNoise %>% 
  group_by(month = as_date(floor_date(created_date, unit="month")), 
           channel = factor(open_data_channel_type)) %>% 
  summarize(complaints = n())

noiseChannelMonth %>% filter(channel != "OTHER") %>%
  ggplot(aes(month, complaints/1000, color=channel)) + geom_line() +
  ggtitle("Monthly Complaints by Channel") + xlab("Year") + ylab("Monthly Complaints / 1000")


#####
# MAPS!

  
# West, South, East, North extents of NYC map
nycExtents <- c( -74.257159, 40.495992, -73.699215,  40.915568)

# Load ggmap tiles for plotting over
nyc <- get_stamenmap(bbox = nycExtents, 
                     zoom = 11, 
                     maptype = "toner-lite", 
                     crop = TRUE)

# scatter heatmap in 2015
map2015 <- ggmap(nyc, darken = 0.6) + 
  stat_density_2d(data = dfNoise %>% 
                    filter(year(created_date)==2015) %>%
                    select(latitude, longitude) %>%
                    na.omit(), 
                  aes(x = longitude, y = latitude, fill = ..level..), 
                  geom="polygon", 
                  alpha = .3, 
                  color=NA) +
  scale_fill_gradient2("Complaint\nDensity", low="dimgrey",mid="yellow",high="red",
                       limits = c(0,100), midpoint = 50) +
  ggtitle("Noise Complaints (2015)") + 
  labs(x = NULL, y = NULL) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

# scatter heatmap in 2018
map2018 <- ggmap(nyc, darken = 0.6) + 
  stat_density_2d(data = dfNoise %>% 
                    filter(year(created_date)==2018) %>%
                    select(latitude, longitude) %>%
                    na.omit(), 
                  aes(x = longitude, y = latitude, fill = ..level..), 
                  geom="polygon", 
                  alpha = .3, 
                  color=NA) +
  scale_fill_gradient2("Complaint\nDensity", low="dimgrey",mid="yellow",high="red",
                       limits = c(0,100), midpoint = 50) +
  ggtitle("Noise Complaints (2018)") + 
  labs(x = NULL, y = NULL) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

# an alternate background for the last map, on greyed out CD blocks
map2018alt<- dfNoise %>% 
  filter(year(created_date)==2018) %>%
  select(latitude, longitude) %>%
  na.omit() %>%
  ggplot() + geom_sf(data = dfCD, fill = "dimgrey") +
  coord_sf(label_graticule = "----", label_axes = "----") +
  stat_density_2d(aes(x = longitude, y = latitude, fill = ..level..), geom="polygon", alpha = .3, color=NA) +
  scale_fill_gradient2("Complaint\nDensity", low="white",mid="yellow",high="red",
                       limits = c(0,100), midpoint = 50) +
  ggtitle("Noise Complaints (2018)") + labs(x = NULL, y = NULL)

# 2018 complaint rate in each CD
cd2018 <- dfNoise %>% 
  filter(year(created_date) == 2018) %>%
  group_by(cd = community_board) %>% 
  summarize(complaints = n()) %>% 
  inner_join(cdPop, by="cd") %>%
  mutate(rate = complaints / (pop2010 / 1000)) %>%
  select(-pop2010)

# map of this CD rate data for 2018
cdMap2018 <- ggmap(nyc, darken = 0.6) + 
  geom_sf(data = dfCD %>% 
            left_join(cd2018, 
                      by = c("boro_cd" = "cd")) %>% 
            na.omit(), 
          inherit.aes = FALSE,
          alpha = 0.4, 
          color = NA,
          aes(fill = rate)) +
  coord_sf(label_graticule = "----", label_axes = "----") +
  scale_fill_gradient2("Complaints\nper 1000", low="dimgrey",mid="yellow",high="red", midpoint = 75) +
  ggtitle("Noise Complaints (2018)") + labs(x = NULL, y = NULL)
  
# drill down on this weird queens thing
top2 <- dfNoise %>% filter(year(created_date) == 2018 & community_board == "08 QUEENS") %>% 
  group_by(latitude, longitude) %>% 
  summarize(Complaints = n()) %>% 
  arrange(desc(Complaints)) %>%
  mutate(`% of Total` = Complaints / sum(.$Complaints)) %>% .[1:2,]


#this is a firehouse
dfNoise %>% filter(latitude == top2$latitude[1] & longitude == top2$longitude[1] & year(created_date) == 2018) %>% 
  group_by(descriptor) %>% summarize(num = n())

#this is a religious building
dfNoise %>% filter(latitude == top2$latitude[2] & longitude == top2$longitude[2] & year(created_date) == 2018) %>% 
  group_by(descriptor) %>% summarize(num = n())



# What  are the districts with the most/least complaints at an affordable rent?
# Let's take a single year, and look at the peak complaint rate and peak rent for the year

peaks2018 <- noiseCDMonth %>% 
  filter(year(month)==2018) %>% 
  group_by(cd) %>%
  summarize(rent = max(rent),
            rate = max(rate),
            borough = borough[1],
            district = str_sub(cd[1], 1, 2)) %>%
  select(borough, district, rent, rate, -cd)

# color blind friendly palette
cbpal <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
    "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

scatter2018 <- peaks2018 %>% ggplot(aes(x = rent, y = rate)) +
  geom_point(color = "black") +
  geom_label_repel(aes(label = district, fill = borough), 
             label.padding = unit(.15,"lines"),
             color = "white",segment.color = "black") +
  scale_fill_manual(values = cbpal[-1], name = "Community\nDistrict #") +
  labs(x = "Median Rent [USD]", 
       y = "Noise Complaints per 1000 People", 
       title = "Peak Monthly Noise Complaints & Median Rent, 2018")

peaks2015 <- noiseCDMonth %>% 
  filter(year(month)==2015) %>% 
  group_by(cd) %>%
  summarize(rent = max(rent),
            rate = max(rate),
            borough = borough[1],
            district = str_sub(cd[1], 1, 2)) %>%
  select(borough, district, rent, rate, -cd)

scatter2015 <- peaks2015 %>% ggplot(aes(x = rent, y = rate)) +
  geom_point(color = "black") +
  geom_label_repel(aes(label = district, fill = borough), 
                   label.padding = unit(.15,"lines"),
                   color = "white",segment.color = "black") +
  scale_fill_manual(values = cbpal[-1], name = "Community\nDistrict #") +
  labs(x = "Median Rent [USD]", 
       y = "Noise Complaints per 1000 People", 
       title = "Peak Monthly Noise Complaints & Median Rent, 2015")



cdMap <- ggmap(nyc, darken = 0.6) + 
  geom_sf(data = dfCD %>% 
            mutate(borough = str_sub(boro_cd, start = 3),
                   district = str_sub(boro_cd, end = 2)) %>% 
            filter(district < 20),
          inherit.aes = FALSE,
          alpha = 0.6, 
          color = "white",
          aes(fill = borough)) +
  coord_sf(label_graticule = "----", label_axes = "----") +
  scale_fill_manual(values = cbpal[-1], name = "Community\nDistrict #") +
  geom_sf_text(data = dfCD %>% 
                 mutate(district = str_sub(boro_cd, end = 2)) %>% 
                 filter(district < 20),
                           aes(label = district),
               inherit.aes = FALSE,
               color = "white") +
  ggtitle("NYC Community Districts") + labs(x = NULL, y = NULL)
