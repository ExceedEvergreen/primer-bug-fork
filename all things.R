# Horrendously formatted and buggy code

# First exercise is to break this up into the different files.

# Then we debug it

# And finally we style according to style guide.

# Load data, planning on weather.
read_csv("file.csv")

library(tidyverse)
library(ggplot2)
library(rnoaa)
library(lubridate)
library(readxl)
library(maps)
library(mapdata)
library(sf)
library(scales)

# Get Data from the NOAA using API
# https://www.weather.gov/documentation/services-web-api

# All other sources for things like the county-zip and the shape files can be
# found here:
# https://www.huduser.gov/portal/datasets/usps_crosswalk.html
# https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html

# To learn more about using the maptools library to create maps, see here:
# https://www.molecularecologist.com/2012/09/18/making-maps-with-r/

# Project goal: Plug in a state and get the precipitation data for each
# latitude/longitude point. Then, map the latitude and longitude points on a map
# of the state. Finally, add circles to represent the range of the radar.

# Step 1: Plug in a county and get the zip codes within that county

# We'll be doing this by finding your county FIPS code. It's essentially an ID
# for your county. Look up the FIPS code on the internet and look it up online.
# This is the FIPS code for Cook County, IL
your_county_name <- "Cook County"
your_county_zip <- 17031

# These are all of the zip codes within your county but in a vector
county_to_zip <- read_excel("COUNTY_ZIP_062021.xlsx") %>%
  filter(county == your_county_zip) %>%
  select(zip)
num_zips <- nrow(county_to_zip)

# Cool print statements, the 3.35 is the average number of stations within each
# zip code and each station would take about a second total to graph.
print(paste0(your_county_name, " has ", num_zips,
             " zip codes located within it. It will take at least ",
             num_zips*3.35, " seconds to finish creating the map."))
print("I'd suggest doing something else because this is going to take a while.")

# Step 2: Get all of the stations within that county by doing a bunch of ZIP
# code requests. This will take a while due to the API limit.

# Initialize starting variables
stations <- tibble()

# Hide the warnings
options(warn = -1)
index <- 1

# Loop through the zip codes and get the values, then combine them together
for(i in county_to_zip$zip){
  # Get the API data and check if it contains anything
  station_list <- ncdc_stations(datasetid  = "GHCND",
                                datatypeid = "PRCP",
                                locationid = paste0("ZIP:", i),
                                limit      = 1000,
                                token      = Sys.getenv("NOAA_API_KEY"))$data
  index <- index + 1
  if(is.null(station_list) | nrow(station_list) == 0){
    next
  }
  
  # Get the columns about the elevation and add the zip code
  station_list <- station_list %>%
    select(-elevationUnit, -elevation) %>%
    mutate(zipcode = i)
  
  # Nice print statement to show that we're doing things in the background
  print(paste0("Zip Code ", i, " has ", nrow(station_list), " station(s). ",
               scales::label_percent()(index/nrow(county_to_zip)), " done."))
  
  # Take the station names that we got and combine them with the current table
  stations <- rbind(stations, as_tibble(station_list))
  
  # Avoid the API limits by doing 4 requests every second
  Sys.sleep(0.25)
}

# Remove any duplicates
stations <- stations %>%
  unique() %>%
  filter(ymd(maxdate) - ymd(mindate) >= 365)

# Cool print statement
print(paste0(your_county_name, " has ", nrow(stations),
             " NCDC weather stations."))

# Step 2: Get the precipitation data in tenths of a millimeter for each station
# and add it on to the station data

prcp_data <- tibble()

for(i in 1:nrow(stations)){
  station <- stations %>% slice(i)
  st_data <- ncdc(datasetid  = "GHCND",
                  datatypeid = "PRCP",
                  stationid  = station$id,
                  startdate  = ymd(station$maxdate) - years(1),
                  enddate    = ymd(station$maxdate),
                  limit      = 366,
                  token      = Sys.getenv("NOAA_API_KEY"))$data
  prcp_data <- rbind(prcp_data, as_tibble(st_data))
  
  # Nice print statement to show that we're doing things in the background
  print(paste0("Station ", station$id, " has ", nrow(st_data), " entries(s). ",
               label_percent()(i/nrow(stations)), " done."))
}

# Turn on warnings
options(warn = 0)

# Step 3: Clean up the data and put it all into one place

# Clean the precipitation data
prcp <- prcp_data %>%
  select(date, station, value) %>%
  rename(id = station)

# Join it with the stations data
prcp_clean <- inner_join(prcp, stations, by = "id")

# Step 4: Create a map for the data

pcontorta <- sf::st_read(dsn = "cb_2018_us_cd116_20m.shp")
?readOGR












