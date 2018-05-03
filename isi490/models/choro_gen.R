# Load packages
library(tidyverse)
library(sf)
library(leaflet)

# Load text classification helper function and model
load(file.path("~", "Desktop", "privacy-analysis", "pa.rda"))
source(file.path("~", "Desktop", "privacy-analysis", "helper.R"))

# Load NYC neighborhoods polygon
nyc_nbhd =
  "http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson" %>%
  st_read %>%
  select(neighborhood, borough)

# Load Tweets
tweets_url =
  "http://isi.csi.cuny.edu/~paolo/download/tweets/"
tweets_files =
  tweets_url %>%
  readLines %>%
  stringr::str_extract("(?<=<a href=\")\\d+_nyc\\.txt\\.gz") %>%
  na.omit
tweets = NULL
for (i in seq(tweets_files)) {
  tweets_con =
    paste0(tweets_url, tweets_files[i]) %>%
    url(open = "r", encoding = "UTF-8") %>%
    gzcon
  tweets =
    tweets_con %>%
    jsonlite::stream_in(.) %>%
    jsonlite::flatten(.) %>%
    filter(!place.name %in% c("Bronx", "Brooklyn", "Manhattan", "New York", "Queens", "Staten Island")) %>%
    mutate(
      privacy = classify(text, pa),
      lon = unlist(lapply(place.bounding_box.coordinates, function(x) mean(x[1:4]))),
      lat = unlist(lapply(place.bounding_box.coordinates, function(x) mean(x[5:8])))
    ) %>%
    select(privacy, lon, lat) %>%
    na.omit %>%
    st_as_sf(coords = c("lon", "lat"), crs = st_crs(nyc_nbhd)$epsg) %>%
    st_join(nyc_nbhd, ., left = F) %>%
    st_set_geometry(NULL) %>% 
    group_by(neighborhood, borough, privacy) %>% 
    summarise(n = n()) %>% 
    spread(privacy, n) %>% 
    replace_na(list(nonprivate = 0, private = 0)) %>% 
    rbind(tweets, .) %>%
    group_by(neighborhood, borough) %>%
    summarize(nonprivate = sum(nonprivate), private = sum(private))
  tweets_con %>% close
}

# Generate final data
nyc_tweets =
  tweets %>%
  mutate(
    prop = nonprivate / (nonprivate + private),
    perc = paste0(round(100 * prop, 2), "%")
  ) %>%
  select(neighborhood, borough, prop, perc) %>%
  inner_join(nyc_nbhd, ., by = c("neighborhood", "borough"))

# Plot NYC neighborhoods
pal = colorNumeric("Blues", NULL)
choro =
  leaflet(nyc_tweets) %>%
    addTiles %>%
    addPolygons(
      label = ~paste0(neighborhood, ": ", perc),
      stroke = F,
      smoothFactor = 0.5,
      fillColor = ~pal(prop),
      fillOpacity = 1
    )
