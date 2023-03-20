library(gbfs)
library(tidycensus)
library(tibble)
library(tidyverse)
library(sf)
library(tmap)

nyc <- tibble(name = "new_york", 
              gbfs_city = "NYC",
              state = list(c("NY", "NJ")),
              county = list(list(c("New York", "Kings", "Queens", "Bronx"), 
                                 c("Hudson"))))

boston <- tibble(name = "boston",
                 gbfs_city = "bluebikes",
                 state = list("MA"),
                 county = list(c("Suffolk","Middlesex", "Essex", "Norfolk")))

chicago <- tibble(name = "chicago",
                  gbfs_city = "Divvy", 
                  state = list("Il"), 
                  county = list("Cook"))

dc <- tibble(name = "dc", 
             gbfs_city = "cabi", 
             state = list(c("DC", "MD", "VA")), 
             county = list(list((NULL), 
                                c("Prince George's", 
                                  "Montgomery"),
                                c("Arlington", 
                                  "Alexandria", 
                                  "Falls Church", 
                                  "Fairfax County", 
                                  "Fairfax City"))))

philadelphia <- tibble(name = "philadelphia",
                       gbfs_city = "bcycle_indego", 
                       state = list("PA"), 
                       county = list("Philadelphia"))

portland <- tibble(name = "portland", 
                   gbfs_city = "biketown_pdx", 
                   state = list("OR"), 
                   county = list("Multnomah"))

df <- rbind(nyc, boston, chicago, dc, philadelphia, portland)




# Import ------------------------------------------------------------------


iterations <- seq_len(nrow(df))

import <- sapply(iterations, function(ind) {
  
  dict <- df[ind, ]
  gbfs <- get_gbfs(city = dict$gbfs_city)[["station_information"]]
  census_iterations <- seq_len(
    length(
      unlist(dict$state)
    )
  )
  census <- lapply(census_iterations, \(iteration) {
    
    state <- unlist(dict$state)[iteration]
    county <- dict$county[[1]][[iteration]]
    
    get_acs(
      state = state,
      county = county,
      geography = "tract",
      variables = c(population = "B01001_001", 
                    income = "B06011_001", 
                    white = "B02001_002",
                    black = "B02001_003"),
      geometry = TRUE,
      year = 2021)
  })
  
  census <- Reduce(rbind, census)
  census <- census[names(census) != "moe"]
  census <- spread(census, variable, estimate)
  
  return(list(dict = dict, 
              gbfs = as_tibble(gbfs),
              census = census))
}, simplify = FALSE, USE.NAMES = TRUE)


# Intersection ------------------------------------------------------------

intersect <- lapply(import, function(city) {
  intersections <- sf::st_intersection(city$gbfs, city$census)
  
  return(list(dict = city$dict, 
              gbfs = city$gbfs,
              census = city$census,
              intersections = intersections))
})



# NYC
nyc <- import[[1]][3] %>% data.frame() %>% 
  st_as_sf(crs = 4326)
nyc_gbfs <- import[[1]][2] %>% data.frame()

nyc_gbfs <- nyc_gbfs %>% 
  st_as_sf(coords = c("gbfs.lon", "gbfs.lat"), crs = 4326) %>% 
  subset(select = c(gbfs.capacity, gbfs.short_name, geometry))

nyc_int <- st_intersection(nyc, nyc_gbfs)

# Boston
boston <- import[[2]][3] %>% data.frame() %>% 
  st_as_sf(crs = 4326)
bos_gbfs <- import[[2]][2] %>% data.frame()

bos_gbfs <- bos_gbfs %>% 
  st_as_sf(coords = c("gbfs.lon", "gbfs.lat"), crs = 4326) %>% 
  subset(select = c(gbfs.capacity, gbfs.short_name, geometry))

bos_int <- st_intersection(boston, bos_gbfs)

bos_test <- bos_int %>% group_by(census.GEOID) %>% 
  summarise(sum(gbfs.capacity))

test2 <- bos_test %>% 
  st_drop_geometry()

boston <- merge(x=boston, y=test2, by ="census.GEOID", all.x = TRUE)

boston$bike_proportion <- ((boston$`sum(gbfs.capacity)`/ boston$census.population)*1000)

# Chicago
chicago <- import[[3]][3] %>% data.frame() %>% 
  st_as_sf(crs = 4326)
chicago_gbfs <- import[[3]][2] %>% data.frame()

chicago_gbfs <- chicago_gbfs %>% 
  st_as_sf(coords = c("gbfs.lon", "gbfs.lat"), crs = 4326) %>% 
  subset(select = c(gbfs.capacity, gbfs.short_name, geometry))

chicago_int <- st_intersection(chicago, chicago_gbfs)

chicago_test <- chicago_int %>% group_by(census.GEOID) %>% 
  summarise(sum(gbfs.capacity)) %>% st_drop_geometry()

test2 <- chicago_test %>% 
  st_drop_geometry()

chicago <- merge(x=chicago, y=test2, by ="census.GEOID", all.x = TRUE)

chicago$bike_proportion <- ((chicago$`sum(gbfs.capacity)`/ chicago$census.population)*1000)

# DC

dc <- import[[4]][3] %>% data.frame() %>% 
  st_as_sf(crs = 4326)
dc_gbfs <- import[[4]][2] %>% data.frame()

dc_gbfs <- dc_gbfs %>% 
  st_as_sf(coords = c("gbfs.lon", "gbfs.lat"), crs = 4326) %>% 
  subset(select = c(gbfs.capacity, gbfs.short_name, geometry))

dc_int <- st_intersection(dc, dc_gbfs)

dc_test <- dc_int %>% group_by(census.GEOID) %>% 
  summarise(sum(gbfs.capacity)) %>% st_drop_geometry()

dc <- merge(x=dc, y=dc_test, by ="census.GEOID", all.x = TRUE)

dc$bike_proportion <- ((dc$`sum(gbfs.capacity)`/ dc$census.population)*1000)

dc <- dc %>% subset(select = -c(`sum(gbfs.capacity).x`,`sum(gbfs.capacity).y`))

# Philly
philly <- import[[5]][3] %>% data.frame() %>% 
  st_as_sf(crs = 4326)
philly_gbfs <- import[[5]][2] %>% data.frame()

# No capacity? will come back to later
philly_gbfs <- philly_gbfs %>% 
  st_as_sf(coords = c("gbfs.lon", "gbfs.lat"), crs = 4326) %>% 
  subset(select = c(gbfs.capacity, gbfs.short_name, geometry))


# portland

portland <- import[[6]][3] %>% data.frame() %>% 
  st_as_sf(crs = 4326)
portland_gbfs <- import[[6]][2] %>% data.frame()

portland_gbfs <- portland_gbfs %>% 
  st_as_sf(coords = c("gbfs.lon", "gbfs.lat"), crs = 4326) %>% 
  subset(select = c(gbfs.capacity, gbfs.station_id, geometry))

portland_int <- st_intersection(portland, portland_gbfs)

portland_test <- portland_int %>% group_by(census.GEOID) %>% 
  summarise(sum(gbfs.capacity)) %>% st_drop_geometry()

portland <- merge(x=portland, y=portland_test, by ="census.GEOID", all.x = TRUE)

portland$bike_proportion <- ((portland$`sum(gbfs.capacity)`/ portland$census.population)*1000)



# buffer ------------------------------------------------------------------

#nyc 
nyc_buffer <- st_buffer(nyc_gbfs, 300)

nyc_int <- st_intersects(nyc_buffer)

nyc_gbfs <-
  nyc_gbfs %>% 
  mutate(buffer_int = lengths(nyc_int))

count_nyc_int <- count(nyc_gbfs, buffer_int)

#boston

bos_buffer <- st_buffer(bos_gbfs, 300)

bos_int <- st_intersects(bos_buffer)

bos_gbfs <-
  bos_gbfs %>% 
  mutate(buffer_int = lengths(bos_int))

count_bos_int <- count(bos_gbfs, buffer_int)


# Save plot ---------------------------------------------------------------

lapply(intersect, function(city) {
  
  plot <- tm_shape(city$census) + 
    tm_fill("bike_proportion")
  
  link_save <- paste0("output/", city$dict$name, ".png")
  tmap_save(plot, filename = link_save)
  
})