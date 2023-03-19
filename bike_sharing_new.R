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
nyc <- import[[1]][3] %>% data.frame()
nyc_gbfs <- import[[1]][2] %>% data.frame()

nyc_gbfs <- nyc_gbfs %>% 
  st_as_sf(coords = c("gbfs.lon", "gbfs.lat"), crs = 4326) %>% 
  subset(select = c(gbfs.capacity, gbfs.short_name, geometry))

nyc_geom <- nyc %>% 
  subset(select = c(census.GEOID, census.geometry)) %>% 
  st_as_sf(crs = 4326)

nyc_int <- st_intersection(nyc_geom, nyc_gbfs)





# Save plot ---------------------------------------------------------------

lapply(intersect, function(city) {
  
  plot <- tm_shape(city$census) + 
    tm_fill("income")
  
  link_save <- paste0("output/", city$dict$name, ".png")
  tmap_save(plot, filename = link_save)
  
})