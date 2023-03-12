library(gbfs)
library(tidycensus)
library(tibble)
library(tidyverse)

nyc <- tibble(name = "new_york", 
              gbfs_city = "NYC",
              state = list(c("NY", "NJ")),
              county = list(list(c("New York", "Kings", "Queens", "Bronx"), 
                                 c("Hudson"))))

boston <- tibble(name = "boston",
                 gbfs_city = "bluebikes",
                 state = list("MA"),
                 county = list(c("Suffolk","Middlesex", "Essex", "Norfolk")))



df <- rbind(nyc, boston)


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

# Save plot ---------------------------------------------------------------

lapply(intersect, function(city) {
  
  plot <- tm_shape(city$census) + 
    tm_fill("income")
  
  link_save <- paste0("output/", city$dict$name, ".png")
  tmap_save(plot, filename = link_save)
  
})