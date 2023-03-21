library(gbfs)
library(tidycensus)
library(tibble)
library(tidyverse)
library(sf)
library(tmap)
library(cancensus)

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


# Import Canada -----------------------------------------------------------
#mtl
options(cancensus.cache_path = getwd()) 
options(cancensus.api_key = "CensusMapper_f3d5e9208cfd98ea52a490fd9e3d63cf")

mtl <- get_census(dataset='CA21', regions=list(CMA="24462"),
                  vectors=c("v_CA21_560"),
                  level='CT', quiet = TRUE, 
                  geo_format = 'sf', labels = 'short')

names(mtl)[names(mtl) == 'v_CA21_560'] <- "income"

mtl <- subset(mtl, select = c(GeoUID, Population, income, geometry))


bixi_stations <- get_gbfs("Bixi_MTL")
bixi_stations <- bixi_stations[["station_information"]]
mtl_gbfs <- bixi_stations %>% 
  st_as_sf(coords = c("lon","lat"), crs = 4326)


# Vancouver

van <- get_census(dataset='CA21', 
                  regions=list(CMA="59933"),
                  vectors=c("v_CA21_560"),
                  level='CT', quiet = TRUE, 
                  geo_format = 'sf', 
                  labels = 'short')

names(van)[names(van) == 'v_CA21_560'] <- "income"

van <- subset(van, select = c(GeoUID, Population, income, geometry))

van_gbfs <- get_gbfs("Mobibikes_CA_Vancouver")
van_gbfs <- van_gbfs[["station_information"]]
van_gbfs <- van_gbfs[-139, ]
van_gbfs <- van_gbfs %>% st_as_sf(coords = c("lon","lat"), crs = 4326)

# toronto
tor <- get_census(dataset='CA21', 
                  regions=list(CMA="35535"),
                  vectors=c("v_CA21_560"),
                  level='CT', quiet = TRUE, 
                  geo_format = 'sf', 
                  labels = 'short')

names(tor)[names(tor) == 'v_CA21_560'] <- "income"

tor <- subset(tor, select = c(GeoUID, Population, income, geometry))

tor_gbfs <- get_gbfs("bike_share_toronto")
tor_gbfs <- tor_gbfs[["station_information"]]
tor_gbfs <- tor_gbfs %>% st_as_sf(coords = c("lon","lat"), crs = 4326)




tm_shape(mtl) +
  tm_polygons() + 
  tm_shape(mtl_gbfs) +
  tm_dots()


mtl_quartile <- mtl %>% 
  filter(bike_proportion != Inf)


tmap_mode(mode = "plot")



tm_shape(mtl) + 
  tm_fill() +
  tm_shape(mtl) +
  tm_borders(col = "White") +
  tm_shape(mtl_quartile) +
  tm_fill(col = "bike_proportion",
          n = 10,
          style = "quantile",
          palette = "PuBuGn") 

mean(mtl_quartile$bike_proportion)
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

write.csv(nyc, "/Users/philipbligh/Downloads/nyc.csv")
         
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

write.csv(boston, "/Users/philipbligh/Downloads/boston.csv")

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

write.csv(chicago, "/Users/philipbligh/Downloads/chicago.csv")
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

write.csv(dc, "/Users/philipbligh/Downloads/dc.csv")
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

write.csv(portland, "/Users/philipbligh/Downloads/portland.csv")
# Montreal

mtl_int <- st_intersection(mtl, mtl_gbfs)

mtl_test <- mtl_int %>% group_by(GeoUID) %>% 
  summarise(sum(capacity)) %>% st_drop_geometry()

mtl <- merge(x=mtl, y=mtl_test, by ="GeoUID", all.x = TRUE)

mtl$bike_proportion <- ((mtl$`sum(capacity)`/ mtl$Population)*1000)

write.csv(mtl, "/Users/philipbligh/Downloads/mtl.csv")
# vancouver

van_int <- st_intersection(van, van_gbfs)

van_test <- van_int %>% group_by(GeoUID) %>% 
  summarise(sum(capacity)) %>% st_drop_geometry()

van <- merge(x=van, y=van_test, by ="GeoUID", all.x = TRUE)

van$bike_proportion <- ((van$`sum(capacity)`/ van$Population)*1000)

van <- van %>% subset(select = -c(`sum(capacity).x`,`sum(capacity).y`))

write.csv(van, "/Users/philipbligh/Downloads/van.csv")
# Toronto

tor_int <- st_intersection(tor, tor_gbfs)

tor_test <- tor_int %>% group_by(GeoUID) %>% 
  summarise(sum(capacity)) %>% st_drop_geometry()

tor <- merge(x=tor, y=tor_test, by ="GeoUID", all.x = TRUE)

tor$bike_proportion <- ((tor$`sum(capacity)`/ tor$Population)*1000)

write.csv(tor, "/Users/philipbligh/Downloads/toronto.csv")
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


#chicago

chicago_buffer <- st_buffer(chicago_gbfs, 300)

chicago_int <- st_intersects(chicago_buffer)

chicago_gbfs <-
  chicago_gbfs %>% 
  mutate(buffer_int = lengths(chicago_int))

count_chicago_int <- count(dc_gbfs, buffer_int)


# DC

dc_buffer <- st_buffer(dc_gbfs, 300)

dc_int <- st_intersects(dc_buffer)

dc_gbfs <-
  dc_gbfs %>% 
  mutate(buffer_int = lengths(dc_int))

count_dc_int <- count(dc_gbfs, buffer_int)

#portland

portland_buffer <- st_buffer(portland_gbfs, 300)

portland_int <- st_intersects(portland_buffer)

portland_gbfs <-
  portland_gbfs %>% 
  mutate(buffer_int = lengths(portland_int))

count_portland_int <- count(portland_gbfs, buffer_int)

# montreal

mtl_buffer <- st_buffer(mtl_gbfs, 300)

mtl_int <- st_intersects(mtl_buffer)

mtl_gbfs <-
  mtl_gbfs %>% 
  mutate(buffer_int = lengths(mtl_int))

count_mtl_int <- count(mtl_gbfs, buffer_int)




# Save plot ---------------------------------------------------------------

lapply(intersect, function(city) {
  
  plot <- tm_shape(city$census) + 
    tm_fill("bike_proportion")
  
  link_save <- paste0("output/", city$dict$name, ".png")
  tmap_save(plot, filename = link_save)
  
})