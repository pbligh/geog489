library(gbfs)
library(tidycensus)
library(tibble)
library(tidyverse)
library(sf)
library(tmap)
library(cancensus)
library(RColorBrewer)
library(ggplot2)

nyc <- tibble(name = "new_york", 
              gbfs_city = "NYC",
              state = list(c("NY", "NJ")),
              county = list(list(c("New York", "Kings", "Queens", "Bronx"), 
                                 c("Hudson"))))

boston <- tibble(name = "boston",
                 gbfs_city = "bluebikes",
                 state = list("MA"),
                 county = list(c("Suffolk",
                                 "Middlesex", 
                                 "Essex", 
                                 "Norfolk")))

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


# for some reason, Boston didn't work as intended, so here it is below

boston <- get_acs(
  state = "MA",
  county = c("Suffolk","Middlesex", "Essex", "Norfolk"),
  geography = "tract",
  variables = c(population = "B01001_001", 
                income = "B06011_001", 
                white = "B02001_002",
                black = "B02001_003"),
  geometry = TRUE,
  year = 2021)

boston <- st_transform(boston, crs = 4326)

boston <- boston %>% 
  subset(select = -c(moe)) %>% 
  spread(variable, estimate)

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

nyc_test <- nyc_int %>% group_by(census.GEOID) %>% 
  summarise(sum(gbfs.capacity)) %>% 
  st_drop_geometry()

test2 <- nyc_test %>% 
  st_drop_geometry()

nyc <- merge(x=nyc, y=test2, by ="census.GEOID", all.x = TRUE)

nyc$bike_proportion <- ((nyc$`sum(gbfs.capacity)`/ nyc$census.population)*1000)


write.csv(nyc, "/Users/philipbligh/Downloads/nyc.csv")
         
# Boston
boston <- import[[2]][3] %>% data.frame() %>% 
  st_as_sf(crs = 4326)
bos_gbfs <- import[[2]][2] %>% data.frame()

bos_gbfs <- bos_gbfs %>% 
  st_as_sf(coords = c("gbfs.lon", "gbfs.lat"), crs = 4326) %>% 
  subset(select = c(gbfs.capacity, gbfs.short_name, geometry))

bos_int <- st_intersection(boston, bos_gbfs)

bos_test <- bos_int %>% group_by(GEOID) %>% 
  summarise(sum(gbfs.capacity)) %>% 
  st_drop_geometry()

boston <- merge(x=boston, y=bos_test, by ="GEOID", all.x = TRUE)

boston$bike_proportion <- ((boston$`sum(gbfs.capacity)`/ boston$population)*1000)

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

count_chicago_int <- count(chicago_gbfs, buffer_int)


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

# vancouver

van_buffer <- st_buffer(van_gbfs, 300)

van_int <- st_intersects(van_buffer)

van_gbfs <-
  van_gbfs %>% 
  mutate(buffer_int = lengths(van_int))

count_van_int <- count(van_gbfs, buffer_int)

#toronto

tor_buffer <- st_buffer(tor_gbfs, 300)

tor_int <- st_intersects(tor_buffer)

tor_gbfs <-
  tor_gbfs %>% 
  mutate(buffer_int = lengths(tor_int))

count_tor_int <- count(tor_gbfs, buffer_int)




# coverage areas ----------------------------------------------------------

#nyc
# combining the buffers into one shape
nyc_area <- st_union(nyc_buffer)

#fixing errors with the union
tmap_options(check.and.fix = TRUE)
nyc_area <- st_cast(nyc_area, "POLYGON") %>% 
  st_make_valid(nyc_area)

#adding column for area of each ct
nyc <- nyc %>% 
  mutate(area = st_area(geometry))

nyc_pop <- st_intersection(nyc, nyc_area)

#finding area of the new cts, now that some have been cut off
nyc_pop <- nyc_pop %>% 
  mutate(area_inter = st_area(geometry)) %>% 
  mutate(weight = area_inter/area)

# multiplying weight by population
nyc_pop <- nyc_pop %>% mutate(true_population = census.population*weight)
sum(nyc_pop$true_population)
sum(nyc_pop$area_inter)
# population = 3876730
# area = 189284837 m^2


# Boston
bos_area <- st_union(bos_buffer)

tmap_options(check.and.fix = TRUE)
bos_area <- st_cast(bos_area, "POLYGON") %>% 
  st_make_valid(bos_area)

boston <- boston %>% 
  mutate(area = st_area(geometry))

bos_pop <- st_intersection(boston, bos_area)

bos_pop <- bos_pop %>% 
  mutate(area_inter = st_area(geometry)) %>% 
  mutate(weight = area_inter/area)

bos_pop <- bos_pop %>% mutate(true_population = census.population*weight)
sum(bos_pop$true_population)
sum(bos_pop$area_inter)
# population = 395763.7
# area = 52377017

# Chicago
chicago_area <- st_union(chicago_buffer)

tmap_options(check.and.fix = TRUE)
chicago_area <- st_cast(chicago_area, "POLYGON") %>% 
  st_make_valid(chicago_area)

chicago <- chicago %>% 
  mutate(area = st_area(geometry))

chicago_pop <- st_intersection(chicago, chicago_area)

chicago_pop <- chicago_pop %>% 
  mutate(area_inter = st_area(geometry)) %>% 
  mutate(weight = area_inter/area)

chicago_pop <- chicago_pop %>% mutate(true_population = census.population*weight)
sum(chicago_pop$true_population)
sum(chicago_pop$area_inter)
# population = 1624476
# area = 266920933 

# dc
dc_area <- st_union(dc_buffer)

tmap_options(check.and.fix = TRUE)
dc_area <- st_cast(dc_area, "POLYGON") %>% 
  st_make_valid(dc_area)

dc <- dc %>% 
  mutate(area = st_area(geometry))

dc_pop <- st_intersection(dc, dc_area)

dc_pop <- dc_pop %>% 
  mutate(area_inter = st_area(geometry)) %>% 
  mutate(weight = area_inter/area)

dc_pop <- dc_pop %>% mutate(true_population = census.population*weight)
sum(dc_pop$true_population)
sum(dc_pop$area_inter)
# population = 691550.2
# area = 142986250

# Portland
portland_area <- st_union(portland_buffer)

tmap_options(check.and.fix = TRUE)
portland_area <- st_cast(portland_area, "POLYGON") %>% 
  st_make_valid(portland_area)

portland <- portland %>% 
  mutate(area = st_area(geometry))

portland_pop <- st_intersection(portland, portland_area)

portland_pop <- portland_pop %>% 
  mutate(area_inter = st_area(geometry)) %>% 
  mutate(weight = area_inter/area)

portland_pop <- portland_pop %>% mutate(true_population = census.population*weight)
sum(portland_pop$true_population)
sum(portland_pop$area_inter)
# population = 164312.3
# area = 46889588

# mtl
mtl_area <- st_union(mtl_buffer)

tmap_options(check.and.fix = TRUE)
mtl_area <- st_cast(mtl_area, "POLYGON") %>% 
  st_make_valid(mtl_area)

mtl <- mtl %>% 
  mutate(area = st_area(geometry))

mtl_pop <- st_intersection(mtl, mtl_area)

mtl_pop <- mtl_pop %>% 
  mutate(area_inter = st_area(geometry)) %>% 
  mutate(weight = area_inter/area)

mtl_pop <- mtl_pop %>% mutate(true_population = Population*weight)
sum(mtl_pop$true_population)
sum(mtl_pop$area_inter)
# population = 945785.8
# area = 117295810

# vancouver
van_area <- st_union(van_buffer)

tmap_options(check.and.fix = TRUE)
van_area <- st_cast(van_area, "POLYGON") %>% 
  st_make_valid(van_area)

van <- van %>% 
  mutate(area = st_area(geometry))

van_pop <- st_intersection(van, van_area)

van_pop <- van_pop %>% 
  mutate(area_inter = st_area(geometry)) %>% 
  mutate(weight = area_inter/area)

van_pop <- van_pop %>% mutate(true_population = Population*weight)
sum(van_pop$true_population)
sum(van_pop$area_inter)
# population = 270528
# area = 29056465

# toronto

tor_area <- st_union(tor_buffer)

tmap_options(check.and.fix = TRUE)
tor_area <- st_cast(tor_area, "POLYGON") %>% 
  st_make_valid(tor_area)

tor <- tor %>% 
  mutate(area = st_area(geometry))

tor_pop <- st_intersection(tor, tor_area)

tor_pop <- tor_pop %>% 
  mutate(area_inter = st_area(geometry)) %>% 
  mutate(weight = area_inter/area)

tor_pop <- tor_pop %>% mutate(true_population = Population*weight)
sum(tor_pop$true_population)
sum(tor_pop$area_inter)
# population = 823294.8
# area = 92141487


# bounding boxes for maps -------------------------------------------------
# use to plug in values, and save below
bbox_new <- st_bbox(tor_quartile) # current bounding box

xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
yrange <- bbox_new$ymax - bbox_new$ymin # range of y values

bbox_new[1] <- bbox_new[1] - (0.1 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.25 * xrange) # xmax - right
bbox_new[2] <- bbox_new[2] - (0.1 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.2 * yrange) # ymax - top


bbox_nyc <- bbox_new
bbox_boston <- bbox_new
bbox_chicago <- bbox_new
bbox_dc <- bbox_new
bbox_portland <- bbox_new
bbox_mtl <- bbox_new
bbox_van <- bbox_new
bbox_tor <- bbox_new


# Maps --------------------------------------------------------------------
dev.off()

#nyc
tmap_mode(mode = "plot")
nyc_quartile <- nyc %>% 
  filter(bike_proportion != Inf)

nyc_map <- tm_shape(nyc, 
                    bbox = nyc_quartile) + 
  tm_fill() +
  tm_shape(nyc) +
  tm_borders(col = "White") +
  tm_shape(nyc_quartile) +
  tm_polygons(col = "bike_proportion",
              title = "Bikes per Residents",
          style = "fixed", 
          breaks = c(0, 5, 10, 15, 20, 25, 30, Inf),
          palette = "YlGn") +
  tm_layout(title = "Bikes per 1000 Residents", 
            legend.title.size = .9) +
  tm_compass() +
  tm_scale_bar()
nyc_map

#boston

boston_quartile <- boston %>% 
  filter(bike_proportion != Inf)

boston_map <- tm_shape(boston,
                       bbox = boston_quartile) + 
  tm_fill() +
  tm_shape(boston) +
  tm_borders(col = "White") +
  tm_shape(boston_quartile) +
  tm_polygons(col = "bike_proportion",
              title = "Bikes per Residents",
          style = "fixed", 
          breaks = c(0, 5, 10, 15, 20, 25, 30, Inf),
          palette = "YlGn") +
  tm_layout(title = "Bikes per 1000 Residents",
            legend.position = c("left", "bottom"),
            legend.title.size = .9) +
  tm_compass() +
  tm_scale_bar()

boston_map

#chicago

chicago_quartile <- chicago %>% 
  filter(bike_proportion != Inf)
chicago <- chicago[-1332, ]

chicago_map <- tm_shape(chicago, 
                        bbox = bbox_chicago) + 
  tm_fill() +
  tm_shape(chicago) +
  tm_borders(col = "White") +
  tm_shape(chicago_quartile) +
  tm_polygons(col = "bike_proportion",
              title = "Bikes per Residents",
          style = "fixed", 
          breaks = c(0, 5, 10, 15, 20, 25, 30, Inf),
          palette = "YlGn") +
  tm_layout(legend.position = c("left", "bottom"),
    title = "Bikes per 1000 Residents", 
    legend.title.size = .9) +
  tm_compass() +
  tm_scale_bar()

chicago_map

#dc 
dc_quartile <- dc %>% 
  filter(bike_proportion != Inf)

dc_map <- tm_shape(dc, 
                   bbox = bbox_dc) + 
  tm_fill() +
  tm_shape(dc) +
  tm_borders(col = "White") +
  tm_shape(dc_quartile) +
  tm_polygons(col = "bike_proportion",
              title = "Bikes per Residents",
          style = "fixed", 
          breaks = c(0, 5, 10, 15, 20, 25, 30, Inf),
          palette = "YlGn") +
  tm_layout(legend.position = c("left", "bottom"), 
            title = "Bikes per 1000 Residents", 
            legend.title.size = .9) +
  tm_compass() +
  tm_scale_bar()

dc_map

# portland

portland_quartile <- portland %>% 
  filter(bike_proportion != Inf)

portland_map <- tm_shape(portland, 
                         bbox = bbox_portland) + 
  tm_fill() +
  tm_shape(portland) +
  tm_borders(col = "White") +
  tm_shape(portland_quartile) +
  tm_polygons(col = "bike_proportion",
              title = "Bikes per Residents",
          style = "fixed", 
          breaks = c(0, 5, 10, 15, 20, 25, 30, Inf),
          palette = "YlGn") +
  tm_layout(legend.position = c("left", "bottom"),
            title = "Bikes per 1000 Residents", 
            legend.title.size = .9) +
  tm_compass() +
  tm_scale_bar()

portland_map

#mtl

mtl_quartile <- mtl %>% 
  filter(bike_proportion != Inf)

mtl_map <- tm_shape(mtl, 
                    bbox = bbox_mtl) + 
  tm_fill() +
  tm_shape(mtl) +
  tm_borders(col = "White") +
  tm_shape(mtl_quartile) +
  tm_polygons(col = "bike_proportion",
              title = "Bikes per Residents",
          style = "fixed", 
          breaks = c(0, 5, 10, 15, 20, 25, 30, Inf),
          palette = "YlGn") +
  tm_layout(title = "Bikes per 1000 Residents", 
            legend.title.size = .9) +
  tm_compass() +
  tm_scale_bar()
mtl_map

# vancouver

van_quartile <- van %>% 
  filter(bike_proportion != Inf)

van_map <- tm_shape(van, 
                    bbox = bbox_van) + 
  tm_fill() +
  tm_shape(van) +
  tm_borders(col = "White") +
  tm_shape(van_quartile) +
  tm_polygons(col = "bike_proportion",
              title = "Bikes per Residents",
          style = "fixed", 
          breaks = c(0, 5, 10, 15, 20, 25, 30, Inf),
          palette = "YlGn") +
  tm_layout(title = "Bikes per 1000 Residents", 
            legend.title.size = .9) +
  tm_compass() +
  tm_scale_bar()

van_map

# toronto 

tor_quartile <- tor %>% 
  filter(bike_proportion != Inf)

toronto_map <- tm_shape(tor, 
                        bbox = bbox_tor) + 
  tm_fill() +
  tm_shape(tor) +
  tm_borders(col = "White") +
  tm_shape(tor_quartile) +
  tm_polygons(col = "bike_proportion",
              title = "Bikes per Residents",
          style = "fixed", 
          breaks = c(0, 5, 10, 15, 20, 25, 30, Inf),
          palette = "YlGn") +
  tm_layout(title = "Bikes per 1000 Residents",
            legend.title.size = .9, 
            legend.position = c("left", "top")) +
  tm_compass() +
  tm_scale_bar()

toronto_map


# All maps ----------------------------------------------------------------


all_maps <- tmap_arrange(nyc_map, boston_map, chicago_map, dc_map, 
           portland_map, mtl_map, van_map, toronto_map)

all_maps

# stats on bike_proportion  -------------------------------------------------------------



quantile(nyc_quartile$bike_proportion)
quantile(boston_quartile$bike_proportion)
quantile(chicago_quartile$bike_proportion)
quantile(dc_quartile$bike_proportion)
quantile(portland_quartile$bike_proportion)
quantile(mtl_quartile$bike_proportion)
quantile(van_quartile$bike_proportion)
quantile(tor_quartile$bike_proportion)

#maybe more histograms, but need to figure out outliers
hist(tor_quartile$bike_proportion)


# Graphs --------------------------------------------------------------


hist(nyc$bike_proportion[nyc$bike_proportion<50])
hist(boston$bike_proportion[boston$bike_proportion<50])
hist(chicago$bike_proportion[chicago$bike_proportion<50])
hist(dc$bike_proportion[dc$bike_proportion<50])
hist(portland$bike_proportion[portland$bike_proportion<50])
hist(mtl$bike_proportion[mtl$bike_proportion<50])
hist(van$bike_proportion[van$bike_proportion<50])
hist(tor$bike_proportion[tor$bike_proportion<50])




barplot_nyc <- barplot(height=count_nyc_int$n, 
        names=count_nyc_int$buffer_int, 
        xlab = "Station Intersections", 
        ylab = "Count",
        main = "CitiBike, NYC",
        las = 1,
        cex.names = 1)

barplot_boston <- barplot(height=count_bos_int$n, 
        names = count_bos_int$buffer_int, 
        xlab = "Station Intersections", 
        ylab = "Count", 
        main = "Bluebikes, Boston")

barplot_chicago <- barplot(height = count_chicago_int$n, 
        names = count_chicago_int$buffer_int,
        xlab = "Station Intersections", 
        ylab = "Count", 
        main = "Divvy, Chicago")

barplot_dc <- barplot(height = count_dc_int$n, 
        names = count_dc_int$buffer_int, 
        xlab = "Station Intersections", 
        ylab = "Count", 
        main = "Capital Bikeshare, DC", 
        cex.names = 1)

barplot_portland <- barplot(height = count_portland_int$n, 
        names = count_portland_int$buffer_int, 
        xlab = "Station Intersections", 
        ylab = "Count", 
        main = "Biketown, Portland")

barplot_mtl <- barplot(height =count_mtl_int$n, 
                       names = count_mtl_int$buffer_int, 
                       xlab = "Station Intersections", 
                       ylab = "Count", 
                       main = "BIXI, Montreal")

barplot_van <- barplot(height = count_van_int$n, 
                       names = count_van_int$buffer_int, 
                       xlab = "Station Intersections", 
                       ylab = "Count", 
                       main = "Mobi, Vancouver")

barplot_tor <- barplot(height = count_tor_int$n, 
                       names = count_tor_int$buffer_int, 
                       xlab = "Station Intersections", 
                       ylab = "Count", 
                       main = "Bike Share Toronto", 
                       cex.names = 1)


# correlation -------------------------------------------------------------

cor.test(nyc$bike_proportion, nyc$census.income, method = "spearman", exact = F)
cor.test(boston$bike_proportion, boston$income, method = "spearman", exact = F)
cor.test(chicago$bike_proportion, chicago$census.income, method = "spearman", exact = F)
cor.test(dc$bike_proportion, dc$census.income, method = "spearman", exact = F)
cor.test(portland$bike_proportion, portland$census.income, method = "spearman", exact = F)
cor.test(mtl$bike_proportion, mtl$income, method = "spearman", exact = F)
cor.test(van$bike_proportion, van$income, method = "spearman", exact = F)
cor.test(tor$bike_proportion, tor$income, method = "spearman", exact = F)

# for race:
# first need to calculate proportion of white residents
cor.test(nyc$census.income, nyc$proportion_white, method = "spearman", exact = F)

nyc$proportion_white <- (nyc$census.white/nyc$census.population)
boston$proportion_white <- (boston$white/boston$population)
chicago$proportion_white <- (chicago$census.white/chicago$census.population)
dc$proportion_white <- (dc$census.white/dc$census.population)
portland$proportion_white <- (portland$census.white/portland$census.population)

cor.test(nyc$bike_proportion, nyc$proportion_white, method = "spearman", exact = F)
cor.test(boston$bike_proportion, boston$proportion_white, method = "spearman", exact = F)
cor.test(chicago$bike_proportion, chicago$proportion_white, method = "spearman", exact = F)
cor.test(dc$bike_proportion, dc$proportion_white, method = "spearman", exact = F)
cor.test(portland$bike_proportion, portland$proportion_white, method = "spearman", exact = F)
# Save plot ---------------------------------------------------------------

lapply(intersect, function(city) {
  
  plot <- tm_shape(city$census) + 
    tm_fill("bike_proportion")
  
  link_save <- paste0("output/", city$dict$name, ".png")
  tmap_save(plot, filename = link_save)
  
})