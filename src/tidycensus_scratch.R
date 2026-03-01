library(tidyverse)
library(tidycensus)
library(sf)
library(tmap)



tidycensus::get_acs()


oh <- get_acs(geography = "county", table = "B15003",
                state = "OH", output = "wide", 
              geometry = TRUE, year = 2020)

oh.pop.c <- get_acs(geography = "county", table = "B01003",
                                state = "OH", output = "wide", 
                                geometry = F, year = 2020)



oh.tract <- get_acs(geography = "tract", table = "B15003",
                    state = "OH", output = "wide", 
                    geometry = TRUE, year = 2020)

oh.pop.t <- get_acs(geography = "tract", table = "B01003",
                  state = "OH", output = "wide", 
                  geometry = F, year = 2020)


oh.joined <- oh.tract.t %>% left_join(., oh.pop, by = "GEOID") %>%
  mutate(propBachelors = B15003_021M / B01003_001E)


tmap::tmap_mode("plot")
oh.joined %>% tm_shape(oh) + tm_polygons(fill = "propBachelors")



oh.c.j <- oh %>% left_join(., oh.pop.c, by = "GEOID") %>%
  mutate(propBachelors = B15003_021M / B01003_001E)

oh.c.j %>% tm_shape(oh) + tm_polygons(fill = "propBachelors")

options(tigris_use_cache = TRUE)

all.states <- get_acs(geography = "county", table = "B15003",
                   output = "wide", 
                      geometry = T, year = 2020)


all.states %>% tm_shape(.) + tm_polygons(fill = "B15003_003E")
