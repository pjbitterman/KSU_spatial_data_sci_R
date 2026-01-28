# week 2 scratch pad

library(tidyverse)
library(terra)
library(sf)

mydf <- read_csv("./data/ohio/oh_counties_DP2020.csv")
glimpse(mydf)

plot(mydf$poptotal, mydf$Hhtotal)


plot(mydf$poptotal, mydf$medianage)

hist(mydf$Hhtotal)
hist(mydf$Hhtotal, breaks = 20)


ggplot(mydf, aes(x = poptotal, y = medianage)) +
  geom_point(colour = "blue") +
  geom_smooth(method = "glm", colour = "red") +
  theme_minimal() +
  labs(x = "Total Population", y = "Median Age", title = "My First ggplot")



mydf2 <- mydf %>% mutate(sizeCategory = ifelse(poptotal > 100000, "big", "small"))
summary(mydf2$sizeCategory)
summary(as.factor(mydf2$sizeCategory))


ggplot(mydf2, aes(x = poptotal, y = medianage)) +
  geom_point(aes(shape = sizeCategory, colour = sizeCategory), size = 3) +
  theme_minimal() +
  labs(x = "Total Population", y = "Median Age",
       title = "My formatted ggplot")


mydf2 %>% ggplot(., aes(x = sizeCategory, y = medianage)) +
  geom_boxplot(aes(fill = sizeCategory)) +
  theme_minimal() + 
  labs(x = "Categorical size",
       y = "Median Age", 
       title = "I made a boxplot",
       subtitle = "...it's handy for comparing groups")



rastmiss <- terra::rast("./data/ts_2016.1007_1013.L4.LCHMP3.CIcyano.MAXIMUM_7day.tif")


p.counties <- "./data/CBW/County_Boundaries.shp"
p.stations <- "./data/CBW/Non-Tidal_Water_Quality_Monitoring_Stations_in_the_Chesapeake_Bay.shp"

#d.counties <- terra::vect(p.counties)
#d.stations <-terra::vect(p.stations)

d.counties <- sf::read_sf(p.counties) %>% sf::st_make_valid()
d.stations <- sf::read_sf(p.stations)


glimpse(d.counties)
glimpse(d.stations)


del.counties <- d.counties %>% dplyr::filter(STATEFP10 == 10)
del.counties <- d.counties %>% terra::subset(d.counties$STATEFP10 == "10")


d.counties %>% sf::st_crs() == d.stations %>% sf::st_crs()


de.stations <- sf::st_intersection(d.stations, del.counties) # might take a bit
glimpse(de.stations)
plot(de.stations)


option_1 <- sf::st_intersection(d.stations, del.counties)
option_2 <- sf::st_intersection(del.counties, d.stations)



### Testing on 2025-01-30

# straightforward way. Create the sums outside of the data.frame
totalArea <- sum(d.counties$ALAND10) + sum(d.counties$AWATER10)

d.counties.area <- d.counties %>% 
  mutate(per = 100 * ALAND10 / totalArea)


# a bit more complicated/compact, but everything is in one line 
# Technically the ".$" below is unnecessary, but it's nice to be verbose at times
d.counties.area <- d.counties %>% 
  mutate(per = 100 * ALAND10 / (sum(.$ALAND10) + sum(.$AWATER10)))


d.counties %>% 
  as_tibble() %>%
  select(-geometry) %>%
  group_by(STATEFP10) %>% 
  #summarise(mycount = n()) %>%
  #count() %>%
  tally()


#

d.stations %>% slice_min(., nchar(STATION_NA))


d.stations %>% mutate(length = nchar(STATION_NA)) %>% slice(which.min(length))


d.stations %>% 
  mutate(myid = stringr::str_sub(STAID, 1, 3)) %>%
  ggplot(., aes(x = Drainage_A)) +
  geom_histogram(aes(fill = myid))


myfun <- function(invector){
  
  
  if(is.numeric(invector)){
    sorted <- sort(invector)
    vec_mean <- mean(invector)
    vec_median <- median(invector)
    
    toReturn <- list(mean = vec_mean, median = vec_median, sorted = sorted)
    
    return(toReturn)  
    
  }
  else{
     print("error in input")
  }
  
}

myfun(c(1, 0, -1))
myfun(c(10, 100, 1000))
myfun(c(.1, .001, 1e-8))
myfun(c("a", "b", "c"))



myint <- sf::st_intersection(d.counties, d.stations)

myint %>% as_tibble() %>% 
  dplyr::select(-geometry) %>%
  group_by(STATEFP10) %>%
  count()


nyareas <- d.counties %>% 
  dplyr::filter(STATEFP10 == 36) %>%
  mutate(calcedarea = sf::st_area(geometry)) 

mean(nyareas$calcedarea)  




d.counties %>% 
  dplyr::filter(STATEFP10 == 36) %>%
  mutate(calcedarea = sf::st_area(geometry)) %>%
  summarise(mean = mean(calcedarea))


myint %>% as_tibble() %>% 
  dplyr::select(-geometry) %>%
  group_by(STATEFP10) %>%
  summarise(meandrain = mean(Drainage_A)) %>%
  slice_max(meandrain)
