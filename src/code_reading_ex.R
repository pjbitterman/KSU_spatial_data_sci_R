# load packages ----
library(tidyverse)
library(sf)
library(terra)
library(fs)
library(lubridate)
library(ggplot2)
library(ggspatial)
library(RColorBrewer)


# HELPER FUNCTIONS ----
### These functions are called/used later
### Why do I need to include them first?


### DESCRIBE HOW THING FUNCTION WORKS
collect_images <- function(im.path) {
  
  # What am I doing here?
  file_names <- fs::dir_ls(im.path)
  ext_names <- fs::path_ext(file_names) # get file extensions
  
  # put in tibble, filter for tif only
  image.files <- data.frame(file_names, ext_names) %>%
    as_tibble() %>%
    mutate(fn_char = as.character(file_names)) %>%
    mutate(file_names = fs_path(file_names %>% as.character)) %>% #lost fs type when put into df
    filter(str_detect(ext_names, "tif")) %>%
    mutate(fn = fs::path_file(file_names)) %>%
    mutate(
      year = stringr::str_sub(fn, 4, 7),
      start_month = stringr::str_sub(fn, 9, 10),
      start_day = stringr::str_sub(fn, 11, 12),
      end_month = stringr::str_sub(fn, 14, 15),
      end_day = stringr::str_sub(fn, 16, 17)
    ) %>%
    mutate(
      start_date = paste0(year, "-", start_month, "-", start_day),
      end_date = paste0(year, "-", end_month, "-", end_day)
    )
}



### DESCRIBE HOW THIS FUNCTION WORKS
clip_raster_to_bounding <- function(in.raster.path, boundingPolygon.path) {
  
  ras <- terra::rast(in.raster.path)
  
  ras.p4 <- terra::crs(ras)
  
  # What's a bounding polygon and how can you know what that poly looks like?
  bb <- sf::read_sf(boundingPolygon.path)
  
  bb.projected <- sf::st_transform(bb, ras.p4) %>% vect(.)
  
  ras.subset <- terra::crop(ras, bb.projected)
}


### DESCRIBE HOW THIS FUNCTION WORKS
calc_area_by_thresholds <- function(in.raster,
                                    boundingPolygon.path,
                                    start_date,
                                    end_date) {
  # ??
  bb <- sf::read_sf(boundingPolygon.path) ## read shapefile
  
  # What's a CRS again?
  ras.p4 <- terra::crs(in.raster)
  
  # project the bounding box to the raster's proj4
  bb.projected <- sf::st_transform(bb, ras.p4) %>% vect(.)
  
  
  # raster properties
  raster.res <- terra::res(in.raster)
  pixel.area.m2 <- raster.res[1] * raster.res[2]
  
  
  # What does terra::crop do?
  ras.subset <- terra::crop(in.raster, bb.projected)
  
  
  # NOAA transform for ERIE data
  # valid points: 2-249 is valid data
  # valid as of 2019-02-01
  masked_raster <- mask(ras.subset, ras.subset >= 2 &
                          ras.subset <= 249)
  transformed_raster <- 10 ** (masked_raster / 100 - 4)
  
  
  ### THRESHOLDS
  # upper bounds on each - in units of CI (hence the divide by 1e8)
  # data from from WHO tables
  thresh.low <- 20000 / 1e8
  thresh.mod <- 100000 / 1e8
  thresh.high <- 10000000 / 1e8
  
  ras.low <- transformed_raster < thresh.low
  ras.mod <- transformed_raster >= thresh.low &
    transformed_raster < thresh.mod
  ras.high <- transformed_raster >= thresh.mod &
    transformed_raster < thresh.high
  ras.very_high <- transformed_raster >= thresh.high
  
  # Why do I need the count of cells in the raster? Any ideas?
  cell.count <- terra::global(ras.subset, fun = "sum", na.rm = T)
  
  area_low <- global(ras.low, fun = "sum", na.rm = TRUE) * pixel.area.m2
  area_mod <- global(ras.mod, fun = "sum", na.rm = TRUE) * pixel.area.m2
  area_high <- global(ras.high, fun = "sum", na.rm = TRUE) * pixel.area.m2
  area_veryhigh <- global(ras.very_high, fun = "sum", na.rm = TRUE) * pixel.area.m2
  
  prop_low <- global(ras.low, fun = "sum", na.rm = TRUE) / cell.count
  prop_mod <- global(ras.mod, fun = "sum", na.rm = TRUE) / cell.count
  prop_high <- global(ras.high, fun = "sum", na.rm = TRUE) / cell.count
  prop_veryhigh <- global(ras.very_high, fun = "sum", na.rm = TRUE) / cell.count
  
  r.low <- data.frame(prop_low, area_low) %>% mutate(whoCat = "low")
  r.mod <- data.frame(prop_mod, area_mod) %>% mutate(whoCat = "moderate")
  r.high <- data.frame(prop_high, area_high) %>% mutate(whoCat = "high")
  r.very_high <- data.frame(prop_veryhigh, area_veryhigh) %>% mutate(whoCat = "very_high")
  
  # bind_rows? What am I binding together?
  toReturn <- bind_rows(r.low, r.mod, r.high, r.very_high) %>%
    mutate(start_date = start_date, end_date = end_date) %>%
    magrittr::set_colnames(c(
      "prop_in_range",
      "area_m2_in_range",
      "whoCat",
      "start_date",
      "end_date"
    )) %>%
    dplyr::select("prop_in_range",
                  "area_m2_in_range",
                  "start_date",
                  "end_date",
                  "whoCat") %>%
    mutate(whoCat = forcats::fct_relevel(whoCat, c("very_high", "high", "moderate", "low"))) %>%
    remove_rownames()
}

# Analysis steps ----

## data reading and preparation ----

## path to NOAA tifs
im.path <- fs::path("./data/erie_cicyano/")

## bounding box path
bb.path <- fs::path("./data/shapes/bb_erie_westernext.shp")

## What does this function call do???
image.files <- collect_images(im.path)

## I have a list of???? And what am I doing to the items in this list???
clipped.images <- list(image.files$fn_char, bb.path) %>%
  pmap(clip_raster_to_bounding)




## calculate ________ within _______ 

## What are the contents of this list and why am I bothering to create a list?
toCalcArea <- list(
  clipped.images,
  bb.path,
  image.files$start_date,
  image.files$end_date
)

# How does pmap_df work, and what is it doing in this context?
areas <- toCalcArea %>% pmap_df(calc_area_by_thresholds)

## What is this plotting
areas %>% unique() %>%
  ggplot(.,
         aes(
           x = lubridate::ymd(start_date),
           y = area_m2_in_range / 1e6,
           fill = whoCat
         )) +
  geom_bar(stat = "identity", width = 7) +
  labs(x = "date",
       y = bquote("bloom area" ~ (km ^ 2)),
       fill = "WHO category") +
  labs(title = "Western Lake Erie maximum weekly bloom extent by WHO intensity") +
  theme_minimal() +
  #scale_fill_viridis_d(option = "viridis") +
  scale_fill_brewer(type = "seq",
                    palette = "RdYlGn",
                    direction = 1) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  geom_vline(xintercept = lubridate::ymd("2017-01-01"), alpha = .5) +
  geom_vline(xintercept = lubridate::ymd("2018-01-01"), alpha = .5) +
  geom_vline(xintercept = lubridate::ymd("2019-01-01"), alpha = .5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(axis.text.x = element_text(size = 12), axis.title.x = element_blank()) +
  theme(axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14)) +
  theme(legend.position = "top")


## What does this do?
ggsave(
  "./plots/erieplot.jpg",
  width = 6.1,
  height = 6.5,
  units = "in",
  dpi = 256
)



# Tasks and questions ----

# 1. Read the script, working through what each line, command, and function does and why

# 2. Modify the plot with a different fill color/palette

# 3. How does the code read the files present in a given directory?

# 4. Why would I bother using the fs library when I can just specify a path?

# 5. Describe how I'm calculating the area of the bloom at different concentration levels

# 6. Are there any extra steps or areas for greater efficiencies you can identify?

# 7. In the collect_images function, how does the code determine which image files to process, 
# and why might it be important to extract date information (e.g., start_date, end_date) from the file names?

# 8. In the clip_raster_to_bounding function, why is it necessary to project 
# the bounding polygon to match the raster’s coordinate reference system before cropping? 
# What might happen if this step is skipped?

# 9. In the calc_area_by_thresholds function, 
# what is the purpose of applying thresholds to the transformed raster data? 
# How do these thresholds relate to environmental concerns (e.g., harmful algal blooms) in Lake Erie?

# 10. The final plot shows bloom area over time categorized by WHO intensity levels. 
# How does the use of color gradients and bar heights help communicate patterns in the data? 
# Can you think of an alternative visualization that might better highlight trends or anomalies?
