#from open website, if you want to get data from there
install.packages("rvest")
library(rvest)
literacy <-read_html()

nhanes_id529$bp_cat <- case_when(nhanes_id529$mean_BP > 140 ~ "high BP",
                                 nhanes_id529$mean_BP >= 90 &nhanes_id529$mean_BP <= 140 ~ "normal BP",
                                 nhanes_id529$mean_BP <= 90 ~ "low BP"
)

table(nhanes_id529$bp_cat, useNA = "ifany")


# Mapping -----------------------------------------------------------------

install.packages('tigris')

library(tigris)
library(ggplot2)

manhattan_roads <- roads("NY", "New York")

ggplot(manhattan_roads) + 
  geom_sf() + 
  theme_void()
library(tidyverse)
library(sf)

# you might have some data in a shapefile -- 
# data/shapefile_unzipped/
#  -- cb_2013_us_county_20m.dbf
#  -- cb_2013_us_county_20m.prj
#  -- cb_2013_us_county_20m.shp
#  -- cb_2013_us_county_20m.shp.iso.xml
#  -- cb_2013_us_county_20m.shp.xml
#  -- cb_2013_us_county_20m.shx
#  -- county_20m.ea.iso.xml

counties <- read_sf(here("data/shapefile_unzipped"))
