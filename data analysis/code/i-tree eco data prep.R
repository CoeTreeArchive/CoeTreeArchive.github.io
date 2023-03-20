#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# i-Tree Eco Data Prep ---------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# 
# Contributor(s) : Evan Perry
# Last Revised : December 12, 2022
# Version : 1
# 
# Purpose : 
#   Prepare data to easily import into i-Tree Eco
# 
# Inputs :
#   - raw data/field_data.csv
#   - raw data/species_xwalk.csv
#   - raw data/tree_coords.kml
# 
# Outputs : 
#   - cleaned data/itree_data_import.xlsx
#
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## Setup -----------------------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Importing packages
library(tidyverse)
library(sf)
library(openxlsx)

# Setting working directory
setwd("C:/Users/eaper/Tree Census/CoeTreeArchive/data analysis")

# Remove scientific notation
options(scipen = 999)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Read in files ----------------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

tree_df <- read_csv("raw data/field_data.csv")
xwalk <- read_csv("raw data/species_xwalk.csv")
tree_shp <- st_read("raw data/tree_coords.kml")

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Clean Data
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Extract the longitude and latitude from the shapefile
coords <- as_tibble(st_coordinates(tree_shp)) %>%
  select(X, Y) %>%
  rename(
    Longitude = X,
    Latitude = Y
  )
coords$TreeID <- as.numeric(tree_shp$Name)

# Crosswalk
itree_df <- merge(tree_df, xwalk, by="identification", all.x=T)
itree_df <- merge(itree_df, coords, by.x="tree_id", by.y="TreeID", all.x = T)

# Clean
itree_df <- itree_df %>% 
  mutate(
    DBH = round(`circum (inches)`/3.14159/2.54, 1),
    `Land Use` = "Institutional",
    Species = paste(common_name, " (", scientific_name, ")", sep=""),
    dbh_height = 4.5
  ) %>% 
  select(
    tree_id, species_id, species_code, Species, DBH, dbh_height, Longitude, Latitude, `Land Use`
  ) %>% 
  na.omit()

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Export
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

write.xlsx(itree_df, "cleaned data/itree_data_import.xlsx")
