#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Clean Field Data -------------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# 
# Contributor(s) : Evan Perry
# Last Revised : 2023-05-12
# Version : 2
# 
# Purpose : 
#   Merge field data, coordinates, and crosswalk so that data collected is 
#   ready to pass to i-Tree Eco for analysis.
# 
# Inputs :
#   - raw-data/field-data.xlsx
#   - raw-data/species-xwalk.csv
#   - raw-data/tree-coordinates.kml
# 
# Outputs : 
#   - cleaned-data/itree-data-import.xlsx
#
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


# Function definition

itree_data_prep <- function(my_directory, coords_ind) {
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ## Set-Up --------------------------------------------------------------------
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  # All packages needed
  my_packages <- c('tidyverse',
                   'sf',
                   'openxlsx')
  
  # Install any packages not already installed
  new_packages <-
    my_packages[!(my_packages %in% installed.packages()[, "Package"])]
  if (length(new_packages)) {
    install.packages(new_packages)
  }
  # Library all packages
  lapply(my_packages, library, character.only = TRUE)
  
  # Reduce scientific notation
  options(scipen = 9)
  
  # Set working directory
  setwd(my_directory)
  
  # Create a cleaned-data subdirectory if one doesn't exist
  dir.create(file.path(my_directory, "cleaned-data"), showWarnings = FALSE)
  
  # Remove unnecessary objects
  rm(new_packages, my_packages)
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ## Read in files -------------------------------------------------------------
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  tree_df <- read.xlsx("raw-data/field-data.xlsx", detectDates = T)
  xwalk <- read_csv("raw-data/species-xwalk.csv")
  if (coords_ind == "No") {
    tree_shp <- st_read("raw-data/tree-coordinates.kml")
  }
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ## Clean Data ----------------------------------------------------------------
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  # Adjust names of field data
  colnames(tree_df) <- c(
    'TreeID',
    'date',
    'circumference',
    'Longitude',
    'Latitude',
    'identification',
    'planters',
    'notes'
  )
  
  # Extract the longitude and latitude from the shapefile
  if (coords_ind == "No") {
    coords <- as_tibble(st_coordinates(tree_shp)) %>%
      select(X, Y) %>%
      rename(Longitude = X,
             Latitude = Y)
    coords$TreeID <- as.numeric(tree_shp$Name)
  }
  
  # Crosswalk with i-Tree Eco codes
  itree_df <- merge(tree_df, xwalk, by = "identification", all.x = T)
  
  # Merge in the coordinates if needed
  if (coords_ind == "No") {
    itree_df <- merge(itree_df %>% select(-c('Longitude', 'Latitude')),
                      coords,
                      by = "TreeID",
                      all.x = T)
  }
  
  # Clean
  itree_df <- itree_df %>%
    mutate(
      DBH = round(circumference / 3.14159 / 2.54, 1),
      `Land Use` = "Institutional",
      Species = paste(common_name, " (", scientific_name, ")", sep = ""),
      dbh_height = 4.5
    ) %>%
    select(
      TreeID,
      species_id,
      species_code,
      Species,
      DBH,
      dbh_height,
      Longitude,
      Latitude,
      `Land Use`
    ) %>%
    na.omit()
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ## Export --------------------------------------------------------------------
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  write_csv(itree_df, "cleaned-data/itree-data-import.csv")
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
}

# End function