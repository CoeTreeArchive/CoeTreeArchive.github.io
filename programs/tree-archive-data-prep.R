#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Clean Tree Data --------------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# 
# Contributor(s) : Evan Perry
# Last Revised : 2023-05-12
# Version: 2
# 
# Purpose : 
#   Prepare final set of archive data for mapping and use around the website.
# 
# Inputs :
#   - SERIES/raw-data/benefits-and-costs.csv
#   - SERIES/raw-data/composition-and-structure.csv
#   - SERIES/cleaned-data/itree-data-import.csv
#   - SERIES/raw-data/field-data.xlsx
#   - SERIES/raw-data/species-xwalk.csv
#   - SERIES/raw-data/faculty-tree-names.csv
#   - SERIES/raw-data/tree-coordinates.kml
# 
# Outputs : 
#   - SERIES/cleaned-data/coe-tree-archive.xlsx
#   - SERIES/cleaned-data/coe-tree-archive.zip
#   - SERIES/cleaned-data/coe-tree-archive.kml
#   - coe-tree-archive-all-years.xlsx
#
# Where SERIES is the directory with the current series of data 
# (e.g., Fall 2022)
# 
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


# Function definition

final_data_prep <- function(my_directory, coords_ind) {
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ## Setup ---------------------------------------------------------------------
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
  
  # Remove unnecessary objects
  rm(new_packages, my_packages)
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ## Read in the Data ----------------------------------------------------------
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  bca <- read_csv("raw-data/benefits-and-costs.csv")
  comp_struct <- read_csv("raw-data/composition-and-structure.csv")
  itree <- read_csv("cleaned-data/itree-data-import.csv")
  field <- read.xlsx("raw-data/field-data.xlsx", detectDates = T)
  xwalk <- read_csv("raw-data/species-xwalk.csv")
  tree_names <- read_csv('raw-data/faculty-tree-names.csv')
  
  if (coords_ind == "No") {
    tree_shp <- st_read("raw-data/tree-coordinates.kml")
  }
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ## Combine Field Data and .KML if Necessary ----------------------------------
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  if (coords_ind == "No") {
    coords <- as_tibble(st_coordinates(tree_shp)) %>%
      select(X, Y) %>%
      rename(Longitude = X,
             Latitude = Y)
    coords$`Tree.ID` <- as.numeric(tree_shp$Name)
    
    field <- merge(field %>% select(-c('Longitude', 'Latitude')),
                   coords,
                   by = "Tree.ID",
                   all.x = T)
  }
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ## Clean All the Data --------------------------------------------------------
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  # Remove duplicate columns
  bca <- bca %>%
    select(-c('Species Name', 'DBH (in)', 'Tree ID'))
  comp_struct <- comp_struct %>%
    select(-c('Species Name', 'DBH (in)', 'Land Use', 'Date', 'Crew', 'Tree ID'))
  
  # Combine all the i-Tree Eco data
  itree <- cbind(itree, comp_struct, bca)
  itree <- itree %>% select(-c('Longitude', 'Latitude', 'DBH'))
  
  # Prep the field data
  field <- field %>%
    mutate(DBH = round(`Circumference.(cm)` / 2.54 / 3.14159, 1)) %>%
    select(-c('Circumference.(cm)'))
  
  # Add tree groups to the field data
  xwalk <- xwalk %>% select(identification, tree_group)
  field <-
    merge(field,
          xwalk,
          by.x = 'Identification',
          by.y = 'identification',
          all.x = T)
  field <- field %>%
    mutate(tree_group = ifelse(is.na(tree_group), 'Other', tree_group))
  
  full_df <-
    merge(field,
          itree,
          by.x = 'Tree.ID',
          by.y = 'TreeID',
          all.x = T)
  
  # Add in tree names
  full_df <-
    merge(
      full_df,
      tree_names,
      by.x = 'Tree.ID',
      by.y = 'TreeID',
      all.x = T
    )
  
  # Also format the tree IDs
  full_df$`Tree.ID` <- str_pad(full_df$`Tree.ID`,
                               width = 3,
                               side = 'left',
                               pad = '0')
  
  full_df <- full_df %>%
    select(
      "Tree.ID",
      "Date",
      "Identification",
      "DBH",
      "Planters",
      "Special.Notes",
      "Name",
      "species_id",
      "species_code",
      "Species",
      "tree_group",
      "dbh_height",
      "Land Use",
      "Height (ft)",
      "Crown Height (ft)",
      "Crown Width (ft)",
      "Canopy Cover (ft^2)",
      "Leaf Area (ft^2)",
      "Leaf Biomass (lb)",
      "Leaf Area Index",
      "Basal Area (ft^2)",
      "Replacement Value ($)",
      "Carbon Storage (lb)",
      "Carbon Storage ($)",
      "Gross Carbon Sequestration (lb/yr)",
      "Gross Carbon Sequestration ($/yr)",
      "Avoided Runoff (gal/yr)",
      "Avoided Runoff ($/yr)",
      "Pollution Removal (oz/yr)",
      "Pollution Removal ($/yr)",
      "Total Annual Benefits ($/yr)",
      "Longitude",
      "Latitude"
    )
  
  colnames(full_df) <- c(
    "Tree ID",
    "Date",
    "Identification",
    "DBH (in)",
    "Planters",
    "Special Notes",
    "Name",
    "Species ID",
    "Species Code",
    "Species",
    "Species Group",
    "DBH Height (ft)",
    "Land Use",
    "Height (ft)",
    "Crown Height (ft)",
    "Crown Width (ft)",
    "Canopy Cover (ft^2)",
    "Leaf Area (ft^2)",
    "Leaf Biomass (lb)",
    "Leaf Area Index",
    "Basal Area (ft^2)",
    "Replacement Value ($)",
    "Carbon Storage (lb)",
    "Carbon Storage ($)",
    "Gross Carbon Sequestration (lb/yr)",
    "Gross Carbon Sequestration ($/yr)",
    "Avoided Runoff (gal/yr)",
    "Avoided Runoff ($/yr)",
    "Pollution Removal (oz/yr)",
    "Pollution Removal ($/yr)",
    "Total Annual Benefits ($/yr)",
    "Longitude",
    "Latitude"
  )
  
  rm(bca,
     comp_struct,
     coords,
     field,
     itree,
     tree_shp,
     xwalk,
     coords_ind,
     tree_names)
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ## Export Data ---------------------------------------------------------------
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  # Export Excel
  write.xlsx(
    full_df,
    "cleaned-data/coe-tree-archive.xlsx",
    asTable = T,
    colNames = T
  )
  
  # Combine with all other data
  tar_files <- list.files(
    path = "..",
    all.files = T,
    recursive = T,
    pattern = 'coe-tree-archive.xlsx'
  )
  tar_files <- as.list(paste('../', tar_files, sep = ''))
  series <- unlist(lapply(tar_files,
                          function (x) {
                            # my_series = sub('.*../', '', x)
                            my_series = sub('/cleaned-data/coe-tree-archive.xlsx', '', x)
                            my_series = sub('../', '', my_series)
                            return(my_series)
                          }))
  tar_files <-
    lapply(tar_files, function(x)
      read.xlsx(x, detectDates = T))
  
  # Add in the series name as an extra variable
  tar_files <- lapply(as.list(1:length(series)),
                      function (x) {
                        df = tar_files[[x]]
                        df$Series = series[x]
                        return(df)
                      })
  # Now combine
  all_series <- bind_rows(tar_files)
  
  # Then write out the combined file
  write.xlsx(
    all_series,
    '../coe-tree-archive-all-years.xlsx',
    asTable = T,
    colNames = T
  )
  
  # Add geometry
  full_shp <-
    st_as_sf(full_df,
             coords = c("Longitude", "Latitude"),
             crs = 4236)
  
  # Abbreviate names for the shapefile
  full_shp <- full_shp %>% select(-c('Special Notes'))
  colnames(full_shp) <- c(
    "TreeID",
    "Date",
    "Iden",
    "DBH",
    "Plntrs",
    "Names",
    "SpcID",
    "SpcCd",
    "Spc",
    "SpcGrp",
    "DBHht",
    "Use",
    "Ht",
    "CrwnHt",
    "CrwnWdth",
    "Cvr",
    "LfAr",
    "LfBmss",
    "LfArIn",
    "BslAr",
    "Rplcmnt",
    "CStrlb",
    "CStrUSD",
    "CSqstlb",
    "CSqstUSD",
    "AvdRnffG",
    "AvdRnffUSD",
    "PlltnOZ",
    "PlltnUSD",
    "AnnBnfts",
    'geometry'
  )
  
  # Easier at this point to move the working directory into the cleaned-data
  setwd(paste(getwd(), '/cleaned-data', sep = ''))
  
  # Export as Shapefile
  st_write(full_shp, "coe-tree-archive.shp", append = FALSE)
  
  zip(
    zipfile = 'coe-tree-archive',
    files = c(
      "coe-tree-archive.dbf",
      "coe-tree-archive.prj",
      "coe-tree-archive.shp",
      "coe-tree-archive.shx"
    )
  )
  
  file.remove(
    c(
      "coe-tree-archive.dbf",
      "coe-tree-archive.prj",
      "coe-tree-archive.shp",
      "coe-tree-archive.shx"
    )
  )
  
  # Export as .kml
  full_shp <- full_shp %>%
    select(TreeID, Iden, geometry) %>%
    rename(Name = TreeID, Description = Iden)
  
  full_shp <- st_zm(full_shp, drop = TRUE, what = "ZM")
  st_write(full_shp,
           'coe-tree-archive.kml',
           driver = 'kml',
           delete_dsn = T)
  
  # Warnings are not important
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
} 

# End function
