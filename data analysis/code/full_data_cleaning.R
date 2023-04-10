#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Cleaned Tree Data ------------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# 
# Contributor(s) : Evan Perry
# Last Revised : April 9, 2023
# Version : 1
# 
# Purpose : 
#   Prepare archive data and prep data for the map
# 
# Inputs :
#   - raw data/field_data.csv
#   - raw data/species_xwalk.csv
#   - raw data/tree_coords.kml
#   - raw data/benefits_data.pdf
# 
# Outputs : 
#   - cleaned data/coe_tree_archive_data.csv
#   - cleaned data/coe_tree_archive.shp
#
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## Setup -----------------------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Importing packages
library(tidyverse)
library(sf)
library(pdftools)
library(stringr)

# Setting working directory
setwd("C:/Users/eaper/CoeTreeArchive.github.io/data analysis")

# Remove scientific notation
options(scipen = 999)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Read/Clean the PDFs ----------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

raw_file <- pdf_text("raw data/benefits_data.pdf")

half_length = as.integer(length(raw_file)/2)

df_list1 <-vector(mode = "list", length = half_length)
df_list2 <-vector(mode = "list", length = half_length)

for (i in 1:half_length){
  
  page = raw_file[2*i - 1]
  
  rows <- scan(textConnection(page), what="character", sep = "\n")
  rows = lapply(rows, function (x) unlist(strsplit(x,"\\s{1,}")))
  
  clean_rows = suppressWarnings(
    lapply(rows, function(x) !is.na(as.integer(x[1]))))
  clean_rows <- unlist(clean_rows)
  
  clean_rows <- rows[clean_rows]
  
  clean_rows = lapply(clean_rows, function(x) gsub(",", "", x))
  clean_rows = lapply(clean_rows, function (x) as.numeric(x))
  clean_rows = lapply(clean_rows, function (x) x[!is.na(x)])
  
  df = as.data.frame(do.call(rbind, clean_rows))
  
  df_list1[[i]] = df
}


for (i in 1:half_length){
  
  page = raw_file[2*i]
  
  rows <- scan(textConnection(page), what="character", sep = "\n")
  rows = lapply(rows, function (x) str_trim(x))
  rows = lapply(rows, function (x) unlist(strsplit(x,"\\s{2,}")))
  
  # rows = lapply(rows, function(x) gsub(" ", "", x))
  
  clean_rows = suppressWarnings(
    lapply(rows, function(x) !is.na(as.integer(x[1]))))
  clean_rows <- unlist(clean_rows)
  
  clean_rows <- rows[clean_rows]
  
  clean_rows = lapply(clean_rows, function (x) as.numeric(x))
  clean_rows = lapply(clean_rows, function (x) x[!is.na(x)])
  
  df = as.data.frame(do.call(rbind, clean_rows))
  
  df_list2[[i]] = df
}

df1 <- bind_rows(df_list1)
df2 <- bind_rows(df_list2)

df <- cbind(df1, df2)

colnames(df) <- c(
  "itree_id",
  "dbh",
  "replacement_value",
  "carbon_storage_lbs",
  "carbon_storage_dollars",
  "gross_carbon_lbs",
  "gross_carbon_dollars",
  "runoff_ft",
  "runoff_dollars",
  "poll_oz",
  "poll_dollars",
  "total_benefits",
  "lon",
  "lat",
  "my_id"
)

rm(clean_rows, df_list1, df_list2, df1, df2, rows, half_length, i, page, raw_file)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Cleaning on the Rest of the Data ---------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

tree_df <- read_csv("raw data/field_data.csv")
xwalk <- read_csv("raw data/species_xwalk.csv")
tree_shp <- st_read("raw data/tree_coords.kml")


tree_shp <- tree_shp %>% 
  rename(TreeID = Name) %>% 
  mutate(TreeID = as.integer(TreeID)) %>% 
  select(TreeID, geometry)

main <- merge(tree_df, xwalk, by="identification", all.x=T)

# Clean
main <- main %>% 
  mutate(
    DBH = round(`circum (inches)`/3.14159/2.54, 1),
    dbh_height = 4.5,
    land_use = "Institutional"
  )

df <- df %>% select(-c("dbh","itree_id","lon","lat"))

main <- merge(main, df, by.x="tree_id", by.y="my_id", all.x=T)

main <- merge(tree_shp, main, by.x="TreeID", by.y="tree_id", all.x=T, all.y=T)

main <- main %>% 
  mutate(
    tree_group = ifelse(is.na(tree_group), "Other", tree_group)
  )


colnames(main) <- c(
  "TreeID",
  "Species",
  "Circumference (cm)",
  "Planters",
  "Date of Collection",
  "i-Tree Species ID",
  "i-Tree Species Code",  
  "i-Tree Species Scientific Name",
  "i-Tree Species Common Name",
  "Tree Map Group",
  "DBH (inches)",
  "DBH Height (ft)",
  "i-Tree Land Use",
  "Replacement Value ($)",
  "Carbon Storage (lb)",
  "Carbon Storage ($)",
  "Gross Carbon (lb/yr)",
  "Gross Carbon ($/yr)",
  "Avoided Runoff (ft3/yr)",
  "Avoided Runoff ($/yr)",
  "Removed Pollution (oz/yr)",
  "Removed Pollution ($/yr)",
  "Total Benefits ($/yr)",
  "geometry"
)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Export
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Export CSV

main_csv <- as_tibble(main) %>% 
  select(-c("geometry"))

coords <- as_tibble(st_coordinates(main)) %>% 
  rename(latitude = Y, longitude = X) %>% 
  select(latitude, longitude)

main_csv <- cbind(main_csv, coords)
  
write_csv(main_csv, "cleaned data/coe_tree_archive.csv", na = "")

# Export as .kml
main_kml <- st_zm(main, drop = TRUE, what = "ZM")

colnames(main_kml) <- c(
  "ID",
  "species",
  "circum",
  "planters",
  "plant",
  "spec_id",
  "spec_code",  
  "spec_sci",
  "spec_comm",
  "tree_grp",
  "dbh",
  "dbh_ht",
  "l_use",
  "rep_val",
  "c_stor_1",
  "c_stor_2",
  "gr_c_1",
  "gr_c_2",
  "runoff_1",
  "runoff_2",
  "poll_1",
  "poll_2",
  "benefits",
  "geometry"
)

st_write(main_kml, "cleaned data/coe_tree_archive.shp", append=FALSE)
st_write(main_kml %>% 
           select(ID, species, geometry) %>% 
           rename(Name = ID, Description = species), 
         "cleaned data/coe_tree_archive.kml", driver = "kml", delete_dsn = TRUE)
