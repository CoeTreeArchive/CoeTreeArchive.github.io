#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Run All Programs -------------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# 
# Contributor(s) : Evan Perry
# Last Revised : 2023-05-14
# Version : 2
# 
# Purpose : 
#   - Compile all necessary programs from one script
#   - For a specified series, this runs:
#       1. Data prep for i-Tree Eco (itree-data-prep.R)
#       2. Data prep for the Coe Tree Archive files (tree-archive-data-prep.R)
#       3. Writing new tree profiles based on new data
# 
# Inputs :
#   - SERIES/raw-data/benefits-and-costs.csv
#   - SERIES/raw-data/composition-and-structure.csv
#   - SERIES/cleaned-data/itree-data-import.csv
#   - SERIES/raw-data/field-data.xlsx
#   - SERIES/raw-data/species-xwalk.csv
#   - SERIES/raw-data/faculty-tree-names.csv
#   - SERIES/raw-data/tree-coordinates.kml
#   - images/tree-profiles/
# 
# Outputs : 
#   - cleaned-data/itree-data-import.xlsx
#   - SERIES/cleaned-data/coe-tree-archive.xlsx
#   - SERIES/cleaned-data/coe-tree-archive.zip
#   - SERIES/cleaned-data/coe-tree-archive.kml
#   - coe-tree-archive-all-years.xlsx
#   - tree-profiles/
#
# NOTE: This script will not be able to run in its entirety if the results from
# i-Tree Eco have not already been prepared. If these results are not yet ready,
# then run only the section of code labeled as able to run before the i-Tree Eco
# results are back, add those results to the series directory, and then finish
# running the remainder of the script.
# 
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## Code to Change When Replicating ---------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Give the file path of the folder with your new data
my_directory <- "C:/Users/eaper/CoeTreeArchive.github.io/data/Fall 2022"

# Are coordinates (longitude/latitude) in the field data sheet? (Either: "Yes" 
# or "No") If "No", then these must be a separate .kml file named 
# tree-coordinates.kml. NOTE: The "No" option is preferable.
coords_ind <- "No"

# Ensure your working directory is set to the project location
setwd('C:/Users/eaper/CoeTreeArchive.github.io')

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## Read Functions --------------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

source('programs/itree-data-prep.R')
source('programs/tree-archive-data-prep.R')
source('programs/write-tree-profiles.R')

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## Pre i-Tree Eco Run ----------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

itree_data_prep(my_directory, coords_ind)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# You must have run the cleaned field data through i-Tree Eco before proceeding

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## Post i-Tree Eco Run ---------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Prepare the final datasets with all field data and results
# (you can ignore the warning at the end)
final_data_prep(my_directory, coords_ind)

# Write new tree profiles for all trees in the series
write_tree_profiles(my_directory)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

