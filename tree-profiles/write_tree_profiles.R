#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Write Tree Profiles ----------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# 
# Contributor(s) : Evan Perry
# Last Revised : 2022-12-13
# Version : 1
# 
# Purpose : 
#   Generate all the .qmd files that we use for the tree profile pages
# 
# Inputs :
#   - CoeTreeArchive/all_tree_images
#   - CoeTreeArchive/data analysis/cleaned data/coe_tree_archive/csv
# 
# Outputs : 
#   
# 
# 
# Outline: (Crtl + Shift + O)
#
#
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## Setup -----------------------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Importing packages
library(tidyverse)

# Setting working directory
setwd("C:/Users/eaper/CoeTreeArchive.github.io")

# Remove scientific notation
options(scipen = 999)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## Read and Clean Data ---------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Read in the data on each tree
df <- read_csv("data analysis/cleaned data/coe_tree_archive.csv")

# Add a variable indicating that 
trees_w_pics <- list.files("tree-profiles/profiles/all_tree_images")
df$TreeID <- str_pad(df$TreeID, 3, "left", pad="0")
df$photo <- ifelse(df$TreeID %in% trees_w_pics, 1, 0)

# Image for the thing
df <- df %>% 
  mutate(
    cover_pic = ifelse(photo == 1, 
                       paste("all_tree_images/", TreeID, "/main.jpg", sep=""),
                       "all_tree_images/not_available.png")
  )

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## Writing the Files -----------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

for (i in 1:length(df$TreeID)){
  
  my_tree <- df$TreeID[i]
  
  # Write Names of Planters
  if(is.na(df$Planters[i])){
    names <- "Unknown"
  } else {
    names <- df$Planters[i]
  }
  
  # Write the i-Tree Eco Estimate lines
  if (df$Species[i] == "Unidentified"){
    i_tree_lines <- c(
      "\n Replacement Value: Not Available",
      "\n Carbon Storage: Not Available", 
      "\n Gross Carbon Removal: Not Available",
      "\n Avoided Runoff: Not Available",
      "\n Air Pollution Removal: Not Available"
    )
  } else {
    i_tree_lines <- c(
      paste("\n Replacement Value: $", df$`Replacement Value ($)`[i]),
      paste("\n Carbon Storage: ", df$`Carbon Storage (lb)`[i], " lbs"),
      paste("\n Gross Carbon Removal: ", df$`Gross Carbon (lb/yr)`[i], " lbs/year"),
      paste("\n Avoided Runoff: ", df$`Avoided Runoff (ft3/yr)`[i], " ft<sup>3</sup>/year"),
      paste("\n Air Pollution Removal: ", df$`Removed Pollution (oz/yr)`[i], " oz/year")
    )
  }
  
  # Write the image lines
  
  if (df$photo[i] == 0){
    image_lines <- c(
      "No images available at this time."
    )
  } else {
    
    temp_file_path <- paste("tree-profiles/profiles/all_tree_images", my_tree, sep="/")
    image_lines <- list.files(temp_file_path)
    image_lines <- paste("\n ![](all_tree_images/", my_tree, "/", image_lines, ")", sep="")
    
  }
  
  
  # Write the file
  
  outfile <- paste("tree-profiles/profiles/tree_", my_tree, ".qmd", sep="")
  fileConn <-file(outfile)
  
  my_lines <- c(
    "---",
    paste("title: ", '"**', df$Species[i], '**"', sep=""),
    paste("description: ", '"Tree ID: ', df$TreeID[i], '"', sep=""),
    paste("image: ", '"', df$cover_pic[i], '"', sep=""),
    "---",
    "",
    "<br>",
    "",
    "## **More Information**",
    "::: column-margin",
    '<center><a class="button" href="https://forms.gle/W6f5nwYxrM33hF4f7">Notice a Mistake?</a></center>',
    ":::",
    "",
    paste("\nDate Recorded: ", df$`Date of Collection`[i]),
    paste("\nDiameter at Breast Height: ", df$`DBH (inches)`[i], " inches"),
    paste("\nPlanters: ", names),
    "",
    "## **i-Tree Eco Estimates**",
    "",
    i_tree_lines,
    "",
    "## **Images**",
    "",
    image_lines
  )
  
  writeLines(my_lines, fileConn)
  
  close(fileConn)
  
}

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


