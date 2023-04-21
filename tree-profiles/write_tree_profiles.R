#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Write Tree Profiles ----------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# 
# Contributor(s) : Evan Perry
# Last Revised : 2023-04-21
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
temp <- read_csv("data analysis/cleaned data/faculty-tree-names.csv")
df <- merge(df, temp, by = "TreeID", all.x = T)
df <- df %>% 
  mutate(
    Name = ifelse(is.na(Name), "Unnamed", Name)
  )

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

table0_lines <- c(
  "```{r}",
  "#| error: false",
  "#| echo: false",
  "#| message: false",
  "#| warning: false",
  "#| tbl-colwidths: [30,40,30]",
  "kable(df0)",
  "```"
)

table1_lines <- c(
  "```{r}",
  "#| error: false",
  "#| echo: false",
  "#| message: false",
  "#| warning: false",
  "#| tbl-colwidths: [30,40,30]",
  "kable(df1)",
  "```"
)

map_builder <- function(i){
  map_code <- c(
    "```{r}", "#| error: false", "#| echo: false", "#| message: false", 
    "#| warning: false", paste("my_tree <- '", df$TreeID[i], "'", sep=""),
    "# Importing packages", "library(tidyverse)", 
    "library(sf)", "library(tmap)", "library(leaflet)", 
    "library(leaflet.extras)", "", "# Remove scientific notation", 
    "options(scipen = 999)", "", 
    "#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::", 
    "", "", 
    "#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::", 
    "## Formatting Set Up -----------------------------------------------------------", 
    "#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::", 
    "", "tree_shp <- st_read(\"all_tree_images/coe_tree_archive.shp\", quiet = TRUE) ", 
    "", "# Formatting the names of planters", "format_names <- function(x){", 
    "  if(is.na(x)){", "    output = \"Unknown\"", "  } else {", 
    "    output = str_replace_all(x, pattern = \", \", replacement=\"<br>\")", 
    "    output = paste(\"<br>\", output, sep=\"\")", "  }", 
    "  return(output)", "}", "", "# Setting up the tree icons", 
    "treeIcons <- iconList(", "  Oak = makeIcon(", 
    "    iconUrl = \"all_tree_images/tree_icons/oak.png\",", 
    "    iconWidth = 25, iconHeight = 50", "  ),", "  Maple = makeIcon(", 
    "    iconUrl = \"all_tree_images/tree_icons/maple.png\",", 
    "    iconWidth = 25, iconHeight = 50", "  ),", 
    "  `Honey Locust` = makeIcon(", 
    "    iconUrl = \"all_tree_images/tree_icons/honeylocust.png\",", 
    "    iconWidth = 25, iconHeight = 50", "  ),", "  Elm = makeIcon(", 
    "    iconUrl = \"all_tree_images/tree_icons/elm.png\",", 
    "    iconWidth = 25, iconHeight = 50", "  ),", 
    "  Ash = makeIcon(", "    iconUrl = \"all_tree_images/tree_icons/ash.png\",", 
    "    iconWidth = 25, iconHeight = 50", "  ),", "  Evergreen = makeIcon(", 
    "    iconUrl = \"all_tree_images/tree_icons/evergreen.png\",", 
    "    iconWidth = 25, iconHeight = 50", "  ),", "  Crabapple = makeIcon(", 
    "    iconUrl = \"all_tree_images/tree_icons/crabapple.png\",", 
    "    iconWidth = 25, iconHeight = 50", "  ),", "  Other = makeIcon(", 
    "    iconUrl = \"all_tree_images/tree_icons/other.png\",", 
    "    iconWidth = 25, iconHeight = 50", "  )", ")", "", 
    "# Processing", "", "tree_shp  <- tree_shp %>% ", "  rowwise %>% ", 
    "  mutate(", "    formatted_names = format_names(planters),", 
    "    ID = str_pad(ID, width = 3, side=\"left\", pad=\"0\")", "  )", "", 
    "#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::", 
    "# The Map ----------------------------------------------------------------------", 
    "#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::", 
    "", "mini_map <- tree_shp %>% filter(ID == my_tree)", 
    "my_coords <- st_coordinates(mini_map)", "", "leaflet(mini_map, ", 
    "        options = leafletOptions(minZoom = 15, maxZoom = 20, maxNativeZoom=20)) %>%", 
    "  addMarkers(", "    icon = ~treeIcons[tree_grp], ", "    popup = ~paste(", 
    "      '<h3>',species,\"</h3>\",", "      \"<hr> </hr>\",", 
    "      \"<table>", "        <tr>", "        <th>Tree ID#:</th>", 
    "        <th>\", ID, \"</th>", "        </tr>\",", "      \"<tr>", 
    "        <td>Diameter:</td>", "        <td>\", dbh, \"in.</td>", 
    "        </tr>\",", "      \"<tr>", 
    "        <td>Carbon Sequestration: &nbsp; &nbsp; &nbsp;</td>", 
    "        <td>\", c_stor_1, \"lbs</td>", "        </tr>\",", 
    "      \"<tr>", "        <td>Avoided Runoff:</td>", 
    "        <td>\", runoff_1, \"ft.<sup>3</sup>/yr</td>", 
    "        </tr>\",", "      \"<tr>", "        <td>Air Pollution Removal:</td>", 
    "        <td>\", poll_1, \"oz/yr</td>", "        </tr>\",", "      \"<tr>", 
    "        <td>Planted By:</td>", "        <td>\", formatted_names, \"</td>", 
    "        </tr>\",", "      \"</table>\"", "    )", "  ) %>% ", 
    "  addResetMapButton() %>%", "  addFullscreenControl()%>% ", 
    "  addProviderTiles(providers$OpenStreetMap.France) %>% ", 
    "  setView(lat = my_coords[2], lng = my_coords[1], zoom = 18)", "", 
    "```"
  )
  
  return(map_code)
}

table_builder <- function(i){
  table_code <- c(
    "```{r}",                                                                             
    "#| error: false",                                                                     
    "#| echo: false",                                                                      
    "#| message: false",                                                                   
    "#| warning: false",                                                                   
    paste("my_tree <- '", df$TreeID[i], "'", sep=""),                                                               
    "library(tidyverse)",                                                                  
    "library(sf)",                                                                         
    "library(knitr)",                                                                      
    "",                                                                                    
    "tree_shp <- st_read(\"all_tree_images/coe_tree_archive.shp\", quiet = TRUE)",        
    "",                                                                                    
    "df <- st_drop_geometry(tree_shp) %>% ",                                               
    "  filter(str_pad(ID, width = 3, side = \"left\", pad = \"0\") == my_tree) %>% ",      
    "  select(",                                                                           
    "    plant, dbh, rep_val, c_stor_1, gr_c_1, runoff_1, poll_1",                         
    "  ) %>% ",                                                                            
    "  pivot_longer(cols = 2:7, values_to = 'i-Tree Eco Estimate', names_to = 'Variable')",
    "",                                                                                    
    "df$Variable <- c(",                                                                   
    "  'Diameter at Breast Height (inches)',",                                             
    "  'Replacement Value ($)',",                                                          
    "  'Carbon Storage (lbs)',",                                                           
    "  'Gross Carbon Removal (lbs/year)',",                                                
    "  'Avoided Runoff (ft<sup>3</sup>/year)',",                                           
    "  'Air Pollution Removal (oz/year)'",                                                 
    ")",                                                                                   
    "",                                                                                    
    "df0 <- df[1,]",                                                                       
    "colnames(df0) <- c('Measurement Date', 'Variable', 'Measured Value')",                
    "",                                                                                    
    "df1 <- df[2:6,]",                                                                     
    "colnames(df1) <- c('Measurement Date', 'Variable', 'i-Tree Eco Estimated Value')",    
    "```"
  )
  
  return(table_code)
}

for (i in 1:length(df$TreeID)){
  
  my_tree <- df$TreeID[i]
  
  # Write Names of Planters
  if(is.na(df$Planters[i])){
    names <- "Unknown"
  } else {
    names <- df$Planters[i]
  }
  
  # # Write the i-Tree Eco Estimate lines
  # if (df$Species[i] == "Unidentified"){
  #   i_tree_lines <- c(
  #     "\n Replacement Value: Not Available",
  #     "\n Carbon Storage: Not Available", 
  #     "\n Gross Carbon Removal: Not Available",
  #     "\n Avoided Runoff: Not Available",
  #     "\n Air Pollution Removal: Not Available"
  #   )
  # } else {
  #   i_tree_lines <- c(
  #     paste("\n Replacement Value: $", df$`Replacement Value ($)`[i], sep = ""),
  #     paste("\n Carbon Storage: ", df$`Carbon Storage (lb)`[i], " lbs"),
  #     paste("\n Gross Carbon Removal: ", df$`Gross Carbon (lb/yr)`[i], " lbs/year"),
  #     paste("\n Avoided Runoff: ", df$`Avoided Runoff (ft3/yr)`[i], " ft<sup>3</sup>/year"),
  #     paste("\n Air Pollution Removal: ", df$`Removed Pollution (oz/yr)`[i], " oz/year")
  #   )
  # }
  
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
    paste('author: ', '"', df$Name[i], '"', sep = ""),
    "author-title: 'Tree Name'",
    "---",
    "",
    "<br>",
    "",
    paste("\nPlanted By: ", names),
    "",
    table_builder(i),
    "## **Size Measurements**",
    "",
    table0_lines,
    "",
    "## **i-Tree Eco Estimates**",
    "",
    table1_lines,
    "",
    "[Read more about these estimates](https://www.itreetools.org/support/resources-overview/i-tree-methods-and-files).",
    "",
    "## **Location**",
    "",
    paste("Latitude, Longitude: ", df$latitude[i], ", ", df$longitude[i], sep=""),
    "",
    map_builder(i),
    "",
    "## **Images**",
    "",
    image_lines,
    "",
    "::: column-margin",
    '<center><a class="button" href="https://forms.gle/W6f5nwYxrM33hF4f7">Notice a Mistake?</a></center>',
    ":::"
  )
  
  writeLines(my_lines, fileConn)
  
  close(fileConn)
  
}

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
