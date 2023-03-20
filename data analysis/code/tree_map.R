#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Coe Tree Archive Map ---------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# 
# Contributor(s) : Evan Perry
# Last Revised : December 12, 2022
# Version : 
# 
# Purpose : 
#   Create the tree map
# 
# Inputs :
#   - cleaned data/coe_tree_archive.shp
# 
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## Setup -----------------------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Importing packages
library(tidyverse)
library(sf)
library(tmap)
library(leaflet)
library(leaflet.extras)

# Setting working directory
setwd("C:/Users/eaper/Tree Census/CoeTreeArchive/data analysis")

# Remove scientific notation
options(scipen = 999)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## Formatting Set Up -----------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

tree_shp <- st_read("cleaned data/coe_tree_archive.shp") 

# Formatting the names of planters
format_names <- function(x){
  if(is.na(x)){
    output = "Unknown"
  } else {
    output = str_replace_all(x, pattern = ", ", replacement="<br>")
    output = paste("<br>", output, sep="")
  }
  return(output)
}

# Setting up the tree icons
treeIcons <- iconList(
  Oak = makeIcon(
    iconUrl = "raw data/tree_icons/oak.png",
    iconWidth = 25, iconHeight = 50
  ),
  Maple = makeIcon(
    iconUrl = "raw data/tree_icons/maple.png",
    iconWidth = 25, iconHeight = 50
  ),
  `Honey Locust` = makeIcon(
    iconUrl = "raw data/tree_icons/honeylocust.png",
    iconWidth = 25, iconHeight = 50
  ),
  Elm = makeIcon(
    iconUrl = "raw data/tree_icons/elm.png",
    iconWidth = 25, iconHeight = 50
  ),
  Ash = makeIcon(
    iconUrl = "raw data/tree_icons/ash.png",
    iconWidth = 25, iconHeight = 50
  ),
  Evergreen = makeIcon(
    iconUrl = "raw data/tree_icons/evergreen.png",
    iconWidth = 25, iconHeight = 50
  ),
  Crabapple = makeIcon(
    iconUrl = "raw data/tree_icons/crabapple.png",
    iconWidth = 25, iconHeight = 50
  ),
  Other = makeIcon(
    iconUrl = "raw data/tree_icons/other.png",
    iconWidth = 25, iconHeight = 50
  )
)

icon_legend <- "<img src='https://github.com/EAPerry/coe-tree-archive/raw/main/tree_icons/oak.png'
style='width:15px;height:30px;'>\tOak<br/> 

<img src='https://github.com/EAPerry/coe-tree-archive/raw/main/tree_icons/maple.png'  
style='width:15px;height:30px;'>\tMaple<br/> 

<img src='https://github.com/EAPerry/coe-tree-archive/raw/main/tree_icons/honeylocust.png'  
style='width:15px;height:30px;'>\tHoney Locust<br/> 

<img src='https://github.com/EAPerry/coe-tree-archive/raw/main/tree_icons/elm.png'  
style='width:15px;height:30px;'>\tElm<br/> 

<img src='https://github.com/EAPerry/coe-tree-archive/raw/main/tree_icons/ash.png'  
style='width:15px;height:30px;'>\tAsh<br/> 

<img src='https://github.com/EAPerry/coe-tree-archive/raw/main/tree_icons/evergreen.png'  
style='width:15px;height:30px;'>\tEvergreen<br/> 

<img src='https://github.com/EAPerry/coe-tree-archive/raw/main/tree_icons/crabapple.png'  
style='width:15px;height:30px;'>\tCrabapple<br/> 

<img src='https://github.com/EAPerry/coe-tree-archive/raw/main/tree_icons/other.png'  
style='width:15px;height:30px;'>\tOther
"

# Processing

tree_shp  <- tree_shp %>% 
  rowwise %>% 
  mutate(
    formatted_names = format_names(planters)
  )

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# The Map ----------------------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

leaflet(tree_shp, options = leafletOptions(minZoom = 15, maxZoom = 20, maxNativeZoom=20)) %>%
  addMarkers(
    icon = ~treeIcons[tree_grp], 
    popup = ~paste(
      "<h3>", species, "</h3>",
      "<hr> </hr>",
      "<table>
        <tr>
        <th>Tree ID#:</th>
        <th>", ID, "</th>
        </tr>",
      "<tr>
        <td>Diameter:</td>
        <td>", dbh, "in.</td>
        </tr>",
      "<tr>
        <td>Carbon Sequestration:</td>
        <td>", c_stor_1, "lbs</td>
        </tr>",
      "<tr>
        <td>Avoided Runoff:</td>
        <td>", runoff_1, "ft.<sup>3</sup>/yr</td>
        </tr>",
      "<tr>
        <td>Pollution Removal:</td>
        <td>", poll_1, "oz/yr</td>
        </tr>",
      "<tr>
        <td>Planted By:</td>
        <td>", formatted_names, "</td>
        </tr>",
      "</table>"
    )
  ) %>% 
  addResetMapButton() %>% 
  addProviderTiles(providers$OpenStreetMap.France) %>% 
  addControl(html = icon_legend, position = "bottomleft") 




