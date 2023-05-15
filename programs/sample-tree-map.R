#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Coe Tree Archive Map ---------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# 
# Contributor(s) : Evan Perry
# Last Revised : 2023-05-12
# Version : 2
# 
# Purpose : 
#   Create the main tree map. Note, this does not generate the actual map on the
#   site, this just a separate example.
# 
# Inputs :
#   - data/coe-tree-archive-all-years.xlsx
# 
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## Code to Change When Replicating ---------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

setwd("C:/Users/eaper/CoeTreeArchive.github.io")

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## Set-Up ----------------------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# All packages needed
my_packages <- c(
  'tidyverse',
  'sf',
  'openxlsx',
  'tmap',
  'leaflet',
  'leaflet.extras'
)

# Install any packages not already installed
new_packages <- my_packages[!(my_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)){
  install.packages(new_packages)
}
# Library all packages
lapply(my_packages, library, character.only = TRUE)

# Reduce scientific notation
options(scipen = 9)

# Remove unnecessary objects
rm(new_packages, my_packages)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## Formatting Set Up -----------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

tree_shp <- read.xlsx("data/coe-tree-archive-all-years.xlsx", detectDates = T) 

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
    iconUrl = "images/map-icons/oak.png", iconWidth = 25, iconHeight = 50),
  Maple = makeIcon(
    iconUrl = "images/map-icons/maple.png", iconWidth = 25, iconHeight = 50), 
  `Honey Locust` = makeIcon(
    iconUrl = "images/map-icons/honeylocust.png", iconWidth = 25, iconHeight = 50),
  Elm = makeIcon(
    iconUrl = "images/map-icons/elm.png", iconWidth = 25, iconHeight = 50), 
  Ash = makeIcon(
    iconUrl = "images/map-icons/ash.png", iconWidth = 25, iconHeight = 50),
  Evergreen = makeIcon(
    iconUrl = "images/map-icons/evergreen.png", iconWidth = 25, iconHeight = 50),
  Crabapple = makeIcon(
    iconUrl = "images/map-icons/crabapple.png", iconWidth = 25, iconHeight = 50),
  Other = makeIcon(
    iconUrl = "images/map-icons/other.png", iconWidth = 25, iconHeight = 50)
)

icon_legend <- "<img src='https://github.com/CoeTreeArchive/CoeTreeArchive.github.io/raw/main/data%20analysis/raw%20data/tree_icons/oak.png'
style='width:15px;height:30px;'>\tOak<br/>

<img src='https://github.com/CoeTreeArchive/CoeTreeArchive.github.io/raw/main/data%20analysis/raw%20data/tree_icons/maple.png'
style='width:15px;height:30px;'>\tMaple<br/>

<img src='https://github.com/CoeTreeArchive/CoeTreeArchive.github.io/raw/main/data%20analysis/raw%20data/tree_icons/honeylocust.png'
style='width:15px;height:30px;'>\tHoney Locust<br/>

<img src='https://github.com/CoeTreeArchive/CoeTreeArchive.github.io/raw/main/data%20analysis/raw%20data/tree_icons/elm.png' style='width:15px;height:30px;'>\tElm<br/>

<img src='https://github.com/CoeTreeArchive/CoeTreeArchive.github.io/raw/main/data%20analysis/raw%20data/tree_icons/ash.png' style='width:15px;height:30px;'>\tAsh<br/>

<img src='https://github.com/CoeTreeArchive/CoeTreeArchive.github.io/raw/main/data%20analysis/raw%20data/tree_icons/evergreen.png'
style='width:15px;height:30px;'>\tEvergreen<br/>

<img src='https://github.com/CoeTreeArchive/CoeTreeArchive.github.io/raw/main/data%20analysis/raw%20data/tree_icons/crabapple.png'
style='width:15px;height:30px;'>\tCrabapple<br/>

<img src='https://github.com/CoeTreeArchive/CoeTreeArchive.github.io/raw/main/data%20analysis/raw%20data/tree_icons/other.png'
style='width:15px;height:30px;'>\tOther
"

# Processing
tree_shp  <- tree_shp %>% 
  rowwise %>% 
  mutate(
    formatted_names = format_names(Planters),
    Name = ifelse(is.na(Name), 'Unnamed', Name),
    my_url = paste(
      "https://coetreearchive.github.io/tree-profiles/tree_", Tree.ID, ".html", 
      sep="")
  )

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# The Map ----------------------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

tree_shp <- st_as_sf(tree_shp, coords = c('Longitude','Latitude'), 
                     crs = 'WGS84')

leaflet(tree_shp, 
        options = leafletOptions(minZoom = 15, maxZoom = 20, maxNativeZoom=20)) %>%
  addMarkers(
    icon = ~treeIcons[Species.Group], 
    group = ~Series,
    popup = ~paste(
      "<h3>", Identification, "</h3>",
      "<hr> </hr>",
      "<table>
        <tr>
        <th>Tree ID#:</th>
        <th>", Tree.ID, "</th>
        </tr>",
      "<tr>
        <td>Name:</td>
        <td>", Name, "</td>
        </tr>",
      "<tr>
        <td>Diameter (in):</td>
        <td>", `DBH.(in)`, "</td>
        </tr>",
      "<tr>
        <td>Carbon Storage (lbs):</td>
        <td>", `Carbon.Storage.(lb)`, "</td>
        </tr>",
      "<tr>
        <td>Avoided Runoff (gal/year):</td>
        <td>", `Avoided.Runoff.(gal/yr)`, "</td>
        </tr>",
      "<tr>
        <td>Pollution Removal (oz/year):</td>
        <td>", `Pollution.Removal.(oz/yr)`, "</td>
        </tr>",
      "<tr>
        <td>Planted By:</td>
        <td>", formatted_names, "</td>
        </tr>",
      "</table>",
      "<a href=", my_url, ">More About This Tree</a>"
    )
  ) %>% 
  addResetMapButton() %>%
  addFullscreenControl()%>% 
  addProviderTiles(providers$OpenStreetMap.France) %>% 
  addControl(html = icon_legend, position = "bottomleft") %>% 
  addLayersControl(overlayGroups = unique(tree_shp$Series))




