#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Write Tree Profiles ----------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
# Contributor(s) : Evan Perry
# Last Revised : 2023-05-12
# Version : 2
#
# Purpose :
#   Generate all the .qmd files that are in the tree profiles page
#
# Inputs :
#   - images/Tree-Profiles/
#   - data/coe-tree-archive-all-years.xlsx
#
# Outputs :
#   - tree-profiles/
#
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


# Function definition

write_tree_profiles <- function(my_directory) {
  
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ## Setup ---------------------------------------------------------------------
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  # All packages needed
  my_packages <- c('tidyverse',
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
  
  # Remove unnecessary objects
  rm(new_packages, my_packages)
  
  # Set working directory to the project location
  my_directory = sub('/data.*', '', my_directory)
  setwd(my_directory)
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ## Read in the Data ----------------------------------------------------------
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  df <-read.xlsx('data/coe-tree-archive-all-years.xlsx', detectDates = T)
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ## Enrich Data ---------------------------------------------------------------
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  # Read in the data on each tree
  df <- df %>% mutate(Name = ifelse(is.na(Name), "Unnamed", Name))
  
  # Add a variable indicating whether or not a tree has pictures associated
  tree_w_pics <-
    list.dirs('images/tree-profiles',
              recursive = T,
              full.names = F)
  tree_w_pics <- sub(".*/", "", tree_w_pics)
  tree_w_pics <- na.omit(suppressWarnings(as.numeric(tree_w_pics)))
  tree_w_pics <-
    str_pad(tree_w_pics,
            width = 3,
            side = 'left',
            pad = '0')
  df$photo <- ifelse(df$`Tree.ID` %in% tree_w_pics, 1, 0)
  
  # Set image to display for tree profile preview
  tree_pics <- list.files(
    'images/tree-profiles',
    all.files = T,
    full.names = F,
    recursive = T
  )
  
  # Gets the file path to the most recent preview pic
  find_cover_pic <- function(TreeID) {
    my_pattern = paste(TreeID, '/main', sep = '')
    pic_tree_k = tree_pics[str_detect(tree_pics, pattern = my_pattern)]
    pic_ind = sub("/.*", "", pic_tree_k)
    pic_ind = which.max(as.numeric(str_sub(
      pic_ind, start = -4, end = -1
    )))
    
    my_pic = paste('/images/tree-profiles/', pic_tree_k[pic_ind], sep = '')
    return(my_pic)
    
  }
  
  df <- df %>%
    rowwise() %>%
    mutate(
      cover_pic = ifelse(
        photo == 1,
        find_cover_pic(Tree.ID),
        '../images/tree-profiles/not-available.PNG'
      )
    )
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ## Writing the Files ---------------------------------------------------------
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
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
  
  map_builder <- function(my_tree) {
    map_code <- c(
      "```{r}",
      "#| error: false",
      "#| echo: false",
      "#| message: false",
      "#| warning: false",
      paste("my_tree <- '", my_tree, "'", sep = ""),
      "# Importing packages",
      "library(tidyverse)",
      "library(sf)",
      "library(tmap)",
      "library(leaflet)",
      "library(openxlsx)",
      "library(leaflet.extras)",
      "",
      "# Remove scientific notation",
      "options(scipen = 999)",
      "",
      "#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::",
      "",
      "",
      "#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::",
      "## Formatting Set Up -----------------------------------------------------------",
      "#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::",
      "",
      "tree_shp <- read.xlsx(\"../data/coe-tree-archive-all-years.xlsx\",
      detectDates = TRUE)",
      "tree_shp <- st_as_sf(tree_shp, coords = c(\"Longitude\", \"Latitude\"),
                            crs = \"WGS84\")",
      "",
      "# Formatting the names of planters",
      "format_names <- function(x){",
      "  if(is.na(x)){",
      "    output = \"Unknown\"",
      "  } else {",
      "    output = str_replace_all(x, pattern = \", \", replacement=\"<br>\")",
      "    output = paste(\"<br>\", output, sep=\"\")",
      "  }",
      "  return(output)",
      "}",
      "",
      "# Setting up the tree icons",
      "treeIcons <- iconList(",
      "  Oak = makeIcon(",
      "    iconUrl = \"../images/map-icons/oak.png\",",
      "    iconWidth = 25, iconHeight = 50",
      "  ),",
      "  Maple = makeIcon(",
      "    iconUrl = \"../images/map-icons/maple.png\",",
      "    iconWidth = 25, iconHeight = 50",
      "  ),",
      "  `Honey Locust` = makeIcon(",
      "    iconUrl = \"../images/map-icons/honeylocust.png\",",
      "    iconWidth = 25, iconHeight = 50",
      "  ),",
      "  Elm = makeIcon(",
      "    iconUrl = \"../images/map-icons/elm.png\",",
      "    iconWidth = 25, iconHeight = 50",
      "  ),",
      "  Ash = makeIcon(",
      "    iconUrl = \"../images/map-icons/ash.png\",",
      "    iconWidth = 25, iconHeight = 50",
      "  ),",
      "  Evergreen = makeIcon(",
      "    iconUrl = \"../images/map-icons/evergreen.png\",",
      "    iconWidth = 25, iconHeight = 50",
      "  ),",
      "  Crabapple = makeIcon(",
      "    iconUrl = \"../images/map-icons/crabapple.png\",",
      "    iconWidth = 25, iconHeight = 50",
      "  ),",
      "  Other = makeIcon(",
      "    iconUrl = \"../images/map-icons/other.png\",",
      "    iconWidth = 25, iconHeight = 50",
      "  )",
      ")",
      "",
      "# Processing",
      "",
      "tree_shp  <- tree_shp %>% ",
      "  rowwise %>% ",
      "  mutate(",
      "    formatted_names = format_names(Planters),",
      "  )",
      "",
      "#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::",
      "# The Map ----------------------------------------------------------------------",
      "#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::",
      "mini_map <- tree_shp %>% filter(Tree.ID == my_tree)",
      "mini_map <- mini_map[1,]",
      "my_coords <- st_coordinates(mini_map)",
      "",
      "leaflet(",
      "   mini_map,",
      "    options = leafletOptions(minZoom = 15, maxZoom = 20, maxNativeZoom=20)",
      "  ) %>%",
      "  addMarkers(",
      "    icon = ~treeIcons[Species.Group],",
      "    popup = ~paste(",
      "      \"<h3>\", Identification, \"</h3>\",",
      "      \"<hr> </hr>\",",
      "      \"<table>",
      "    <tr>",
      "    <th>Tree ID#:</th>",
      "    <th>\", Tree.ID, \"</th>",
      "    </tr>\",",
      "      \"</table>\"",
      "    )",
      "  ) %>%",
      "  addResetMapButton() %>%",
      "  addFullscreenControl() %>%",
      "  addProviderTiles(providers$OpenStreetMap.France) %>%",
      "  setView(lat = my_coords[2], lng = my_coords[1], zoom = 18)",
      "",
      "```"
    )
    
    return(map_code)
  }
  
  table_builder <- function(my_tree) {
    table_code <- c(
      "```{r}",
      "#| error: false",
      "#| echo: false",
      "#| message: false",
      "#| warning: false",
      paste("my_tree <- '", my_tree, "'", sep = ""),
      "library(tidyverse)",
      "library(sf)",
      "library(knitr)",
      "library(openxlsx)",
      "",
      "df <- read.xlsx(\"../data/coe-tree-archive-all-years.xlsx\", detectDates = TRUE)",
      "",
      "df <- df %>%",
      "filter(Tree.ID == my_tree) %>%",
      "select(",
      "  Date, `DBH.(in)`, `Height.(ft)`, `Crown.Height.(ft)`, `Crown.Width.(ft)`,",
      "  `Replacement.Value.($)`, `Carbon.Storage.(lb)`,",
      "  `Gross.Carbon.Sequestration.(lb/yr)`, `Avoided.Runoff.(gal/yr)`,",
      "  `Pollution.Removal.(oz/yr)`",
      ")",
      "",
      "var_names <- c(",
      "   'Date',",
      "   'Diameter at Breast Height (inches)',",
      "   'Height (ft)',",
      "   'Crown Height (ft)',",
      "   'Crown Width (ft)',",
      "   'Replacement Value (\\\\$)',",
      "   'Carbon Storage (lbs)',",
      "   'Carbon Removal (lbs/year)',",
      "   'Avoided Runoff (gallons/year)',",
      "   'Air Pollution Removal (oz/year)'",
      ")",
      "",
      "colnames(df) <- var_names",
      "",
      "df <- df %>%",
      "  pivot_longer(cols = 2:10, values_to = 'i-Tree Eco Estimate', names_to = 'Variable')",
      "",
      "df <- df %>%",
      "  mutate(Variable = factor(Variable, levels = var_names[-1])) %>%",
      "  arrange(Variable, Date)",
      "",
      "df0 <- df %>% filter(Variable == 'Diameter at Breast Height (inches)')",
      "colnames(df0) <- c('Measurement Date', 'Variable', 'Measured Value')",
      "",
      "df1 <- df %>% filter(Variable != 'Diameter at Breast Height (inches)')",
      "colnames(df1) <- c('Measurement Date', 'Variable', 'i-Tree Eco Estimated Value')",
      "",
      "```"
    )
    
    return(table_code)
  }
  
  tree_pics <- list.files(
    "images/tree-profiles",
    all.files = T,
    full.names = T,
    recursive = T
  )
  
  # Write the image lines
  tree_images <- function(my_tree) {
    if (temp_df$photo[1] == 0) {
      image_lines = c("No images available at this time.")
    } else {
      my_pattern = paste('/', my_tree, '/', sep = '')
      my_pics = tree_pics[str_detect(tree_pics, pattern = my_pattern)]
      my_series = sub(paste(my_pattern, ".*", sep = ''), "", my_pics)
      my_series = sub('images/tree-profiles/', '', my_series)
      image_lines <-
        paste("\n ![", my_series, "](/",  my_pics, ")", sep = "")
    }
    return(image_lines)
  }
  
  
  unq_trees <- unique(df$Tree.ID)
  
  for (i in 1:length(unq_trees)) {
    my_tree <- unq_trees[i]
    temp_df <- df %>%
      filter(Tree.ID == my_tree)
    temp_df <- temp_df[nrow(temp_df), ]
    
    # Write Names of Planters
    if (is.na(df$Planters[i])) {
      names <- "Unknown"
    } else {
      names <- df$Planters[i]
    }
    
    # Write the file
    outfile <- paste("tree-profiles/tree_", my_tree, ".qmd", sep = "")
    fileConn <- file(outfile)
    
    my_lines <- c(
      "---",
      paste("title: ", '"**', temp_df$Identification[1], '**"', sep = ""),
      paste("description: ", '"Tree ID: ', my_tree, '"', sep = ""),
      paste("image: ", '"', temp_df$cover_pic[1], '"', sep = ""),
      paste('author: ', '"', temp_df$Name[1], '"', sep = ""),
      "author-title: 'Tree Name'",
      "---",
      "",
      "<br>",
      "",
      paste("\nPlanted By: ", names),
      "",
      ifelse(
        is.na(temp_df$Special.Notes[1]),
        "",
        temp_df$Special.Notes[1]
      ),
      "",
      table_builder(my_tree),
      "## **Size Measurements**",
      "",
      table0_lines,
      "",
      "## **i-Tree Eco Estimates**",
      "",
      table1_lines,
      "",
      "",
      "[Read more about these estimates](https://www.itreetools.org/support/resources-overview/i-tree-methods-and-files).",
      "",
      "## **Location**",
      "",
      paste(
        "Latitude, Longitude: ",
        temp_df$Latitude[1],
        ", ",
        temp_df$Longitude[1],
        sep = ""
      ),
      "",
      map_builder(my_tree),
      "",
      "## **Images**",
      "",
      tree_images(my_tree),
      "",
      "::: column-margin",
      '<center><a class="button" href="https://forms.gle/muU6Zn6NZFq4YES9A">Notice a Mistake?</a></center>',
      ":::"
    )
    
    writeLines(my_lines, fileConn)
    
    close(fileConn)
    
  }
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  
}

# End function
