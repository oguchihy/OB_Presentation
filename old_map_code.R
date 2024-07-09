# Load necessary libraries
library(sf)
library(dplyr)
library(leaflet)
library(rmapshaper)

# Read the sf object
mtry_zip_sf <- readRDS("CSVS service area with Zipcode and Population  with sf and shp.RDS")

ob_data_sf <- readRDS("Complete Ready OB Dataset for Analytics.RDS") %>% 
  group_by(zip) %>%
  summarise(count = n()) %>%
  head(., -1) 

# Ensure that 'zip' in mdl.ds.map and 'ZCTA5CE20' in zip_code_sf are correctly formatted
ob_data_sf$zip <- as.character(ob_data_sf$zip)


# Join the dataframe with the sf object based on ZIP code
joined_sf <- mtry_zip_sf %>%
  left_join(ob_data_sf, by = c("ZCTA5CE20" = "zip"))

# Remove rows with NAs in critical columns (e.g., frequency)
joined_sf_clean <- joined_sf %>%
  filter_if(~ any(is.na(.)), all_vars(!is.na(.)))

library(RColorBrewer)

# Assuming mdl.ds.map.joined is your sf object prepared earlier

# Create a color palette function for dynamic coloring based on 'count'
colorPalette <- colorQuantile(palette = "YlOrRd", domain = joined_sf_clean$count, n = 5)

leaflet(data = joined_sf_clean) %>%
  addTiles() %>%
  addPolygons(fillColor = ~colorPalette(count),
              color = "#BDBDC3",
              fillOpacity = 0.7,
              weight = 1,
              opacity = 1,
              highlight = highlightOptions(weight = 3,
                                           color = "#666",
                                           fillOpacity = 0.7,
                                           bringToFront = TRUE),
              label = ~paste("ZIP Code:", ZCTA5CE20, "<br/>Count:", count),
              labelOptions = labelOptions(direction = 'auto', noHide = F, textOnly = TRUE)) %>%
  addLegend(pal = colorPalette, values = ~count, opacity = 0.7, title = "Count",
            position = "bottomright") %>%
  setView(lng = -121.895, lat = 36.674, zoom = 9)  # Adjust the center and zoom level as needed
