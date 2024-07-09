library(leaflet)
library(sf)
library(dplyr)

# Assuming 'mdl.ds' contains your data and 'zip_code_sf' is your sf template for the area
zip_code_sf <- readRDS("CSVS service area with Zipcode and Population  with sf and shp.RDS")

# Ensure ZIP codes are in the correct format (first 5 digits only)
mdl.ds.map <- readRDS("mdl.ds.RDS") %>%
    group_by(zip) %>%
    summarise(count = n()) %>%
    head(., -1) 


# Ensure that 'zip' in mdl.ds.map and 'ZCTA5CE20' in zip_code_sf are correctly formatted
mdl.ds.map$zip <- as.character(mdl.ds.map$zip)

# Perform the join, ensuring that the sf object (zip_code_sf) is on the left
mdl.ds.map.joined <- zip_code_sf %>%
    left_join(mdl.ds.map, by = c("ZCTA5CE20" = "zip"))

# Check the structure of the joined data to confirm it retains its sf properties
#str(mdl.ds.map.joined)

library(leaflet)
library(sf)
library(RColorBrewer)

# Assuming mdl.ds.map.joined is your sf object prepared earlier

# Create a color palette function for dynamic coloring based on 'count'
colorPalette <- colorQuantile(palette = "YlOrRd", domain = mdl.ds.map.joined$count, n = 5)

leaflet(data = mdl.ds.map.joined) %>%
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
