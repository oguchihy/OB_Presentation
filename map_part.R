# Load necessary libraries
library(sf)
library(dplyr)
library(leaflet)
library(rmapshaper)

# Read the sf object
mtry_zip_sf <- readRDS("CSVS service area with Zipcode and Population  with sf and shp.RDS")

# Join the dataframe with the sf object based on ZIP code
joined_sf <- mtry_zip_sf %>%
  left_join(ob_data_sf, by = c("ZCTA5CE20" = "zip"))

# Transform the CRS to WGS84 (EPSG:4326)
joined_sf <- st_transform(joined_sf, crs = 4326)

# Simplify the geometries
joined_sf_simplified <- ms_simplify(joined_sf, keep = 0.05)

# Check for invalid geometries
invalid_geometries <- st_is_valid(joined_sf_simplified, reason = TRUE)
print(invalid_geometries)

# Ensure the joined_sf$population column has no NAs
joined_sf_simplified <- joined_sf_simplified %>%
  mutate(population = ifelse(is.na(population), 0, population))

# Step 1: Calculate ZIP code frequency and percentage
zip_frequency <- joined_sf_simplified %>%
  st_set_geometry(NULL) %>%
  count(ZCTA5CE20, name = "frequency") %>%
  mutate(percentage = frequency / sum(frequency) * 100)

# Step 2: Join the frequency data back to the spatial data
joined_sf_simplified <- joined_sf_simplified %>%
  left_join(zip_frequency, by = "ZCTA5CE20")

# Step 3: Define a color palette function for the frequency
pal <- colorNumeric(palette = "viridis", domain = joined_sf_simplified$frequency)

# Step 4: Create a Leaflet map with colored polygons based on frequency and interactive tooltips
leaflet(data = joined_sf_simplified) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~pal(frequency),
    color = "#BDBDC3",
    weight = 1,
    opacity = 1,
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 2,
      color = "#666",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = ~paste("ZIP Code:", ZCTA5CE20, "<br>Frequency:", frequency, "<br>Percentage:", round(percentage, 2), "%"),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
  ) %>%
  addLegend(pal = pal, values = ~frequency, opacity = 0.7, title = "ZIP Code Frequency",
            position = "bottomright")
