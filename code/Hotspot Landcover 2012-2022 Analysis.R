library(raster)
library(ggplot2)
library(dplyr)
library(sf)
library(lubridate)

# --- 1. Data Loading and Setup ---
data_dir <- "C:/Users/sky_c/Desktop/DSS5103 Geospatial Data Analysis/Regional Wx Data/Indonesia land cover/LC 2011-2022 Sumatra"
hotspot_file <- "C:/Users/sky_c/Desktop/DSS5103 Geospatial Data Analysis/Regional Wx Data/Indonesia hotspot/Hotspot 2012-2024 Sumatra.gpkg"
output_dir <- "C:/Users/sky_c/Desktop/DSS5103 Geospatial Data Analysis/CA/DSS5103 CA3&4"

if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

land_cover_df <- data.frame(
  Class = c("No Data", "Agriculture", "Agriculture", "Agriculture", "Agriculture", "Agriculture", "Agriculture", 
            "Forest", "Forest", "Forest", "Forest", "Forest", "Forest", "Forest", "Forest", "Forest", "Forest",
            "Forest", "Forest", "Grassland", "Shrubland", "Shrubland", "Shrubland", "Grassland",
            "Sparse Vegetation", "Sparse Vegetation", "Sparse Vegetation", "Sparse Vegetation", "Sparse Vegetation",
            "Forest", "Forest", "Wetland", "Settlement",
            "Bare Area", "Bare Area", "Bare Area", "Water", "Snow and Ice"),
  Value = c(0, 10, 11, 12, 20, 30, 40, 50, 60, 61, 62, 70, 71, 72, 80, 81, 82, 90, 100, 110, 120, 121, 122, 130, 140, 150, 151, 152, 153, 160, 170, 180, 190, 200, 201, 202, 210, 220),
  Label = c("No Data", "Cropland, rainfed", "Cropland, rainfed", "Cropland, rainfed", "Cropland, irrigated or post-flooding", 
            "Mosaic cropland (>50%) / natural vegetation (tree, shrub, herbaceous cover) (<50%)", 
            "Mosaic natural vegetation (tree, shrub, herbaceous cover) (>50%) / cropland (<50%)", 
            "Tree cover, broadleaved, evergreen, closed to open (>15%)", 
            "Tree cover, broadleaved, deciduous, closed to open (>15%)", 
            "Tree cover, broadleaved, deciduous, closed to open (>15%)",
            "Tree cover, broadleaved, deciduous, closed to open (>15%)", 
            "Tree cover, needleleaved, evergreen, closed to open (>15%)", 
            "Tree cover, needleleaved, evergreen, closed to open (>15%)", 
            "Tree cover, needleleaved, evergreen, closed to open (>15%)", 
            "Tree cover, needleleaved, deciduous, closed to open (>15%)", 
            "Tree cover, needleleaved, deciduous, closed to open (>15%)", 
            "Tree cover, needleleaved, deciduous, closed to open (>15%)", 
            "Tree cover, mixed leaf type (broadleaved and needleleaved)", 
            "Mosaic tree and shrub (>50%) / herbaceous cover (<50%)", 
            "Mosaic herbaceous cover (>50%) / tree and shrub (<50%)", 
            "Shrubland", "Shrubland", "Shrubland", "Grassland", "Lichens and mosses", 
            "Sparse vegetation (tree, shrub, herbaceous cover) (<15%)", "Sparse vegetation (tree, shrub, herbaceous cover) (<15%)", 
            "Sparse vegetation (tree, shrub, herbaceous cover) (<15%)", "Sparse vegetation (tree, shrub, herbaceous cover) (<15%)",
            "Tree cover, flooded, fresh or brackish water", 
            "Tree cover, flooded, saline water", 
            "Shrub or herbaceous cover, flooded, fresh/saline/brackish water", 
            "Urban areas", "Bare areas", "Bare areas", "Bare areas", "Water bodies", "Permanent snow and ice"),
  R = c(0, 255, 255, 255, 170, 220, 200, 0, 0, 0, 0, 0, 0, 0, 40, 40, 40, 120, 140, 190, 150, 150, 150, 255, 255, 255, 255, 255, 255, 0, 0, 0, 195, 255, 255, 255, 0, 255),
  G = c(0, 255, 255, 255, 240, 240, 200, 100, 160, 160, 160, 60, 60, 60, 80, 80, 80, 130, 160, 150, 100, 100, 100, 180, 220, 235, 235, 235, 235, 120, 150, 220, 20, 245, 245, 245, 70, 255),
  B = c(0, 100, 100, 100, 240, 100, 100, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 50, 210, 175, 175, 175, 175, 90, 120, 130, 0, 215, 215, 215, 200, 255)
)

# --- 2. Load and Prepare Hotspot Data ---
hotspot_data <- st_read(hotspot_file) %>%
  mutate(ACQ_DATE = as.Date(ACQ_DATE)) %>%
  filter(year(ACQ_DATE) >= 2012 & year(ACQ_DATE) <= 2022) %>%
  mutate(year = year(ACQ_DATE))

# --- 3. Calculate and Plot Correlation (Improved) ---

# Extract raster values at hotspot locations
hotspot_points <- st_transform(hotspot_data, crs(raster_data))
hotspot_values <- raster::extract(raster_data, st_coordinates(hotspot_points))

# Count hotspots by land cover class and year
landcover_hotspot_counts <- hotspot_points %>%
  mutate(Value = hotspot_values) %>%
  st_drop_geometry() %>%
  group_by(year, Value) %>%
  summarise(hotspot_count = n(), .groups = 'drop') %>%
  inner_join(land_cover_df, by = "Value") %>%
  group_by(year, Class) %>%
  summarise(hotspot_count = sum(hotspot_count), .groups = 'drop')

# Create RGB colors from R, G, B columns, using the first RGB for each class
class_colors <- land_cover_df %>%
  group_by(Class) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(bar_color = rgb(R, G, B, maxColorValue = 255)) %>%
  select(Class, bar_color)

landcover_hotspot_counts <- landcover_hotspot_counts %>%
  left_join(class_colors, by = "Class")

# Remove "No Data" from the data and colors
landcover_hotspot_counts <- landcover_hotspot_counts %>%
  filter(Class != "No Data")
class_colors <- class_colors %>%
  filter(Class != "No Data")

# Generate stacked area plot
landcover_hotspot_area_plot <- ggplot(landcover_hotspot_counts, aes(x = year, y = hotspot_count, fill = Class)) +
  geom_area() +
  labs(title = "Annual Hotspot Count by Land Cover Class",
       x = "Year",
       y = "Hotspot Count",
       fill = "Land Cover Class") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = setNames(class_colors$bar_color, class_colors$Class)) +
  scale_x_continuous(breaks = seq(2012, 2022, 1)) +
  scale_y_continuous(labels = scales::comma)

# Generate overlapping area plot
landcover_hotspot_overlap_plot <- ggplot(landcover_hotspot_counts, aes(x = year, y = hotspot_count, fill = Class)) +
  geom_area(position = "identity", alpha = 0.6) + # Slightly increased alpha
  labs(title = "Overlapping Annual Hotspot Count by Land Cover Class",
       x = "Year",
       y = "Hotspot Count",
       fill = "Land Cover Class") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = setNames(class_colors$bar_color, class_colors$Class)) +
  scale_x_continuous(breaks = seq(2012, 2022, 1)) +
  scale_y_continuous(labels = scales::comma)

# Save the plots
ggsave(filename = paste0(output_dir, "/LandCover_Hotspot_Area.png"), plot = landcover_hotspot_area_plot, width = 12, height = 8, units = "in", dpi = 300)
ggsave(filename = paste0(output_dir, "/LandCover_Hotspot_Overlap.png"), plot = landcover_hotspot_overlap_plot, width = 12, height = 8, units = "in", dpi = 300)
