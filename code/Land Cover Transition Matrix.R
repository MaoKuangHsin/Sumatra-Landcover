library(raster)
library(dplyr)
library(ggplot2)

# --- 1. Data Loading and Setup ---
data_dir <- "C:/Users/sky_c/Desktop/DSS5103 Geospatial Data Analysis/Regional Wx Data/Indonesia land cover/LC 2011-2022 Sumatra"
output_dir <- "C:/Users/sky_c/Desktop/DSS5103 Geospatial Data Analysis/CA/DSS5103 CA3&4"

# Combined data frame with Land Cover Classes, Values, and Labels based on image provided
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

raster_files <- list.files(data_dir, pattern = "\\.tif$", full.names = TRUE)
raster_years <- as.numeric(gsub("Land Cover (\\d{4}).*", "\\1", basename(raster_files)))
raster_list <- lapply(raster_files, raster::raster)

# --- 10. Land Cover Transition Matrix ---
calculate_transition_matrix <- function(raster_list, land_cover_df, year1_index, year2_index) {
  raster_year1 <- raster_list[[year1_index]]
  raster_year2 <- raster_list[[year2_index]]
  
  if (!compareRaster(raster_year1, raster_year2, crs = TRUE, res = TRUE, tolerance = 1e-05)) {
    stop("Rasters for the two years are not compatible.")
  }
  
  matrix_year1 <- as.matrix(raster_year1)
  matrix_year2 <- as.matrix(raster_year2)
  
  aggregated_land_cover_df <- land_cover_df %>%
    distinct(Class, .keep_all = TRUE)
  
  unique_classes <- aggregated_land_cover_df$Class[aggregated_land_cover_df$Class != "No Data"]
  
  transition_matrix <- matrix(0, nrow = length(unique_classes), ncol = length(unique_classes),
                              dimnames = list(unique_classes, unique_classes))
  
  for (i in 1:nrow(matrix_year1)) {
    for (j in 1:ncol(matrix_year1)) {
      from_value <- matrix_year1[i, j]
      to_value <- matrix_year2[i, j]
      if (from_value != 0 && to_value != 0) {
        from_class <- land_cover_df$Class[land_cover_df$Value == from_value]
        to_class <- land_cover_df$Class[land_cover_df$Value == to_value]
        if (from_class != to_class) {
          transition_matrix[from_class, to_class] <- transition_matrix[from_class, to_class] + 1
        }
      }
    }
  }
  
  transition_df_long <- as.data.frame.table(transition_matrix, responseName = "Count") %>%
    rename(From_Label = Var1, To_Label = Var2)
  
  return(transition_df_long)
}

transition_2011_2022 <- calculate_transition_matrix(raster_list, land_cover_df, 1, length(raster_list))

transition_2011_2022 <- transition_2011_2022 %>%
  group_by(From_Label) %>%
  mutate(Total_Change_From = sum(Count[From_Label != To_Label])) %>%
  mutate(Percentage = ifelse(Total_Change_From > 0, (Count / Total_Change_From) * 100, 0)) %>%
  ungroup()

color_gradient <- c("white", "yellow", "orange", "red", "darkred")

transition_matrix_plot <- ggplot(transition_2011_2022, aes(x = From_Label, y = To_Label, fill = Percentage)) +
  geom_tile(color = "black", size = 0.5) +
  geom_text(aes(label = ifelse(From_Label != To_Label, sprintf("%.1f%%", Percentage), "")),
            color = "black", size = 4) + # Increased font size here
  scale_fill_gradientn(colors = color_gradient,
                       guide = guide_colorbar(title.position = "top")) +
  labs(title = "Land Cover Transition Matrix",
       x = "From First Year",
       y = "To Final Year",
       fill = "Percentage Change") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), # Increased font size here
        axis.text.y = element_text(size = 12), # Increased font size here
        title = element_text(size = 14, face = "bold"),
        panel.background = element_rect(fill = "white"),
        legend.title.position = "top",
        panel.border = element_rect(color = "black", fill = NA, size = 1)
  )

# Save the plot
output_file_transition_matrix <- file.path(output_dir, "transition_matrix_plot.png") # Define the output file path
ggsave(output_file_transition_matrix, plot = transition_matrix_plot, width = 14, height = 8, dpi = 300) # Save the plot
print(paste("Saved file transition matrix to:", output_file_transition_matrix)) # Print the saved file path

