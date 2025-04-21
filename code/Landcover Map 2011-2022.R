library(raster)
library(dplyr)
library(ggplot2)
library(stringr)

# --- 1. Data Loading and Setup ---
data_dir <- "C:/Users/sky_c/Desktop/DSS5103 Geospatial Data Analysis/Regional Wx Data/Indonesia land cover/LC 2011-2022 Sumatra"
output_dir <- "C:/Users/sky_c/Desktop/DSS5103 Geospatial Data Analysis/CA/DSS5103 CA3&4"

land_cover_df <- data.frame(
  Class = c("No Data", "Agriculture", "Agriculture", "Agriculture", "Agriculture", "Agriculture", "Agriculture",
            "Forest", "Forest", "Forest", "Forest", "Forest", "Forest", "Forest", "Forest", "Forest", "Forest",
            "Forest", "Forest", "Grassland", "Shrubland", "Shrubland", "Shrubland", "Grassland",
            "Sparse Vegetation", "Sparse Vegetation", "Sparse Vegetation", "Sparse Vegetation", "Sparse Vegetation",
            "Forest", "Forest", "Wetland", "Settlement", "Bare Area", "Bare Area", "Bare Area", "Water", "Snow and Ice"),
  Value = c(0, 10, 11, 12, 20, 30, 40, 50, 60, 61, 62, 70, 71, 72, 80, 81, 82, 90, 100, 110, 120, 121, 122, 130, 140, 150, 151, 152, 153, 160, 170, 180, 190, 200, 201, 202, 210, 220),
  R = c(0, 255, 255, 255, 170, 220, 200, 0, 0, 0, 0, 0, 0, 0, 40, 40, 40, 120, 140, 190, 150, 150, 150, 255, 255, 255, 255, 255, 255, 0, 0, 0, 195, 255, 255, 255, 0, 255),
  G = c(0, 255, 255, 255, 240, 240, 200, 100, 160, 160, 160, 60, 60, 60, 80, 80, 80, 130, 160, 150, 100, 100, 100, 180, 220, 235, 235, 235, 235, 120, 150, 220, 20, 245, 245, 245, 70, 255),
  B = c(0, 100, 100, 100, 240, 100, 100, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 50, 210, 175, 175, 175, 175, 90, 120, 130, 0, 215, 215, 215, 200, 255)
)

raster_files <- list.files(data_dir, pattern = "\\.tif$", full.names = TRUE)
raster_years <- as.numeric(gsub("Land Cover (\\d{4}).*", "\\1", basename(raster_files)))
raster_list <- lapply(raster_files, raster::raster)

calculate_area_class <- function(raster_obj, land_cover_df) {
  raster_values <- values(raster_obj)
  valid_values <- raster_values[raster_values != 0]
  total_valid_pixels <- length(valid_values)
  
  class_percentages <- table(raster_values) / total_valid_pixels * 100
  class_percentages <- class_percentages[names(class_percentages) != "0"]
  
  year <- raster_years[which(sapply(raster_list, identical, raster_obj))]
  
  data.frame(
    Year = year,
    Value = as.numeric(names(class_percentages)),
    Percentage = as.vector(class_percentages)
  ) %>%
    inner_join(land_cover_df, by = "Value")
}

area_class_df <- do.call(rbind, lapply(raster_list, calculate_area_class, land_cover_df = land_cover_df))

process_and_plot <- function(raster_path, output_dir, land_cover_df) {
  year <- as.integer(str_extract(basename(raster_path), "\\d{4}"))
  raster_data <- raster(raster_path)
  raster_df <- as.data.frame(rasterToPoints(raster_data))
  colnames(raster_df) <- c("x", "y", "Value")
  raster_df <- merge(raster_df, land_cover_df, by = "Value", all.x = TRUE)
  raster_df$Value <- as.factor(raster_df$Value)
  
  color_mapping <- setNames(
    mapply(function(r, g, b) rgb(r, g, b, maxColorValue = 255),
           land_cover_df$R[match(levels(raster_df$Value), land_cover_df$Value)],
           land_cover_df$G[match(levels(raster_df$Value), land_cover_df$Value)],
           land_cover_df$B[match(levels(raster_df$Value), land_cover_df$Value)]),
    levels(raster_df$Value)
  )
  color_mapping["0"] <- "transparent"
  
  percentages <- area_class_df %>%
    filter(Year == year) %>%
    group_by(Class) %>%
    summarise(Total_Percentage = sum(Percentage)) %>%
    mutate(label = paste0(Class, " (", round(Total_Percentage, 2), "%)")) %>%
    ungroup()
  
  present_values <- as.numeric(levels(raster_df$Value))
  present_labels <- percentages$label[match(land_cover_df$Class[match(present_values, land_cover_df$Value)], percentages$Class)]
  present_labels[is.na(present_labels)] <- paste(land_cover_df$Class[match(present_values[is.na(present_labels)], land_cover_df$Value)], "(0%)")
  
  # Remove "No Data (0%)" from legend
  legend_values <- present_values[present_labels != "No Data (0%)"]
  legend_labels <- present_labels[present_labels != "No Data (0%)"]
  
  # Remove duplicate classes from legend labels
  unique_labels <- !duplicated(sub(" \\(.*", "", legend_labels))
  legend_values <- legend_values[unique_labels]
  legend_labels <- legend_labels[unique_labels]
  
  custom_labels <- paste("\u25A0", legend_labels)
  
  landcover_map <- ggplot(raster_df) +
    geom_tile(aes(x = x, y = y, fill = Value)) +
    scale_fill_manual(values = color_mapping[as.character(present_values)], labels = custom_labels, drop = FALSE, breaks = as.character(legend_values)) +  # Corrected breaks
    labs(title = paste("Land Cover of Sumatra", year), fill = "Land Cover Class (Percentage)") +
    theme_minimal() +
    coord_quickmap() +
    scale_x_continuous(expand = c(0, 0), name = "Longitude", breaks = seq(95, 107, 2)) +
    scale_y_continuous(expand = c(0, 0), name = "Latitude", breaks = seq(-7, 7, 2)) +
    guides(fill = guide_legend(override.aes = list(shape = 22, size = 8, color = color_mapping[as.character(legend_values)]), label.hjust = 0))
  
  ggsave(file.path(output_dir, paste0("landcover_", year, ".png")), plot = landcover_map, width = 10, height = 8, dpi = 300)
  print(paste("Saved map to:", file.path(output_dir, paste0("landcover_", year, ".png"))))
}

lapply(raster_files, process_and_plot, output_dir = output_dir, land_cover_df = land_cover_df)

