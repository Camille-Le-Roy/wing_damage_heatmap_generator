# ______________________________________________________________________________
# Butterfly Wing Damage Heatmap Analysis
# ______________________________________________________________________________
# Author: Camille Le Roy
# Description: Analysis of wing damage frequency patterns in butterfly specimens
#              using geometric morphometrics and image processing techniques
# First developed in 2019, last updated in 2025.
# ______________________________________________________________________________

## install required packages
# install.packages("htmlwidgets")
# install.packages("BiocManager")
# BiocManager::install("EBImage")
# install.packages("tiff", type = "binary")



# Load required libraries
suppressPackageStartupMessages({
  library(MASS)
  library(geomorph)
  library(EBImage)
  library(autoimage)
  library(tiff)
  library(fields)
  library(here)      # for relative paths
  library(tidyverse) # for data manipulation
})

# ______________________________________________________________________________
####  CONFIGURATION ####
# ______________________________________________________________________________


# Set up relative paths (replace with your actual data structure)
# Get the directory of the currently opened script (works in RStudio)
local_path <- dirname(rstudioapi::getSourceEditorContext()$path)

# Set up paths using the local path as the root
config <- list(
  data_dir = file.path(local_path, "data"),
  output_dir = file.path(local_path, "output"),
  wing_data_file = file.path(local_path, "FOUR_wings.tps"),
  id_file = file.path(local_path, "All_ID.csv"),
  model_templates = list(
    forewing = file.path(local_path, "MODEL_achilles31bis_FWL.png"),
    hindwing_left = file.path(local_path, "040_achilles31bis_HWL.png"),
    hindwing_right = file.path(local_path, "041_achilles31bis_HWR.png")
  ),
  image_dirs = list(
    all_wings = file.path(local_path, "wings_for_heatmap"),
    fw_left = file.path(local_path, "FWL_for_heatmap"),
    fw_right = file.path(local_path, "FWR_for_heatmap"),
    hw_all = file.path(local_path, "hindwings_for_heatmap"),
    hw_left = file.path(local_path, "HWL_for_heatmap"),
    hw_right = file.path(local_path, "HWR_for_heatmap")
  )
)


# Color thresholds for image processing
COLOR_THRESHOLDS <- list(
  red = 0.7294118,
  black = 0.0000000,
  background = 1.0000000
)

# Create output directories if they don't exist
if (!dir.exists(config$output_dir)) {
  dir.create(config$output_dir, recursive = TRUE)
}

# ______________________________________________________________________________
#### UTILITY FUNCTIONS ####
# ______________________________________________________________________________

#' Flip wing image to correct orientation
#' @param x Matrix representing the wing image
#' @return Flipped matrix
flip_wing <- function(x) {
  x2 <- apply(x, 1, rev)
  t(x2)
}

#' Clean image pixels by removing unwanted colors
#' @param img_matrix Matrix representing the image
#' @param red_value The red color value to preserve
#' @return Cleaned matrix
clean_image_pixels <- function(img_matrix, red_value) {
  for (i in seq_len(nrow(img_matrix))) {
    for (j in seq_len(ncol(img_matrix))) {
      if (img_matrix[i, j] != red_value &&
          img_matrix[i, j] != COLOR_THRESHOLDS$black &&
          img_matrix[i, j] != COLOR_THRESHOLDS$background) {
        img_matrix[i, j] <- COLOR_THRESHOLDS$black
      }
    }
  }
  img_matrix
}

#' Plot individual wing shape
#' @param wing_data 3D array of wing landmark data
#' @param specimen_id Specimen ID data frame
#' @param index Index of specimen to plot
#' @param wing_type Type of wing (for title)
plot_wing_shape <- function(wing_data, specimen_id, index, wing_type = "Wing") {
  plot(wing_data[, , index], 
       pch = 16, 
       type = 'l', 
       lwd = 2, 
       col = 'black', 
       cex = 0.7, 
       axes = FALSE, 
       xlab = '', 
       ylab = '', 
       asp = TRUE, 
       main = paste(specimen_id[index, 2], "-", specimen_id[index, 5], "(", wing_type, ")", sep = " ")
  )
}

#' Save wing shape plots as PDF series
#' @param wing_data 3D array of wing landmark data
#' @param specimen_id Specimen ID data frame
#' @param indices Vector of indices to plot
#' @param output_dir Output directory path
#' @param prefix File prefix for output files
#' @param wing_type Wing type for titles
save_wing_series <- function(wing_data, specimen_id, indices, output_dir, prefix, wing_type) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  cat(paste("Saving", length(indices), wing_type, "plots...\n"))
  
  for (i in indices) {
    filename <- paste0(prefix, "_", specimen_id[i, 2], "_", specimen_id[i, 5], ".pdf")
    filepath <- file.path(output_dir, filename)
    
    pdf(filepath)
    plot_wing_shape(wing_data, specimen_id, i, wing_type)
    dev.off()
    
    if (i %% 10 == 0) {
      cat(paste("Processed", i, "/", max(indices), "files\n"))
    }
  }
  
  cat(paste0("Completed saving ", wing_type, " series to ", output_dir, "\n"))
}

# ______________________________________________________________________________
#### HEATMAP GENERATION FUNCTIONS ####
# ______________________________________________________________________________

#' Initialize heatmap matrix from template image
#' @param template_path Path to template image file
#' @return Initialized heatmap matrix
initialize_heatmap <- function(template_path) {
  if (!file.exists(template_path)) {
    stop(paste0("Template file not found: ", template_path)) 
  }
  
  # Load and process template image
  template <- readImage(template_path)
  hmap <- imageData(template)[, , 1]
  hmap <- flip_wing(hmap)
  
  # Set background pixels to 0
  hmap[hmap == 1] <- 0
  
  hmap
}

#' Process a single wing image and add to heatmap
#' @param img_path Path to wing image
#' @param heatmap Current heatmap matrix
#' @param red_value Red color value for this image set
#' @return Updated heatmap matrix
process_wing_image <- function(img_path, heatmap, red_value) {
  if (!file.exists(img_path)) {
    warning(paste0("Image file not found: ", img_path)) 
    return(heatmap)
  }
  
  # Load and process image
  img <- readImage(img_path)
  img_matrix <- imageData(img)[, , 1]
  img_matrix <- flip_wing(img_matrix)
  
  # Clean unwanted pixels
  img_matrix <- clean_image_pixels(img_matrix, red_value)
  
  # Add damage pixels to heatmap
  damage_pixels <- (img_matrix == red_value)
  heatmap[damage_pixels] <- heatmap[damage_pixels] + 1
  
  heatmap
}

#' Generate heatmap for a collection of wing images
#' @param image_dir Directory containing wing images
#' @param template_path Path to template image
#' @param output_name Name for output files
#' @param red_coords Coordinates to extract red color value c(row, col)
#' @return List containing heatmap matrix and summary statistics
generate_wing_heatmap <- function(image_dir, template_path, output_name, red_coords = c(46, 10)) {
  cat(paste0("\n=== Generating heatmap for ", output_name, " ===\n")) 
  
  # Initialize heatmap
  heatmap <- initialize_heatmap(template_path)
  
  # Get list of PNG files
  img_files <- list.files(image_dir, pattern = "\\.png$", full.names = TRUE)
  
  if (length(img_files) == 0) {
    stop(paste0("No PNG files found in directory: ", image_dir)) 
  }
  
  cat(paste0("Found ", length(img_files), " image files\n")) 
  
  # Extract red color value from first image
  first_img <- readImage(img_files[1])
  first_img_matrix <- imageData(first_img)[, , 1]
  first_img_matrix <- flip_wing(first_img_matrix)
  red_value <- first_img_matrix[red_coords[1], red_coords[2]]
  
  cat(paste0("Using red color value: ", red_value, "\n")) 
  
  # Process each image
  for (i in seq_along(img_files)) {
    heatmap <- process_wing_image(img_files[i], heatmap, red_value)
    
    # Progress indicator
    if (i %% 10 == 0 || i == length(img_files)) {
      progress <- round((i / length(img_files)) * 100, 2)
      cat(paste0("Progress: ", progress, "% (", i, "/", length(img_files), " files)\n")) 
    }
  }
  
  # Calculate statistics
  max_damage <- max(heatmap)
  total_specimens <- length(img_files)
  max_frequency_percent <- round((max_damage / total_specimens) * 100, 2)
  
  cat(paste0("Maximum damage frequency: ", max_damage, "/", total_specimens, " (", max_frequency_percent, "%)\n")) 
  
  list(
    heatmap = heatmap,
    max_damage = max_damage,
    total_specimens = total_specimens,
    max_frequency_percent = max_frequency_percent,
    red_value = red_value
  )
}

#' Create and save heatmap visualization
#' @param heatmap_result Result from generate_wing_heatmap()
#' @param output_name Name for output files
#' @param width Width of output in inches
#' @param height Height of output in inches
create_heatmap_plot <- function(heatmap_result, output_name, width = 12, height = 7) {
  heatmap <- heatmap_result$heatmap
  
  # Create filename
  pdf_filename <- file.path(config$output_dir, paste0(output_name, "_heatmap.pdf")) 
  
  # Generate plot
  pdf(pdf_filename, width = width, height = height)
  
  autoimage(
    x = seq(1, dim(heatmap)[1], by = 1),
    y = seq(1, dim(heatmap)[2], by = 1),
    z = heatmap,
    asp = TRUE,
    col = tim.colors(126),
    xlab = "X coordinate",
    ylab = "Y coordinate",
    main = paste0("Wing Damage Frequency Heatmap - ", str_to_title(str_replace_all(output_name, '_', ' '))), 
    sub = paste0("Max frequency: ", heatmap_result$max_frequency_percent, "% (", heatmap_result$max_damage, "/", heatmap_result$total_specimens, " specimens)") 
  )
  
  dev.off()
  
  cat(paste0("Heatmap saved as: ", pdf_filename, "\n")) 
  
  # Also save as RDS for further analysis
  rds_filename <- file.path(config$output_dir, paste0(output_name, "_heatmap_data.rds")) 
  saveRDS(heatmap_result, rds_filename)
  
  cat(paste0("Heatmap data saved as: ", rds_filename, "\n")) 
}

# ______________________________________________________________________________
#### MAIN ANALYSIS WORKFLOW ####
# ______________________________________________________________________________

#' Main function to run the complete wing damage analysis
run_wing_damage_analysis <- function() {
  cat("______________________________________________________________________________\n")
  cat("BUTTERFLY WING DAMAGE HEATMAP ANALYSIS\n")
  cat("______________________________________________________________________________\n")
  
  # Load wing morphometric data (if available)
  wing_data_path <- file.path(config$data_dir, basename(config$wing_data_file))
  id_data_path <- file.path(config$data_dir, basename(config$id_file))
  
  if (file.exists(wing_data_path) && file.exists(id_data_path)) {
    cat("Loading morphometric data...\n")
    wing_data <- readland.tps(wing_data_path)
    specimen_id <- read.csv(id_data_path, header = TRUE, sep = ";")
    
    cat(paste0("Loaded data for ", dim(wing_data)[3], " specimens\n")) 
    
    # Example: Plot a single wing
    cat("Creating example wing plot...\n")
    pdf(file.path(config$output_dir, "example_wing_shape.pdf"))
    plot_wing_shape(wing_data, specimen_id, 178, "Example Wing")
    dev.off()
    
  } else {
    cat("Morphometric data files not found, skipping shape analysis...\n")
  }
  
  # Generate heatmaps for different wing categories
  heatmap_configs <- list(
    list(
      name = "all_forewings",
      dir = file.path(config$data_dir, basename(config$image_dirs$all_wings)),
      template = file.path(config$data_dir,  basename(config$model_templates$forewing)),
      coords = c(46, 10)
    ),
    list(
      name = "forewings_left",
      dir = file.path(config$data_dir,  basename(config$image_dirs$fw_left)),
      template = file.path(config$data_dir,  basename(config$model_templates$forewing)),
      coords = c(46, 10)
    ),
    list(
      name = "forewings_right",
      dir = file.path(config$data_dir,  basename(config$image_dirs$fw_right)),
      template = file.path(config$data_dir,  basename(config$model_templates$forewing)),
      coords = c(46, 10)
    ),
    list(
      name = "all_hindwings",
      dir = file.path(config$data_dir,  basename(config$image_dirs$hw_all)),
      template = file.path(config$data_dir,  basename(config$model_templates$hindwing_left)),
      coords = c(62, 6)
    ),
    list(
      name = "hindwings_left",
      dir = file.path(config$data_dir,  basename(config$image_dirs$hw_left)),
      template = file.path(config$data_dir,  basename(config$model_templates$hindwing_left)),
      coords = c(62, 6)
    ),
    list(
      name = "hindwings_right",
      dir = file.path(config$data_dir,  basename(config$image_dirs$hw_right)),
      template = file.path(config$data_dir,  basename(config$model_templates$hindwing_right)),
      coords = c(62, 6)
    )
  )
  
  # Process each heatmap configuration
  results <- list()
  
  for (config_item in heatmap_configs) {
    if (dir.exists(config_item$dir) && file.exists(config_item$template)) {
      tryCatch({
        result <- generate_wing_heatmap(
          config_item$dir,
          config_item$template,
          config_item$name,
          config_item$coords
        )
        
        create_heatmap_plot(result, config_item$name)
        results[[config_item$name]] <- result
        
      }, error = function(e) {
        cat(paste0("Error processing ", config_item$name, ": ", e$message, "\n")) 
      })
    } else {
      cat(paste0("Skipping ", config_item$name, " - directory or template not found\n")) 
    }
  }
  
  # Save summary results
  if (length(results) > 0) {
    summary_data <- map_dfr(results, function(x) {
      tibble(
        max_damage = x$max_damage,
        total_specimens = x$total_specimens,
        max_frequency_percent = x$max_frequency_percent
      )
    }, .id = "wing_type")
    
    summary_file <- file.path(config$output_dir, "heatmap_summary.csv")
    write_csv(summary_data, summary_file)
    
    cat("\n=== ANALYSIS SUMMARY ===\n")
    print(summary_data)
    cat(paste0("\nSummary saved to: ", summary_file, "\n")) 
  }
  
  cat("\n______________________________________________________________________________\n")
  cat("ANALYSIS COMPLETE\n")
  cat(paste0("Results saved to: ", config$output_dir, "\n")) 
  cat("______________________________________________________________________________\n")
  
  invisible(results)
}

# ______________________________________________________________________________
#### EXECUTION ####
# ______________________________________________________________________________

# Run the analysis (uncomment to execute)
# results <- run_wing_damage_analysis()

# ______________________________________________________________________________
#### ADDITIONAL ANALYSIS FUNCTIONS ####
# ______________________________________________________________________________

#' Compare damage patterns between left and right wings
#' @param results Results list from main analysis
compare_bilateral_damage <- function(results) {
  if ("forewings_left" %in% names(results) && "forewings_right" %in% names(results)) {
    cat("Comparing bilateral forewing damage patterns...\n")
    
    fw_left <- results$forewings_left$heatmap
    fw_right <- results$forewings_right$heatmap
    
    # Calculate correlation
    correlation <- cor(as.vector(fw_left), as.vector(fw_right))
    cat(paste0("Forewing L-R damage correlation: ", round(correlation, 3), "\n")) 
    
    # Create difference map
    diff_map <- fw_left - fw_right
    
    pdf(file.path(config$output_dir, "forewing_bilateral_comparison.pdf"), width = 15, height = 5)
    par(mfrow = c(1, 3))
    
    image(fw_left, main = "Left Forewing Damage", col = tim.colors(126))
    image(fw_right, main = "Right Forewing Damage", col = tim.colors(126))
    image(diff_map, main = "Difference (L - R)", col = tim.colors(126))
    
    dev.off()
  }
}

#' Generate statistical summary of damage patterns
#' @param heatmap_matrix Heatmap matrix
#' @return List of statistics
calculate_damage_statistics <- function(heatmap_matrix) {
  damage_values <- as.vector(heatmap_matrix)
  damage_values <- damage_values[damage_values > 0]  # Remove background
  
  list(
    mean_damage = mean(damage_values),
    median_damage = median(damage_values),
    sd_damage = sd(damage_values),
    max_damage = max(damage_values),
    damaged_pixels = length(damage_values),
    total_pixels = length(as.vector(heatmap_matrix))
  )
}

# ______________________________________________________________________________
#### EXAMPLE USAGE AND TESTING ####
# ______________________________________________________________________________

# Example of how to use individual functions:
# 
# # Generate a single heatmap
# fw_result <- generate_wing_heatmap(
#   image_dir = "path/to/forewing/images",
#   template_path = "path/to/template.png",
#   output_name = "forewings",
#   red_coords = c(46, 10)
# )
# 
# # Create visualization
# create_heatmap_plot(fw_result, "forewings")
# 
# # Calculate statistics
# stats <- calculate_damage_statistics(fw_result$heatmap)

cat("Script loaded successfully. Run 'run_wing_damage_analysis()' to execute the full analysis.\n")


