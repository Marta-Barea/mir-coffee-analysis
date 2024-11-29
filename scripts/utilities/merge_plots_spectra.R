# Load necessary libraries
library(tiff)
library(grid)
library(gridExtra)

# Directory containing the plot files
spectra_dir <- "plots/spectra"
combined_dir <- "plots/combined"

# Create the 'combined' directory if it doesn't exist
if (!dir.exists(combined_dir)) {
    dir.create(combined_dir, recursive = TRUE)
}

# List all TIFF files with "spectra" in their name but exclude "combined"
plot_files <- list.files(spectra_dir, pattern = "spectra.*\\.tiff?$", full.names = TRUE)

# Check if there are any files in the folder
if (length(plot_files) == 0) {
    stop("No TIFF files containing 'spectra' found in the 'plots/spectra' directory.")
}

# Ensure the file with "raw" is first
raw_file <- grep("raw", plot_files, value = TRUE) # Find the file with "raw"
if (length(raw_file) > 0) {
    plot_files <- c(raw_file, setdiff(plot_files, raw_file)) # Place "raw" file at the beginning
}

# Add labels (A, B, C, etc.) to each image
labels <- LETTERS[1:length(plot_files)]

# Load the TIFF images and add labels
plots <- lapply(seq_along(plot_files), function(i) {
    file <- plot_files[i]
    img <- readTIFF(file, native = FALSE)

    # Create a rasterGrob for the image
    raster <- rasterGrob(img, interpolate = TRUE)

    # Add a label (A, B, C, etc.) using a grob layout
    label <- textGrob(labels[i],
        x = 0.05, y = 0.95, just = c("left", "top"),
        gp = gpar(fontsize = 16, fontface = "bold", col = "black")
    ) # Black text

    # Combine the raster image and the label
    gridExtra::arrangeGrob(raster, top = label)
})

# Combine all plots into a single layout
combined_plot <- grid.arrange(grobs = plots, ncol = 2, nrow = ceiling(length(plots) / 2))

# Save the combined image as a high-resolution TIFF file
output_file <- file.path(combined_dir, "combined_spectra_plot.tiff")
tiff(output_file, width = 10, height = 8, units = "in", res = 300)
grid.draw(combined_plot)
dev.off()

cat(paste("The combined image has been saved as:", output_file))
