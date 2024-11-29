# Load necessary libraries
library(ggplot2)
library(reshape2)
library(prospectr)
library(viridis)

# Load the data
mir_coffee_data <- read.csv("data/Instant_Coffee_Quadrum.csv")

# Remove "X" from the column names if present
colnames(mir_coffee_data) <- gsub("^X", "", colnames(mir_coffee_data))

# Remove the first three columns
spectral_data <- as.matrix(mir_coffee_data[, -(1:3)])

# Apply MSC correction
msc_data <- msc(spectral_data)

# Add corrected data to a new data frame
mir_coffee_msc <- data.frame(Varietal = mir_coffee_data$Varietal, msc_data)

# Calculate the mean for each Varietal
mir_coffee_mean <- aggregate(. ~ Varietal, mir_coffee_msc, mean)

# Melt the data to make it suitable for ggplot (long format)
mir_coffee_long <- melt(mir_coffee_mean,
    id.vars = "Varietal",
    variable.name = "Wavelength", value.name = "Absorbance"
)
# Remove the "X" from the Wavelength labels (if it's present)
mir_coffee_long$Wavelength <- gsub("^X", "", mir_coffee_long$Wavelength)

# Raw spectra plot
msc_spectra <- ggplot(
    data = mir_coffee_long,
    aes(
        x = Wavelength,
        y = Absorbance,
        color = Varietal,
        group = Varietal
    )
) +
    geom_line() +
    labs(
        title = "MSC Corrected Spectra of Instant Coffee Varietals",
        x = expression(Wavenumber ~ (cm^{
            -1
        })),
        y = "Absorbance"
    ) +
    theme_test() +
    theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        axis.text = element_text(
            size = 8,
            hjust = 1,
            angle = 90
        ),
        axis.title = element_text(size = 8)
    ) +
    scale_color_viridis_d(option = "viridis") +
    scale_x_discrete(
        limits = mir_coffee_long$Wavelength,
        breaks = mir_coffee_long$Wavelength[
            seq(1, length(mir_coffee_long$Wavelength), by = 15)
        ]
    ) +
    scale_y_continuous(
        breaks = seq(0, max(mir_coffee_long$Absorbance, na.rm = TRUE), by = 5)
    )

msc_spectra

# Create the 'plots/spectra' directory if it doesn't exist
plots_dir <- "plots/spectra"
if (!dir.exists(plots_dir)) {
    dir.create(plots_dir, recursive = TRUE)
}

# Save the plot as a TIFF file in 'plots/spectra'
ggsave(
    filename = file.path(plots_dir, "msc_spectra_plot.tiff"),
    plot = msc_spectra,
    width = 10,
    height = 6,
    dpi = 300
)
