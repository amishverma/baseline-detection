
<!-- README.md is generated from README.Rmd. Please edit that file -->

# baselinedetection

<!-- badges: start -->
<!-- badges: end -->

The goal of baselinedetection is to process and analyze spectral data.
It provides tools for importing spectral data, performing baseline
correction, and visualizing spectra.

## Installation

You can install the development version of baselinedetection from
[GitHub](https://github.com/amishverma/baseline-detection) with:

``` r
# install.packages("devtools")
devtools::install_github("amishverma/baseline-detection",build_vignettes=TRUE)
```

## Example

This is a basic example.

``` r
# Load the package
library(baselinedetection)

# Sample data preparation
# Define wavelength and intensity for testing if you don't have a CSV file.
wavelength <- seq(400, 700, length.out = 100)
intensity <- sin(wavelength / 100) + rnorm(100, sd = 0.05)

# Step 1: Create a Spectra Object
# This function creates an object of class "Spectra" from wavelength and intensity vectors.
spectra <- as.Spectra(wavelength, intensity)
#print(spectra)  # Print the object to view its structure and summary.

# Step 2: Import Spectral Data from CSV
# Uncomment if you have a CSV file in the proper format (with "wavelength" and "intensity" columns).
# file_path <- "path_to_your_csv.csv"  # Replace with the path to your file
# spectra <- import_spectra(file_path)  # Import the spectra from CSV

# Step 3: Calculate Convex Hull for Baseline Detection
# The convex hull represents the "envelope" that will help to remove the baseline.
hull_points <- convex_hull_Spectra(spectra)
#print(hull_points)  # Output the points in the convex hull

# Step 4: Perform Baseline Correction
# This function uses the convex hull to calculate and remove the baseline, returning a corrected spectrum.
corrected_spectra <- baseline_correct_Spectra(spectra)
print(corrected_spectra)  # View the corrected spectra data
#> Spectra Object
#> Number of points: 100 
#> Wavelength range: 400 700 
#> Intensity range: 0 0.2134537

# Step 5: Plot the Original Spectrum, Convex Hull, and Corrected Spectrum
# Use plot.Spectra to visualize the original spectrum with hull and corrected spectrum.
plot(spectra, hull_points = hull_points, corrected_spectrum = corrected_spectra)
```

<img src="man/figures/README-example-1.png" width="100%" />

``` r

# Step 6: Plot Only the Corrected Spectrum
# This gives a clear view of the baseline-corrected spectrum.
plot_spectra_corrected(corrected_spectrum = corrected_spectra)
```

<img src="man/figures/README-example-2.png" width="100%" />
