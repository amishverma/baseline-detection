#' @useDynLib baselinedetection, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom utils read.csv
NULL

# 1. Creating and Printing Spectra Objects

#' Create a Spectra Object
#'
#' This function creates a \code{Spectra} object, which contains wavelength and intensity data.
#'
#' @param wavelength A numeric vector of wavelengths.
#' @param intensity A numeric vector of intensities corresponding to the wavelengths.
#' @return An object of class \code{Spectra}.
#' @examples
#' wavelength <- seq(400, 700, length.out = 100)
#' intensity <- sin(wavelength / 100) + rnorm(100, sd = 0.05)
#' spectra <- as.Spectra(wavelength, intensity)
#' print(spectra)
#' @export
as.Spectra <- function(wavelength, intensity) {
  # Check for empty inputs
  if (length(wavelength) == 0 || length(intensity) == 0) {
    stop("Wavelength and intensity must not be empty")
  }

  # Check for non-numeric inputs
  if (!is.numeric(wavelength) || !is.numeric(intensity)) {
    stop("Wavelength and intensity must be numeric")
  }

  # Check for length mismatch
  if (length(wavelength) != length(intensity)) {
    stop("Wavelength and intensity must have the same length")
  }

  structure(list(wavelength = wavelength, intensity = intensity), class = "Spectra")
}

#' Print a Spectra Object
#'
#' This function prints the summary information of a \code{Spectra} object.
#'
#' @param x An object of class \code{Spectra}.
#' @param ... Additional arguments passed to the print method.
#' @examples
#' wavelength <- seq(400, 700, length.out = 100)
#' intensity <- sin(wavelength / 100) + rnorm(100, sd = 0.05)
#' spectra <- as.Spectra(wavelength, intensity)
#' print(spectra)
#' @export
print.Spectra <- function(x, ...) {
  cat("Spectra Object\n")
  cat("Number of points:", length(x$wavelength), "\n")
  cat("Wavelength range:", range(x$wavelength), "\n")
  cat("Intensity range:", range(x$intensity), "\n")
}

# 2. Convex Hull Calculation

#' Calculate Convex Hull for Spectra
#'
#' This function calculates the convex hull for a \code{Spectra} object using the convex hull algorithm.
#'
#' @param x An object of class \code{Spectra}.
#' @return A matrix of points representing the convex hull.
#' @examples
#' wavelength <- seq(400, 700, length.out = 100)
#' intensity <- sin(wavelength / 100) + rnorm(100, sd = 0.05)
#' spectra <- as.Spectra(wavelength, intensity)
#' hull_points <- convex_hull_Spectra(spectra)
#' plot(spectra, hull_points = hull_points)
#' @export
convex_hull_Spectra <- function(x) {
  points <- cbind(x$wavelength, x$intensity)
  hull_points <- convex_hull_cpp(points)  # Assuming convex_hull_cpp is sourced from C++ code
  return(hull_points)
}

#' Generic Convex Hull Function
#'
#' This function checks the class of the input and calls the appropriate convex hull calculation function.
#'
#' @param x The object for which the convex hull should be calculated.
#' @param ... Additional arguments passed to specific methods.
#' @return The output depends on the specific method applied.
#' @examples
#' wavelength <- seq(400, 700, length.out = 100)
#' intensity <- sin(wavelength / 100) + rnorm(100, sd = 0.05)
#' spectra <- as.Spectra(wavelength, intensity)
#' hull_points <- convex_hull(spectra)
#' @export
convex_hull <- function(x, ...) {
  if (inherits(x, "Spectra")) {
    return(convex_hull_Spectra(x, ...))
  } else {
    stop("convex_hull is not implemented for this type of object")
  }
}

# 3. Baseline Correction

#' Calculate Baseline-Corrected Spectrum
#'
#' This function calculates the baseline-corrected spectrum using the convex hull method.
#'
#' @param x An object of class \code{Spectra}.
#' @return A \code{Spectra} object representing the baseline-corrected spectrum.
#' @examples
#' wavelength <- seq(400, 700, length.out = 100)
#' intensity <- sin(wavelength / 100) + rnorm(100, sd = 0.05)
#' spectra <- as.Spectra(wavelength, intensity)
#' corrected_spectrum <- baseline_correct_Spectra(spectra)
#' plot(spectra, corrected_spectrum = corrected_spectrum)
#' @importFrom stats approx
#' @export
baseline_correct_Spectra <- function(x) {
  hull_points <- convex_hull_Spectra(x)
  interpolated_baseline <- approx(hull_points[, 1], hull_points[, 2], xout = x$wavelength)$y
  corrected_intensity <- x$intensity - interpolated_baseline
  corrected_spectrum <- as.Spectra(x$wavelength, corrected_intensity)
  return(corrected_spectrum)
}

# 4. Plotting Spectra

#' Plot a Spectra Object
#'
#' This function plots the \code{Spectra} object, with optional overlays for convex hull and corrected spectrum.
#'
#' @param x An object of class \code{Spectra}.
#' @param hull_points A matrix of convex hull points to overlay on the plot (optional).
#' @param corrected_spectrum A \code{Spectra} object representing the baseline-corrected spectrum (optional).
#' @param ... Additional graphical parameters to be passed to \code{plot}.
#' @examples
#' wavelength <- seq(400, 700, length.out = 100)
#' intensity <- sin(wavelength / 100) + rnorm(100, sd = 0.05)
#' spectra <- as.Spectra(wavelength, intensity)
#' plot(spectra)
#' @importFrom graphics legend lines points
#' @export
plot.Spectra <- function(x, hull_points = NULL, corrected_spectrum = NULL, ...) {
  wavelength <- x$wavelength
  intensity <- x$intensity
  plot(wavelength, intensity, main = "Spectra Plot", xlab = "Wavelength", ylab = "Intensity", pch = 19, col = "blue", ...)
  if (!is.null(hull_points)) {
    points(hull_points[, 1], hull_points[, 2], col = "red", pch = 19)
    lines(hull_points[, 1], hull_points[, 2], col = "red")
  }
  if (!is.null(corrected_spectrum)) {
    if (!inherits(corrected_spectrum, "Spectra")) {
      stop("corrected_spectrum must be of class 'Spectra'")
    }
    corrected_wavelength <- corrected_spectrum$wavelength
    corrected_intensity <- corrected_spectrum$intensity
    lines(corrected_wavelength, corrected_intensity, col = "purple", lty = 1)
    legend("topleft", legend = c("Original Spectrum", "Convex Hull", "Corrected Spectrum"),
           col = c("blue", "red", "purple"), lty = c(NA, 1, 1), pch = c(19, 19, NA))
  } else {
    legend("topleft", legend = c("Original Spectrum", "Convex Hull"),
           col = c("blue", "red"), lty = c(NA, 1), pch = c(19, 19))
  }
}



#' Plot Corrected Spectra Object
#'
#' This function plots the baseline-corrected \code{Spectra} object.
#'
#' @param corrected_spectrum A \code{Spectra} object representing the baseline-corrected spectrum.
#' @param ... Additional graphical parameters to be passed to \code{plot}.
#' @examples
#' wavelength <- seq(400, 700, length.out = 100)
#' intensity <- sin(wavelength / 100) + rnorm(100, sd = 0.05)
#' spectra <- as.Spectra(wavelength, intensity)
#' corrected_spectrum <- baseline_correct_Spectra(spectra)
#' plot_spectra_corrected(corrected_spectrum)
#' @export
plot_spectra_corrected <- function(corrected_spectrum, ...) {
  if (!inherits(corrected_spectrum, "Spectra")) {
    stop("corrected_spectrum must be of class 'Spectra'")
  }

  plot(corrected_spectrum$wavelength, corrected_spectrum$intensity,
       main = "Corrected Spectra Plot",
       xlab = "Wavelength", ylab = "Corrected Intensity",
       type = "l", col = "purple", ...)
}





####################### Till here everything is working well......#####################

#' Import a Dataset from a CSV File and Create a Spectra Object
#'
#' This function imports a dataset from a CSV file, checks for the required column names,
#' and then creates a \code{Spectra} object. The user can specify which columns to use for wavelength and intensity.
#'
#' @param file_path The path to the CSV file containing the dataset.
#' @param wavelength_col The name of the column containing wavelength data. Defaults to "wavelength".
#' @param intensity_col The name of the column containing intensity data. Defaults to "intensity".
#' @return An object of class \code{Spectra}.
#' @examples
#' file_path <- system.file("extdata", "test_spectra.csv", package = "baselinedetection")
#' spectra <- import_spectra(file_path)
#' print(spectra)
#'
#' @export
import_spectra <- function(file_path, wavelength_col = "wavelength", intensity_col = "intensity") {
  # Read the CSV file
  data <- read.csv(file_path)

  # Check if the specified columns exist
  if (!(wavelength_col %in% names(data))) {
    stop(paste("The specified wavelength column", wavelength_col, "is not found in the dataset."))
  }

  if (!(intensity_col %in% names(data))) {
    stop(paste("The specified intensity column", intensity_col, "is not found in the dataset."))
  }

  # Check if the dataset has at least 3 rows
  if (nrow(data) < 3) {
    stop("The dataset must contain at least 3 rows for convex hull calculation.")
  }

  # Create and return the Spectra object using the specified columns
  spectra <- as.Spectra(data[[wavelength_col]], data[[intensity_col]])
  return(spectra)
}



#' Convert a Spectra Object to a Data Frame
#'
#' This function converts a \code{Spectra} object to a data frame.
#'
#' @param x A \code{Spectra} object.
#' @param ... Additional arguments (ignored in this method).
#' @return A data frame with two columns: wavelength and intensity.
#' @export
as.data.frame.Spectra <- function(x, ...) {
  data.frame(wavelength = x$wavelength, intensity = x$intensity)
}



