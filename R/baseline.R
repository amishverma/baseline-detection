#' @useDynLib baselinedetection, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL

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
  # Extract wavelength and intensity from the Spectra object
  wavelength <- x$wavelength
  intensity <- x$intensity

  # Plot the original spectra
  plot(wavelength, intensity, main = "Spectra Plot", xlab = "Wavelength", ylab = "Intensity", pch = 19, col = "blue", ...)

  # Overlay the convex hull points if provided
  if (!is.null(hull_points)) {
    points(hull_points[, 1], hull_points[, 2], col = "red", pch = 19)
    lines(hull_points[, 1], hull_points[, 2], col = "red")
  }

  # Overlay the corrected spectrum if provided
  if (!is.null(corrected_spectrum)) {
    lines(corrected_spectrum[, 1], corrected_spectrum[, 2], col = "purple", lty = 1)
    legend("topright", legend = c("Corrected Spectrum"), col = "purple", lty = 1)
  }
}
