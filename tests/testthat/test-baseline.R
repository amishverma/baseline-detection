library(testthat)
#library(baselinedetection)

test_that("as.Spectra creates a Spectra object correctly", {
  wavelength <- seq(400, 700, length.out = 100)
  intensity <- sin(wavelength / 100) + rnorm(100, sd = 0.05)

  spectra <- as.Spectra(wavelength, intensity)

  expect_s3_class(spectra, "Spectra")
  expect_equal(length(spectra$wavelength), 100)
  expect_equal(length(spectra$intensity), 100)
})

test_that("as.Spectra handles empty and invalid inputs", {
  # Empty inputs
  expect_error(as.Spectra(numeric(0), numeric(0)), "Wavelength and intensity must not be empty")

  # Mismatched lengths
  expect_error(as.Spectra(1:10, 1:5), "Wavelength and intensity must have the same length")

  # Non-numeric inputs
  expect_error(as.Spectra("wavelength", "intensity"), "Wavelength and intensity must be numeric")
})

# Testing the print.Spectra function
test_that("print.Spectra prints correctly", {
  wavelength <- seq(400, 700, length.out = 100)
  intensity <- sin(wavelength / 100) + rnorm(100, sd = 0.05)

  spectra <- as.Spectra(wavelength, intensity)

  expect_output(print(spectra), "Spectra Object")
  expect_output(print(spectra), "Number of points: 100")
  expect_output(print(spectra), "Wavelength range")
  expect_output(print(spectra), "Intensity range")
})

# Testing the convex_hull_Spectra function
test_that("convex_hull_Spectra calculates convex hull correctly", {
  wavelength <- c(400, 450, 500, 550, 600, 650, 700)
  intensity <- c(0.1, 0.5, 0.2, 0.8, 0.2, 0.5, 0.1)
  spectra <- as.Spectra(wavelength, intensity)

  hull_points <- convex_hull_Spectra(spectra)

  expect_true(is.matrix(hull_points))
  expect_equal(ncol(hull_points), 2)
  expect_true(all(hull_points[, 1] >= 400 & hull_points[, 1] <= 700))
})

# Testing the baseline_correct_Spectra function
test_that("baseline_correct_Spectra returns a corrected Spectra object", {
  wavelength <- c(400, 450, 500, 550, 600, 650, 700)
  intensity <- c(0.1, 0.5, 0.2, 0.8, 0.2, 0.5, 0.1)
  spectra <- as.Spectra(wavelength, intensity)

  corrected_spectrum <- baseline_correct_Spectra(spectra)

  expect_s3_class(corrected_spectrum, "Spectra")
  expect_equal(length(corrected_spectrum$wavelength), 7)
  expect_equal(length(corrected_spectrum$intensity), 7)
})

# Testing the plot.Spectra function
test_that("plot.Spectra creates a plot without error", {
  wavelength <- seq(400, 700, length.out = 100)
  intensity <- sin(wavelength / 100) + rnorm(100, sd = 0.05)
  spectra <- as.Spectra(wavelength, intensity)

  expect_silent(plot(spectra))
})

# Testing the import_spectra function with a valid file
test_that("import_spectra reads and creates a Spectra object from CSV", {
  file_path <- system.file("extdata", "test_spectra.csv", package = "baselinedetection")

  spectra <- import_spectra(file_path)

  expect_s3_class(spectra, "Spectra")
  expect_true(nrow(as.data.frame(spectra)) >= 3)
})


# Testing import_spectra with another valid file
test_that("import_spectra reads and creates a Spectra object from another CSV", {
  file_path <- system.file("extdata", "test_spectra.csv", package = "baselinedetection")

  spectra <- import_spectra(file_path)

  expect_s3_class(spectra, "Spectra")
  expect_true(nrow(as.data.frame(spectra)) >= 3)
})

# Testing as.Spectra with large datasets
test_that("as.Spectra handles large datasets efficiently", {
  wavelength <- seq(400, 700, length.out = 1e6)
  intensity <- sin(wavelength / 100) + rnorm(1e6, sd = 0.05)

  spectra <- as.Spectra(wavelength, intensity)

  expect_s3_class(spectra, "Spectra")
  expect_equal(length(spectra$wavelength), 1e6)
})



test_that("baseline_correct_Spectra correctly adjusts the intensity", {
  wavelength <- seq(400, 700, length.out = 100)
  intensity <- sin(wavelength / 100) + rnorm(100, sd = 0.05)
  spectra <- as.Spectra(wavelength, intensity)

  corrected_spectrum <- baseline_correct_Spectra(spectra)

  expect_s3_class(corrected_spectrum, "Spectra")
  expect_equal(corrected_spectrum$wavelength, spectra$wavelength)
  expect_false(all(corrected_spectrum$intensity == spectra$intensity))
  expect_true(is.numeric(corrected_spectrum$intensity))
})

test_that("convex_hull_Spectra correctly calculates the convex hull", {
  wavelength <- c(400, 450, 500, 550, 600, 650, 700)
  intensity <- c(0.1, 0.5, 0.2, 0.8, 0.2, 0.5, 0.1)
  spectra <- as.Spectra(wavelength, intensity)

  hull_points <- convex_hull_Spectra(spectra)

  expect_true(is.matrix(hull_points))
  expect_equal(ncol(hull_points), 2)
  expect_true(all(hull_points[, 1] >= 400 & hull_points[, 1] <= 700))
  expect_true(all(hull_points[, 2] >= 0.1 & hull_points[, 2] <= 0.8))
})

test_that("plot.Spectra correctly plots the original spectrum, convex hull, and corrected spectrum", {
  wavelength <- seq(400, 700, length.out = 100)
  intensity <- sin(wavelength / 100) + rnorm(100, sd = 0.05)
  spectra <- as.Spectra(wavelength, intensity)

  hull_points <- convex_hull_Spectra(spectra)
  corrected_spectrum <- baseline_correct_Spectra(spectra)

  expect_silent(plot(spectra, hull_points = hull_points, corrected_spectrum = corrected_spectrum))
})



test_that("import_spectra correctly handles datasets with additional columns", {
  # Creating a mock dataset with additional columns
  data <- data.frame(
    wavelength = seq(400, 700, length.out = 10),
    intensity = sin(seq(400, 700, length.out = 10) / 100) + rnorm(10, sd = 0.05),
    additional_col = rnorm(10)
  )
  write.csv(data, "test_additional_columns.csv", row.names = FALSE)

  spectra <- import_spectra("test_additional_columns.csv")

  # Ensure the object is of class Spectra
  expect_s3_class(spectra, "Spectra")

  # Ensure the wavelength and intensity are correct
  expect_equal(spectra$wavelength, data$wavelength)
  expect_equal(spectra$intensity, data$intensity)

  unlink("test_additional_columns.csv")  # Clean up test file
})


test_that("import_spectra throws an error with incorrect column names", {
  # Creating a mock dataset with correct column names
  data <- data.frame(
    wave = seq(400, 700, length.out = 10),
    intens = sin(seq(400, 700, length.out = 10) / 100) + rnorm(10, sd = 0.05)
  )
  write.csv(data, "test_incorrect_columns.csv", row.names = FALSE)

  # This should throw an error because the columns "wavelength" and "intensity" do not exist
  expect_error(import_spectra("test_incorrect_columns.csv"), "The specified wavelength column wavelength is not found in the dataset.")

  # Now testing with incorrect intensity column name
  expect_error(import_spectra("test_incorrect_columns.csv", wavelength_col = "wave", intensity_col = "intensity"), "The specified intensity column intensity is not found in the dataset.")

  unlink("test_incorrect_columns.csv")  # Clean up test file
})


