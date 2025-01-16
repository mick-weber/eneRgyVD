test_that("Check all datasets have expected colnames based on extadata/ located file", {

  # Read file containing all colnames used to make nice translations for table display etc.
  load_colnames <- read.csv2("./inst/extdata/colnames_replacement_display.csv")

  # Make named list of dataframes containing the colnames
  expected_colnames <- split(
    load_colnames$colname,
    load_colnames$dataset
  )

  # Extract similar structure for all available datasets
  energy_colnames <- energy_datasets |>
    purrr::map(\(df){colnames(df)})

  mobility_colnames <- mobility_datasets |>
    purrr::map(\(df){colnames(df)})

  adaptation_colnames <- adaptation_datasets |>
    purrr::map(\(df){colnames(df)})

  # Check the names of the dataset match
  testthat::expect_true(label = "Check all expected energy_datasets names (not colnames) match loaded datasets",
                        all(names(energy_colnames) %in% names(expected_colnames)))
  testthat::expect_true(label = "Check all expected mobility_datasets names (not colnames) match loaded datasets",
                        all(names(mobility_colnames) %in% names(expected_colnames)))
  testthat::expect_true(label = "Check all expected adaptation_datasets names (not colnames) match loaded datasets",
                        all(names(adaptation_colnames) %in% names(expected_colnames)))

  # Now the dataset names
  actual_colnames <- c(energy_colnames, mobility_colnames, adaptation_colnames)

  common_names <- intersect(names(actual_colnames), names(expected_colnames))

  # Streamline tests pairwise expected/actual colnames by dataset
  test_colnames <- purrr::pwalk(
    .l = list(
      dataset_name = common_names,
      expected = expected_colnames[common_names],
      actual = actual_colnames[common_names]
    ),
    .f = \(dataset_name, expected, actual) {

      testthat::expect_equal(
        expected = sort(expected),
        object = sort(actual),
        label = glue::glue(
          "{dataset_name} : colnames in 'extdata/colnames_replacement_display.csv' don't match loaded variables in utils_helpers.R!"
        )
      )
    }
  )
})
