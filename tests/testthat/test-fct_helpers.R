# sf_layers ----
test_that("Check that map layers are valid components for a leaflet map",
          {

            # Most basic map with the app 3 layers
            map <- create_select_leaflet(
              sf_districts = eneRgyVD::sf_districts,
              sf_lacs = eneRgyVD::sf_lacs,
              sf_communes = eneRgyVD::sf_communes
            )
            # Check class
            expect_equal(class(map),
                         c("leaflet", "htmlwidget"))

          })


# convert_units() ----
test_that("Check unit conversion works as expected",
          {

            # Actual values
            actual_values <- c(
             eneRgyVD::convert_units(data = data.frame(kwh = 1e3),colnames = "kwh", unit_from = "kWh", unit_to = "kWh")[[1]],
             eneRgyVD::convert_units(data = data.frame(kwh = 1e3),colnames = "kwh", unit_from = "kWh", unit_to = "MWh")[[1]],
             eneRgyVD::convert_units(data = data.frame(kwh = 1e3),colnames = "kwh", unit_from = "kWh", unit_to = "GWh")[[1]],
             eneRgyVD::convert_units(data = data.frame(kwh = 1e3),colnames = "kwh", unit_from = "kWh", unit_to = "TJ")[[1]]
            )

            # Expected values
            expected_values <- c(1e3, 1, 1e-3, 3.6e-3)

            # Test
            expect_equal(object = actual_values, expected = expected_values)

          }
)

# add_colname_units() ----
test_that("Check unit addition in colname works as expected",
          {
            df_in <- eneRgyVD::add_colname_units(data = data.frame(consommation = 100), unit = "MWh")
            df_out <- data.frame(`consommation [MWh]` = 100, check.names = FALSE)

            expect_equal(df_in, df_out)
          }
)

