test_that("Check that map layers are valid components for a leaflet map",
          {

            # Most basic map with the app 3 layers
            map <- leaflet::leaflet() |>
              # Couche de base des districts si un district est sélectionné
              leaflet::addPolygons(data = sf_districts) |>
              # Couche des lacs
              leaflet::addPolygons(data = sf_lacs) |>
              # Première couche des communes (en blanc, état non-sélectionné)
              leaflet::addPolygons(data = sf_communes)

            # Check class
            expect_equal(class(map),
                         c("leaflet", "htmlwidget"))

          })



