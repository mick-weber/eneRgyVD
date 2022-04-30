# class should return two items : "leaflet" and "htmlwidgets". We look at the first one for simplicity
test_that("leaflet map is a true leaflet object", {
  expect_equal(class(create_select_leaflet(sf_districts = sf_districts,
                                           sf_lacs = sf_lacs,
                                           sf_communes = sf_communes))[1], "leaflet")
})
