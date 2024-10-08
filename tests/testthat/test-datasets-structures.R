# These tests aim at checking that supplied datasets have the expected structure
#  both in terms of variable names and lengths. Variable names order does not matter.


test_that("Check structure of sf_layers for leaflet map",{


  ## |------------------------------------------------------------------------------|
  ##     1) Check that all used variables in fct_helpers.R / app_server.R exist
  ## |------------------------------------------------------------------------------|

  vars_canton <- c("geometry")
  vars_communes <- c("geometry", "NOM_MIN", "NO_COM_FED")
  vars_districts <- c("geometry", "NOM_MAJ")
  vars_lacs <- c("geometry")


  invisible( # avoid console printing
    mapply(function(actual_colnames, vars_required)
    expect_contains(object = actual_colnames, expected = vars_required),
    # pass colnames of each sf layer as the object arg for expect_contains
         sapply(list(sf_canton, sf_communes, sf_districts, sf_lacs), FUN = colnames),
    # pass expected colnames as the expected arg for expect contains
         list(vars_canton, vars_communes, vars_districts, vars_lacs)
         )
  )


  ## |---------------------------------------------------------------|
  ##      2) Check all sf layers have the same 'EPSG:4326' CRS
  ## |---------------------------------------------------------------|

  expect_equal(
    sapply(X = list(sf_canton, sf_communes, sf_districts, sf_lacs),
               FUN = \(sf_object) sf::st_crs(sf_object)$input) |> unique(),
    expected = "EPSG:4326"
  )

})


test_that("Check all REGENER related datasets", {

  # Prepare ordered expected colnames

  expected_cons_aff_colnames <- c("commune", "ae", "consommation", "pct_commune",
                                  "co2_direct", "affectation", "etat") |> sort()

  expected_cons_use_colnames <- c("commune", "ae", "consommation", "co2_direct",
                                  "pct_commune", "usage", "etat")|> sort()

  expected_needs_colnames <- c("commune", "type", "statut", "besoins", "etat") |> sort()

  expected_misc_colnames <- c("commune", "SRE", "N_EGID", "N_NO_RENOV",
                              "N_NEW_POST_2000","N_RENOV_L_POST_2000",
                              "N_RENOV_H_POST_2000", "N_NO_GBAUJ", "etat") |> sort()

  # Run tests
  expect_equal(label = "Expected colnames for RegEner datasets",
               expected.label = "RegEner colnames do not match",
               regener_cons_ae_aff |> colnames() |> sort(),
               expected_cons_aff_colnames)

  expect_equal(label = "Expected colnames for RegEner datasets",
               expected.label = "RegEner colnames do not match",
               regener_cons_ae_use |> colnames() |> sort(),
               expected_cons_use_colnames)

  expect_equal(label = "Expected colnames for RegEner datasets",
               expected.label = "RegEner colnames do not match",
               regener_needs |> colnames() |> sort(),
               expected_needs_colnames)

  expect_equal(label = "Expected colnames for RegEner datasets",
               expected.label = "RegEner colnames do not match",
               regener_misc |> colnames() |> sort(),
               expected_misc_colnames)


})


test_that("Check all ELEC_CONS related datasets", {

  # Prepare ordered expected colnames

  expected_elec_cons_colnames <- c("annee", "commune", "consommation", "secteur") |> sort()

  # Run tests
  expect_equal(label = "Expected colnames for elec_cons dataset",
               expected.label = "colnames do not match",
               elec_cons|> colnames() |> sort(),
               expected_elec_cons_colnames)

})


test_that("Check all ELEC_PROD related datasets", {

  # Prepare ordered expected colnames

  expected_elec_prod_colnames <- c("categorie", "commune", "annee", "puissance_electrique_installee",
                                   "injection", "autoconsommation", "production", "numero_de_la_commune") |>sort()

  # Run tests
  expect_equal(label = "Expected colnames for elec_prod dataset",
               expected.label = "colnames do not match",
               elec_prod |> colnames() |> sort(),
               expected_elec_prod_colnames)

})


test_that("Check all SUBSIDIES related datasets", {

  # Prepare ordered expected colnames

  expected_subsidies_building_colnames <- c("commune", "etat", "subv_type", "N_EGID",
                                            "SRE", "detail_chauffage") |> sort()

  expected_subsidies_measure_colnames <- c("commune", "annee", "mesure", "detail_mesure",
                            "mesure_simplifiee", "nombre") |> sort()

  # Run tests
  expect_equal(label = "Expected colnames for subsidies dataset",
               expected.label = "colnames do not match",
               subsidies_by_building |> colnames() |> sort(),
               expected_subsidies_building_colnames)

  expect_equal(label = "Expected colnames for subsidies dataset",
               expected.label = "colnames do not match",
               subsidies_by_measure |> colnames() |> sort(),
               expected_subsidies_measure_colnames)



})
