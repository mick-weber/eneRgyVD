test_that("Check `commune` variable from datasets are valid choices from input$selected_communes (choices_canton_communes in utils_helpers.R)",
          {
            # Define vector of clean choices (requires unlisting)
            values_canton_communes <- unlist(choices_canton_communes, use.names = FALSE)

            # elec_prod datasets ----

            expect_in((unique(elec_prod$commune)),
                      values_canton_communes)

            # subsidies datasets ----

            expect_in(unique(subsidies_by_building$commune),
                      values_canton_communes)

            expect_in(unique(subsidies_by_measure$commune),
                      values_canton_communes)

            # regener datasets ----

            expect_in(unique(regener_cons_ae_aff$commune),
                      values_canton_communes)

            expect_in(unique(regener_cons_ae_use$commune),
                      values_canton_communes)

            expect_in(unique(regener_needs$commune),
                      values_canton_communes)

            expect_in(unique(regener_misc$commune),
                      values_canton_communes)


          })


test_that("Check for each dataset that the value for 'Canton de Vaud' equals the sum of all other `commune` entries",
          {
            # Define vector of clean choices (requires unlisting)

            # elec_prod datasets ----
            # Will yield error as long as there's a specific NA placeholder for commmunes having n<3 installations by category

            expect_equal(label = "The sum of 'Canton de Vaud' (GWh)",
                         expected.label = "the sum of all communes. This is expected as long as there's filter for N<3 installations in the raw-data -> .rda script",
                         sum(elec_prod$production[elec_prod$commune == "Canton de Vaud"], na.rm = TRUE)/1e6,
                         sum(elec_prod$production[elec_prod$commune != "Canton de Vaud"], na.rm = TRUE)/1e6
            )
            # regener datasets ----

            expect_equal(
              sum(regener_cons_ae_aff$consommation[regener_cons_ae_aff$commune == "Canton de Vaud"], na.rm = TRUE)/1e6,
              sum(regener_cons_ae_aff$consommation[regener_cons_ae_aff$commune != "Canton de Vaud"], na.rm = TRUE)/1e6
            )

            expect_equal(
              sum(regener_cons_ae_use$consommation[regener_cons_ae_use$commune == "Canton de Vaud"], na.rm = TRUE)/1e6,
              sum(regener_cons_ae_use$consommation[regener_cons_ae_use$commune != "Canton de Vaud"], na.rm = TRUE)/1e6
            )

            expect_equal(
              sum(regener_needs$besoins[regener_needs$commune == "Canton de Vaud"], na.rm = TRUE)/1e6,
              sum(regener_needs$besoins[regener_needs$commune != "Canton de Vaud"], na.rm = TRUE)/1e6
            )

            # subsidies datasets ----

            expect_equal(
              sum(subsidies_by_building$N_EGID[subsidies_by_building$commune == "Canton de Vaud"], na.rm = TRUE)/1e6,
              sum(subsidies_by_building$N_EGID[subsidies_by_building$commune != "Canton de Vaud"], na.rm = TRUE)/1e6
            )

            expect_equal(
              sum(subsidies_by_building$SRE[subsidies_by_building$commune == "Canton de Vaud"], na.rm = TRUE)/1e6,
              sum(subsidies_by_building$SRE[subsidies_by_building$commune != "Canton de Vaud"], na.rm = TRUE)/1e6
            )

            expect_equal(
              sum(subsidies_by_measure$nombre[subsidies_by_measure$commune == "Canton de Vaud"], na.rm = TRUE)/1e6,
              sum(subsidies_by_measure$nombre[subsidies_by_measure$commune != "Canton de Vaud"], na.rm = TRUE)/1e6
            )


          })

