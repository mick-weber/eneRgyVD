# These tests aim at checking that supplied communes are valid,
#  and that the sum of all communes equals the `Canton de Vaud` entry.

test_that("Check `commune` variable from all datasets are valid choices from input$selected_communes (choices_canton_communes in utils_helpers.R)",
          {
            # Define vector of clean choices (requires unlisting)
            values_canton_communes <- unlist(choices_canton_communes, use.names = FALSE)


            purrr::map(energy_datasets, \(df) expect_in(unique(df$commune), values_canton_communes))
            purrr::map(mobility_datasets, \(df) expect_in(unique(df$commune), values_canton_communes))
            purrr::map(adaptation_datasets, \(df) expect_in(unique(df$commune), values_canton_communes))


          })

test_that("Check all existing 'commune' are present in every dataset so we explicitely plot/table communes without values",
          {
            # Define vector of clean choices (requires unlisting)
            values_canton_communes <- unlist(choices_canton_communes, use.names = FALSE)


            purrr::map(energy_datasets, \(df) expect_in(values_canton_communes, unique(df$commune)))
            purrr::map(mobility_datasets, \(df) expect_in(values_canton_communes, unique(df$commune)))
            purrr::map(adaptation_datasets, \(df) expect_in(values_canton_communes, unique(df$commune)))


          })



test_that("Check for each dataset that the value for 'Canton de Vaud' equals the sum of all other `commune` entries",
          {
            # Define vector of clean choices (requires unlisting)
            # elec_prod datasets ----
            # Will yield error as long as there's a specific NA placeholder for commmunes having n<3 installations by category

            # expect_equal(label = "The sum of 'Canton de Vaud' (GWh)",
            #              expected.label = "the sum of all communes. This is expected as long as there's filter for N<3 installations in the raw-data -> .rda script",
            #              sum(elec_prod$production[elec_prod$commune == "Canton de Vaud"], na.rm = TRUE)/1e6,
            #              sum(elec_prod$production[elec_prod$commune != "Canton de Vaud"], na.rm = TRUE)/1e6
            # )
            # regener datasets ----

            expect_equal(
              sum(energy_datasets$elec_cons$consommation[energy_datasets$elec_cons$commune == "Canton de Vaud"], na.rm = TRUE)/1e6,
              sum(energy_datasets$elec_cons$consommation[energy_datasets$elec_cons$commune != "Canton de Vaud"], na.rm = TRUE)/1e6
            )

            expect_equal(
              sum(energy_datasets$ng_cons$consommation[energy_datasets$ng_cons$commune == "Canton de Vaud"], na.rm = TRUE)/1e6,
              sum(energy_datasets$ng_cons$consommation[energy_datasets$ng_cons$commune != "Canton de Vaud"], na.rm = TRUE)/1e6
            )

            expect_equal(
              sum(energy_datasets$regener_cons_ae_aff$consommation[regener_cons_ae_aff$commune == "Canton de Vaud"], na.rm = TRUE)/1e6,
              sum(energy_datasets$regener_cons_ae_aff$consommation[regener_cons_ae_aff$commune != "Canton de Vaud"], na.rm = TRUE)/1e6
            )

            expect_equal(
              sum(energy_datasets$regener_cons_ae_aff$consommation[regener_cons_ae_aff$commune == "Canton de Vaud"], na.rm = TRUE)/1e6,
              sum(energy_datasets$regener_cons_ae_aff$consommation[regener_cons_ae_aff$commune != "Canton de Vaud"], na.rm = TRUE)/1e6
            )

            expect_equal(
              sum(energy_datasets$regener_cons_ae_use$consommation[regener_cons_ae_use$commune == "Canton de Vaud"], na.rm = TRUE)/1e6,
              sum(energy_datasets$regener_cons_ae_use$consommation[regener_cons_ae_use$commune != "Canton de Vaud"], na.rm = TRUE)/1e6
            )

            expect_equal(
              sum(energy_datasets$regener_needs$besoins[regener_needs$commune == "Canton de Vaud"], na.rm = TRUE)/1e6,
              sum(energy_datasets$regener_needs$besoins[regener_needs$commune != "Canton de Vaud"], na.rm = TRUE)/1e6
            )

            # subsidies datasets ----

            expect_equal(
              sum(energy_datasets$subsidies_by_building$N_EGID[subsidies_by_building$commune == "Canton de Vaud"], na.rm = TRUE)/1e6,
              sum(energy_datasets$subsidies_by_building$N_EGID[subsidies_by_building$commune != "Canton de Vaud"], na.rm = TRUE)/1e6
            )

            expect_equal(
              sum(energy_datasets$subsidies_by_building$SRE[subsidies_by_building$commune == "Canton de Vaud"], na.rm = TRUE)/1e6,
              sum(energy_datasets$subsidies_by_building$SRE[subsidies_by_building$commune != "Canton de Vaud"], na.rm = TRUE)/1e6
            )

            expect_equal(
              sum(energy_datasets$subsidies_by_measure$nombre[subsidies_by_measure$commune == "Canton de Vaud"], na.rm = TRUE)/1e6,
              sum(energy_datasets$subsidies_by_measure$nombre[subsidies_by_measure$commune != "Canton de Vaud"], na.rm = TRUE)/1e6
            )


          })

