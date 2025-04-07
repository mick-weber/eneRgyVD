# regener_cons_* values ----
test_that("Check that all regener_cons_ datasets have the same results by year and commune",
          {

            # Summarize function, we tolerate accuracy check until 3rd decimal so we avoid comparing 25 decimals...
            summarize_regener_cons <- function(regener_cons){
              regener_cons |>
                dplyr::group_by(etat, commune) |>
                dplyr::summarise(consommation = sum(consommation, na.rm = T),
                          co2_direct = sum(co2_direct, na.rm = T)) |>
                dplyr::ungroup() |>
                dplyr::arrange(commune, desc(etat))

            }

            # Summarize each datashet
            dfs <- list(
              regener_cons_ae_year,
              regener_cons_ae_use,
              regener_cons_ae_aff
              ) |>
              purrr::map(\(df){
                summarize_regener_cons(df)
              })


            # Check each dataset is identical, by pair 1<->2<->3
            expect_equal(dfs[[1]], dfs[[2]])
            expect_equal(dfs[[2]], dfs[[3]])

          })

