#' Limits for canton de Vaud - Canton
#'
#' A simplified polygon layer for canton de Vaud, taken from viageo.ch.
#' @keywords datasets
#' @format A tibble with various rows and columns.
#' @source <https://viageo.ch/md/5be7ce8a-62b8-4031-8caa-5dfe7c0ef089>
#' @export
"sf_canton"

#' Limits for canton de Vaud - Communes
#'
#' A simplified polygon layer for communes in Canton de Vaud, taken from viageo.ch.
#' @keywords datasets
#' @format A tibble with various rows and columns.
#' @source <https://viageo.ch/md/5be7ce8a-62b8-4031-8caa-5dfe7c0ef089>
#' @export
"sf_communes"

#' Limits for canton de Vaud - Districts
#'
#' A simplified polygon layer for districts in Canton de Vaud, taken from viageo.ch.
#' @keywords datasets
#' @format A tibble with various rows and columns.
#' @source <https://viageo.ch/md/5be7ce8a-62b8-4031-8caa-5dfe7c0ef089>
#' @export
"sf_districts"

#' Limits for canton de Vaud - Lakes
#'
#' A simplified polygon layer for lakes in Canton de Vaud, taken from viageo.ch.
#' @keywords datasets
#' @format A tibble with various rows and columns.
#' @source <https://viageo.ch/md/5be7ce8a-62b8-4031-8caa-5dfe7c0ef089>
#' @export
"sf_lacs"

#' Building subsidies by commune for canton de Vaud
#'
#' The dataset is an aggregation and simplification of the main subsidies paid by
#' Etat de Vaud for renovation and changes of heat producers in buildings. The
#' program is called "Programme Bâtiments", and the primary data source is
#' IWF's platform used to process the subsidies. Data is acquired
#' through an authenticated API by DGE-DIREN, and data is pre-processed locally
#' before making it available to the application.
#' Energy-units should be expressed in kWh, and CO2 in tons.
#' @name subsidies_by_building
#' @keywords datasets
#' @format A tibble with various rows and cols which are not detailed yet.
#' @source <https://www.leprogrammebatiments.ch/fr>
#' @export
"subsidies_by_building"

#' Measure subsidies by commune for canton de Vaud
#'
#' The dataset is a reflection of the main subsidies paid by Etat de Vaud for
#' renovation and changes of heat producers in buildings. The
#' program is called "Programme Bâtiments", and the primary data source is
#' IWF's platform used to process the subsidies. Data is acquired
#' through an authenticated API by DGE-DIREN, and data is pre-processed locally
#' before making it available to the application.
#' Energy-units should be expressed in kWh, and CO2 in tons.
#' @name subsidies_by_measure
#' @keywords datasets
#' @format A tibble with various rows and cols which are not detailed yet
#' @source <https://www.leprogrammebatiments.ch/fr>
#' @export
"subsidies_by_measure"

#' @rdname subsidies_by_measure
#' @export
"subsidies_doc"

#' Electricity consumption by commune for canton de Vaud
#'
#' The dataset is an aggregation and simplification of all electricity delivery datapoints
#' which are yearly transmitted to DGE-DIREN by the distribution network managers.
#' Data is cleaned, affiliated to municipalities then aggregated and sent to the app.
#' @name elec_cons
#' @keywords datasets
#' @format A tibble with various rows and cols which are not detailed yet
#' @source DGE-DIREN(<https://www.vd.ch/>)
#' @export
"elec_cons"

#' @rdname elec_cons
#' @export
"elec_cons_doc"

#' Electricity production by commune for canton de Vaud
#'
#' The dataset is an aggregation and simplification of all electricity production
#' installations recorded in Canton de Vaud by PRONOVO AG. The data is split by
#' category of installation (i.e., photovoltaics, wind turbines, etc.). The data
#' results in a fairly complex methodology where DGE-DIREN provides some estimates
#' for some types of installation whose production is not recorded, or for self-consumption.
#' The data is accompanied by a separate dictionary dataset `elec_prod_doc`.
#' Energy-units should be expressed in kWh, and CO2 in tons.
#' @name elec_prod
#' @keywords datasets
#' @format A tibble with various rows and cols which are not detailed yet
#' @source <https://www.pronovo.ch/fr>
#' @export
"elec_prod"

#' @rdname elec_prod
#' @export
"elec_prod_doc"

#' RegEner datasets
#'
#' These datasets all originate from different aggregations of the original
#' RegEner (registre énergétique des bâtiments vaudois) which is created,
#' updated, and maintained by DGE-DIREN. This dataset provides estimates of
#' the energy consumed for heating and hot water of all heated VD buildings.
#' `regener_cons_ae_aff` and `regener_cons_ae_use` show consumption by energy source
#' and affectation (respectively final use) by commune. `regener_needs` shows
#' heating needs, `regener_misc` shows non-energetic data (surface, number of buildings, etc.)
#' and finally `regener_doc` documents the important variables.
#' Energy-units should be expressed in kWh, and CO2 in tons.
#' @name regener_datasets
#' @keywords datasets
#' @format A tibble with various rows and cols which are not detailed yet
#' @source <https://www.vd.ch/toutes-les-autorites/departements/departement-de-la-jeunesse-de-lenvironnement-et-de-la-securite-djes/direction-generale-de-lenvironnement-dge/diren-energie>
#' @export

#' @rdname regener_datasets
#' @export
"regener_cons_ae_aff"

#' @rdname regener_datasets
#' @export
"regener_cons_ae_use"

#' @rdname regener_datasets
#' @export
"regener_needs"

#' @rdname regener_datasets
#' @export
"regener_misc"

#' @rdname regener_datasets
#' @export
"regener_doc"

#' Natural gas distribution by commune for canton de Vaud
#'
#' The dataset is an aggregation and simplification of all natural gas delivery datapoints
#' which are yearly transmitted to DGE-DIREN by the distribution network managers.
#' Data is cleaned, affiliated to municipalities then aggregated and sent to the app.
#' @name ng_cons
#' @keywords datasets
#' @format A tibble with various rows and cols which are not detailed yet
#' @source DGE-DIREN(<https://www.vd.ch/>)
#' @export
"ng_cons"

#' @rdname ng_cons
#' @export
"ng_cons_doc"

#' Glossary
#'
#' This dataset retrieves some specific terms which may not be obvious for lay people and provides explanations.
#' @name glossary
#' @keywords datasets
#' @format A tibble with one column for the term, and one for the explanation.
#' @export
"glossary"

#' Motorisation rate by commune for canton de Vaud
#'
#' The dataset comes from DGMR office and quantifies the number of motorised
#' vehicles per 1'000 inhabitants.
#' @keywords datasets
#' @format A tibble with various rows and cols which are not detailed yet
#' @source DGMR (<https://www.vd.ch/>)
#' @export
"taux_motorisation"

#' Share of electric vehicles by commune for canton de Vaud
#'
#' The dataset comes from DGMR office and quantifies the share of (exclusively)
#' electric passenger cars among all the passenger cars registered in Canton de Vaud.
#' @name part_voit_elec
#' @keywords datasets
#' @format A tibble with various rows and cols which are not detailed yet
#' @source DGMR (<https://www.vd.ch/>)
#' @export
"part_voit_elec"

#' Quality index for public transport services by commune for canton de Vaud
#'
#' The dataset comes from DGMR office and quantifies public transport services.
#' Two indices are calculated, by population and employment.
#' @name qualite_desserte
#' @keywords datasets
#' @format A tibble with various rows and cols which
"qualite_desserte"

#' Urban canopy surface by commune for canton de Vaud
#'
#' The dataset comes from DGE-BIODIV office and quantifies how much of the
#' urban area is covered by more or less than 3m canopy height.
#' @name surface_canopee
#' @keywords datasets
#' @format A tibble with various rows and cols which
"surface_canopee"

#' Buildings exposed to natural hazards
#'
#' The dataset comes from DGE-UDN office and quantifies how many heated buildings
#' are exposed to mild/high natural hazards.
#' @name batiment_danger
#' @keywords datasets
#' @format A tibble with various rows and cols which
"batiment_danger"
