#' Limits for Canton de Vaud - Canton
#'
#' A simplified polygon layer for Canton de Vaud, taken from viageo.ch.
#' @keywords datasets
#' @format A tibble with various rows and columns.
#' @source <https://viageo.ch/md/5be7ce8a-62b8-4031-8caa-5dfe7c0ef089>
#' @export
"sf_canton"

#' Limits for Canton de Vaud - Communes
#'
#' A simplified polygon layer for communes in Canton de Vaud, taken from viageo.ch.
#' @keywords datasets
#' @format A tibble with various rows and columns.
#' @source <https://viageo.ch/md/5be7ce8a-62b8-4031-8caa-5dfe7c0ef089>
#' @export
"sf_communes"

#' Limits for Canton de Vaud - Districts
#'
#' A simplified polygon layer for districts in Canton de Vaud, taken from viageo.ch.
#' @keywords datasets
#' @format A tibble with various rows and columns.
#' @source <https://viageo.ch/md/5be7ce8a-62b8-4031-8caa-5dfe7c0ef089>
#' @export
"sf_districts"

#' Limits for Canton de Vaud - Lakes
#'
#' A simplified polygon layer for lakes in Canton de Vaud, taken from viageo.ch.
#' @keywords datasets
#' @format A tibble with various rows and columns.
#' @source <https://viageo.ch/md/5be7ce8a-62b8-4031-8caa-5dfe7c0ef089>
#' @export
"sf_lacs"

#' Building subsidies by commune for Canton de Vaud
#'
#' The dataset is an agregation and simplification of the main subsidies paid by
#' Etat de Vaud for renovation and changes of heat producers in buildings. The
#' programm is called "Programme Bâtiments", and the primary data source is
#' IWF's platform used to process the subsidies. Data is acquired
#' through a authentified API by DGE-DIREN, and data is pre-processed locally
#' before making it available to the application.
#' Energy-units should be expressed in kWh, and CO2 in tons.
#' @name subsidies_by_building
#' @keywords datasets
#' @format A tibble with various rows and cols which are not detailed yet.
#' @source <https://www.leprogrammebatiments.ch/fr>


"subsidies_by_building"

#' Measure subsidies by commune for Canton de Vaud
#'
#' The dataset is a reflect of the main subsidies paid by Etat de Vaud for
#' renovation and changes of heat producers in buildings. The
#' programm is called "Programme Bâtiments", and the primary data source is
#' IWF's platform used to process the subsidies. Data is acquired
#' through a authentified API by DGE-DIREN, and data is pre-processed locally
#' before making it available to the application.
#' Energy-units should be expressed in kWh, and CO2 in tons.
#' @name subsidies_by_measure
#' @keywords datasets
#' @format A tibble with various rows and cols which are not detailed yet
#' @source <https://www.leprogrammebatiments.ch/fr>


"subsidies_by_measure"


#' Electricity consumption by commune for Canton de Vaud
#'
#' The dataset is an agregation and simplification of all electricity delivery datapoints
#' which are yearly transmitted to DGE-DIREN by the distribution network managers.
#' Data is cleaned, affiliated to municipalities then aggregated and sent to the app.
#' @name elec_cons
#' @keywords datasets
#' @format A tibble with various rows and cols which are not detailed yet
#' @source DGE-DIREN(<https://www.vd.ch/>)


"elec_cons"

#' @rdname elec_cons
#' @export
"elec_cons_doc"


#' Electricity production by commune for Canton de Vaud
#'
#' The dataset is an agregation and simplification of all electricity production
#' installations recorded in canton de Vaud by PRONOVO AG. The data is split by
#' category of installation (i.e. photovoltaïcs, wind turbines, etc.). The data
#' results in a fairly complex methodology where DGE-DIREN provides some estimates
#' for some type of installation which production is not recorded, or for self-consumption.
#' The data is accompanied with a separate dictionnary dataset `elec_prod_doc`.
#' Energy-units should be expressed in kWh, and CO2 in tons.
#' @name elec_prod
#' @keywords datasets
#' @format A tibble with various rows and cols which are not detailed yet
#' @source <https://www.pronovo.ch/fr>


"elec_prod"

#' @rdname elec_prod
#' @export
"elec_prod_doc"

#' RegEner datasets
#'
#' These datasets all originate from different agregations of the original
#' RegEner (registre énergétique des bâtiments vaudois) which is created,
#' updated and maintained by DGE-DIREN. This dataset provides estimates of
#' the energy consumed for heating and hot water of all heated VD buildings.
#' `regener_cons_ae_aff` and `regener_cons_ae_use` show consumption by energy source
#' and affectation (respectively final use) by commune. `regener_needs` show
#' heating needs, `regener_misc` show non-energetic data (surface, number of buildings, etc.)
#' and finally `regener_doc` documents the important variables.
#' Energy-units should be expressed in kWh, and CO2 in tons.
#' @name regener_datasets
#' @keywords datasets
#' @format A tibble with various rows and cols which are not detailed yet
#' @source <https://www.vd.ch/toutes-les-autorites/departements/departement-de-la-jeunesse-de-lenvironnement-et-de-la-securite-djes/direction-generale-de-lenvironnement-dge/diren-energie>


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


#' Natural gas distribution by commune for Canton de Vaud
#'
#' The dataset is an agregation and simplification of all natural gas delivery datapoints
#' which are yearly transmitted to DGE-DIREN by the distribution network managers.
#' Data is cleaned, affiliated to municipalities then aggregated and sent to the app.
#' @name ng_cons
#' @keywords datasets
#' @format A tibble with various rows and cols which are not detailed yet
#' @source DGE-DIREN(<https://www.vd.ch/>)


"ng_cons"

#' @rdname ng_cons
#' @export
"ng_cons_doc"


#' Glossary
#'
#' This dataset retrieves some specific terms which may not be obvious for lay people and provides explanations
#' @name glossary
#' @keywords datasets
#' @format A tibble with one column for the term, and one for the explanation.

"glossary"
