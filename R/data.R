#' Limits for Canton de Vaud
#'
#' The different layers required to map the Canton de Vaud are stored in
#' `sf_canton`, `sf_communes`, `sf_districts` and `sf_lacs`. These are
#' simplified polygons (accuracy is not required here) taken from viageo.ch (see source)
#' and loaded as shapefiles with {sf} library
#' These layers must be updated when some merging between communes happens
#' @name sf_layers
#' @keywords datasets
#' @format Each is a tibble with various rows and cols which are not detailed yet
#' @source <https://viageo.ch/md/5be7ce8a-62b8-4031-8caa-5dfe7c0ef089>
#' @examples
#' sf_canton

"sf_canton"

#' @rdname sf_layers
"sf_communes"

#' @rdname sf_layers
"sf_districts"

#' @rdname sf_layers
"sf_lacs"



#' Building subsidies by commune for Canton de Vaud
#'
#' The dataset is an agregation and simplification of the main subsidies paid by
#' Etat de Vaud for renovation and changes of heat producers in buildings. The
#' programm is called "Programme Bâtiments", and the primary data source is
#' IWF's platform used to process the subsidies. Data is acquired
#' through a authentified API by DGE-DIREN, and data is pre-processed locally
#' before making it available to the application.
#' @name subsidies_by_building
#' @keywords datasets
#' @format A tibble with various rows and cols which are not detailed yet
#' @source <https://www.leprogrammebatiments.ch/fr>
#' @examples
#' subsidies_by_building

"subsidies_by_measure"

#' Measure subsidies by commune for Canton de Vaud
#'
#' The dataset is a reflect of the main subsidies paid by Etat de Vaud for
#' renovation and changes of heat producers in buildings. The
#' programm is called "Programme Bâtiments", and the primary data source is
#' IWF's platform used to process the subsidies. Data is acquired
#' through a authentified API by DGE-DIREN, and data is pre-processed locally
#' before making it available to the application.
#' @name subsidies_by_measure
#' @keywords datasets
#' @format A tibble with various rows and cols which are not detailed yet
#' @source <https://www.leprogrammebatiments.ch/fr>
#' @examples
#' subsidies_by_measure

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
#' @examples
#' elec_cons.rda

"elec_cons"

#' @rdname elec_cons
"elec_cons_doc"


#' Electricity production by commune for Canton de Vaud
#'
#' The dataset is an agregation and simplification of all electricity production
#' installations recorded in canton de Vaud by PRONOVO AG. The data is split by
#' category of installation (i.e. photovoltaïcs, wind turbines, etc.). The data
#' results in a fairly complex methodology where DGE-DIREN provides some estimates
#' for some type of installation which production is not recorded, or for self-consumption.
#' The data is accompanied with a separate dictionnary dataset `elec_prod_doc`.
#' @name elec_prod
#' @keywords datasets
#' @format A tibble with various rows and cols which are not detailed yet
#' @source <https://www.pronovo.ch/fr>
#' @examples
#' elec_prod.rda

"elec_prod"

#' @rdname elec_prod
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
#' and finally `regener_doc` documents the important variables
#' @name regener_datasets
#' @keywords datasets
#' @format A tibble with various rows and cols which are not detailed yet
#' @source <https://www.vd.ch/toutes-les-autorites/departements/departement-de-la-jeunesse-de-lenvironnement-et-de-la-securite-djes/direction-generale-de-lenvironnement-dge/diren-energie>
#' @examples
#' regener_cons_ae_aff

#' @rdname regener_datasets
"regener_cons_ae_aff"

#' @rdname regener_datasets
"regener_cons_ae_use"

#' @rdname regener_datasets
"regener_needs"

#' @rdname regener_datasets
"regener_misc"

#' @rdname regener_datasets
"regener_doc"
