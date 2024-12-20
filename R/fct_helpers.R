# Info fn ----

#' info_dev_message()
#' shiny modal message to inform the user about the app, and offer the possibility to have a guided tour (introjs)
#' @return A a modal object when opening the app

info_dev_message <- function(){

  showModal(
    modalDialog(size = "l",
                fade = TRUE,
                easyClose = TRUE,
                footer = tagList(
                  tags$div(class = "d-flex justify-content-evenly w-100",
                           actionButton(inputId = "modal_info", "En savoir plus", class = "btn-outline-secondary"), # will be used to redirect to a page,
                           actionButton(inputId = "introjs", "Tour guidé de l'application", class = "btn-outline-secondary"), # will run introJS in app_server.R
                           modalButton(label = "C'est parti !")
                  )
                ),
                # Modal content
                tagList(
                  # Header section with company logo
                  div(class = "modal-header d-flex align-items-start flex-column",
                      img(src = "www/vd-logo-black.svg", height = "35px", alt = "Etat de Vaud", class = "customLogo"),
                      h6("Bienvenue sur le", style = "align-self:left;padding-top:30px;"),
                      h4("Profil climatique des communes vaudoises", style = "align-self:left;")
                  ),
                  tags$br(),
                  # Middle text
                  div(class = "px-4",
                      tags$p("Cette application est mise à disposition par l'",
                             tags$a(href = link_ocdc, target = "_blank", "Office cantonal de la durabilité et du climat"),
                             "et la ",
                             tags$a(href = link_diren, target = "_blank", "Direction de l'énergie"),
                             " pour diffuser des données énergétiques et climatiques à l'échelle des communes vaudoises, notamment pour la réalisation des ",
                             tags$a(href = link_pecc, target = "_blank", "plans énergie et climat communaux (PECC).")

                      )
                  ),
                  # Grey warning area
                  div(class = "modal-warning d-flex align-items-center",
                      shiny::icon("warning", class = "fa-2x me-3"), # Add margin to the right of the icon
                      div(
                        "Les données sont susceptibles de changer de manière rétroactive.",
                        "Il est donc important de les interpréter avec précaution car des améliorations méthodologiques peuvent avoir lieu."
                      )
                  )
                )
    )
  )

}


# Statbox items ----

#' make_statbox_item()
#' custom replacement for valueboxes (too rigid) to fill statboxes with icon, title, value and year info
#' @return a HTML div container
#' @export
#' @import shiny
#'
#' @examples
#' make_statbox_item(iconBgClass = NULL, title = "Rescues", value = 100, unit = "people", year = 2022)
make_statbox_item <- function(iconBgClass,
                              title,
                              value,
                              unit,
                              year){


  tags$div(class = glue::glue("text-center padding-top-1 rounded {iconBgClass}"),

           p(HTML(title), class = "p-0 m-0", style = "font-size:1.1rem;font-weight:500;"),
           tags$div(
             # Nicely format value (rounded + big.mark) and add unit below as newline
             strong(HTML(format(round(value, digits = 0),
                                big.mark = "'",
                                zero.print = "-" # ! important when no commune is selected,0 is passed
             )),
             style = "font-size:1.3rem;"),
             strong(p(unit, style = "font-size:1.2rem;")),
             p(year, style = "font-size:1.1rem;font-weight:500;")
           )
  )

}

# Accordion documentation ----

#' generate_doc_accordion_panels()
#' Create HTML `bslib::accordion_panel()` items for each dataset documented in ./data-doc. These are then used and
#' spliced as arguments inside a `bslib::accordion()` item. Each markdown h2 heading is used as a `title` argument for
#' `accordion_panel()` and each paragraph (i.e. not a h2 header) is used as content to fill the `accordion_panel()`.
#' @param md_file the documentation filename stored in `./data-doc`
#' @import dplyr
#' @importFrom tidyr tibble fill
#' @importFrom stringr str_detect str_starts str_remove
#' @importFrom purrr map2
#' @importFrom bslib accordion_panel
#' @importFrom shiny markdown
#' @importFrom bsicons bs_icon
#'
#' @return a list of HTML accordion_panels to be spliced in a `bslib::accordion()`
#' @export

generate_doc_accordion_panels <- function(md_file){

  # Read the doc md file into a vector of lines
  markdown_lines <- readLines(md_file, skipNul = TRUE)

  # Which pattern identifies a section for accordion_panel()
  section_pattern <- "## "

  # Create a tibble with section headers and paragraphs
  # this code allows returns of line within a paragraph due to summarise call
  parsed_content <- tidyr::tibble(line = markdown_lines) |>
    # Remove comments and empty lines
    dplyr::filter(!stringr::str_detect(line, "<!---|^\\s*$")) |>
    # Remove main header (1x #)
    dplyr::filter(!stringr::str_starts(line, "#(?!#)")) |>
    # Identify section headers
    dplyr::mutate(section_header = ifelse(stringr::str_starts(line, section_pattern), stringr::str_remove(line, "^##\\s*"), NA)) |>
    # Fill section headers down for associated paragraphs
    tidyr::fill(section_header, .direction = "down") |>
    # Identify paragraphs
    dplyr::mutate(paragraph = ifelse(stringr::str_starts(line, section_pattern), NA, line)) |>
    # Drop section headers without paragraph
    dplyr::filter(!is.na(paragraph)) |>
    # Recombine full paragraphs (replaces line breaks by a space)
    dplyr::summarise(paragraph = paste(paragraph, collapse = " "),
                     .by = section_header)

  # Map titles & values into separate accordion_panels
  items <- purrr::map2(parsed_content$section_header,
                       parsed_content$paragraph,
                       function(title, paragraph){

    bslib::accordion_panel(
      title,
      icon = bsicons::bs_icon("question-circle", class = "text-secondary"),
      shiny::tags$div(class = "customPanel",
                      shiny::markdown(paragraph))
    )

  })

  # Return list of accordion_panels
  return(items)

}

# Graph fns ----

#' create_select_leaflet()
#'
#' @description Creates the non-reactive part of the home leaflet map to select municipalities and interact with selectInputs.
#' @import leaflet
#' @return A leaflet base map without reactivity.
#'
#' @noRd

create_select_leaflet <- function(sf_districts,
                                  sf_lacs,
                                  sf_communes){

  leaflet::leaflet(options = leaflet::leafletOptions(
    zoomControl = TRUE,
    zoomSnap = 0.1, # small scroll zoom
    zoomDelta = 0.2, # bigger button zoom
    minZoom = 9, # lock the back zoom range
    maxZoom = 12, # limit zoom max
    attributionControl = F # remove leaflet url
  )) |>
    # Couche de base des districts si un district est sélectionné
    leaflet::addPolygons(data = sf_districts,
                         fillColor = NULL,
                         fillOpacity = 0,
                         color = "black",
                         label = ~NOM_MAJ,
                         weight = 2,
                         # group is then used in leafem::addHomeButton()
                         group = "Vue cantonale") |>
    # Couche des lacs
    leaflet::addPolygons(data = sf_lacs,
                         fillColor = "lightblue",
                         color = "grey",
                         weight = 1,
                         # Si clickable : l'app crash car lac =/= commune !
                         options = leaflet::pathOptions(clickable = FALSE)) |>
    # Première couche des communes (en blanc, état non-sélectionné)
    leaflet::addPolygons(data = sf_communes,
                         fillColor = "white",
                         color = "grey",
                         weight = 1,
                         # layerID is needed to throw back the right item when clicked
                         layerId = ~NOM_MIN,
                         label = ~NOM_MIN,
                         group = "regions",
                         highlightOptions = leaflet::highlightOptions(
                           weight = 5,
                           fillColor = NULL,
                           fillOpacity = NULL,
                           color = "#3A862D",
                           bringToFront = FALSE)) |>
    # Seconde couche des communes (en rouge, état sélectionné)
    leaflet::addPolygons(data = sf_communes,
                         fillColor = "#3A862D",
                         fillOpacity = 1,
                         weight = 1,
                         color = "black",
                         stroke = TRUE,
                         layerId = ~NO_COM_FED, # any random layerId other than NOM_MIN (required)
                         label = ~NOM_MIN,
                         group = ~NOM_MIN,
                         highlightOptions = leaflet::highlightOptions(
                           weight = 5,
                           fillColor = NULL,
                           fillOpacity = NULL,
                           color = "#CFCFCF",
                           bringToFront = FALSE)) |>
    # This will be switched on/off through the code below using click events
    leaflet::hideGroup(group = sf_communes$NOM_MIN)   |>
    # Set the background to white
    leaflet.extras::setMapWidgetStyle(list(background= "white")) |>
    # fitbounds with coordinates. ! tweak along with zoomSnap/zoomDelta
    # leaflet::setView(lng = 6.617,
    #                  lat = 46.63,
    #                  zoom = 9.3) |> # or fitBounds(lng1 = 6.50, lat1 = 46.18, lng2 = 6.54, lat2 = 47.15
    # Set max limits to avoid panning away from the map
    leaflet::setMaxBounds(lng1 = 5.7,
                          lat1 = 45.9,
                          lng2 = 7.8,
                          lat2 = 47.4)

}


#' create_bar_ggiraph()
#'
#'@description Creates a girafe object from a facetted ggplot bar plot for use in renderGirafe
#'
#' @param data the data to provide
#'
#'
#' @import ggplot2
#' @importFrom ggiraph girafe
#' @return an interactive girafe object
#' @export

create_bar_ggiraph <- function(data,
                               n_communes,
                               var_year,
                               var_commune,
                               unit, # input$energy_unit, or other unit value retrieved in app_server
                               var_cat, # one of secteur, categorie...
                               var_values, # one of consommation, production_totale...
                               color_palette, # utils_helpers.R,
                               dodge = FALSE, # stacked by default
                               free_y = FALSE,
                               legend_title = NULL,
                               height_svg,
                               width_svg,
                               ... # free
){

  # Compute totals for conditional geom_text (if stacked)
  data_totals <- data |>
    dplyr::group_by(.data[[var_year]], .data[[var_commune]]) |>
    dplyr::summarise(total = sum(.data[[var_values]], na.rm = TRUE))

  # Compute ggplot2
  ggplot <- data |>
    ggplot2::ggplot(ggplot2::aes(x = as.factor(.data[[var_year]]),
                                 y = .data[[var_values]],
                                 # built conditionnally if var_cat is supplied else nothing
                                 fill = if(!is.null(var_cat)){.data[[var_cat]]},

                                 # built conditionnally if var_cat is supplied else var_year only
                                 data_id = if(!is.null(var_cat)){paste0(.data[[var_year]], .data[[var_cat]])}else{.data[[var_year]]},

                                 # built conditionnally if var_cat is supplied and if unit is %
                                 tooltip = paste0(
                                   if(!is.null(var_cat)){paste0(.data[[var_cat]], "\n")},
                                   # if unit percent --> show percents nicely
                                   if(unit == "%"){
                                     paste(
                                       scales::percent(.data[[var_values]], accuracy = 0.1), " en ", .data[[var_year]]
                                     )
                                   }else{
                                     paste(
                                     format(round(.data[[var_values]], digits = 0), big.mark = "'"),
                                     unit, "en ", .data[[var_year]]
                                     )
                                   }
    )
    ))


  # geom_text conditionnally --> only if bars are stacked & no var_cat supplied

    if (!isTRUE(dodge) & !is.null(var_cat)) {
      ggplot <- ggplot +
        ggiraph::geom_text_interactive(
          data = data_totals,
          ggplot2::aes(x = as.factor(.data[[var_year]]),
                       y = total,
                       label = if(unit == "%"){
                         scales::percent(total, accuracy = 0.01)}else{
                           format(total, big.mark = "'", digits = 1, scientific = FALSE)
                         }
          ),
          vjust = -0.5,
          size = 10,
          size.unit = "pt", # defaults to mm...
          fontface = "plain",
          color = "grey20",
          inherit.aes = FALSE # ensures we loock at data_totals and not data
        )
    }


  # Fill bars according to two options : either 'color_palette' is a single value when var_cat is NULL ; or it's a palette
  # If one color is supplied : we must pass it to 'fill' of geom_col(), scale_fill_manual would not accept it
  if(length(color_palette) == 1){
    ggplot <- ggplot +
      ggiraph::geom_col_interactive(position = dplyr::if_else(condition = dodge, # arg
                                                              true = "dodge",
                                                              false = "stack"),
                                    fill = color_palette)
  }else{
    # If a palette is passed : we must pass it fo 'scale_fill_manual' with a legend name
    ggplot <- ggplot +
      ggiraph::geom_col_interactive(position = dplyr::if_else(condition = dodge, # arg
                                                              true = "dodge",
                                                              false = "stack"))+
      ggplot2::scale_fill_manual(
        name = legend_title, # passed from arg
        values = color_palette # palette defined in utils_helpers.R
      )
  }


  # Scales, facts, theme options etc.
  ggplot <- ggplot+
    ggplot2::scale_y_continuous(labels = ifelse(unit == "%",
                                                scales::percent,
                                                scales::label_number(big.mark = "'")
                                                ),
                                expand = ggplot2::expansion(mult = c(0, 0.15))
    )+
    ggplot2::labs(x = "", y = unit)+
    ggiraph::facet_wrap_interactive(facets = ggplot2::vars(.data[[var_commune]]),
                                    ncol = 2,
                                    # if the toggle linked to the free_y argument is TRUE, then free y axis
                                    scales = ifelse(free_y, "free_y", "fixed"))+
    ggplot2::theme_bw(base_size = 12)+
    ggplot2::theme(
      legend.position = "top",        # Move legend to the top
      legend.direction = "horizontal", # Arrange items horizontally
      legend.justification = c(0, 0), # Align legend box to the top-left corner
      legend.box.just = "left",
      #change the labels of facet wrap. main_color defined in utils_helpers.R
      strip.background = ggplot2::element_rect(color="black", fill=main_color, linewidth = 0.5, linetype="solid"),
      strip.text = ggplot2::element_text(color = "white", size = 12),
      legend.background = ggplot2::element_rect(fill = NA), # transparent
      panel.spacing.x = ggplot2::unit(.05, "cm"),
      panel.spacing.y = ggplot2::unit(0.5, "cm"),
      panel.grid.major.x = element_blank()
    )

  ggiraph::girafe(ggobj = ggplot,
                  height_svg = height_svg,
                  width_svg = width_svg,
                  options = list(
                    ggiraph::opts_sizing(
                      rescale = TRUE, width = 1
                    ),
                    ggiraph::opts_hover(
                      css = ""
                    ),
                    ggiraph::opts_hover_inv(
                      css = "opacity:0.6;"
                    ),
                    ggiraph::opts_toolbar(
                      position = "topleft",
                      fixed = TRUE,
                      pngname = "profil_climatique",
                      tooltips = list(
                        saveaspng = "Télécharger (.png)"
                      )
                    ),
                    ggiraph::opts_tooltip(
                      opacity = 0.8
                    ),
                    ggiraph::opts_selection(
                      type = "none"
                    )
                  )
  )


}


#' create_alluvial_chart()
#' creates a ggplot2 alluvial plot using ggalluvial library and uses labels and variable
#' names as arguments for a flexible data input
#'
#' @param data the dataset used to create the ggalluvial plot
#' @param var_commune the variable holding the commune name
#' @param label_from legend located below the left side of the alluvial
#' @param label_to legend located below the right side of the alluvial
#' @param var_from the variable for the left stratum
#' @param var_flow the variable that quantifies the flows from `var_from` to `var_to`
#' @param var_to the variable for the right stratum
#' creates a ggplot2 alluvial chart using the ggalluvial library and heat building
#' consumption data from an aggregated RegEner dataset
#' @return a ggplot2 object for regener alluvial visualisations
#' @export

create_alluvial_chart <- function(data,
                                  var_commune,
                                  var_flow,
                                  var_from,
                                  label_from,
                                  var_to,
                                  label_to){

  # Following https://stackoverflow.com/questions/67142718/embracing-operator-inside-mutate-function
  # Very tough subject, no idea why this ' := ' or {{ }} are required


  # data plotting
  data |>
    ggplot2::ggplot(ggplot2::aes(axis1 = .data[[var_from]],
                                 axis2 = .data[[var_to]],
                                 y = .data[[var_flow]],
                                 label = .data[[var_flow]]))+
    ggalluvial::geom_alluvium(ggplot2::aes(fill = .data[[var_from]]),
                              width = 3/8, reverse = FALSE) +
    ggalluvial::geom_stratum(alpha = .25, width = 3/8, reverse = FALSE) +
    ggplot2::geom_label(stat = ggalluvial::StatStratum,
                        alpha = .65, size = 4.5,
                        ggplot2::aes(label = paste0(after_stat(stratum),
                                                    " ",
                                                    scales::percent(after_stat(prop), accuracy = 0.1))),
                        reverse = FALSE) +
    ggplot2::scale_x_continuous(breaks = 1:2, labels = c(label_from, label_to)) +
    ggplot2::scale_fill_manual(values =  return_palette_regener())+ # fcts_helpers.R
    ggplot2::facet_wrap(facets = var_commune,
                        scales = "free",
                        ncol = 2)+
    ggplot2::theme_void()+
    ggplot2::theme(legend.position = "none",
                   strip.text = element_text(size =16),
                   axis.text.x = element_text(size = 14)) |>
    suppressWarnings() # avoid annoying warning due to 'Autres' <fct> in both strata

}


#' lump_alluvial_factors()
#' takes a dataframe structured for ggalluvial and lumps the factor variables (var_from, var_to)
#' according to two {forcats} functions which arguments should be modified in the code
#'
#' @param data the dataset used to create the ggalluvial plot
#' @param var_from the variable for the left stratum
#' @param var_flow the variable that quantifies the flows from `var_from` to `var_to`
#' @param var_to the variable for the right stratum
#'
#' @return a ggplot object
#' @export

lump_alluvial_factors <- function(data,
                                  var_commune,
                                  var_from,
                                  var_flow,
                                  var_to){

  # lumping factors both left and right of alluvia to 4 max (for readability)
  # fct_lump_prop won't lump a factor ALONE, there needs to be 2 factors to meet the prop criteria

  data |>
    dplyr::group_by(.data[[var_commune]]) |> # !! must group_by commune
    dplyr::mutate({{var_from}} := forcats::fct_lump_n(f = .data[[var_from]],
                                                      n = 3,
                                                      w = .data[[var_flow]],
                                                      other_level = "Autres sources")) |>
    dplyr::mutate({{var_to}} := forcats::fct_lump_prop(f = .data[[var_to]],
                                                       prop = 0.1, # 10% min to appear individually
                                                       w = .data[[var_flow]],
                                                       other_level = "Autres"))

}


# Table fns ----

#' create_table_dt()
#'
#' @param data Specific electricity production, DGE-DIREN data to transform to datatable
#' Must follow Pronovo's outputs and utils_helpers.R format
#' @param unit Unit currently selected inside the app
#' @param DT_dom datatable 'dom' Option, see datatable documentation. Likely Bfrtip or frtip
#'
#' @import dplyr
#' @importFrom stringr str_replace_all str_to_title
#' @return A DT table with export functionalities
#' @export

create_prod_table_dt <- function(data,
                                 energy_unit,
                                 DT_dom = "Bfrtip" # we set default with Buttons
){

  data |>
    dplyr::arrange(desc(annee)) |>
    # Basic clean up for table output
    dplyr::mutate(
      # change year to factor |>
      annee = as.factor(annee),
      # format numeric cols
      #  because of the NA->"Confidentiel" JS code in DT options (see below) we need
      #  to keep NAs alive with an if_else statement (only needed for this fn)

      across(where(is.numeric), ~ if_else(condition = !is.na(.x),
                                          true = format(.x,
                                                        big.mark = "'",
                                                        digits = 3,
                                                        drop0trailing = TRUE,
                                                        scientific = FALSE),
                                          false = NA_character_ )
      )
    ) |>

    dplyr::select(-c(numero_de_la_commune)) |>
    dplyr::relocate(commune, annee, categorie,
                    production, injection, autoconsommation,
                    puissance_electrique_installee) |>
    # add icons HTML tags from utils_helpers.R
    dplyr::left_join(prod_icons, by = "categorie") |>
    dplyr::relocate(icon, .before = categorie) |>
    dplyr::rename(" " = "icon") |> # empty colname for icons
    add_colname_unit(colnames = c("puissance_electrique_installee",
                                  "injection",
                                  "autoconsommation",
                                  "production"),
                     unit = energy_unit) |>  # fct_helpers.R
    rename_fr_colnames() |>  # fct_helpers.R
    #turn to DT
    DT::datatable(escape = F, # rendering the icons instead of text
                  extensions = 'Buttons',
                  options = list(paging = TRUE,    # paginate the output
                                 pageLength = 8,  # number of rows to output for each page
                                 scrollY = TRUE,   # enable scrolling on Y axis
                                 autoWidth = TRUE, # use smart column width handling
                                 server = FALSE,   # use server-side processing
                                 dom = DT_dom,
                                 buttons = list(
                                   list(extend = 'copy', text = "Copier"),
                                   list(extend = 'excel', filename = paste0("elec_prod_vd_", Sys.Date()))
                                 ),
                                 columnDefs = list(list(
                                   targets = "_all",
                                   className = 'dt-center',
                                   # NA to custom string : see https://github.com/rstudio/DT/issues/322
                                   render = DT::JS(
                                     "function(data, type, row, meta) {",
                                     "return data === null ? '(Confidentiel)' : data;",
                                     "}")
                                 )),


                                 # https://rstudio.github.io/DT/004-i18n.html   for languages
                                 language = DT_fr_language # from utils_helpers.R !
                  ),
                  selection = 'single', # enable selection of a single row
                  rownames = FALSE      # don't show row numbers/names
    ) # End DT
}


#' create_cons_table_dt()
#'
#' @param data Specific electricity consumption, DGE-DIREN data to transform to datatable
#' Must follow specific data format which can be found in /data
#' @param unit Unit currently selected inside the app
#' @param DT_dom datatable 'dom' Option, see datatable documentation. Likely Bfrtip or frtip
#'
#' @import dplyr
#' @return A DT table with export functionalities
#' @export

create_cons_table_dt <- function(data,
                                 energy_unit,
                                 DT_dom = "Bfrtip" # we set default with Buttons
){

  data |>
    dplyr::arrange(desc(annee)) |>
    # Basic clean up for table output
    dplyr::mutate(
      # change year to factor
      annee = as.factor(annee),
      # format numeric cols
      dplyr::across(where(is.numeric), ~format(.x,
                                               big.mark = "'",
                                               digits = 3,
                                               drop0trailing = TRUE,
                                               scientific = FALSE))) |>
    # put installed power in the last position
    dplyr::relocate(commune, annee, secteur, consommation) |>
    # add icons HTML tags from utils_helpers.R
    dplyr::left_join(cons_icons, by = "secteur") |>
    dplyr::relocate(icon, .before = secteur) |>
    dplyr::rename(" " = "icon") |> # empty colname for icons
    add_colname_unit(colnames = "consommation",
                      unit = energy_unit) |>  # fct_helpers.R
    rename_fr_colnames() |> # fct_helpers.R
    #turn to DT
    DT::datatable(escape = F, # rendering the icons instead of text
                  extensions = 'Buttons',
                  options = list(paging = TRUE,    # paginate the output
                                 pageLength = 15,  # number of rows to output for each page
                                 scrollY = TRUE,   # enable scrolling on Y axis
                                 autoWidth = TRUE, # use smart column width handling
                                 server = FALSE,   # use server-side processing
                                 dom = DT_dom, # dynamic according to needs
                                 buttons = list(
                                   list(extend = 'copy', text = "Copier"),
                                   list(extend = 'excel', filename = paste0("elec_prod_vd_", Sys.Date()))
                                 ),
                                 columnDefs = list(list(targets = "_all", className = 'dt-center')),
                                 # https://rstudio.github.io/DT/004-i18n.html   for languages
                                 language = DT_fr_language # from utils_helpers.R !
                  ),
                  selection = 'single', ## enable selection of a single row
                  rownames = FALSE               ## don't show row numbers/names
    ) # End DT
}

#' create_rg_needs_table_dt()
#' @param data Specific regener needs dataset to transform to datatable
#' Must follow specific data format which can be found in /data
#' @param unit Unit currently selected inside the app
#' @param DT_dom datatable 'dom' Option, see datatable documentation. Likely Bfrtip or frtip
#'
#' @return a DT table with export functionnalities
#' @export

create_rg_needs_table_dt <- function(data,
                                     unit,
                                     DT_dom = "Bfrtip" # we set default with Buttons
){

  data |>
    dplyr::arrange(desc(etat)) |>
    # Basic clean up for table output
    dplyr::mutate(
      # change year (etat for rg dataset) to factor
      etat = as.factor(etat),
      # format numeric cols
      dplyr::across(where(is.numeric), ~format(.x,
                                               big.mark = "'",
                                               digits = 3,
                                               drop0trailing = TRUE,
                                               scientific = FALSE))) |>
    # add icons HTML tags from utils_helpers.R
    dplyr::left_join(regener_icons_type, by = "type") |>
    # relocate call
    dplyr::relocate(commune, etat, icon, type) |>
    dplyr::rename(" " = "icon") |> # empty colname for icons
    add_colname_unit(colnames = dplyr::contains("besoins"), # fct_helpers.R
                      unit = unit) |>
    rename_fr_colnames() |> # fct_helpers.R
    #turn to DT
    DT::datatable(escape = F, # rendering the icons instead of text
                  extensions = 'Buttons',
                  options = list(paging = TRUE,    # paginate the output
                                 pageLength = 15,  # number of rows to output for each page
                                 scrollY = TRUE,   # enable scrolling on Y axis
                                 autoWidth = TRUE, # use smart column width handling
                                 server = FALSE,   # use server-side processing
                                 dom = DT_dom, # dynamic according to needs
                                 buttons = list(
                                   list(extend = 'copy', text = "Copier"),
                                   list(extend = 'excel', filename = paste0("rgr_needs_vd_", Sys.Date()))
                                 ),
                                 columnDefs = list(list(targets = "_all", className = 'dt-center')),
                                 # https://rstudio.github.io/DT/004-i18n.html   for languages
                                 language = DT_fr_language # from utils_helpers.R !
                  ),
                  selection = 'single', ## enable selection of a single row
                  rownames = FALSE               ## don't show row numbers/names
    )# End DT

}


#' create_regener_table_dt()
#' creates a DT table with custom formatting and html icons
#' @param data Specific regener consumption table, DGE-DIREN data to transform to datatable
#' @param unit Unit currently selected inside the app
#' @param DT_dom datatable 'dom' Option, see datatable documentation. Likely Bfrtip or frtip
#'
#' @return a datatable object for regener datasets
#' @export

create_regener_table_dt <- function(data,
                                    energy_unit,
                                    co2_unit,
                                    DT_dom = "Bfrtip" # we set default with Buttons
){

  data |>
    dplyr::arrange(desc(etat)) |>
    # Set pct_commune to % display (output for dl is in numeric)
    dplyr::mutate(pct_commune = scales::percent(
      pct_commune, accuracy = 0.01)) |>
    dplyr::mutate(
      # change year to factor
      etat = as.factor(etat), # if needed later
      # format numeric cols
      dplyr::across(where(is.numeric), ~format(.x,
                                               big.mark = "'",
                                               digits = 3,
                                               drop0trailing = TRUE,
                                               scientific = FALSE))) |>
    # add icons HTML tags from utils_helpers.R
    dplyr::left_join(regener_icons, by = "ae") |>
    # relocate call
    dplyr::relocate(commune, etat, icon, ae,
                    tidyselect::any_of(c("usage", "affectation")),
                    consommation, pct_commune) |>
    dplyr::rename(" " = "icon") |> # empty colname for icons
    #turn to DT
    add_colname_unit(colnames = "consommation", unit = energy_unit) |>  # fct_helpers.R
    add_colname_unit(colnames = "co2_direct", unit = co2_unit) |> # fct_helpers.R
    add_colname_unit(colnames = "pct_commune", unit = "%") |>
    rename_fr_colnames() |> # fct_helpers.R
    DT::datatable(escape = F, # rendering the icons instead of text
                  extensions = 'Buttons',
                  options = list(paging = TRUE,    # paginate the output
                                 pageLength = 15,  # number of rows to output for each page
                                 scrollY = TRUE,   # enable scrolling on Y axis
                                 autoWidth = TRUE, # use smart column width handling
                                 server = FALSE,   # use server-side processing
                                 dom = DT_dom, # dynamic according to needs
                                 buttons = list(
                                   list(extend = 'copy', text = "Copier"),
                                   list(extend = 'excel', filename = paste0("rgr_table_vd_", Sys.Date()))
                                 ),
                                 columnDefs = list(list(targets = "_all", className = 'dt-center')),
                                 # https://rstudio.github.io/DT/004-i18n.html   for languages
                                 language = DT_fr_language # from utils_helpers.R !
                  ),
                  selection = 'single', ## enable selection of a single row
                  rownames = FALSE               ## don't show row numbers/names
    ) # End DT

}


#' create_rg_misc_table_dt()
#' Creates datatable for regener_misc dataset
#' @param data the dataset containing variables and descriptions
#' @param unit Unit currently selected inside the app
#' @param DT_dom datatable 'dom' Option, see datatable documentation. Likely Bfrtip or frtip
#'
#' @return a DT object
#' @export

create_rg_misc_table_dt <- function(data,
                                    # unit arg not needed for misc data
                                    DT_dom = "Bfrtip" # we set default with Buttons
){

  data |>
    dplyr::arrange(desc(etat)) |>
    # Basic clean up for table output
    dplyr::mutate(
      # change year (etat for rg dataset) to factor
      etat = as.factor(etat),
      # format numeric cols
      dplyr::across(where(is.numeric), ~format(.x,
                                               big.mark = "'",
                                               digits = 3,
                                               drop0trailing = TRUE,
                                               scientific = FALSE))) |>
    dplyr::relocate(commune, etat) |>
    rename_misc_colnames() |>
    # add_colname_unit(unit = unit) |>  # fct_helpers.R
    #turn to DT
    DT::datatable(escape = F, # rendering the icons instead of text
                  extensions = 'Buttons',
                  options = list(paging = TRUE,    # paginate the output
                                 pageLength = 15,  # number of rows to output for each page
                                 scrollY = TRUE,   # enable scrolling on Y axis
                                 autoWidth = TRUE, # use smart column width handling
                                 server = FALSE,   # use server-side processing
                                 dom = DT_dom, # dynamic according to needs
                                 buttons = list(
                                   list(extend = 'copy', text = "Copier"),
                                   list(extend = 'excel', filename = paste0("rgr_misc_vd_", Sys.Date()))
                                 ),
                                 columnDefs = list(list(targets = "_all", className = 'dt-center')),
                                 # https://rstudio.github.io/DT/004-i18n.html   for languages
                                 language = DT_fr_language # from utils_helpers.R !
                  ),

                  selection = 'single', ## enable selection of a single row
                  rownames = FALSE               ## don't show row numbers/names
    )# End DT

}

#' create_doc_table_dt
#' Creates minimalistic documentation table with download feature
#' @param data the dataset containing variables and descriptions
#'
#' @return A DT object
#' @export

create_doc_table_dt <- function(data,
                                doc_prefix){

  data |>
    DT::datatable(rownames = FALSE, # no index col
                  extensions = "Buttons",
                  options = list(
                    dom = "Bfti", # Button ; filter; table ; information summary
                    buttons = list(
                      list(className = "btn btn-outline-secondary mx-1",
                           extend = 'csv',
                           filename = paste0(doc_prefix, Sys.Date()),
                           title = NULL),
                      list(className = "btn btn-outline-secondary mx-1",
                           extend = 'excel',
                           filename = paste0(doc_prefix, Sys.Date()),
                           title = NULL)),
                    columnDefs = list(list(targets = 0, className = 'dt-center')), # or "_all"
                    paging = TRUE,
                    pageLength = 20,
                    scrollY = FALSE,
                    autoWidth = TRUE,
                    language = DT_fr_language # utils_helpers.R
                  ))

}



#' create_subsidies_table_dt()
#' Creates datatable for subsidies dataset. Flexible enough to handle both datasets
#' `subsidies_by_building` and `subsidies_by_measure` hence its arguments for variables.
#' @param data the subsidies dataset
#' @param var_year the time variable
#' @param DT_dom datatable 'dom' Option, see datatable documentation. Likely Bfrtip or frtip
#'
#' @return a DT object
#' @export

create_subsidies_table_dt <- function(data,
                                      var_year,
                                      var_cat,
                                      icon_list,
                                      DT_dom = "Bfrtip" # we set default with Buttons
){

  data |>
    dplyr::arrange(desc(.data[[var_year]])) |>
    # Clear undesired vars
    dplyr::select(-any_of("mesure_simplifiee")) |>
    # Basic clean up for table output
    dplyr::mutate({{var_year}} := as.factor(.data[[var_year]])) |>
    dplyr::mutate(
      # format numeric cols
      across(where(is.numeric), ~ format(.x,
                                         big.mark = "'",
                                         digits = 3,
                                         drop0trailing = TRUE,
                                         scientific = FALSE))
    )|>
    # add icons HTML tags from utils_helpers.R
    dplyr::left_join(icon_list, by = var_cat) |>
    dplyr::relocate(icon, .before = any_of(var_cat)) |>
    dplyr::rename(" " = "icon") |> # empty colname for icons
    rename_misc_colnames() |> # fct_helpers.R
    rename_fr_colnames() |>  # fct_helpers.R
    #turn to DT
    DT::datatable(escape = F, # rendering the icons instead of text
                  extensions = 'Buttons',
                  options = list(paging = TRUE,    # paginate the output
                                 pageLength = 15,  # number of rows to output for each page
                                 scrollY = TRUE,   # enable scrolling on Y axis
                                 autoWidth = TRUE, # use smart column width handling
                                 server = FALSE,   # use server-side processing
                                 dom = DT_dom,
                                 buttons = list(
                                   list(extend = 'copy', text = "Copier"),
                                   list(extend = 'excel', filename = paste0("elec_prod_vd_", Sys.Date()))
                                 ),
                                 columnDefs = list(list(
                                   targets = "_all",
                                   className = 'dt-center'
                                 )),


                                 # https://rstudio.github.io/DT/004-i18n.html   for languages
                                 language = DT_fr_language # from utils_helpers.R !
                  ),
                  selection = 'single', # enable selection of a single row
                  rownames = FALSE      # don't show row numbers/names
    ) # End DT
}

#' create_generic_table_dt
#' Creates datatable for generic datasets. Flexible enough to handle all datasets, but loses in
#'  customizability compared to other energy-specific datatable functions.
#' @param data the generic dataset
#' @param var_year the name of the year variable to sort from
#' @param DT_dom the datatable domain options to provide (e.g. 'Bfrtip')
#' @return a DT object
#' @export

create_generic_table_dt <- function(data,
                                    var_commune,
                                    var_year,
                                    var_values,
                                    var_cat = NULL,
                                    unit = unit,
                                    DT_dom = "Bfrtip" # we set default with Buttons
){

  # First format <var_values> (could be more than one variable) nicely according to unit
  if(unit == "%"){
    data <- data |>
      dplyr::mutate(
        dplyr::across(
          all_of(var_values),
          ~ scales::percent(.x, accuracy = 0.01)
        )
      )
  }else{
    data <- data |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(var_values),
          ~ format(.x, big.mark = "'", digits = 3, drop0trailing = TRUE, scientific = FALSE)
        )) |> # format if starts with 'part_' for flexibility (canopy)
          dplyr::mutate(
            dplyr::across(
              dplyr::starts_with("part"),
              ~ scales::percent(.x, accuracy = 0.01))
            )
  }

  # Then proceed with the rest of the table code
  data |>
    # Basic clean up for table output
    dplyr::arrange(.data[[var_commune]], desc(.data[[var_year]])) |>
    dplyr::mutate({{var_year}} := as.factor(.data[[var_year]])) |>
    # any_of() allows to pass var_car even if it does not exist
    dplyr::relocate(.data[[var_commune]], .data[[var_year]], dplyr::any_of(var_cat)) |>
    add_colname_unit(colnames = var_values, unit = unit) |> # fct_helpers.R
    rename_misc_colnames() |> # fct_helpers.R
    rename_fr_colnames() |>  # fct_helpers.R
    #turn to DT
    DT::datatable(escape = F, # rendering the icons instead of text
                  extensions = 'Buttons',
                  options = list(paging = TRUE,    # paginate the output
                                 pageLength = 15,  # number of rows to output for each page
                                 scrollY = TRUE,   # enable scrolling on Y axis
                                 autoWidth = TRUE, # use smart column width handling
                                 server = FALSE,   # use server-side processing
                                 dom = DT_dom,
                                 buttons = list(
                                   list(extend = 'copy', text = "Copier"),
                                   list(extend = 'excel', filename = paste0("elec_prod_vd_", Sys.Date()))
                                 ),
                                 columnDefs = list(list(
                                   targets = "_all",
                                   className = 'dt-center'
                                 )),


                                 # https://rstudio.github.io/DT/004-i18n.html   for languages
                                 language = DT_fr_language # from utils_helpers.R !
                  ),
                  selection = 'single', # enable selection of a single row
                  rownames = FALSE      # don't show row numbers/names
    ) # End DT

}



# Palette fns ----

#' return_palette_elec_prod
#' Returns the color palette for categories in the electricity production dataset
#' @return a vector with categorical data and hex color strings
#' @export

return_palette_elec_prod <- function(){

  return(colors_categories) # in utils_helpers.R

}

#' return_palette_elec_cons
#' Returns the color palette for sectors in the electricity consumption dataset
#' @return a vector with categorical data and hex color strings
#' @export

return_palette_elec_cons <- function(){

  return(colors_sectors) # in utils_helpers.R

}



#' return_palette_regener
#' returns the color palette for energy sources in the regener dataset
#' @return a vector with categorical data and hex color strings
#' @export

return_palette_regener <- function(){

  # We take the initial AE colors and we add a 'Autres sources' which is
  # the 'other_level' from fct_lump_n() used inside the 'create_alluvial_chart()' fun

  colors_ae_others <- c(colors_ae,
                        "Autres sources" = "#6CFF6C")

  return(colors_ae_others)

}

# Icons fns ----

# This one is required because create_subsidies_table_dt is flexible enough to host
# two palettes (building+measure) but we need a function to return the palette
# if we want to be using the rmarkdown feature ! Otherwise not needed.

#' return_icons_subsidies
#' returns one of two icons palette for subsidies data (either building or measure icons)
#' @param which either `building` to return the building palette,
#' or `measure` to return the measure palette
#' @return a tibble with icon as html and a categorical variable to match icons
#' @export

return_icons_subsidies <- function(which){

  # According to which palette is asked, we return one of the objects from utils_helpers.R
  icons_subsidies <- switch(which,
                            "building" = subsidies_building_icons,
                            "measure" = subsidies_measure_icons)

  return(icons_subsidies)

}


#' create_geoportail_tag
#' Creates a icon+link combination (tag) which redirects towards a specified geoportail link where geodata
#' can be viewed for Canton de Vaud
#' @param link
#'
#' @return a span tag with icon and an html <a> tag with the redirect link
#' @export
#'
#' @examples create_geoportail_tag(link = "https://google.com")
create_geoportail_tag <- function(link){

  tags$span(
    shiny::icon("map"), strong("Géodonnées détaillées disponibles sur", tags$a(href = link, 'geo.vd.ch', target = '_blank'))
  )

}



# Unit conversion & suffix fns ----

#' convert_units()
#' Converts the values of a vector (colname) or a direct value (data) given its starting and end unit.
#' conversion tables are defined in utils_helpers.R
#' @param data the dataframe containing the columns where to convert units
#' @param colnames the colnames where to convert units. If multiple are provided they will all be converted the same
#' @param unit_from the unit to convert from
#' @param unit_to the unit to convert to. Choice between "kWh", "MWh", "GWh", "TJ"
#'
#' @return the same dataframe with updated units on target colnames
#' @export

convert_units <- function(data,
                          colnames = NULL, # to be defined how it should be passed
                          unit_from, # kind of value from dataset ; energy must be kWh and co2 must be tCO2
                          unit_to){ # check if it's worth saving the widget selected unit in a specific variable before (in mod_inputs.R)

  ## |---------------------------------------------------------------|
  ##          Identify conversion factor
  ## |---------------------------------------------------------------|
  # Retrieves the right conversion factor unit_table in utils_helpers.R according to unit_to
  conversion_factor <- if(unit_from %in% energy_units_table$unit){

    # datasets must be suppied in kWh if energy-related !
    energy_units_table |>
      dplyr::filter(unit == unit_to) |>
      dplyr::pull(factor)

  }else if(unit_from %in% co2_units_table$unit){

    # datasets must be suppied in tCO2 if co2-related !
    co2_units_table |>
      dplyr::filter(unit == unit_to) |>
      dplyr::pull(factor)

  }else {
    message("Unité `unit_from` non reconnue !")
  }

  ## |-------------------------------------------------------------------------------------|
  ##          Apply conversion factor either in <colnames> or in <data> directly if numeric
  ## |-------------------------------------------------------------------------------------|

  if(is.data.frame(data)){

    # Applies the conversion factor to the target colnames of the dataframe (division)
    data |>
      dplyr::mutate(dplyr::across(dplyr::any_of(colnames), ~.x / conversion_factor))

  }else if(is.numeric(data)){

    # if numeric value (not df) we directly apply the conversion factor
    data/conversion_factor

  }else{

    stop("`data` is neither dataframe nor numeric ! No conversion can be made.")

  }
}


#' add_colname_unit
#' Add units to target colnames usually right before display uses (datatable, download, etc.)
#'
#' @param data the input data containing the colnames on which to append unit.
#' @param colnames the colnames, as string or with tidyselect syntax, on which to append units
#' @param unit the unit to append to `colnames` wrapped in brackets
#'
#' @return dataframe with renamed columns (units added)
#' @export

add_colname_unit <- function(data,
                             colnames,
                             unit){

  power_keyword <- "puissance"

  data <- data |>
    dplyr::rename_with(.cols = dplyr::any_of(colnames[!stringr::str_detect(colnames, pattern = power_keyword)]),
                       .fn = \(col) paste0(col, " [", unit, "]"))

  # Energy specificity : if a power_col_keyword colname is detected AND unit is energy
  # Then transform energy unit to power unit according to this rule
  if(any(stringr::str_detect(colnames, power_keyword))){

    data <- data |>
      dplyr::rename_with(.cols = dplyr::contains(power_keyword, # utils_helpers.R
                                          ignore.case = TRUE),
                         ~paste0(.x, " [", # The colnames + the [unit] extension according to ifelse() below
                                 ifelse(
                                   test = stringr::str_detect(string = unit, pattern = "Wh"), # search for *[Wh] in unit
                                   yes = stringr::str_remove(string = unit, pattern = "h"), # (k)Wh -> (k)W in cols
                                   no = paste0(unit, "/h") # TJ -> TJ/h, and all other non-Wh units
                                 ), "]") # closing bracket for unit
      )

  }else{
    data
  }

  return(data)

}

# Colname renaming fns ----

#' rename_fr_colnames()
#' A generic function that aims at adding correct french accents inside the create_x_table_dt functions
#' @return a dataframe with modified colnames, title case, accents where needed and space instead of underscore.
#' @export

rename_fr_colnames <- function(data){

  data |>
    # Standard renaming
    rename_with(.cols = dplyr::everything(),
                \(x){
                  # Temporarily remove the units in square brackets
                  x_no_units <- stringr::str_replace_all(x, "\\[.*?\\]", "<unit>")

                  # Apply sentence case to the rest of the name
                  x_no_units <- stringr::str_to_sentence(x_no_units)

                  # Reinsert the units back into their original places
                  stringr::str_replace_all(x_no_units, "<unit>", stringr::str_extract(x, "\\[.*?\\]"))
                }) |>
    rename_with(.cols = dplyr::everything(), .fn = stringr::str_replace_all,
                pattern = "_", replacement = " ") |>
    rename_with(.cols = everything(),
                .fn = stringr::str_replace_all, replace_fr_accents) # utils_helpers.R


}


#' rename_misc_columns()
#' Specific function to rename specific columns that can't be fixed with `rename_fr_colnames()`
#' It relies on a named vector of specific columns to rename adequately.
#' @return a renamed dataframe
#' @export

rename_misc_colnames <- function(data){

  data |>
    dplyr::rename(dplyr::any_of(cols_renaming_vector)) # utils_helpers.R

}

