# Info fn ----

#' welcome_modal()
#' shiny modal message to inform the user about the app, and offer the possibility to have a guided tour (introjs)
#' @return A a modal object when opening the app

welcome_modal <- function(){

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
#' @importFrom phosphoricons ph
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
                           icon = phosphoricons::ph(title = NULL, "question"),
                           shiny::tags$div(class = "customPanel",
                                           shiny::markdown(paragraph))
                         )

                       })

  # Return list of accordion_panels
  return(items)

}

# Streamlined sliderInputs ----


#' make_slider_input_years
#' creates a shiny sliderInput() widget based on a vector of two years
#' @param id the widget inputId, passed with a ns() to avoid namespace conflicts if inside a module
#' @param years a vector of two numeric years to be used as min, max, and default values
#' @param ... <data-masking>
#' @return a shiny sliderInput widget
#' @export
#'
#' @examples make_slider_input_years(id = "slider1", years = c(2010, 2025))

make_slider_input_years <- function(id,
                                    years,
                                    ...
                                    ){

  shiny::sliderInput(inputId = id,
                     label = "Sélection des années",
                     min = years[1],
                     max = years[2],
                     value = years,
                     step = 1L,
                     sep = "",
                     ticks = FALSE,
                     dragRange = T
  )
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


#' create_plot_ggiraph()
#'
#'@description Creates a girafe object from a facetted ggplot bar plot for use in renderGirafe
#'
#' @param data the data to provide
#' @param n_communes number of selected communes, used to control the width of facets
#' @param var_year the year variable
#' @param var_commune the commune variable
#' @param unit the unit to append in <var_values>
#' @param var_cat the optional categorical variable
#' @param var_values the variables containing the values
#' @param geom the type of geom for the plot : either 'col' or 'line'
#' @param color_palette a named vector of values-colors if <var_cat> is supplied, that should match <var_cat> items
#' @param dodge if geom = 'col' and <var_cat> is supplied : controls whether the cols are in a stacked or dodge position
#' @param free_y if <n_communes> is higher than 1, controls whether the y axis is independent for each facet or not
#' @param legend_title a string containing the legend title if <var_cat> is supplied
#' @param height_svg,width_svg dimensions of ggiraph output
#'
#'
#' @import ggplot2
#' @importFrom ggiraph girafe
#' @return an interactive girafe object
#' @export

create_plot_ggiraph <- function(data,
                                n_communes,
                                var_year,
                                var_commune,
                                unit,
                                var_cat,
                                var_values,
                                geom,
                                color_palette,
                                dodge = FALSE,
                                free_y = FALSE,
                                legend_title = NULL,
                                height_svg,
                                width_svg) {

  # If several values/units are passed (for table options etc.) we take the first one (convention) for the plot
  first_var_value <- var_values[1]
  first_unit <- unit[1]

  # Compute totals for conditional geom_text (if stacked)
  # Note : we take the first var_values if several values are passed
  data_totals <- data |>
    dplyr::group_by(.data[[var_year]], .data[[var_commune]]) |>
    dplyr::summarise(total = sum(.data[[first_var_value]], na.rm = TRUE))

  # Compute ggplot2
  ggplot <- data |>
    ggplot2::ggplot(ggplot2::aes(
      x = as.factor(.data[[var_year]]),
      y = .data[[first_var_value]],
      fill = if (geom == "col" & !is.null(var_cat)) .data[[var_cat]] else NULL,  # Use fill only for bars
      color = if (geom == "line" & !is.null(var_cat)) .data[[var_cat]] else NULL, # Use color for lines
      data_id = if (!is.null(var_cat)) {
        paste0(.data[[var_year]], .data[[var_cat]])
      } else {
        .data[[var_year]]
      },
      tooltip = paste0(
        if (!is.null(var_cat)) paste0(.data[[var_cat]], "\n"),
        if (first_unit == "%") {
          paste(
            scales::percent(.data[[first_var_value]], accuracy = 0.1),
            " en ",
            .data[[var_year]]
          )
        } else {
          paste(
            format(round(.data[[first_var_value]], digits = 0), big.mark = "'"),
            first_unit,
            "en ",
            .data[[var_year]]
          )
        }
      )
    ))

  # Conditional geom_text labels for stacked bars
  if (geom == "col" & !is.null(var_cat) & !isTRUE(dodge)) {
    ggplot <- ggplot +
      ggiraph::geom_text_interactive(
        data = data_totals,
        ggplot2::aes(
          x = as.factor(.data[[var_year]]),
          y = total,
          label = if (first_unit == "%") {
            scales::percent(total, accuracy = 0.01)
          } else {
            format(total, big.mark = "'", digits = 1, scientific = FALSE)
          }
        ),
        vjust = -0.5,
        size = 10,
        size.unit = "pt",
        fontface = "plain",
        color = "grey20", # color of the text label !
        inherit.aes = FALSE
      )
  }

  # Add geometries conditionally (col acc. to var_cat & line (var_cat implicitly passed otherwise col))
  if (geom == "col" & !is.null(var_cat)) {
    ggplot <- ggplot +
      ggiraph::geom_col_interactive(
        position = dplyr::if_else(condition = dodge, true = "dodge", false = "stack")
      )+
      ggplot2::scale_fill_manual(
        name = legend_title,
        values = color_palette
      )
  } else if(geom == "col" & is.null(var_cat)){
    ggplot <- ggplot +
      ggiraph::geom_col_interactive(
        position = dplyr::if_else(condition = dodge, true = "dodge", false = "stack"),
        fill = color_palette[1] # even if more are supplied take only the first as var_cat is null
      )
  } else if (geom == "line" & !is.null(var_cat)) {
    ggplot <- ggplot +
      ggiraph::geom_line_interactive(
        size = 1,
        ggplot2::aes(group = if (!is.null(var_cat)) .data[[var_cat]] else 1)) +
      ggiraph::geom_point_interactive(size = 2.5) +
      ggplot2::scale_color_manual(
        name = legend_title,
        values = color_palette
      )
  } else if (geom == "line" & is.null(var_cat)){
    ggplot <- ggplot +
      ggiraph::geom_line_interactive(
        size = 1,
        ggplot2::aes(group = if (!is.null(var_cat)) .data[[var_cat]] else 1),
        color = color_palette[1]) +
      ggiraph::geom_point_interactive(size = 2.5, color = color_palette[1])
  } else {
    stop("Invalid value for 'geom'. Use 'col' for bar chart or 'line' for line chart.")
  }

  # Add scales, facets, and theme
  ggplot <- ggplot +
    ggplot2::scale_y_continuous(labels = ifelse(
      first_unit == "%",
      scales::percent,
      scales::label_number(big.mark = "'")
    ),
    limits = c(0, NA), # Force the Y-axis to start at 0
    expand = ggplot2::expansion(mult = c(0, 0.15)) # Add some area around, vertically
    ) +
    ggplot2::labs(x = "", y = first_unit) +
    ggiraph::facet_wrap_interactive(
      facets = ggplot2::vars(.data[[var_commune]]),
      ncol = 2,
      scales = ifelse(free_y, "free_y", "fixed")
    ) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      legend.position = "top",
      legend.direction = "horizontal",
      legend.justification = c(0, 0),
      legend.box.just = "left",
      strip.background = ggplot2::element_rect(color = "black", fill = main_color, linewidth = 0.5, linetype = "solid"),
      strip.text = ggplot2::element_text(color = "white", size = 12),
      legend.background = ggplot2::element_rect(fill = NA),
      panel.spacing.x = ggplot2::unit(0.05, "cm"),
      panel.spacing.y = ggplot2::unit(0.5, "cm"),
      panel.grid.major.x = ggplot2::element_blank()
    )

  ggiraph::girafe(
    ggobj = ggplot,
    height_svg = height_svg,
    width_svg = width_svg,
    options = list(
      ggiraph::opts_sizing(rescale = TRUE, width = 1),
      ggiraph::opts_hover(css = ""),
      ggiraph::opts_hover_inv(css = "opacity:0.6;"),
      ggiraph::opts_toolbar(
        position = "topleft",
        fixed = TRUE,
        pngname = "profil_climatique",
        tooltips = list(saveaspng = "Télécharger (.png)")
      ),
      ggiraph::opts_tooltip(opacity = 0.8),
      ggiraph::opts_selection(type = "none")
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
#' according to two forcats functions which arguments should be modified in the code
#'
#' @param data the dataset used to create the ggalluvial plot
#' @param var_commune the municipality variable
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

#' format_numbers_heuristic
#' a custom function to display large numbers (below 1000) without any decimal, and small numbers with one decimal
#' all values are displayed with thousand separator, no scientific notation and trailing zeroes are dropped
#' @param number the number to format
#'
#' @return a formatted number as a string
#' @export

format_numbers_heuristic <- function(number) {
  ifelse(
    abs(number) < 1000,               # Check if the number is less than 1000 (absolute value)
    format(round(number, 1), nsmall = 2, big.mark = "'", scientific = FALSE, drop0trailing = TRUE),  # Format with one decimal place
    format(round(number), nsmall = 0, big.mark = "'", scientific = FALSE, drop0trailing = TRUE)      # Format without any decimal places
  )
}


#' make_table_dt
#' a function that prepares data (add colnames, relocate, etc.) and makes a DT table
#' which also formats numbers nicely
#' @param data the dataframe or tibble to turn as a datatable
#' @param var_commune colname corresponding to the municipality (passed as a string)
#' @param var_year colname corresponding to the year (passed as a string)
#' @param var_values colname corresponding to the value(s) (passed as a string, or a vector of string)
#' @param var_cat colname corresponding to the categorical variable (passed as a string)
#' @param unit the unit to display inside brackets in the supplied <var_values> variables
#' @param icons_palette a dataframe of icons with two variables : the categorical variable (`var_cat`) and an `icon` variable with the full html code for the icon and color (see `utils_helpers.R`)
#' @param na_string a string specifying how should NAs be displayed, as it can change from one dataset to another (defaults to : 'Non disponible')
#' @param DT_dom  the DT domain values to specify which DT extensions should be applied
#'
#' @return a dataTable object
#' @export

make_table_dt <- function(data,
                          var_commune,
                          var_year,
                          var_values,
                          var_cat,
                          unit,
                          icons_palette,
                          na_string = "Non disponible",
                          DT_dom = "frtip"){

  ## |---------------------------------------------------------------|
  ##          Prepare data
  ## |---------------------------------------------------------------|
  if(!is.null(icons_palette) & !is.null(var_cat)){

    data <- data |>
      dplyr::left_join(icons_palette, by = var_cat) |>
      dplyr::relocate(icon, .before = var_cat) |>
      dplyr::rename(" " = "icon")# empty colname for icons
  }


  data_prep <- data |>
    dplyr::mutate(!!var_year := as.factor(.data[[var_year]])) |>
    # any_of() allows to pass var_car even if it does not exist
    dplyr::relocate(.data[[var_commune]],
                    .data[[var_year]],
                    dplyr::any_of(" "), # optional if icons are passed (hence any_of)
                    dplyr::any_of(var_cat),
                    dplyr::any_of(var_values)) |>
    dplyr::arrange(.data[[var_commune]], desc(.data[[var_year]])) |>
    add_colname_unit(colnames = var_values, unit = unit) |> # fct_helpers.R
    rename_columns_output()


  ## |---------------------------------------------------------------|
  ##          Format
  ## |---------------------------------------------------------------|
  data_prep <- data_prep |>
    dplyr::mutate(
      dplyr::across(everything(), \(col) {

        # Check if the column name starts with "part" (case-insensitive)
        if (grepl("^part", dplyr::cur_column(), ignore.case = TRUE)) {
          return(scales::percent(round(col, digits = 4), big.mark = "'")) # Format as percentage
        }else if(is.numeric(col)){
          return(format_numbers_heuristic(col))#format(col, big.mark = "'", digits = 4, drop0trailing = TRUE, scientific = FALSE))
        }else{
          return(col) # Leave column unchanged
        }
      })
    )

  ## |--------------------------------------------------------------------|
  ##          Datatable options
  ## |--------------------------------------------------------------------|

  column_defs <- list(
    # Apply custom rendering for all columns
    list(targets = list(0,1),           # Apply to first cols (commune/year)
         className = 'dt-center'),

    list(targets = "_all",
         render = DT::JS(
           "function(data, type, row, meta) {",
           glue::glue("return data === null ? '({na_string})' : data;"),
           "}"))
    )

  # Conditionally add the icons_palette logic to remove ordering widget + align right next to var_cat
  if (!is.null(icons_palette) & !is.null(var_cat)) {
    column_defs <- append(
      column_defs,
      list(list(targets = " ",
                width = "50px",
                className = 'dt-right',
                orderable = FALSE))
    )
  }

  ## |---------------------------------------------------------------|
  ##          Datatable
  ## |---------------------------------------------------------------|


  dt_table <- data_prep |>
    DT::datatable(
      class = "compact hover",         # Compact display + hover effect
      escape = FALSE,                  # Render HTML (e.g., icons) instead of text
      extensions = 'Buttons',          # Enable the Buttons extension (for export buttons)
      selection = 'single',            # Allow single row selection
      rownames = FALSE,                # Hide row numbers/names
      options = list(
        paging = TRUE,                 # Enable pagination
        pageLength = 10,               # Number of rows per page
        scrollY = TRUE,                # Enable vertical scrolling
        autoWidth = TRUE,              # Smart column width handling
        server = FALSE,                # Use client-side processing
        dom = DT_dom,                  # Define DOM positioning
        language = DT_fr_language,     # Language settings
        columnDefs = column_defs
      )
    )

  return(dt_table)
}

#' make_doc_table_dt
#' Creates minimalistic documentation table with download features
#' @param data the dataset containing variables and descriptions
#' @param doc_prefix the file prefix before the download date
#'
#' @return A DT object
#' @export

make_doc_table_dt <- function(data,
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
#' create_geoportail_tag
#' Creates a icon+link combination (tag) which redirects towards a specified geoportail link where geodata
#' can be viewed for Canton de Vaud
#' @param link the geoportail link that will be used inside the tag (https://geo.vd.ch/...)
#' @return a span tag with icon and an html <a> tag with the redirect link
#' @export
#'
#' @examples create_geoportail_tag(link = "https://geo.vd.ch")

create_geoportail_tag <- function(link, text){

  shiny::tags$span(
    phosphoricons::ph(title = NULL, "map-trifold"), shiny::strong("Géodonnées détaillées disponibles sur", shiny::tags$a(href = link, text, target = '_blank'))
  )
}



# Unit conversion & prefix ----

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

add_colname_unit <- function(data, colnames, unit){

  # If any null, do nothing in this function
  if(is.null(unit) | is.null(colnames)){
    return(data)
  }

  # Ensure that colnames and units are of equal length, otherwise replicate unit as needed
  if(length(colnames) > length(unit)) {
    unit <- rep(unit, length(colnames))
  }

  if(length(colnames) < length(unit)) {
    stop("Number of units passed to `add_colname_unit()` is more important than the target colnames !")
  }

  power_keyword = "puissance"


  for(i in seq_along(colnames)) {
    current_colname <- colnames[[i]]
    current_unit <- unit[[i]]

    if(stringr::str_detect(current_colname, power_keyword)){

      data <- data |>
        dplyr::rename_with(.cols = dplyr::contains(power_keyword, # utils_helpers.R
                                                   ignore.case = TRUE),
                           ~paste0(.x, " [", # The colnames + the [unit] extension according to ifelse() below
                                   ifelse(
                                     test = stringr::str_detect(string = current_unit, pattern = "Wh"), # search for *[Wh] in unit
                                     yes = stringr::str_remove(string = current_unit, pattern = "h"), # (k)Wh -> (k)W in cols
                                     no = paste0(unit, "/h") # TJ -> TJ/h, and all other non-Wh units
                                   ), "]") # closing bracket for unit
        )


    }else{

      data <- data |>
        dplyr::rename_with(.cols = dplyr::any_of(current_colname),
                           .fn = \(col) paste0(col, " [", current_unit, "]"))

    }
  }
  return(data)
}


# Colname renaming fns ----

#' rename_columns_output()
#' Uses the csv file in app/extdata/ to convert initial colnames to nicely formatted ones
#' the function matches only the start of the initial colname to find a match, allowing the
#' unit to be added before this function via 'add_colname_unit()'
#' @param data the dataset to rename with the default <colnames_replacement_display> object
#' @return a renamed dataframe
#' @export

rename_columns_output <- function(data) {

  # Renaming columns
  renamed_data <- data |>
    dplyr::rename_with(~ stringr::str_replace_all(.x, setNames(
      colnames_replacement_display$replacement,
      paste0("^", colnames_replacement_display$colname) # Only replace when it matches
    )), dplyr::everything())

  return(renamed_data)
}


