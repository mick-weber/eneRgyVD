# Theme called in app_ui.R ----

## Color used for multiple ui items ----

main_color <- "#3A862D"
main_color_active <- "#343A40"

## Font selection : if bslib::font_google() fails due to proxy issue, then fallback on Arial sans-serif



## Custom theme
profil_theme <- bslib::bs_theme(version = 5,
                                preset = "bootstrap",
                                font_scale = 1.2,
                                base_font = "Open Sans, Arial, sans-serif", # Use local 'Open Sans' (in www/fonts), fallback to Arial
                                "navbar-bg" = "white",
                                "modal-footer-margin-between" = "0rem",
                                primary = main_color,
                                secondary = main_color_active
) |>
  # add some variables
  bslib::bs_add_variables(
    "dropdown-link-active-bg" = "white",         # nav menus
    "dropdown-link-active-color" = "$secondary", # nav menus
    "dropdown-link-color" = "white",

    "accordion-button-active-bg" = "$secondary", # bg color when activated
    "accordion-button-active-color" = "white",   # text color when activated
    "accordion-button-focus-box-shadow" = "none", # remove shadow when accordion buttons are active ; e.g. "0 0 0 $btn-focus-width rgba($secondary, 0.25)"

    .where = "declarations") |> # test 'rules' ?
  # add link to custom.scss + tell where to find bootstrap librairies called in it ('@import' calls)
    bslib::bs_add_rules(
      #sass::sass_file("inst/app/www/custom.scss")
      sass::sass_file(
        sass::sass(input = sass::sass_file("inst/app/www/custom.scss"),
                   output = "inst/app/www/custom.css",
                   cache = NULL,
                   options = sass::sass_options(
                     # target the bs5 path where all components are stored
                     include_path = system.file("lib/bs5/scss/", package = "bslib")))
      )
      )

