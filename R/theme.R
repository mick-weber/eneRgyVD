# Theme called in app_ui.R ----

## Color used for multiple ui items ----

main_color <- "#3A862D"
main_color_active <- "#343A40"

## Custom {fresh} theme passed to bs4Dash in app_ui.R
# Example from https://dreamrs.github.io/fresh/

profil_theme <- bslib::bs_theme(version = 5,
                                preset = "bootstrap",
                                font_scale = 1.2,
                                "navbar-bg" = main_color,
                                "modal-footer-margin-between" = "0rem",
                                primary = main_color,
                                secondary = main_color_active
) |>
  # add some variables
  bslib::bs_add_variables(
    "dropdown-link-active-bg" = "white",         # nav menus
    "dropdown-link-active-color" = "$secondary", # nav menus
    "accordion-button-active-bg" = "$secondary", # bg color when activated
    "accordion-button-active-color" = "white",   # text color when activated
    "accordion-button-focus-box-shadow" = "none", # remove shadow when accordion buttons are active ; e.g. "0 0 0 $btn-focus-width rgba($secondary, 0.25)"

    .where = "declarations") |>
  bslib::bs_add_rules(sass::sass_file("./inst/app/www/custom_bs5.scss")) # add complementary sass file

