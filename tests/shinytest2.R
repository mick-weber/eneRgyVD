library(eneRgyVD)

# https://rstudio.github.io/shinytest2/articles/using-monkey-testing.html

headless_app <- AppDriver$new(
  app_dir = ".",view = T,
  name = "monkey-test" # ,
  # shiny_args = list(port = 3515)
)

# chromote::set_chrome_args(
#   c(
#     chromote::default_chrome_args(),
#     # Custom flags: see https://peter.sh/experiments/chromium-command-line-switches/
#   )
# )

# Injecting gremlins ----

headless_app$run_js("
  let s = document.createElement('script');
  s.src = 'https://unpkg.com/gremlins.js';
  document.body.appendChild(s);
")

headless_app$get_html("script", outer_html = TRUE)
headless_app$get_js("typeof window.gremlins")


headless_app$run_js("
  let s = document.createElement('script');
  s.src = './gremlins/gremlins.min.js';
  document.body.appendChild(s);
")
headless_app$get_html("script", outer_html = TRUE)
headless_app$get_js("typeof window.gremlins")

# Run this in Chrome's console

# const horde = gremlins.createHorde();
# horde.unleash();

# horde
# .unleash()
# .then(() => {
#   console.log('Gremlins test success')
# });


