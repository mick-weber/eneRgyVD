# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

future::plan(future::multisession) # Allow async programming (mod_download_rmd.R)

pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
options("golem.app.prod" = TRUE)

eneRgyVD::run_app() # add parameters here (if any)

