# Run profvis
profvis::profvis({ shiny::runApp() }
        , prof_output = './dev')

# Load profile
p <- profvis::profvis(prof_input = './dev/file592c2f3a6f8c.Rprof')

# Turn to html
htmlwidgets::saveWidget(p, "./dev/profile.html")
