## app.R — entry point; Shiny auto-loads global.R, ui.R, server.R
## Run with: shiny::runApp("path/to/MeninMethylClass_V3")

source("global.R")
source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)
