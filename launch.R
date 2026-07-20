# Resolve the app directory from this script's own location, so the launcher is
# portable (works on any machine / any folder), not tied to an absolute path.
.args    <- commandArgs(trailingOnly = FALSE)
.fileArg <- sub("^--file=", "", .args[grep("^--file=", .args)])
app_dir  <- if (length(.fileArg)) dirname(normalizePath(.fileArg)) else getwd()
shiny::runApp(app_dir, port = 7771, launch.browser = TRUE)