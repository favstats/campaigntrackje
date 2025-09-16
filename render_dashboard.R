# Render the coding dashboard
# This script renders the R Markdown dashboard

# Load required packages
library(rmarkdown)

source("irr.R")

# Render the dashboard
rmarkdown::render(
  input = "coding_dashboard.Rmd",
  output_file = "coding_dashboard.html",
  output_dir = ".",
  quiet = FALSE
)

cat("Dashboard rendered successfully!\n")
cat("Open 'coding_dashboard.html' in your web browser to view the interactive dashboard.\n")
