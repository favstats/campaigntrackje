#!/usr/bin/env Rscript

# Render Quarto Dashboard
# This script renders the Quarto dashboard with screenshots

library(quarto)

cat("Starting Quarto dashboard rendering...\n")

# Render the Quarto document
quarto::quarto_render(
  input = "coding_dashboard.qmd",
  output_format = "html",
  execute_params = NULL
)

cat("Quarto dashboard rendered successfully!\n")
cat("Open 'coding_dashboard.html' in your web browser to view the interactive dashboard.\n")
