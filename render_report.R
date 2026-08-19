# Renders 02_main_analysis/analysis_report_v2.Rmd end to end, regenerating all Table_*.csv,
# model_coefficients_*.csv, and figures. Uses a project-local package library (.Rlib) alongside
# the base R install, and pandoc from the module system.

.libPaths(c("your_path_here/.Rlib",
            .libPaths()))

Sys.setenv(RSTUDIO_PANDOC = Sys.getenv("PANDOC_HOME", unset = dirname(Sys.which("pandoc"))))
cat("pandoc:", Sys.which("pandoc"), "\n")

rmarkdown::render(
  "your_path_here/02_main_analysis/analysis_report_v2.Rmd",
  output_dir = "your_path_here/02_main_analysis"
)
