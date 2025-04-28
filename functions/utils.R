if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

# List of required packages
required_packages <- c("patchwork", "tidyverse", "ggpattern", "nlme", "mblm", "ggeffects", "cowplot", "grid")

if (exists(".fulldata") && isTRUE(.fulldata)) {
  required_packages <- c(required_packages, "daymetr")
}

# Identify missing packages
missing_packages <- setdiff(required_packages, rownames(installed.packages()))

# Install if there are missing packages
if (length(missing_packages) > 0) {
  for (package in missing_packages) {
    install.packages(package, repos = "https://cloud.r-project.org/")
  }
} else {
  message("All required packages are already installed. Skipping installation.")
}

pacman::p_load("tidyverse")
pacman::p_load("patchwork")

.path <- list( # hidden variable won't be removed
  dat_process = "../alldata/processed/2023-04-25/",
  ana_fun = "../functions/",
  input = "../data/input/",
  intermediate = "../data/intermediate/",
  output = "../data/output/"
)

options(scipen = 999)
