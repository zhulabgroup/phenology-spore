Use this R package _phenologyspore_ to reproduce key steps in the manuscript 

Wu, R., Song, Y., Head, J.R., Katz, D.S.W., Peay, K.G., Shedden, K., Zhu, K. Fungal spore seasons advanced across the US over two decades of climate change.

## Installation
Please install our package following the steps below.

1. Download the zip folder on Zenodo. Remember the location where you've downloaded it to.
2. Unzip the downloaded zip file. You'll get a folder named phenology-spore.
3. Open RStudio or your preferred R environment.
4. Open the R project phenology-spore/phenology-spore.Rproj.
5. Install devtools using the following command in R
```R
install.packages("devtools")
```
6. Install _phenologyspore_ with
```R
devtools::install(quick = TRUE)
```

## Vignettes
To reproduce the analysis, you can build and read the package vignette. It is like a long form documentation or usage guide for the package.

There are three vignettes in the package:

- `0_processing.Rmd` Performs all data wrangling and processing.

- `1_main_text.Rmd` Generates the figures used in the main text of the manuscript.

- `2_SI.Rmd` Generates the figures and tables included in the Supplementary Information.

### Option 1
You may build the vignettes as you install the package, by setting `build_vignettes = TRUE` in
```R
devtools::install(build_vignettes = TRUE)
```

You can now view the vignette using
```R
browseVignettes(package = "phenologyspore")
```

Then, click on the HTML link of the vignettes to open them in your browser.

### Option 2

You may build the vignettes manually by knitting the R markdown files in the `vignettes/` folder. You can do this in RStudio by clicking the "Knit" button.

### Option 3
You can also run the code chunks in the vignettes in your R environment and explore the functions demonstrated.

## Additional files

You can find pre-loaded intermediate data files in the `data/` folder.

You can find main and supplementary display items in the `inst/figures/` and `inst/tables/` folders.

