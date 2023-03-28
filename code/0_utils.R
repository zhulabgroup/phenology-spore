library(pacman)
p_load(tidyverse)
p_load(readxl)
p_load(fuzzyjoin)
p_load(taxize)
p_load(RcppRoll)
p_load(rlist)
p_load(scales)
if (!require(greenbrown)) {
  p_load(Kendall, bfast, phenopix)
  install.packages("greenbrown", repos = "http://R-Forge.R-project.org")
}
p_load(greenbrown)
p_load(nimble)
p_load(daymetr)
p_load(lubridate)
p_load(gridExtra)
p_load(ggrepel)
p_load(ggpubr)
p_load(zoo)
p_load(padr)
p_load(ptw)
p_load(forestmangr)
p_load(magrittr)
p_load(maps)
p_load(mapproj)
p_load(knitr)
p_load(patchwork)

p_unload("all")

pacman::p_load(tidyverse)
pacman::p_load(nimble)
pacman::p_load(patchwork)


# ln -s /nfs/turbo/seas-zhukai/phenology/phenology_fungal_spore/ data
# ln -s /nfs/turbo/seas-zhukai/phenology/nab/ data/nab

.path <- list( # hidden variable won't be removed
  nab_raw = "data/nab/raw/",
  nab_clean = "data/nab/clean/",
  dat_process = "data/processed/",
  out_simple_model = "data/pheno_model/simple/",
  out_bayes_model = "data/pheno_model/bayes/",
  out_fig = "output/figures/"
)
