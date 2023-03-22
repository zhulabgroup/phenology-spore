library(pacman)
p_load(tidyverse)
p_load(readxl)
p_load(fuzzyjoin)
p_load(taxize)
p_load(RcppRoll)
p_load(rlist)
p_load(scales)
if (!require(greenbrown)) {
  p_load(Kendall,bfast,phenopix)
  install.packages("greenbrown", repos="http://R-Forge.R-project.org")
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

.path <- list( # hidden variable won't be removed
  nab_raw = "/nfs/turbo/seas-zhukai/phenology/nab/raw/",
  nab_clean = "/nfs/turbo/seas-zhukai/phenology/nab/clean/",
  out_fig = "output/figures/")