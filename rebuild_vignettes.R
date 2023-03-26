# Adapted from https://www.kloppenborg.ca/2021/06/long-running-vignettes/

base_dir <- getwd()

setwd("vignettes/")
knitr::knit("cond_effect.Rmd.original", output = "cond_effect.Rmd")
# knitr::knit("moderation.Rmd.original", output = "moderation.Rmd")
# knitr::knit("std_selected.Rmd.original", output = "std_selected.Rmd")
setwd(base_dir)
