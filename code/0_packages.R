if(system.file(package="pacman")=="") utils::install.packages("pacman")
library(pacman)
p_load(rio,
       tidyverse,
       tidylog,
       ###
       broom,
       broom.mixed,
       corrr,
       flextable,
       glue,
       gt,
       gtsummary,
       janitor,
       lme4,
       renv,
       sjlabelled)

# renv::init()
renv::snapshot()




