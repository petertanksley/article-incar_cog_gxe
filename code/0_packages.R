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
       ggforce,
       ggridges,
       glue,
       gt,
       gtsummary,
       janitor,
       lme4,
       renv,
       sjlabelled,
       webshot2)

# renv::init()
renv::snapshot()




