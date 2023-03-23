source("0_packages.R")

hrs_analytic <- import("hrs_analytic.rds")

#get case/observation numbers
cases <- n_distinct(hrs_analytic$hhidpn)
obs <- nrow(hrs_analytic)

#generate descriptives table
table_1 <- hrs_analytic %>% 
  select(study, 
         sex, 
         age,
         cogfunction,
         ad_pgs, 
         incar_ever, 
         stroke_ever,
         apoe_info99_4ct,
         social_origins,
         ses) %>% 
  var_labels(study           = "HRS cohort", 
             sex             = "Self-reported sex", 
             age             = "Age",
             cogfunction     = "Cognitive function",
             ad_pgs          = "AD polygenic score", 
             incar_ever      = "Lifetime incarceration", 
             stroke_ever     = "History of stroke",
             apoe_info99_4ct = "APOE-4 count",
             social_origins  = "Social origins index",
             ses             = "Socioecnomic status") %>% 
  mutate(incar_ever = fct_recode(incar_ever, 
                                 "Yes" = "Incarcerated", 
                                 "No" = "Not Incarcerated")) %>% 
  mutate(study = fct_drop(study)) %>% 
  mutate(social_origins = as_numeric(social_origins)) %>% 
  tbl_summary(by=incar_ever) %>% 
  add_p() %>% 
  add_overall() %>% 
  modify_caption(glue('**Table 1**. Health and Retirement Study sample descriptives by lifetime incarceration history\n
                      Observations={format(obs, big.mark=",")}; Cases={format(cases, big.mark=",")}')) %>% 
  modify_header(label ~ "**Variable**") %>% 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Ever incarcerated?**")

#export .html table
# gtsave(as_gt(table_1), "../output/results/tab1_descriptives.html")

#export .docx table
table_1 %>% 
  as_flex_table() %>%
  flextable::save_as_docx(path="../output/results/tab1_descriptives.docx")

