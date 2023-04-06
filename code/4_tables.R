source("0_packages.R")

hrs <- import("hrs_analytic.rds")

#set table theme
set_gtsummary_theme(theme_gtsummary_compact())

#=Table 1 - Descriptives=======================================================

#get case/observation numbers
cases <- n_distinct(hrs$hhidpn)
obs <- nrow(hrs)

#generate descriptives table
table_1 <- hrs %>% 
  select(study, 
         sex, 
         age,
         cog_2cat,
         ad_pgs, 
         incar_ever, 
         stroke_ever,
         apoe_info99_4ct,
         social_origins,
         # ses
         ) %>% 
  var_labels(study           = "HRS cohort", 
             sex             = "Self-reported sex", 
             age             = "Age",
             cog_2cat        = "Cognitive function",
             ad_pgs          = "AD polygenic score", 
             incar_ever      = "Lifetime incarceration", 
             stroke_ever     = "History of stroke",
             apoe_info99_4ct = "APOE-4 count",
             social_origins  = "Social origins index",
             # ses             = "Socioecnomic status"
             ) %>% 
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
table_1

#export .html table
# gtsave(as_gt(table_1), "../output/results/tab1_descriptives.html")

#export .docx table
table_1 %>% 
  as_flex_table() %>%
  flextable::save_as_docx(path="../output/results/tab1_descriptives.docx")

#=Table 2 - Additional table===================================================
#=Table 3 - Main results=======================================================

main_results <- import_list("../output/results/tab3_main_results.rdata") 
list2env(main_results, .GlobalEnv)
rm(main_results)

tab31 <- tbl_regression(m1, exponentiate=TRUE, 
                        include=c("scale(ad_pgs_resid)"),
                        label=c("scale(ad_pgs_resid)"="AD PGS (z-score)"))
tab32 <- tbl_regression(m2, exponentiate=TRUE, 
                        include=c("factor(apoe_info99_4ct)"),
                        label=c("factor(apoe_info99_4ct)"="APOE-4 allele count"))
tab33 <- tbl_regression(m3, exponentiate=TRUE, 
                        include=c("scale(ad_pgs_resid)", "factor(apoe_info99_4ct)"),
                        label=list("scale(ad_pgs_resid)"="AD PGS (z-score)",
                                   "factor(apoe_info99_4ct)"="APOE-4 allele count"))
tab34 <- tbl_regression(m4, exponentiate=TRUE, 
                        include=c("factor(incar_ever)"),
                        label=c("factor(incar_ever)"="Lifetime incarceration"))
tab35 <- tbl_regression(m5, exponentiate=TRUE, 
                        include=c("factor(incar_ever)","scale(ad_pgs_resid)", "factor(apoe_info99_4ct)"),
                        label=list("scale(ad_pgs_resid)"="AD PGS (z-score)",
                                   "factor(apoe_info99_4ct)"="APOE-4 allele count",
                                   "factor(incar_ever)"="Lifetime incarceration"))
tab36 <- tbl_regression(m6, exponentiate=TRUE, 
                        include=c("factor(incar_ever)","scale(ad_pgs_resid)","factor(apoe_info99_4ct)",
                                                         "factor(incar_ever):factor(apoe_info99_4ct)",
                                                         "factor(incar_ever):scale(ad_pgs_resid)"),
                        label=list("scale(ad_pgs_resid)"="AD PGS (z-score)",
                                   "factor(apoe_info99_4ct)"="APOE-4 allele count",
                                   "factor(incar_ever)"="Lifetime incarceration",
                                   "factor(incar_ever):factor(apoe_info99_4ct)"="Lifetime Incarceration*APOE-4 allele count",
                                   "factor(incar_ever):scale(ad_pgs_resid)"="Lifetime Incarceration*AD PGS (z-score)"))

tab3_mods <- list(tab31,
                  tab32,
                  tab33,
                  tab34,
                  tab35,
                  tab36)
#write function to update gtsumamry tables
tab_updates <- function(x){
  x %>% 
    add_significance_stars(hide_ci = FALSE, hide_p = TRUE, hide_se = TRUE) %>% 
    remove_row_type(type = "reference") %>% 
    modify_table_body(~ .x %>%
      dplyr::mutate(ci = ifelse(!is.na(ci), paste0("[", ci, "]"), "")))
}

tab1_mods_update <- lapply(tab3_mods, tab_updates)

tab3_all <- tbl_merge(tab1_mods_update, tab_spanner = paste("Model", 1:6, sep = " ")) %>% 
  modify_header(label = "**Variable**") %>% 
  modify_caption(glue('**Table 3**. Mixed effect Poisson regression of cognitive status on genetic factors and lifetime incarceration (Observations={format(obs, big.mark=",")}; Cases={format(cases, big.mark=",")})')) %>% 
  modify_footnote(label ~ "All models also adjusted for age, sex, smoking history, stroke history, and social origins index.") 
tab3_all

tab3_all %>% 
  as_gt() %>% 
  gtsave("../output/results/tab3_main_res.html")

tab3_all %>% 
  as_flex_table() %>%
  flextable::save_as_docx(path="../output/results/tab3_main_res.docx")


