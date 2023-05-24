source("0_packages.R")

hrs <- import("hrs_full_analytic.rds")

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
         race_ethn,
         edu,
         cog_2cat,
         # ad_pgs_resid, 
         incar_ever, 
         stroke_ever,
         smoke_ever,
         apoe_info99_4ct,
         social_origins,
         # ses
         ) %>% 
  var_labels(study           = "HRS cohort", 
             sex             = "Self-reported sex", 
             race_ethn       = "Race/Ethnicity",
             edu             = "HS completion",
             age             = "Age",
             cog_2cat        = "Cognitive function",
             # ad_pgs          = "AD polygenic score", 
             incar_ever      = "Lifetime incarceration", 
             stroke_ever     = "History of stroke",
             smoke_ever      = "Smoking status",
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
gtsave(as_gt(table_1), "../output/results/tab1_descriptives.html")

# #export .docx table
# table_1 %>% 
#   as_flex_table() %>%
#   flextable::save_as_docx(path="../output/results/tab1_descriptives.docx")

# #=Table 2 - Cross-tab of stratification variable===============================
# 
# strat_race <- table(hrs$cog_2cat,
#       hrs$incar_ever,
#       hrs$race_ethn)
# plot(strat_race)
# 
# strat_sex <- table(hrs$cog_2cat,
#                     hrs$incar_ever,
#                     hrs$sex)
# plot(strat_sex)
# 
# strat_edu <- table(hrs$cog_2cat,
#                     hrs$incar_ever,
#                     hrs$edu) 
# plot(strat_edu)
# 
#=Table 3 - Main results=======================================================

main_results <- import_list("../output/results/tab3_main_results.rdata") 
list2env(main_results, .GlobalEnv)
rm(main_results)

#function to calculate delta r2
delta_r2 <- function(model, base=m0){
  r2_0 <- r2(m0) %>% unlist()
  r2_plus1 <- r2(model) %>% unlist()
  delta_r2 <- r2_plus1[2] - r2_0[2]
  print(delta_r2)
}


tab31 <- tbl_regression(m11, exponentiate=TRUE, 
                        include=c("factor(apoe_info99_4ct)"),
                        label=c("factor(apoe_info99_4ct)"="APOE-4 allele count"))
tab32 <- tbl_regression(m12, exponentiate=TRUE, 
                        include=c("factor(incar_ever)"),
                        label=c("factor(incar_ever)"="Lifetime incarceration"))
tab33 <- tbl_regression(m13, exponentiate=TRUE, 
                        include=c("factor(incar_ever)","factor(apoe_info99_4ct)"),
                        label=list("factor(apoe_info99_4ct)"="APOE-4 allele count",
                                   "factor(incar_ever)"="Lifetime incarceration"))
tab34 <- tbl_regression(m14, exponentiate=TRUE, 
                        include=c("factor(incar_ever)","factor(apoe_info99_4ct)",
                                                         "factor(incar_ever):factor(apoe_info99_4ct)"),
                        label=list("factor(apoe_info99_4ct)"="APOE-4 allele count",
                                   "factor(incar_ever)"="Lifetime incarceration",
                                   "factor(incar_ever):factor(apoe_info99_4ct)"="Lifetime Incarceration*APOE-4 allele count"))

tab3_mods <- list(tab31,
                  tab32,
                  tab33,
                  tab34)
#write function to update gtsumamry tables
tab_updates <- function(x){
  x %>% 
    add_significance_stars(hide_ci = FALSE, hide_p = TRUE, hide_se = TRUE) %>% 
    remove_row_type(type = "reference") %>% 
    modify_table_body(~ .x %>%
      dplyr::mutate(ci = ifelse(!is.na(ci), paste0("[", ci, "]"), "")))
}

tab1_mods_update <- lapply(tab3_mods, tab_updates)

tab3_all <- tbl_merge(tab1_mods_update, tab_spanner = paste("Model", 1:4, sep = " ")) %>% 
  modify_header(label = "**Variable**") %>% 
  modify_caption(glue('**Table 3**. Mixed effect Poisson regression of cognitive status on APOE-4 genotype and lifetime incarceration (Observations={format(obs, big.mark=",")}; Cases={format(cases, big.mark=",")})')) %>% 
  modify_footnote(label ~ "All models also adjusted for age, sex, race/ethnicity, high school completion, smoking history, stroke history, and social origins index.") 
tab3_all

tab3_all %>% 
  as_gt() %>% 
  gtsave("../output/results/tab3_main_res.html")

# tab3_all %>% 
#   as_flex_table() %>%
#   flextable::save_as_docx(path="../output/results/tab3_main_res.docx")


