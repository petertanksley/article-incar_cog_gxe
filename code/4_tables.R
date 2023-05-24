source("0_packages.R")

hrs <- import("hrs_full_analytic.rds")

#get case/observation numbers
cases <- n_distinct(hrs$hhidpn)
obs <- nrow(hrs)

#set table theme
set_gtsummary_theme(theme_gtsummary_compact())

#=Table 1 - Descriptives=======================================================

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

#=Table S1 - polygenic risk (EUR)==============================================

hrs_pgs_eur <- import("hrs_pgs_analytic.rds") %>% filter(race_ethn=="White")

#get case/observation numbers
cases <- n_distinct(hrs_pgs_eur$hhidpn)
obs <- nrow(hrs_pgs_eur)


poly_eur_results <- import_list("../output/results/tab_s1_eur.rdata") 
list2env(poly_eur_results, .GlobalEnv)

#function to calculate delta r2
delta_r2 <- function(model, base=m0){
  r2_0 <- r2(m0) %>% unlist()
  r2_plus1 <- r2(model) %>% unlist()
  delta_r2 <- r2_plus1[2] - r2_0[2]
  print(delta_r2)
}


tab_s1 <- tbl_regression(m21, exponentiate=TRUE, 
                         include=c("scale(ad_pgs_resid)"),
                         label=c("scale(ad_pgs_resid)"="PGS AD"))
tab_s2 <- tbl_regression(m22, exponentiate=TRUE, 
                        include=c("factor(apoe_info99_4ct)"),
                        label=c("factor(apoe_info99_4ct)"="APOE-4 allele count"))
tab_s3 <- tbl_regression(m23, exponentiate=TRUE, 
                         include=c("scale(ad_pgs_resid)","factor(apoe_info99_4ct)"),
                         label=c("scale(ad_pgs_resid)"="PGS AD",
                                 "factor(apoe_info99_4ct)"="APOE-4 allele count"))
tab_s4 <- tbl_regression(m24, exponentiate=TRUE, 
                        include=c("factor(incar_ever)"),
                        label=c("factor(incar_ever)"="Lifetime incarceration"))
tab_s5 <- tbl_regression(m25, exponentiate=TRUE, 
                        include=c("factor(incar_ever)","scale(ad_pgs_resid)","factor(apoe_info99_4ct)"),
                        label=list("scale(ad_pgs_resid)"="PGS AD",
                                   "factor(apoe_info99_4ct)"="APOE-4 allele count",
                                   "factor(incar_ever)"="Lifetime incarceration"))
tab_s6 <- tbl_regression(m26, exponentiate=TRUE, 
                        include=c("factor(incar_ever)","scale(ad_pgs_resid)","factor(apoe_info99_4ct)",
                                  "factor(incar_ever):scale(ad_pgs_resid)",
                                  "factor(incar_ever):factor(apoe_info99_4ct)"),
                        label=list("scale(ad_pgs_resid)"="PGS AD",
                                   "factor(apoe_info99_4ct)"="APOE-4 allele count",
                                   "factor(incar_ever)"="Lifetime incarceration",
                                   "factor(incar_ever):factor(apoe_info99_4ct)"="PGS AD",
                                   "factor(incar_ever):factor(apoe_info99_4ct)"="Lifetime Incarceration*APOE-4 allele count"
                                   ))

tab_s1_mods <- list(tab_s1,
                    tab_s2,
                    tab_s3,
                    tab_s4,
                    tab_s5,
                    tab_s6)

#write function to update gtsumamry tables
tab_updates <- function(x){
  x %>% 
    add_significance_stars(hide_ci = FALSE, hide_p = TRUE, hide_se = TRUE) %>% 
    remove_row_type(type = "reference") %>% 
    modify_table_body(~ .x %>%
                        dplyr::mutate(ci = ifelse(!is.na(ci), paste0("[", ci, "]"), "")))
}

tab_s1_mods_update <- lapply(tab_s1_mods, tab_updates)

tab_s1_all <- tbl_merge(tab_s1_mods_update, tab_spanner = paste("Model", 1:6, sep = " ")) %>% 
  modify_header(label = "**Variable**") %>% 
  modify_caption(glue('**Table S1**. Mixed effect Poisson regression of cognitive status on mono-/polygenic risk and lifetime incarceration among White HRS participants (Observations={format(obs, big.mark=",")}; Cases={format(cases, big.mark=",")})')) %>% 
  modify_footnote(label ~ "All models also adjusted for age, sex, race/ethnicity, high school completion, smoking history, stroke history, and social origins index.") 
tab_s1_all

tab_s1_all %>% 
  as_gt() %>% 
  gtsave("../output/results/tab_s1_eur.html")

# tab_s1_all %>% 
#   as_flex_table() %>%
#   flextable::save_as_docx(path="../output/results/tab_s1_eur.docx")



#=Table S1 - polygenic risk (AFR)==============================================

hrs_pgs_afr <- import("hrs_pgs_analytic.rds") %>% filter(race_ethn=="Black")

#get case/observation numbers
cases <- n_distinct(hrs_pgs_afr$hhidpn)
obs <- nrow(hrs_pgs_afr)


poly_afr_results <- import_list("../output/results/tab_s1_afr.rdata") 
list2env(poly_afr_results, .GlobalEnv)

#function to calculate delta r2
delta_r2 <- function(model, base=m0){
  r2_0 <- r2(m0) %>% unlist()
  r2_plus1 <- r2(model) %>% unlist()
  delta_r2 <- r2_plus1[2] - r2_0[2]
  print(delta_r2)
}


tab_s1 <- tbl_regression(m31, exponentiate=TRUE, 
                         include=c("scale(ad_pgs_resid)"),
                         label=c("scale(ad_pgs_resid)"="PGS AD"))
tab_s2 <- tbl_regression(m32, exponentiate=TRUE, 
                         include=c("factor(apoe_info99_4ct)"),
                         label=c("factor(apoe_info99_4ct)"="APOE-4 allele count"))
tab_s3 <- tbl_regression(m33, exponentiate=TRUE, 
                         include=c("scale(ad_pgs_resid)","factor(apoe_info99_4ct)"),
                         label=c("scale(ad_pgs_resid)"="PGS AD",
                                 "factor(apoe_info99_4ct)"="APOE-4 allele count"))
tab_s4 <- tbl_regression(m34, exponentiate=TRUE, 
                         include=c("factor(incar_ever)"),
                         label=c("factor(incar_ever)"="Lifetime incarceration"))
tab_s5 <- tbl_regression(m35, exponentiate=TRUE, 
                         include=c("factor(incar_ever)","scale(ad_pgs_resid)","factor(apoe_info99_4ct)"),
                         label=list("scale(ad_pgs_resid)"="PGS AD",
                                    "factor(apoe_info99_4ct)"="APOE-4 allele count",
                                    "factor(incar_ever)"="Lifetime incarceration"))
tab_s6 <- tbl_regression(m36, exponentiate=TRUE, 
                         include=c("factor(incar_ever)","scale(ad_pgs_resid)","factor(apoe_info99_4ct)",
                                   "factor(incar_ever):scale(ad_pgs_resid)",
                                   "factor(incar_ever):factor(apoe_info99_4ct)"),
                         label=list("scale(ad_pgs_resid)"="PGS AD",
                                    "factor(apoe_info99_4ct)"="APOE-4 allele count",
                                    "factor(incar_ever)"="Lifetime incarceration",
                                    "factor(incar_ever):factor(apoe_info99_4ct)"="PGS AD",
                                    "factor(incar_ever):factor(apoe_info99_4ct)"="Lifetime Incarceration*APOE-4 allele count"
                         ))

tab_s2_mods <- list(tab_s1,
                    tab_s2,
                    tab_s3,
                    tab_s4,
                    tab_s5,
                    tab_s6)

#write function to update gtsumamry tables
tab_updates <- function(x){
  x %>% 
    add_significance_stars(hide_ci = FALSE, hide_p = TRUE, hide_se = TRUE) %>% 
    remove_row_type(type = "reference") %>% 
    modify_table_body(~ .x %>%
                        dplyr::mutate(ci = ifelse(!is.na(ci), paste0("[", ci, "]"), "")))
}

tab_s2_mods_update <- lapply(tab_s2_mods, tab_updates)

tab_s2_all <- tbl_merge(tab_s1_mods_update, tab_spanner = paste("Model", 1:6, sep = " ")) %>% 
  modify_header(label = "**Variable**") %>% 
  modify_caption(glue('**Table S2**. Mixed effect Poisson regression of cognitive status on mono-/polygenic risk and lifetime incarceration among Black HRS participants (Observations={format(obs, big.mark=",")}; Cases={format(cases, big.mark=",")})')) %>% 
  modify_footnote(label ~ "All models also adjusted for age, sex, race/ethnicity, high school completion, smoking history, stroke history, and social origins index.") 
tab_s2_all

tab_s2_all %>% 
  as_gt() %>% 
  gtsave("../output/results/tab_s2_afr.html")

# tab_s2_all %>% 
#   as_flex_table() %>%
#   flextable::save_as_docx(path="../output/results/tab_s2_afr.docx")



