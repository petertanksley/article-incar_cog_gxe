source("0_packages.R")

hrs <- import("hrs_full_analytic.rds")

#get case/observation numbers
cases <- n_distinct(hrs$hhidpn)
obs <- nrow(hrs)

#set table theme
set_gtsummary_theme(theme_gtsummary_compact())

#=Table 1 - Descriptives=======================================================

#generate descriptives table
hrs_labs <- hrs %>% 
  select(hhidpn, year, firstiw, age, 
         dod_yr,
         race_ethn, sex, study, birthyr,
         cog_2cat, 
         apoe_info99_4ct,
         incar_ever, incar_time_3cat,
         alc_daily_avg, 
         bmi_combo,
         cesd_3cat,
         diab,
         edu,
         hear_sr_2cat,
         hibp,
         income_hh_10k,
         actx_lt_fct,
         smoke_first_iw,
         social_origins,
         stroke_ever,
         tbi_ever) %>% 
  mutate(incar_ever = fct_recode(incar_ever, 
                                 "Yes" = "Incarcerated", 
                                 "No" = "Not Incarcerated"),
         smoke_first_iw = ifelse(smoke_first_iw==1, "Yes", "No"),
         smoke_first_iw = fct_relevel(smoke_first_iw, "Yes"),
         edu = fct_recode(edu, 
                          "High school or more" = "hs or more",
                          "Less than high school" = "less than hs"),
         apoe_info99_4ct = fct_recode(apoe_info99_4ct,
                                      "Zero copies" = "zero copies",
                                      "One copy" = "one copy", 
                                      "Two copies" = "two copies"),
         stroke_ever=fct_recode(as_factor(stroke_ever), "No"="0", "Yes"="1"),
         stroke_ever = fct_relevel(stroke_ever, "Yes"),
         diab = ifelse(diab=="yes", "Diagnosed", "Never diagnosed"),
         diab = fct_relevel(as.factor(diab), "Never diagnosed"),
         hear_sr_2cat = ifelse(hear_sr_2cat=="normal", "Normal", "Impaired"),
         hear_sr_2cat = fct_relevel(as.factor(hear_sr_2cat), "Normal", "Impaired"),
         hibp = ifelse(hibp=="yes", "Yes", "No"),
         hibp = fct_relevel(as.factor(hibp), "Yes", "No"),
         tbi_ever = ifelse(tbi_ever=="yes", "Yes", "No"),
         tbi_ever = fct_relevel(as.factor(tbi_ever), "Yes", "No"),
         study = case_when((study=="AHEAD") ~ "AHEAD (<1924)",
                           (study=="CODA")  ~ "CODA (1924-30)",
                           (study=="HRS")   ~ "HRS (1931-41)",
                           (study=="WB")    ~ "WB (1942-47)",
                           (study=="EBB")   ~ "EBB (1948-53)",
                           (study=="MBB")   ~ "MBB (1954-59)"),
         study = fct_relevel(as.factor(study), 
                             "AHEAD (<1924)",
                             "CODA (1924-30)",
                             "HRS (1931-41)",
                             "WB (1942-47)",
                             "EBB (1948-53)",
                             "MBB (1954-59)")) %>% 
  mutate(study = fct_drop(study)) %>%
  mutate(social_origins = as_numeric(social_origins)) %>% 
  var_labels(study           = "HRS cohort (birth year)",             #time-invariant 
             sex             = "Self-reported sex", 
             race_ethn       = "Race/Ethnicity",
             edu             = "HS completion",
             incar_ever      = "Lifetime incarceration",
             incar_time_3cat = "Lifetime incarceration duration",
             apoe_info99_4ct = "APOE-4 count",
             smoke_first_iw  = "Smoking status at baseline",
             social_origins  = "Social origins index",
             tbi_ever        = "Childhood TBI",
             cog_2cat        = "Cognitive function",     #time-varying
             year            = "Study year",
             age             = "Age",
             income_hh_10k   = "Household income (10K)",
             alc_daily_avg   = "Alcohol (daily avg.)",
             bmi_combo       = "Body-mass index",
             cesd_3cat       = "Depressive symptoms",
             diab            = "Diabetes",
             hear_sr_2cat    = "Hearing difficulty",
             hibp            = "Hypertension",
             actx_lt_fct     = "Physical activity (light)",
             stroke_ever     = "Stroke status"
  ) 

#time-independent variables
tab1_top <- hrs_labs %>% 
  select(hhidpn,
         incar_time_3cat,
         apoe_info99_4ct,
         sex,
         race_ethn,
         edu,
         incar_ever,
         smoke_first_iw,
         social_origins ,
         tbi_ever,
         study) %>% 
  distinct(hhidpn, .keep_all=TRUE) %>% 
  select(-c(hhidpn, 
  )) %>% 
  tbl_summary(by=incar_ever,
              type = list(c(social_origins) ~ "continuous",
                          c(incar_time_3cat, study, sex, race_ethn, edu, apoe_info99_4ct, smoke_first_iw, tbi_ever) ~ "categorical"),
              statistic = all_continuous() ~ "{mean} ({sd})") %>% 
  add_p(include=-incar_time_3cat) %>%
  # add_p(test.args = all_tests("fisher.test") ~ list(simulate.p.value = TRUE)) %>% 
  bold_p() %>%
  add_overall() %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Ever incarcerated?**")
tab1_top

gtsave(as_gt(tab1_top), "../output/results/tab1_top_descriptives.html")


#time-dependent variables
tab1_bottom <- hrs_labs %>%
  select(hhidpn, 
         incar_ever,
         age, 
         cog_2cat,     
         income_hh_10k,
         alc_daily_avg,
         bmi_combo,    
         cesd_3cat,    
         diab,         
         hear_sr_2cat,
         hibp,         
         actx_lt_fct,  
         stroke_ever,
         year) %>%
  mutate(year=as_factor(year)) %>%
  # group_by(hhidpn) %>%
  # mutate(across(c(age, income_hh_10k, alc_daily_avg, bmi_combo), ~mean(.))) %>%
  # var_labels(age = "Age",
  #            income_hh_10k = "Household income (10K)",
  #            alc_daily_avg = "Alcohol (daily avg.)",
  #            bmi_combo     = "BMI") %>%
  # ungroup() %>%
  tbl_summary(by=incar_ever,
              type = list(c(age, 
                            income_hh_10k, 
                            alc_daily_avg, 
                            bmi_combo) ~ "continuous",
                          c(cesd_3cat,    
                            diab,         
                            hear_sr_2cat,
                            hibp,         
                            actx_lt_fct,  
                            stroke_ever,
                            year) ~ "categorical"),
              statistic = all_continuous() ~ "{mean} ({sd})",
              include = -hhidpn,
              digits = all_continuous() ~ 2) %>%
  add_overall() %>%
  modify_header(label ~ "**Variable**") %>% 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Ever incarcerated?**") %>% 
  add_p(include=year) %>% 
  bold_p()
tab1_bottom

gtsave(as_gt(tab1_bottom), "../output/results/tab1_bottom_descriptives.html")


#stack tables
tab1_combined <- tbl_stack(tbls = list(tab1_top, tab1_bottom),
          group_header = c(glue("Individuals (N = {style_number(cases,big.mark=',')})"), 
                           glue("Observations (N = {style_number(obs,big.mark=',')})"))) %>% 
  modify_caption('**Table 1**. Individual and observation-level descriptive statistics of the Health and Retirement Study.')
tab1_combined

#export .html table
gtsave(as_gt(tab1_combined), "../output/results/tab1_descriptives.html")

#export .docx table
tab1_combined %>%
  as_flex_table() %>%
  flextable::save_as_docx(path="../output/results/tab1_descriptives.docx")

#=====get pvalues 
#=cognitive function
cog_pval <- glmer(cog_2cat_num ~ factor(incar_ever) + (1|hhidpn), 
      data=hrs, 
      family=binomial, 
      control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5))) %>% 
  tidy() %>% 
  filter(term=="factor(incar_ever)Incarcerated") %>% 
  pull(p.value)
# cog_pval
# [1] 2.18628e-22

#stroke
strok_pval <- glmer(stroke_ever ~ factor(incar_ever) + (1|hhidpn), 
                  data=hrs, 
                  family=binomial, 
                  control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5))) %>% 
  tidy() %>% 
  filter(term=="factor(incar_ever)Incarcerated") %>% 
  pull(p.value)
# strok_pval
# [1] 0.2276284

# diabetes
diab_pval <- glmer(factor(diab) ~ factor(incar_ever) + (1|hhidpn), 
                    data=hrs, 
                    family=binomial, 
                    control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5))) %>% 
  tidy() %>% 
  filter(term=="factor(incar_ever)Incarcerated") %>% 
  pull(p.value)
# diab_pval
# [1] 0.008436289

# hearing
hear_pval <- glmer(factor(hear_sr_2cat) ~ factor(incar_ever) + (1|hhidpn), 
                   data=hrs, 
                   family=binomial, 
                   control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5))) %>% 
  tidy() %>% 
  filter(term=="factor(incar_ever)Incarcerated") %>% 
  pull(p.value)
# hear_pval
# [1] 2.61667e-15

# hypertension
hibp_pval <- glmer(factor(hibp) ~ factor(incar_ever) + (1|hhidpn), 
                   data=hrs, 
                   family=binomial, 
                   control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5))) %>% 
  tidy() %>% 
  filter(term=="factor(incar_ever)Incarcerated") %>% 
  pull(p.value)
# hibp_pval
# [1] 2.945989e-05

#continuous, time-dependent
contin_timedep <- hrs_labs %>% 
  select(hhidpn, incar_ever, cesd_3cat, actx_lt_fct, age, income_hh_10k, alc_daily_avg, bmi_combo) %>% 
  group_by(hhidpn) %>% 
  mutate(across(c(cesd_3cat, actx_lt_fct, age, income_hh_10k, alc_daily_avg, bmi_combo), ~as.numeric(.)),
         across(c(cesd_3cat, actx_lt_fct, age, income_hh_10k, alc_daily_avg, bmi_combo), ~mean(.))) %>% 
  distinct() %>% 
  ungroup() %>% 
  tbl_summary(include = -hhidpn,
              by=incar_ever) %>% 
  add_p()
contin_timedep


#=Table 2 - Main results=======================================================

main_results <- import_list("../output/results/tab3_main_results.rdata")
list2env(main_results, .GlobalEnv)
rm(main_results)


tab11 <- tbl_regression(m12, exponentiate=TRUE,
                        include="factor(apoe_info99_4ct)",
                        label=list("factor(apoe_info99_4ct)"="APOE-4 allele count"),
                        intercept=TRUE)
tab12 <- tbl_regression(m11, exponentiate=TRUE,
                        include="factor(incar_ever)",
                        label=list("factor(incar_ever)"="Lifetime incarceration"),
                        intercept=TRUE)
tab21 <- tbl_regression(m21, exponentiate=TRUE,
                        include=c("factor(incar_ever)","factor(apoe_info99_4ct)"),
                        label=list("factor(apoe_info99_4ct)"="APOE-4 allele count",
                                   "factor(incar_ever)"="Lifetime incarceration"),
                        intercept=TRUE)
tab22 <- tbl_regression(m22, exponentiate=TRUE,
                        include=c("factor(incar_ever)","factor(apoe_info99_4ct)"),
                        label=list("factor(apoe_info99_4ct)"="APOE-4 allele count",
                                   "factor(incar_ever)"="Lifetime incarceration"),
                        intercept=TRUE)
tab31 <- tbl_regression(m31, exponentiate=TRUE,
                        include=c("factor(incar_ever)","factor(apoe_info99_4ct)",
                                  "factor(incar_ever):factor(apoe_info99_4ct)"),
                        label=list("factor(apoe_info99_4ct)"="APOE-4 allele count",
                                   "factor(incar_ever)"="Lifetime incarceration",
                                   "factor(incar_ever):factor(apoe_info99_4ct)"="Lifetime Incarceration*APOE-4 allele count"),
                        intercept=TRUE)
tab32 <- tbl_regression(m32, exponentiate=TRUE,
                        include=c("factor(incar_ever)","factor(apoe_info99_4ct)",
                                  "factor(incar_ever):factor(apoe_info99_4ct)"),
                        label=list("factor(apoe_info99_4ct)"="APOE-4 allele count",
                                   "factor(incar_ever)"="Lifetime incarceration",
                                   "factor(incar_ever):factor(apoe_info99_4ct)"="Lifetime Incarceration*APOE-4 allele count"),
                        intercept=TRUE)

tab2_mods <- list(tab11,
                  tab12,
                  tab21,
                  tab22,
                  tab31,
                  tab32)
#write function to update gtsumamry tables
tab_updates <- function(x){
  x %>%
    add_significance_stars(hide_ci = FALSE, hide_p = TRUE, hide_se = TRUE) %>%
    remove_row_type(type = "reference") %>%
    modify_table_body(~ .x %>%
                        dplyr::mutate(ci = ifelse(!is.na(ci), paste0("[", ci, "]"), "")))
}

tab1_mods_update <- lapply(tab2_mods, tab_updates)

tab2_all <- tbl_merge(tab1_mods_update, tab_spanner = paste("Model 2.", 1:6, sep = "")) %>%
  modify_header(label = "**Variable**") %>%
  modify_caption(glue('**Table 2**. Mixed effect Poisson regression of cognitive status on *APOE-&epsilon;4* genotype and 
                      lifetime incarceration in the HRS 
                      (*N<sub>Observation</sub>*={style_number(obs,big.mark=',')}; 
                      *N<sub>Cases</sub>*={style_number(cases,big.mark=',')})')) %>%
  modify_footnote(label ~ "The “baseline” adjustment for all models included age, sex, race/ethnicity, high school 
                  completion, and HRS cohort. The “full” adjustment (models 2.4, 2.6) also adjusted for stroke status, 
                  alcohol intake, BMI, depression symptoms, diabetes status, hearing difficulty, hypertension, household 
                  income, (light) physical activity level, smoking history, childhood financial hardship, and childhood 
                  traumatic brain injury.")
tab2_all

tab2_all %>%
  as_gt() %>%
  gtsave("../output/results/tab2_main_res.html")

# tab2_all %>%
#   as_flex_table() %>%
#   flextable::save_as_docx(path="../output/results/tab2_main_res.docx")


#=Table S2 - sensitivity=======================================================

time_results <- import_list("../output/results/incar_time_results.rdata")
list2env(time_results, .GlobalEnv)
rm(time_results)

tab_time1 <- tbl_regression(m41, exponentiate=TRUE,
                          include=c("factor(incar_time_3cat)", "factor(apoe_info99_4ct)"),
                          label=list("factor(incar_time_3cat)"      ="Lifetime incarceration duration",
                                     "factor(apoe_info99_4ct)" = "APOE-4 allele count"))
tab_time2 <- tbl_regression(m42, exponentiate=TRUE,
                           include=c("factor(incar_time_3cat)", "factor(apoe_info99_4ct)"),
                           label=list("factor(incar_time_3cat)"      ="Lifetime incarceration duration",
                                      "factor(apoe_info99_4ct)" = "APOE-4 allele count"))

tab_time_mods <- list(tab_time1, tab_time2)

#write function to update gtsumamry tables
tab_updates <- function(x){
  x %>%
    add_significance_stars(hide_ci = FALSE, hide_p = TRUE, hide_se = TRUE) %>%
    remove_row_type(type = "reference") %>%
    modify_table_body(~ .x %>%
                        dplyr::mutate(ci = ifelse(!is.na(ci), paste0("[", ci, "]"), ""))) 
}

tab_time_mods_update <- lapply(tab_time_mods, tab_updates)
tab_time_all <- tbl_merge(tab_time_mods_update, tab_spanner = glue("Model S2.{1:2}")) %>%
  modify_header(label = "**Variable**") %>% 
  modify_footnote(label ~ "The “baseline” adjustment for all models included age, sex, race/ethnicity, 
                  high school completion, and HRS cohort. The “full” adjustment (models 2.4, 2.6) 
                  also adjusted for stroke status, alcohol intake, BMI, depression symptoms, diabetes 
                  status, hearing difficulty, hypertension, household income, (light) physical 
                  activity level, smoking history, childhood financial hardship, and childhood 
                  traumatic brain injury.") %>% 
  modify_caption(caption = "**Table S2**. ")
tab_time_all


#=Table 3 - Stratified results==================================================


strat_results <- import_list("../output/results/strat_results.rdata")
list2env(strat_results, .GlobalEnv)
rm(strat_results)

#format each table
tab_main_reduc <- tbl_regression(m21_race_main, exponentiate=TRUE,
                           include=c("factor(apoe_info99_4ct)", "factor(incar_ever)", 
                                     "factor(race_ethn)"),
                           label=list("factor(incar_ever)"      = "Lifetime incarceration",
                                      "factor(apoe_info99_4ct)" = "APOE-4 allele count",
                                      "factor(race_ethn)"       = "Race/ethnicity"),
                           intercept=TRUE)
tab_main <- tbl_regression(m21, exponentiate=TRUE,
                          include=c("factor(apoe_info99_4ct)", "factor(incar_ever)", 
                                    "factor(sex)",
                                    "factor(edu)"),
                          label=list("factor(incar_ever)"      = "Lifetime incarceration",
                                     "factor(apoe_info99_4ct)" = "APOE-4 allele count",
                                     "factor(sex)"             = "Male",
                                     "factor(edu)"             = "High school completion"),
                          intercept=TRUE)
tab_sex <- tbl_regression(m21_sex, exponentiate=TRUE,
                          include=c("factor(apoe_info99_4ct)", "factor(incar_ever)", 
                                    "factor(sex)",
                                    "factor(sex):factor(apoe_info99_4ct)",
                                    "factor(incar_ever):factor(sex)"),
                          label=list("factor(incar_ever)"                  = "Lifetime incarceration",
                                     "factor(apoe_info99_4ct)"             = "APOE-4 allele count",
                                     "factor(sex)"                         = "Male",
                                     "factor(sex):factor(apoe_info99_4ct)" = "Sex*",
                                     "factor(sex):factor(apoe_info99_4ct)" = "Sex*",
                                     "factor(incar_ever):factor(sex)"      = "Sex*"),
                          intercept=TRUE)
tab_race <- tbl_regression(m21_race_int, exponentiate=TRUE,
                          include=c("factor(apoe_info99_4ct)", "factor(incar_ever)", 
                                    "factor(race_ethn)",
                                    "factor(incar_ever):factor(race_ethn)",
                                    "factor(race_ethn):factor(apoe_info99_4ct)"),
                          label=list("factor(incar_ever)"                        = "Lifetime incarceration",
                                     "factor(apoe_info99_4ct)"                   = "APOE-4 allele count",
                                     "factor(race_ethn)"                         = "Race/ethnicity",
                                     "factor(race_ethn):factor(apoe_info99_4ct)" = "Race/ethnicity*",
                                     "factor(race_ethn):factor(apoe_info99_4ct)" = "Race/ethnicity*",
                                     "factor(incar_ever):factor(race_ethn)"      = "Race/ethnicity*"),
                          intercept=TRUE)
tab_edu <- tbl_regression(m21_edu, exponentiate=TRUE,
                        include=c("factor(apoe_info99_4ct)", "factor(incar_ever)", 
                                  "factor(edu)",
                                  "factor(incar_ever):factor(edu)",
                                  "factor(edu):factor(apoe_info99_4ct)"),
                        label=list("factor(incar_ever)"                  = "Lifetime incarceration",
                                   "factor(apoe_info99_4ct)"             = "APOE-4 allele count",
                                   "factor(edu)"                         = "High school completion",
                                   "factor(edu):factor(apoe_info99_4ct)" = "High school completion*",
                                   "factor(edu):factor(apoe_info99_4ct)" = "High school completion*",
                                   "factor(incar_ever):factor(edu)"      = "High school completion*"),
                        intercept=TRUE)

tab3_mods <- list(tab_main_reduc,
                  tab_race,
                  tab_main,
                  tab_sex,
                  tab_edu)

#write function to update gtsumamry tables
tab_updates <- function(x){
  x %>%
    add_significance_stars(hide_ci = FALSE, hide_p = TRUE, hide_se = TRUE) %>%
    remove_row_type(type = "reference") %>%
    modify_table_body(~ .x %>%
                        dplyr::mutate(ci = ifelse(!is.na(ci), paste0("[", ci, "]"), ""))) 
}

tab3_mods_update <- lapply(tab3_mods, tab_updates)
tab3_all <- tbl_merge(tab3_mods_update, tab_spanner = glue("Model 3.{1:5}")) %>%
  modify_header(label = "**Variable**") %>% 
  modify_footnote(label ~ "All models also adjusted for age, sex, race/ethnicity, high school completion, and HRS cohort.") 
tab3_all

#save out table
tab3_all %>%
  as_gt() %>%
  gtsave("../output/results/tab2_strat.html")

#=Table S1 - Survival results==================================================

# hrs <- import("hrs_full_analytic.rds")


surv_results <- import_list("../output/results/main_results_surv_models.rdata")
list2env(surv_results, .GlobalEnv)
rm(surv_results)


tab_cox11 <- tbl_regression(cox12, exponentiate = TRUE,
                              include=c("factor(apoe_info99_4ct)one copy",
                                        "factor(apoe_info99_4ct)two copies"),
                              label = list("factor(apoe_info99_4ct)one copy" = "One APOE-4 allele",
                                           "factor(apoe_info99_4ct)two copies" = "Two APOE-4 alleles"))
tab_cox12 <- tbl_regression(cox11, exponentiate = TRUE,
                            include = "factor(incar_ever)Incarcerated",
                            label = list("factor(incar_ever)Incarcerated" = "Lifetime Incarceration"))
tab_cox21 <- tbl_regression(cox21, exponentiate = TRUE,
                              include=c("factor(apoe_info99_4ct)one copy",
                                        "factor(apoe_info99_4ct)two copies",
                                        "factor(incar_ever)Incarcerated"),
                              label = list("factor(apoe_info99_4ct)one copy" = "One APOE-4 allele",
                                           "factor(apoe_info99_4ct)two copies" = "Two APOE-4 alleles",
                                           "factor(incar_ever)Incarcerated" = "Lifetime Incarceration"))
tab_cox22 <- tbl_regression(cox22, exponentiate = TRUE,
                              include=c("factor(apoe_info99_4ct)one copy",
                                        "factor(apoe_info99_4ct)two copies",
                                        "factor(incar_ever)Incarcerated"),
                              label = list("factor(apoe_info99_4ct)one copy" = "One APOE-4 allele",
                                           "factor(apoe_info99_4ct)two copies" = "Two APOE-4 alleles",
                                           "factor(incar_ever)Incarcerated" = "Lifetime Incarceration"))
tab_cox31 <- tbl_regression(cox31, exponentiate = TRUE,
                              include=c("factor(apoe_info99_4ct)one copy",
                                        "factor(apoe_info99_4ct)two copies",
                                        "factor(incar_ever)Incarcerated",
                                        "factor(incar_ever)Incarcerated:factor(apoe_info99_4ct)one copy",
                                        "factor(incar_ever)Incarcerated:factor(apoe_info99_4ct)two copies"),
                              label=list("factor(apoe_info99_4ct)one copy" = "One APOE-4 allele",
                                         "factor(apoe_info99_4ct)two copies" = "Two APOE-4 alleles",
                                         "factor(incar_ever)Incarcerated" = "Lifetime Incarceration",
                                         "factor(incar_ever)Incarcerated:factor(apoe_info99_4ct)one copy" = 
                                           "Lifetime Incarceration*One APOE-4 allele",
                                         "factor(incar_ever)Incarcerated:factor(apoe_info99_4ct)two copies" = 
                                           "Lifetime Incarceration*Two APOE-4 alleles"))
tab_cox32 <- tbl_regression(cox32, exponentiate = TRUE,
                               include=c("factor(apoe_info99_4ct)one copy",
                                         "factor(apoe_info99_4ct)two copies",
                                         "factor(incar_ever)Incarcerated",
                                         "factor(incar_ever)Incarcerated:factor(apoe_info99_4ct)one copy",
                                         "factor(incar_ever)Incarcerated:factor(apoe_info99_4ct)two copies"),
                               label=list("factor(apoe_info99_4ct)one copy" = "One APOE-4 allele",
                                          "factor(apoe_info99_4ct)two copies" = "Two APOE-4 alleles",
                                          "factor(incar_ever)Incarcerated" = "Lifetime Incarceration",
                                          "factor(incar_ever)Incarcerated:factor(apoe_info99_4ct)one copy" = 
                                            "Lifetime Incarceration*One APOE-4 allele",
                                          "factor(incar_ever)Incarcerated:factor(apoe_info99_4ct)two copies" = 
                                            "Lifetime Incarceration*Two APOE-4 alleles"))

#group summary tables
tab_mods <- list(tab_cox11,
                    tab_cox12,
                    tab_cox21,
                    tab_cox22,
                    tab_cox31,
                    tab_cox32)

#function to update tables
tab_updates <- function(x){
  x %>%
    add_significance_stars(hide_ci = FALSE, hide_p = TRUE, hide_se = TRUE) %>%
    remove_row_type(type = "reference") %>%
    modify_table_body(~ .x %>%
                        dplyr::mutate(ci = ifelse(!is.na(ci), paste0("[", ci, "]"), "")))
}

#apply updates
tab_mods_update <- lapply(tab_mods, tab_updates)

#merge models
tab_mods_all <- tbl_merge(tab_mods_update, tab_spanner = paste("Model S1.", 1:6, sep = "")) %>% 
  modify_caption(glue("**Table S1.** Cox proportional hazard model of lifetime incarceration and 
                      APOE-4 genotype on first cognitive impairment.")) %>% 
  modify_footnote(label ~ "The “baseline” adjustment for all models included sex, 
                  race/ethnicity, high school completion, and HRS cohort. The “full” adjustment 
                  (models S1.4, S1.6) also adjusted for stroke status, alcohol intake, BMI, 
                  depression symptoms, diabetes status, hearing difficulty, hypertension, 
                  household income, (light) physical activity level, smoking history, childhood 
                  financial hardship, and childhood traumatic brain injury.") %>% 
  modify_header(label = "**Variable**") 
tab_mods_all


#save out table
tab_mods_all %>%
  as_gt() %>%
  gtsave("../output/results/tab_s1_all.html")

# tab_s1_combined %>%
#   as_flex_table() %>%
#   flextable::save_as_docx(path="../output/results/tab_s1_combined.docx")


tbl_stack(tbls=list(tab_main, tab_sex))


