source("0_packages.R")

hrs_full <- import("hrs_full_analytic.rds") 

#=Main analysis================================================================

covars_min <- "factor(sex) + factor(race_ethn) + scale(age) + factor(stroke_ever) + factor(study) + factor(edu)"
covars_full <- "factor(sex) + factor(race_ethn) + scale(age) + factor(stroke_ever) + factor(study) + factor(edu) +
scale(alc_daily_avg_logc1) + scale(bmi_combo) + factor(cesd_3cat) + factor(diab) + factor(hear_sr_2cat) +
factor(hibp) + scale(income_hh_logc1) + factor(actx_lt_fct) + factor(smoke_first_iw) + scale(social_origins) + factor(tbi_ever)"

#set model formulas 
m11_base     <- formula(glue("cog_2cat_num ~ {covars_min} + (1|hhidpn)"))
m12_base     <- formula(glue("cog_2cat_num ~ {covars_full} + (1|hhidpn)"))
m21_main_eff <- formula(glue("cog_2cat_num ~ factor(incar_ever) + factor(apoe_info99_4ct) + {covars_min}  + (1|hhidpn)"))
m22_main_eff <- formula(glue("cog_2cat_num ~ factor(incar_ever) + factor(apoe_info99_4ct) + {covars_full} + (1|hhidpn)"))
m31_int_eff  <- formula(glue("cog_2cat_num ~ factor(incar_ever)*factor(apoe_info99_4ct) + {covars_min}  + (1|hhidpn)"))
m32_int_eff  <- formula(glue("cog_2cat_num ~ factor(incar_ever)*factor(apoe_info99_4ct) + {covars_full} + (1|hhidpn)"))

#run models (running into convergence issues; test nAGQ=0)
# m21      <- glmer(m21_main_eff,  data=hrs_full, family=poisson(link="log"), control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
# m21_test <- glmer(m21_main_eff,  data=hrs_full, family=poisson(link="log"), nAGQ = 0, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
# test <- bind_rows(tidy(m21, exponentiate=TRUE, conf.int=TRUE) %>% mutate(model="base"), 
#                   tidy(m21_test, exponentiate=TRUE, conf.int=TRUE) %>% mutate(model="test")) %>% 
#   filter(!grepl("study|Intercept", term)) 
# 
# ggplot(test, aes(estimate, term, col=model)) +
#   geom_point(position = position_dodge(width = .5)) +
#   geom_errorbarh(aes(xmin=conf.low, xmax=conf.high), position = position_dodge(width = .5)) +
#   theme_set(theme_classic())


# m11 <- glmer(m11_base,      data=hrs_full, family=poisson(link="log"), nAGQ = 0, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
m11 <- glmer(m11_base,      data=hrs_full, family=poisson(link="log"), control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
m12 <- glmer(m12_base,      data=hrs_full, family=poisson(link="log"), control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
m21 <- glmer(m21_main_eff,  data=hrs_full, family=poisson(link="log"), control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
m22 <- glmer(m22_main_eff,  data=hrs_full, family=poisson(link="log"), control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
m31 <- glmer(m31_int_eff,   data=hrs_full, family=poisson(link="log"), control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
m32 <- glmer(m32_int_eff,   data=hrs_full, family=poisson(link="log"), control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

#check linear probabily model
# lmer(m14_int_eff,   data=hrs_full) %>% broom.mixed::tidy(conf.int=TRUE) %>% view()

rio::export(c("m11","m12","m21","m22","m31","m32"),
            "../output/results/tab3_main_results.rdata")

#table models
res_11 <- tidy(m11, exponentiate=TRUE) %>% mutate(model="model 11") %>% mutate(glance(m11)) 
res_12 <- tidy(m12, exponentiate=TRUE) %>% mutate(model="model 12") %>% mutate(glance(m12)) 
res_21 <- tidy(m21, exponentiate=TRUE) %>% mutate(model="model 21") %>% mutate(glance(m21))
res_22 <- tidy(m22, exponentiate=TRUE) %>% mutate(model="model 22") %>% mutate(glance(m22))
res_31 <- tidy(m31, exponentiate=TRUE) %>% mutate(model="model 31") %>% mutate(glance(m31))
res_32 <- tidy(m32, exponentiate=TRUE) %>% mutate(model="model 32") %>% mutate(glance(m32))


main_results <- bind_rows(res_11,
                          res_12,
                          res_21,
                          res_22,
                          res_31,
                          res_32)

#export results
rio::export(main_results, "../output/results/tab3_main_results.csv")




#=robustness check - incarceration duration ===================================

#crosstab
cross_incar_time <- hrs_full %>% 
  distinct(hhidpn, .keep_all=TRUE) %>%
  mutate(apoe = case_when((apoe_info99_4ct=="zero copies") ~ "Zero copies",
                          (apoe_info99_4ct=="one copy")    ~ "One copy"   ,
                          (apoe_info99_4ct=="two copies")  ~ "Two copies" ),
         apoe = fct_relevel(as.factor(apoe), "Zero copies","One copy","Two copies")) %>% 
  var_labels(incar_time_3cat = "Lifetime incarceration duration",
             apoe = "APOE-e4") %>% 
  tbl_cross(incar_time_3cat, apoe, margin = NULL)
cross_incar_time

#set model formulas 
m41_main_eff <- formula(glue("cog_2cat_num ~ factor(incar_time_3cat) + factor(apoe_info99_4ct) + {covars_min}  + (1|hhidpn)"))
m42_main_eff <- formula(glue("cog_2cat_num ~ factor(incar_time_3cat) + factor(apoe_info99_4ct) + {covars_full} + (1|hhidpn)"))
m51_int_eff  <- formula(glue("cog_2cat_num ~ factor(incar_time_3cat)*factor(apoe_info99_4ct) + {covars_min}  + (1|hhidpn)"))
m52_int_eff  <- formula(glue("cog_2cat_num ~ factor(incar_time_3cat)*factor(apoe_info99_4ct) + {covars_full} + (1|hhidpn)"))

#run models 

m41 <- glmer(m41_main_eff,  data=hrs_full, family=poisson(link="log"), control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
m42 <- glmer(m42_main_eff,  data=hrs_full, family=poisson(link="log"), control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5))) 
m51 <- glmer(m51_int_eff,   data=hrs_full, family=poisson(link="log"), control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
m52 <- glmer(m52_int_eff,   data=hrs_full, family=poisson(link="log"), control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

#export model objects
rio::export(c("m41", "m42", "m51", "m52"),
            "../output/results/incar_time_results.rdata")

#tidy results
res_m41 <- tidy(m41, exponentiate=TRUE, conf.int=TRUE) %>% mutate(model = "model 41")
res_m42 <- tidy(m42, exponentiate=TRUE, conf.int=TRUE) %>% mutate(model = "model 42")
res_m51 <- tidy(m51, exponentiate=TRUE, conf.int=TRUE) %>% mutate(model = "model 51")
res_m52 <- tidy(m52, exponentiate=TRUE, conf.int=TRUE) %>% mutate(model = "model 52")

res_incar_time <- bind_rows(res_m41,
                            res_m42,
                            res_m51,
                            res_m52)



#=sub-group analysis===========================================================

#=====Crosstabs of incar, apoe, and sex/race/edu===============================

#sex
cross_sex <- hrs_full %>% 
  distinct(hhidpn, .keep_all=TRUE) %>%
  mutate(apoe = case_when((apoe_info99_4ct=="zero copies") ~ "Zero copies",
                          (apoe_info99_4ct=="one copy")    ~ "One copy"   ,
                          (apoe_info99_4ct=="two copies")  ~ "Two copies" ),
         apoe = fct_relevel(as.factor(apoe), "Zero copies","One copy","Two copies")) %>% 
  mutate(sex = fct_relevel(sex, "Male")) %>% 
  var_labels(sex = "Sex") %>% 
  tbl_strata(strata = incar_ever,
             .tbl_fun = ~ .x %>% tbl_cross(sex, apoe, margin = NULL))
#race_ethn
cross_race <- hrs_full %>% 
  distinct(hhidpn, .keep_all=TRUE) %>%
  mutate(apoe = case_when((apoe_info99_4ct=="zero copies") ~ "Zero copies",
                          (apoe_info99_4ct=="one copy")    ~ "One copy"   ,
                          (apoe_info99_4ct=="two copies")  ~ "Two copies" ),
         apoe = fct_relevel(as.factor(apoe), "Zero copies","One copy","Two copies")) %>% 
  var_labels(race_ethn = "Race/ethnicity") %>% 
  tbl_strata(strata = incar_ever,
             .tbl_fun = ~ .x %>% tbl_cross(race_ethn, apoe, margin = NULL))
#edu
cross_edu <- hrs_full %>% 
  distinct(hhidpn, .keep_all=TRUE) %>%
  mutate(edu = ifelse(edu=="hs or more", "Completed high school", "Less than high school"),
         edu = fct_relevel(as.factor(edu), "Less than high school")) %>% 
  var_labels(edu = "High school completion") %>% 
  mutate(apoe = case_when((apoe_info99_4ct=="zero copies") ~ "Zero copies",
                          (apoe_info99_4ct=="one copy")    ~ "One copy"   ,
                          (apoe_info99_4ct=="two copies")  ~ "Two copies" ),
         apoe = fct_relevel(as.factor(apoe), "Zero copies","One copy","Two copies")) %>% 
  var_labels(edu = "High school completion") %>% 
  tbl_strata(strata = incar_ever,
             .tbl_fun = ~ .x %>% tbl_cross(edu, apoe, margin = NULL))

cross_comb <- tbl_stack(list(cross_sex, cross_race, cross_edu))
cross_comb

cross_comb %>% 
  as_gt() %>% 
  gtsave("../output/results/tab_s3_crosstabs_sexraceedu.html")

#======interaction models with minimal covariates==============================

#sex
m21_sex_int <- formula(glue("cog_2cat_num ~ factor(incar_ever)*factor(sex) + factor(apoe_info99_4ct)*factor(sex) + {covars_min} + (1|hhidpn)"))
# m21_sex <- glmer(m21_sex_int,  data=hrs_full, family=poisson(link="log"), nAGQ = 0, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
m21_sex <- glmer(m21_sex_int,  data=hrs_full, family=poisson(link="log"), control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
res_m21_sex <- tidy(m21_sex, exponentiate=TRUE, conf.int=TRUE) %>% mutate(model = "sex")

#race
m21_race_int     <- formula(glue("cog_2cat_num ~ factor(incar_ever)*factor(race_ethn) + factor(apoe_info99_4ct)*factor(race_ethn) + {covars_min} + (1|hhidpn)"))
# m21_race <- glmer(m21_race_int,  data=hrs_full, family=poisson(link="log"), nAGQ = 0, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
m21_race <- glmer(m21_race_int,  data=hrs_full, family=poisson(link="log"), control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
res_m21_race <- tidy(m21_race, exponentiate=TRUE, conf.int=TRUE) %>% mutate(model = "race") #statistically significant result for Hispanics

#edu
m21_edu_int     <- formula(glue("cog_2cat_num ~ factor(incar_ever)*factor(edu) + factor(apoe_info99_4ct)*factor(edu) + {covars_min} + (1|hhidpn)"))
# m21_edu <- glmer(m21_edu_int,  data=hrs_full, family=poisson(link="log"), nAGQ = 0, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
m21_edu <- glmer(m21_edu_int,  data=hrs_full, family=poisson(link="log"), control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
res_m21_edu <- tidy(m21_edu, exponentiate=TRUE, conf.int=TRUE)  %>% mutate(model = "edu")

#export model objects
rio::export(c("m21_sex", "m21_race", "m21_edu"),
            "../output/results/strat_results.rdata")

res_strat <- bind_rows(res_m21_sex,
                       res_m21_race,
                       res_m21_edu) %>% 
  select(-group)

rio::export(res_strat, "../output/results/tab_s3_results.csv")



