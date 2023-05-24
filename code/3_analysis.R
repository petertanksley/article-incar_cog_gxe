source("0_packages.R")

hrs_full <- import("hrs_full_analytic.rds") 
hrs_pgs <- import("hrs_pgs_analytic.rds") 

#=H1: prevalence===============================================================

#set model formulas 
m10_base <- formula("cog_2cat_num ~ 
                   factor(sex) + factor(race_ethn) + scale(age) + factor(edu) + scale(social_origins) + factor(smoke_ever) + factor(stroke_ever) + factor(study) +
                   (1|hhidpn)")
m11_apoe <- formula("cog_2cat_num ~ factor(apoe_info99_4ct) +
                   factor(sex) + factor(race_ethn) + scale(age) + factor(edu) + scale(social_origins) + factor(smoke_ever) + factor(stroke_ever) + factor(study) +
                   (1|hhidpn)")
m12_incar <- formula("cog_2cat_num ~ factor(incar_ever) + 
                   factor(sex) + factor(race_ethn) + scale(age) + factor(edu) + scale(social_origins) + factor(smoke_ever) + factor(stroke_ever) + factor(study) +
                   (1|hhidpn)")
m13_main_eff <- formula("cog_2cat_num ~ factor(incar_ever) + factor(apoe_info99_4ct) +
                   factor(sex) + factor(race_ethn) + scale(age) + factor(edu) + scale(social_origins) + factor(smoke_ever) + factor(stroke_ever) + factor(study) +
                   (1|hhidpn)")
m14_int_eff <- formula("cog_2cat_num ~ factor(incar_ever)*factor(apoe_info99_4ct) +
                   factor(sex) + factor(race_ethn) + scale(age) + factor(edu) + scale(social_origins) + factor(smoke_ever) + factor(stroke_ever) + factor(study) +
                   (1|hhidpn)")

#run models 
m10 <- glmer(m10_base,      data=hrs_full, family=poisson(link="log"), control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
m11 <- glmer(m11_apoe,      data=hrs_full, family=poisson(link="log"), control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
m12 <- glmer(m12_incar,     data=hrs_full, family=poisson(link="log"), control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
m13 <- glmer(m13_main_eff,  data=hrs_full, family=poisson(link="log"), control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
m14 <- glmer(m14_int_eff,   data=hrs_full, family=poisson(link="log"), control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

rio::export(c("m10","m11","m12","m13","m14"),
       "../output/results/tab3_main_results.rdata")

#table models
res_10 <- tidy(m10, exponentiate=TRUE) %>% mutate(model="model base") %>% mutate(glance(m10)) 
res_11 <- tidy(m11, exponentiate=TRUE) %>% mutate(model="model 1")    %>% mutate(glance(m11)) 
res_12 <- tidy(m12, exponentiate=TRUE) %>% mutate(model="model 2")    %>% mutate(glance(m12))
res_13 <- tidy(m13, exponentiate=TRUE) %>% mutate(model="model 3")    %>% mutate(glance(m13))
res_14 <- tidy(m14, exponentiate=TRUE) %>% mutate(model="model 4")    %>% mutate(glance(m14))


main_results <- bind_rows(res_10,
                          res_11,
                          res_12,
                          res_13,
                          res_14)

#export results
rio::export(main_results, "../output/results/tab3_main_results.csv")

# #=PGS analysis================================================================
# 
# #set model formulas 
# m0_base <- formula("cog_2cat_num ~ 
#                    factor(sex) + scale(age) + scale(social_origins) + factor(stroke_ever) + factor(study) +
#                    (1|hhidpn)")
# m1_pgs <- formula("cog_2cat_num ~ scale(ad_pgs_resid) + 
#                    factor(sex) + scale(age) + scale(social_origins) + factor(stroke_ever) + factor(study) +
#                    (1|hhidpn)")
# m2_apoe <- formula("cog_2cat_num ~ factor(apoe_info99_4ct) +
#                    factor(sex) + scale(age) + scale(social_origins) + factor(stroke_ever) + factor(study) +
#                    (1|hhidpn)")
# m3_pgs_apoe <- formula("cog_2cat_num ~ scale(ad_pgs_resid) + factor(apoe_info99_4ct) +
#                    factor(sex) + scale(age) + scale(social_origins) + factor(stroke_ever) + factor(study) +
#                    (1|hhidpn)")
# m4_incar <- formula("cog_2cat_num ~ factor(incar_ever) + 
#                    factor(sex) + scale(age) + scale(social_origins) + factor(stroke_ever) + factor(study) +
#                    (1|hhidpn)")
# m5_main_eff <- formula("cog_2cat_num ~ factor(incar_ever) + scale(ad_pgs_resid) + factor(apoe_info99_4ct) +
#                    factor(sex) + scale(age) + scale(social_origins) + factor(stroke_ever) + factor(study) +
#                    (1|hhidpn)")
# m6_int_eff <- formula("cog_2cat_num ~ factor(incar_ever)*scale(ad_pgs_resid) + factor(incar_ever)*factor(apoe_info99_4ct) +
#                    factor(sex) + scale(age) + scale(social_origins) + factor(stroke_ever) + factor(study) +
#                    (1|hhidpn)")
# 
# #run models 
# m0 <- glmer(m0_base,     data=hrs, family=poisson(link="log"), control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
# m1 <- glmer(m1_pgs,      data=hrs, family=poisson(link="log"), control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
# m2 <- glmer(m2_apoe,     data=hrs, family=poisson(link="log"), control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
# m3 <- glmer(m3_pgs_apoe, data=hrs, family=poisson(link="log"), control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
# m4 <- glmer(m4_incar,    data=hrs, family=poisson(link="log"), control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
# m5 <- glmer(m5_main_eff, data=hrs, family=poisson(link="log"), control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
# m6 <- glmer(m6_int_eff,  data=hrs, family=poisson(link="log"), control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
# 
# export(c("m0","m1","m2","m3","m4","m5","m6"),
#             "../output/results/tab3_main_results.rdata")
# 
# 
# #table models
# res_0 <- tidy(m0, exponentiate=TRUE) %>% mutate(model="model base") %>% mutate(glance(m0)) 
# res_1 <- tidy(m1, exponentiate=TRUE) %>% mutate(model="model 1")    %>% mutate(glance(m1)) 
# res_2 <- tidy(m2, exponentiate=TRUE) %>% mutate(model="model 2")    %>% mutate(glance(m2))
# res_3 <- tidy(m3, exponentiate=TRUE) %>% mutate(model="model 3")    %>% mutate(glance(m3))
# res_4 <- tidy(m4, exponentiate=TRUE) %>% mutate(model="model 4")    %>% mutate(glance(m4))
# res_5 <- tidy(m5, exponentiate=TRUE) %>% mutate(model="model 5")    %>% mutate(glance(m5))
# res_6 <- tidy(m6, exponentiate=TRUE) %>% mutate(model="model 6")    %>% mutate(glance(m6))
# 
# main_results <- bind_rows(res_0,
#                           res_1,
#                           res_2,
#                           res_3,
#                           res_4,
#                           res_5,
#                           res_6)
# 
# #export results
# export(main_results, "../output/results/tab3_main_results.csv")
# 
#=END==========================================================================











