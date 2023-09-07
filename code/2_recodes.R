source("0_packages.R")

hrs_merged <- import("hrs_merged.rds")



#=Recodes======================================================================
# age, two category version of cogfunction, drop HRS/LBB cohorts (too few) 
#=============================================================================#


hrs_recodes <- hrs_merged %>% 
  mutate(age = year-birthyr) %>% 
  mutate(cog_2cat = case_when((cogfunction=="normal") ~ "Normal",
                              (cogfunction %in% c("cind", "demented")) ~ "Impaired",
                              TRUE ~ NA_character_),
         cog_2cat_num = case_when((cogfunction=="normal") ~ 0,
                                  (cogfunction %in% c("cind", "demented")) ~ 1,
                                  TRUE ~ NA_real_)) %>% 
  mutate(edu = ifelse(edu_yrs>=12, "hs or more", "less than hs"),
         edu = fct_relevel(edu, "hs or more", "less than hs")) %>% 
  mutate(alc_daily_avg_logc1 = log(alc_daily_avg+1),
         income_hh_logc1 = log(income_hh+1)) %>% 
  mutate(cesd_3cat = case_when((cesd==0)      ~ "no symptoms",
                               (cesd%in% 1:2) ~ "1-2 symptoms",
                               (cesd>=3)      ~ "3+ symptoms",
                               TRUE ~ NA_character_),
         cesd_3cat = fct_relevel(as.factor(cesd_3cat), 
                                 "no symptoms",
                                 "1-2 symptoms",
                                 "3+ symptoms")) %>% 
  filter(!study=="LBB") #removed 16,940 rows (5%), 324,559 rows remaining

#==============================================================================
# Create analytic samples
#=============================================================================#

#create analytic sample
hrs_full <- hrs_recodes %>% 
  select(hhidpn, year, firstiw, age, 
         dod_yr,
         race_ethn, sex, study, birthyr,
         cog_2cat, cog_2cat_num,
         apoe_info99_4ct,
         incar_ever, incar_time_3cat,
         alc_daily_avg, alc_daily_avg_logc1,
         bmi_combo,
         cesd_3cat,
         diab,
         edu,
         hear_sr_2cat,
         hibp,
         income_hh_logc1,
         actx_lt_fct,
         smoke_stat,
         social_origins,
         stroke_ever,
         tbi_ever
  ) %>%
  drop_na(-dod_yr) #removed 248,975 rows (77%), 75,584 rows remaining

test <- hrs_recodes %>% 
  filter(study=="HRS") 


# #check case count
# hrs_full %>% count(cases = n_distinct(hhidpn))
# # cases     n
# #  6872 42661


rio::export(hrs_full, "hrs_full_analytic.rds")

#clean up environment
rm(list=ls())

