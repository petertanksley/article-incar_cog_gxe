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
  filter(!study %in% c("HRS", "LBB"))

#==============================================================================
# Create analytic samples: with PGS (W/B), without PGS (ALL)
#=============================================================================#

#analytic sample with PGS
hrs_pgs <- hrs_recodes %>% 
  select(hhidpn,
         study, race_ethn, sex, birthyr, year, age,
         starts_with("cog_2cat"),cogfunction,
         ad_pgs, starts_with("pc"),
         incar_ever, incar_time_3cat,
         stroke_ever,
         apoe_info99_4ct,
         social_origins,
         # edu_yrs
  ) %>%
  drop_na() #removed 80,293 rows (66%), 41,364 rows remaining


# #check case count
# hrs_pgs %>% count(cases = n_distinct(hhidpn))
# # cases     n
# #  5502 41364

#create analytic sample
hrs_full <- hrs_recodes %>% 
  select(hhidpn,
         study, race_ethn, sex, birthyr, year, age,
         starts_with("cog_2cat"), cogfunction,
         # ad_pgs, starts_with("pc"),
         incar_ever, incar_time_3cat,
         stroke_ever,
         apoe_info99_4ct,
         social_origins,
         # edu_yrs
  ) %>%
  drop_na() #removed 70,891 rows (58%), 50,766 rows remaining


# #check case count
# hrs %>% count(cases = n_distinct(hhidpn))
# # cases     n
# #  6998 50776

#=PGS - Recodes======================================================================
# residualize for PCs 1-10 BY RACE

hrs_pgs_resid <- hrs_pgs %>% 
  group_by(race_ethn) %>% 
  mutate(ad_pgs_resid = residuals(lm(ad_pgs ~ pc1_5a + pc1_5b + pc1_5c + pc1_5d + pc1_5e + 
                                       pc6_10a + pc6_10b + pc6_10c + pc6_10d + pc6_10e))) %>% 
  ungroup() %>% 
  select(-starts_with("pc"))

#export analytic dataframes
export(hrs_pgs_resid, "hrs_pgs_analytic.rds")
export(hrs_full, "hrs_full_analytic.rds")

