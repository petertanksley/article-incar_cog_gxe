source("0_packages.R")

hrs_merged <- import("hrs_merged.rds")

#create analytic sample
hrs <- hrs_merged %>% 
  filter(as_numeric(dod_yr) >= 2012 | is.na(dod_yr)) %>% #remove cases deceased prior to 2012
  select(hhidpn, 
         study, race_ethn, sex, birthyr, year,
         cogfunction,
         ad_pgs, starts_with("pc"),
         incar_ever, incar_time_3cat,
         stroke_ever,
         apoe_info99_4ct,
         social_origins,
         ses) %>% 
  drop_na()

# #check case count
# hrs %>% count(cases = n_distinct(hhidpn))
# # cases     n
# #  4392 34492

#=Recodes======================================================================
# age, two category version of cogfunction 

hrs_recodes <- hrs %>% 
  mutate(age = year-birthyr) %>% 
  mutate(cog_2cat = case_when((cogfunction=="normal") ~ 0,
                              (cogfunction %in% c("cind", "demented")) ~ 1,
                              # (cogfunction=="demented") ~ "demented",
                              TRUE ~ NA_real_)) %>% 
  drop_na()
  
pgs_resid <- lm(ad_pgs ~ pc1_5a + pc1_5b + pc1_5c + pc1_5d + pc1_5e + 
                  pc6_10a + pc6_10b + pc6_10c + pc6_10d + pc6_10e, data = hrs_recodes) %>% 
  augment() %>% 
  select(ad_pgs_resid = .std.resid)

hrs_final <- bind_cols(hrs_recodes, pgs_resid) %>% 
  select(-starts_with("pc"))


# #check case count
# hrs_recodes %>% count(cases = n_distinct(hhidpn))
# cases     n
#  4392 34492

#export analytic dataframe
export(hrs_final, "hrs_analytic.rds")

