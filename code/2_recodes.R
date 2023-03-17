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
  mutate(cog_2cat = case_when((cogfunction=="normal") ~ "normal",
                              (cogfunction %in% c("cind", "demented")) ~ "impaired",
                              # (cogfunction=="demented") ~ "demented",
                              TRUE ~ NA_character_),
         cog_2cat = fct_relevel(cog_2cat, "normal", "impaired")) %>% 
  drop_na()

# #check case count
# hrs_recodes %>% count(cases = n_distinct(hhidpn))
# cases     n
#  4392 34492

#export analytic dataframe
export(hrs_recodes, "hrs_analytic.rds")

