source("0_packages.R")

#=import cleaned HRS data======================================================
#path to cleaned data directory
hrs_clean_dir <- "../../../../Desktop/Data/HRS/cleaned_variables/"

#DV: Cognitive status
cog_stat <- import(glue("{hrs_clean_dir}dementia_status/dementia_status_clean.rds"))

#IVs: incarceration history; AD pgs (EUR/AFR) & genetic ancestry PCs
incar   <- import(glue("{hrs_clean_dir}incarceration/incarceration_cleaned.rds")) 

#CVs: demographics; APOE status; history of stroke; social origins; educational attainment; smoking status
#minimal set
demo     <- import(glue("{hrs_clean_dir}demographics/demo_cleaned.rds"))           
apoe     <- import(glue("{hrs_clean_dir}serotonin_apoe/serotonin_apoe_clean.rds")) %>% select(hhidpn, apoe_info99_4ct)
#full set
alc      <- import(glue("{hrs_clean_dir}/alcohol/alcohol_clean.rds")) %>% select(hhidpn, year, alc_daily_avg)
bmi      <- import(glue("{hrs_clean_dir}/bmi/bmi_clean.rds")) %>% select(hhidpn, year, bmi_combo)
dep      <- import(glue("{hrs_clean_dir}/depression/depression_clean.rds"))
diab     <- import(glue("{hrs_clean_dir}/diabetes/diabetes_clean.rds"))
edu      <- import(glue("{hrs_clean_dir}education/edu_tracker_clean.rds"))
hear     <- import(glue("{hrs_clean_dir}/hearing/hearing_clean.rds")) %>% select(-hear_sr) %>% distinct()
hyper    <- import(glue("{hrs_clean_dir}/hypertension/hypertension_clean.rds"))
income   <- import(glue("{hrs_clean_dir}income/income_clean.rds"))
phys_act <- import(glue("{hrs_clean_dir}/physical_activity/physical_activity_clean.rds")) %>% select(hhidpn, year, actx_lt_fct)
smoke    <- import(glue("{hrs_clean_dir}/smoke/smoke_clean.rds")) %>% select(hhidpn, smoke_first_iw) %>% distinct()
soc_iso  <- import(glue("{hrs_clean_dir}social_isolation/social_iso_clean.rds")) %>% select(hhidpn, year, soc_iso_index_pro_intr) %>% distinct()
soc_orig <- import(glue("{hrs_clean_dir}social_origins/social_origin_clean.rds")) %>% select(hhidpn, social_origins) 
stroke   <- import(glue("{hrs_clean_dir}stroke/stroke_clean.rds"))   
tbi      <- import(glue("{hrs_clean_dir}/traumatic_brain_injury/tbi_clean.rds"))
#death
death <- import(glue("{hrs_clean_dir}/death/death_clean.rds")) %>% select(hhidpn, year, dod_yr)

#=merge cleaned HRS data=======================================================

# convert all year columns to factor
year_num <- function(x) {
  x %>%
    clean_names() %>% 
    mutate(year = as_numeric(year))
}

# convert all hhidpn columns to factor
id_fct <- function(x) {
  x %>% 
    clean_names() %>% 
    mutate(hhidpn = as_factor(hhidpn))
}

#longitudinal dataframes: convert year to numeric 
dfs_years <- list(cog_stat, death,
                  alc, bmi, dep, diab, hear, hyper, 
                  income, phys_act, 
                  soc_iso,
                  stroke) %>% 
  lapply(., year_num)

#all dataframes: convert id for factor
dfs_ids <- c(dfs_years, list(incar, demo, apoe, soc_orig, edu, smoke, tbi
                             )) %>% 
  lapply(., id_fct)

#clear up memory
rm(list = setdiff(ls(), "dfs_ids"))

#merge all dataframes
hrs_merged <- reduce(dfs_ids, full_join)
rm(dfs_ids)

#=Select study variables; drop observations who died before interview

#select study variables
hrs_merged_studyvars <- hrs_merged %>% 
  select(hhidpn, year, firstiw,
         dod_yr,
         race_ethn, sex, study, birthyr,
         cogfunction,
         apoe_info99_4ct,
         incar_ever, incar_time_3cat,
         alc_daily_avg,
         bmi_combo,
         cesd,
         diab,
         edu_yrs,
         hear_sr_2cat,
         hibp,
         income_hh,
         actx_lt_fct,
         smoke_first_iw,
         soc_iso_index_pro_intr,
         social_origins,
         stroke_ever,
         tbi_ever
         ) %>% 
  # filter(race_ethn %in% c("White", "Black")) %>% 
  filter(firstiw<=year) %>% #removed 255,512 rows (32%), 548,480 rows remaining
  filter(as_numeric(dod_yr)>=year | is.na(dod_yr)) %>% #removed 118,886 rows (22%), 429,594 rows remaining
  filter(as_numeric(dod_yr)>2012 | is.na(dod_yr))  #rremoved 88,183 rows (21%), 341,411 rows remaining
  # select(-firstiw)


#export final merged dataframe
export(hrs_merged_studyvars, "hrs_merged.rds")
rm(list=ls())

#=END==========================================================================

