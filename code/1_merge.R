source("0_packages.R")

#=import cleaned HRS data======================================================
#path to cleaned data directory
hrs_clean_dir <- "../../../../Desktop/Data/HRS/cleaned_variables/"

#DV: Cognitive status
cog_stat <- import(glue("{hrs_clean_dir}dementia_status/dementia_status_clean.rds"))

#IVs: incarceration history; AD pgs (EUR/AFR) & genetic ancestry PCs
incar   <- import(glue("{hrs_clean_dir}incarceration/incarceration_cleaned.rds")) 
# pgs_eur <- import(glue("{hrs_clean_dir}polygenic_scores/pgs_v4_eur_clean.rds")) %>% 
#   select(hhidpn, "ad_pgs" = E4_01AD2NA_IGAP19, starts_with(c("PC1_5", "PC6_10")))
# pgs_afr <- import(glue("{hrs_clean_dir}polygenic_scores/pgs_v4_afr_clean.rds")) %>% 
#   select(hhidpn, "ad_pgs" = A4_01AD2NA_IGAP19, starts_with(c("PC1_5", "PC6_10")))
# pgs <- bind_rows(pgs_eur, pgs_afr)
# rm(pgs_eur, pgs_afr)

#CVs: demographics; APOE status; history of stroke; social origins; educational attainment; smoking status
#minimal set
demo     <- import(glue("{hrs_clean_dir}demographics/demo_cleaned.rds"))           
apoe     <- import(glue("{hrs_clean_dir}serotonin_apoe/serotonin_apoe_clean.rds")) %>% select(hhidpn, apoe_info99_4ct)
stroke   <- import(glue("{hrs_clean_dir}stroke/stroke_clean.rds"))   
#full set
alc      <- import(glue("{hrs_clean_dir}/alcohol/alcohol_clean.rds")) %>% select(hhidpn, year, alc_daily_avg)
bmi      <- import(glue("{hrs_clean_dir}/bmi/bmi_clean.rds")) %>% select(hhidpn, year, bmi_combo)
dep      <- import(glue("{hrs_clean_dir}/depression/depression_clean.rds"))
diab     <- import(glue("{hrs_clean_dir}/diabetes/diabetes_clean.rds"))
edu      <- import(glue("{hrs_clean_dir}education/edu_tracker_clean.rds"))
hear     <- import(glue("{hrs_clean_dir}/hearing/hearing_clean.rds")) %>% select(-hear_sr_2cat)
hyper    <- import(glue("{hrs_clean_dir}/hypertension/hypertension_clean.rds"))
income   <- import(glue("{hrs_clean_dir}income/income_clean.rds"))
phys_act <- import(glue("{hrs_clean_dir}/physical_activity/physical_activity_clean.rds")) %>% select(hhidpn, year, actx_lt_fct)
smoke    <- import(glue("{hrs_clean_dir}/smoke/smoke_clean.rds")) %>% select(-smoke_ever)
soc_orig <- import(glue("{hrs_clean_dir}social_origins/social_origin_clean.rds")) %>% select(hhidpn, social_origins)
tbi      <- import(glue("{hrs_clean_dir}/traumatic_brain_injury/tbi_clean.rds"))
#death
death <- import(glue("{hrs_clean_dir}/death/death_clean.rds")) %>% select(hhidpn, dod_yr)

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

#longitudinal dataframes: convert year to numeric ##### NOT WORKING ############
dfs_years <- list(cog_stat, death,
                  alc, bmi, dep, diab, hear, hyper, 
                  income, phys_act, stroke) %>% 
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
         edu_degree,
         hear_sr,
         hibp,
         income_hh,
         actx_lt_fct,
         smoke_stat,
         social_origins,
         stroke_ever,
         tbi_ever
         ) %>% 
  # filter(race_ethn %in% c("White", "Black")) %>% 
  filter(firstiw<=year) %>% #removed 446,585 rows (45%), 548,568 rows remaining
  filter(as_numeric(dod_yr)>=year | is.na(dod_yr)) %>% #removed 118,886 rows (22%), 429,682 rows remaining
  filter(as_numeric(dod_yr)>2012 | is.na(dod_yr))  #removed 88,183 rows (21%), 341,499 rows remaining
  # select(-firstiw)
#drop rows with missing on all columns (ignore time-stable variables)
var_list <- c("cogfunction",
              "incar_ever", "incar_time_3cat",
              "stroke_ever", "smoke_stat",
              "apoe_info99_4ct")

hrs_merged_studyvars_sparse <- hrs_merged_studyvars %>% 
  filter(!if_all(all_of(var_list), is.na)) #removed 59,902 rows (18%), 281,597 rows remaining

#export final merged dataframe
export(hrs_merged_studyvars_sparse, "hrs_merged.rds")
rm(list=ls())

#=END==========================================================================

