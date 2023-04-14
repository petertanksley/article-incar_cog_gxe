source("0_packages.R")

#=import cleaned HRS data======================================================
#path to cleaned data directory
hrs_clean_dir <- "../../../../Desktop/Data/HRS/cleaned_variables/"

#DV: Cognitive status
cog_stat <- import(glue("{hrs_clean_dir}dementia_status/dementia_status_clean.rds"))

#IVs: incarceration history; AD pgs (EUR/AFR) & genetic ancestry PCs
incar   <- import(glue("{hrs_clean_dir}incarceration/incarceration_cleaned.rds")) 
pgs_eur <- import(glue("{hrs_clean_dir}polygenic_scores/pgs_v4_eur_clean.rds")) %>% 
  select(hhidpn, "ad_pgs" = E4_01AD2NA_IGAP19, starts_with(c("PC1_5", "PC6_10")))
pgs_afr <- import(glue("{hrs_clean_dir}polygenic_scores/pgs_v4_afr_clean.rds")) %>% 
  select(hhidpn, "ad_pgs" = A4_01AD2NA_IGAP19, starts_with(c("PC1_5", "PC6_10")))
pgs <- bind_rows(pgs_eur, pgs_afr)
rm(pgs_eur, pgs_afr)

#CVs: demographics; APOE status; history of stroke; social origins; educational attainment; income
demo     <- import(glue("{hrs_clean_dir}demographics/demo_cleaned.rds"))           
apoe     <- import(glue("{hrs_clean_dir}serotonin_apoe/serotonin_apoe_clean.rds")) 
stroke   <- import(glue("{hrs_clean_dir}stroke/stroke_clean.rds"))                 
soc_orig <- import(glue("{hrs_clean_dir}social_origins/social_origin_clean.rds"))  
# ses      <- import(glue("{hrs_clean_dir}ses/ses_clean.rds"))
# edu      <- import(glue("{hrs_clean_dir}education/edu_clean.rds")) %>% distinct(hhidpn, edu_yrs)           
# income   <- import(glue("{hrs_clean_dir}income/income_clean.rds"))    
death    <- import(glue("{hrs_clean_dir}death/death_clean.rds"))

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
dfs_years <- list(cog_stat, stroke, death #income, ses
                  ) %>% 
  lapply(., year_num)

#all dataframes: convert id for factor
dfs_ids <- c(dfs_years, list(incar, pgs, demo, apoe, soc_orig #edu
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
         race_ethn, sex, study, birthyr,
         dod_yr, alive,
         cogfunction,
         incar_ever, incar_time_3cat,
         stroke_ever,
         apoe_info99_4ct,
         social_origins,
         # ses,
         # edu_yrs, 
         # income_hh,
         ad_pgs, starts_with(c("pc1_5", "pc6_10"))) %>% 
  # filter(race_ethn %in% c("White", "Black")) %>% 
  filter(firstiw<=year) %>% #removed 446,112 rows (46%), 526,267 rows remaining
  filter(as_numeric(dod_yr)>=year | is.na(dod_yr)) %>% #removed 118,886 rows (23%), 407,381 rows remaining
  filter(as_numeric(dod_yr)>2012 | is.na(dod_yr)) %>% #removed 88,183 rows (22%), 319,198 rows remaining
  select(-firstiw)
#drop rows with missing on all columns (ignore time-stable variables)
var_list <- c("ad_pgs",
              "cogfunction",
              "incar_ever", "incar_time_3cat",
              "stroke_ever",
              "apoe_info99_4ct")

hrs_merged_studyvars_sparse <- hrs_merged_studyvars %>% 
  filter(!if_all(all_of(var_list), is.na)) #removed 64,324 rows (20%), 254,874 rows remaining

#export final merged dataframe
export(hrs_merged_studyvars_sparse, "hrs_merged.rds")
rm(list=ls())

#=END==========================================================================

