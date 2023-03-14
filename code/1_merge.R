source("0_packages.R")

#=import cleaned HRS data======================================================
#path to cleaned data directory
hrs_clean_dir <- "../../../../Desktop/Data/HRS/cleaned_variables/"

#DV: Cognitive status
cog_stat <- import(glue("{hrs_clean_dir}dementia_status/dementia_status_clean.rds"))

#IVs: incarceration history; AD pgs (EUR/AFR) & genetic ancestry PCs
incar   <- import(glue("{hrs_clean_dir}incarceration/incarceration_cleaned.rds")) 
pgs     <- import(glue("{hrs_clean_dir}polygenic_scores/pgs_v4_eur_clean.rds"))   
# pgs_afr <- import(glue("{hrs_clean_dir}polygenic_scores/pgs_v4_afr_clean.rds"))   

#CVs: demographics; APOE status; history of stroke; social origins; educational attainment; income
demo     <- import(glue("{hrs_clean_dir}demographics/demo_cleaned.rds"))           
apoe     <- import(glue("{hrs_clean_dir}serotonin_apoe/serotonin_apoe_clean.rds")) 
stroke   <- import(glue("{hrs_clean_dir}stroke/stroke_clean.rds"))                 
soc_orig <- import(glue("{hrs_clean_dir}social_origins/social_origin_clean.rds"))  
edu      <- import(glue("{hrs_clean_dir}education/edu_clean.rds"))                 
income   <- import(glue("{hrs_clean_dir}income/income_clean.rds"))                 

#=merge cleaned HRS data=======================================================

# convert all year columns to factor
year_fct <- function(x) {
  x %>%
    clean_names() %>% 
    mutate(year = fct_reorder(as_factor(year), year))
}

# convert all hhidpn columns to factor
id_fct <- function(x) {
  x %>% 
    clean_names() %>% 
    mutate(hhidpn = as_factor(hhidpn))
}

#longitudinal dataframes: convert year to factor
dfs_years <- list(cog_stat, stroke, edu, income) %>% 
  lapply(., year_fct)

#all dataframes: convert id for factor
dfs_ids <- c(dfs_years, list(incar, pgs, demo, apoe, soc_orig)) %>% 
  lapply(., id_fct)

#clear up memory
rm(list = setdiff(ls(), "dfs_ids"))

#merge all dataframes
hrs_merged <- reduce(dfs_ids, full_join)
rm(dfs_ids)

#=trim dataframe; drop rows will only missing data; drop unneeded variables====

#drop rows with missing on all columns (ignore time-stable variables)
var_list <- setdiff(names(hrs_merged), c("hhidpn", "year", "race_ethn", "sex", "study", "birthyr",
                                         "social_origins", "so_faunem", "so_movfin", "so_fmfinh","so_famfin"))
hrs_merged_sparse <- hrs_merged %>% 
  filter(!if_all(var_list, is.na)) #removed 146,196 rows (19%), 636,564 rows remaining

#select study variables
hrs_merged_sparse_studyvars <- hrs_merged_sparse %>% 
  select(hhidpn, year,
         race_ethn, sex, study, birthyr,
         "ad_pgs" = e4_01ad2na_igap19, starts_with(c("pc1_5", "pc6_10")),
         starts_with("incar"),
         starts_with("strok"),
         starts_with("apoe"),
         social_origins,
         edu_yrs,
         income_hh)

#export final merged dataframe
export(hrs_merged_sparse_studyvars, "hrs_merged.rds")
rm(list=ls())

#=END==========================================================================

