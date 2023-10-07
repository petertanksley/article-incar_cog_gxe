source("0_packages.R")

hrs_merged <- import("hrs_merged_recodes.rds")
hrs_final_ids <- import("hrs_full_analytic.rds") %>% 
  select(hhidpn, year) %>% 
  mutate(analytic =1)

collapse_years <- tibble(year=1992:2021,
                         year_2=rep(seq(1992, 2020, by=2), each=2))

#set ggplot theme
theme_set(theme_classic2())

#=Recodes======================================================================
# age, two category version of cogfunction, drop HRS/LBB cohorts (too few) 
#=============================================================================#

hrs_miss <- hrs_merged %>% 
  left_join(hrs_final_ids, by=c("hhidpn", "year")) %>% 
  left_join(collapse_years, by="year") %>% 
  mutate(year = year_2) %>% 
  mutate(analytic = ifelse(is.na(analytic), 0, analytic)) %>% 
  mutate(incar_miss = ifelse(is.na(incar_ever), "Missing", "Not Missing"))

#select only time-independent variables with missingness (and analytic sample ID)
hrs_miss_ind <- hrs_miss %>% 
  select(hhidpn, year,# firstiw,
         age, 
         dod_yr,
         race_ethn, sex, study, birthyr,
         # cog_2cat, cog_2cat_num,
         apoe_info99_4ct,
         incar_ever, incar_time_3cat, 
         # alc_daily_avg, alc_daily_avg_logc1,
         # bmi_combo,
         # cesd_3cat,
         # diab,
         # edu,
         # hear_sr_2cat,
         # hibp,
         # income_hh_logc1, income_hh_10k,
         # actx_lt_fct,
         smoke_first_iw,
         # soc_iso_index_pro_intr,
         social_origins,
         # stroke_ever,
         tbi_ever,
         
         analytic, incar_miss
         ) %>% 
  # group_by(hhidpn, year) %>% 
  arrange(desc(analytic)) %>%
  distinct(hhidpn, .keep_all=TRUE) #removed 287,053 rows (88%), 37,506 rows remaining

#NOTE: I had trouble getting the number of analytic cases to match what was in 
#the full analytic sample. This happened because when I called "distinct" it
#was selecting the first observation of a case, not necessarily the observation
#that was included in the analytic sample. Using "arrange" solved this by placing
#analytic cases first.



table(hrs_miss$analytic) #should match observations (73511)
# 0      1 
# 251048  73511 

table(hrs_miss_ind$analytic) #should match cases (11268)
# 0     1 
# 26238 11268 


#select only time-independent variables with missingness (and analytic sample ID)
hrs_miss_dep <- hrs_miss %>% 
  unite("incar_miss.year", incar_miss, year) %>% 
  select(hhidpn, #year, firstiw,
         # age, 
         # dod_yr,
         # race_ethn, sex, study, birthyr,
         cog_2cat, #cog_2cat_num,
         # apoe_info99_4ct,
         # incar_ever, incar_time_3cat, 
         alc_daily_avg, #alc_daily_avg_logc1,
         bmi_combo,
         cesd_3cat,
         diab,
         # edu,
         hear_sr_2cat,
         hibp,
         income_hh_logc1, #income_hh_10k,
         actx_lt_fct,
         # smoke_first_iw,
         soc_iso_index_pro_intr,
         # social_origins,
         stroke_ever,
         # tbi_ever
         
         analytic, #incar_miss
         incar_miss.year
  ) %>% 
  filter(!if_all(c(cog_2cat,
                   alc_daily_avg, 
                   bmi_combo,
                   cesd_3cat,
                   diab,
                   hear_sr_2cat,
                   hibp,
                   income_hh_logc1, 
                   actx_lt_fct,
                   soc_iso_index_pro_intr,
                   stroke_ever),
                 is.na)) #removed 89,273 rows (28%), 235,286 rows remaining

#=====Visualize================================================================

#time-independent variables
incar_miss_ind <- gg_miss_var(hrs_miss_ind, facet=incar_miss, show_pct = )[["data"]] 

miss_constant <- incar_miss_ind %>% 
  mutate(variable = case_when((variable=="incar_time_3cat") ~ "Lifetime incarceration\nduration",
                              (variable=="apoe_info99_4ct") ~ "APOE-e4",
                              (variable=="tbi_ever")        ~ "TBI",
                              (variable=="social_origins")  ~ "Childhood financial\ndifficulty",
                              (variable=="smoke_first_iw")  ~ "Smoking status",
                              TRUE ~ NA_character_
                              )) %>% 
  drop_na() %>% 
  mutate(variable_sort = fct_reorder(as.factor(variable), n_miss)) %>% 
  filter(n_miss>80) %>% 
  filter(pct_miss<100) %>% 
  filter(!variable %in% c("incar_ever", "dod_yr")) %>%
  ggplot(aes(pct_miss, variable_sort, col=incar_miss)) +
  geom_line(aes(group=variable), color="black") +
  geom_point(aes(shape=incar_miss, size=incar_miss), alpha=.75) +
  theme(panel.grid.major.x = element_line(linetype = "dotted", color="grey")) +
  labs(x="Percent (%) missing",
       y="") +
  scale_color_manual(name="Incarceration Data",
                     values=c("red", "blue")) +
  scale_shape_manual(name="Incarceration Data",
                     values = c(15, 18)) +
  scale_size_manual(name="Incarceration Data",
                    values = c(2.5, 4))
# miss_constant

#time-dependent variables
incar_miss_dep <- gg_miss_var(hrs_miss_dep, facet=incar_miss.year, show_pct = T)[["data"]] %>% 
  separate_wider_delim(incar_miss.year, names=c("incar_miss", "year"), delim="_") 

miss_by_year <- incar_miss_dep %>% 
  
  filter(year>=2004) %>% 
  filter(!grepl("incar|dod|hhidpn|analytic", variable)) %>% 
  mutate(variable = case_when((variable=="cog_2cat")               ~ "Cognitive impairment",
                              (variable=="alc_daily_avg")          ~ "Daily alcohol (avg.)",
                              (variable=="bmi_combo")              ~ "BMI",
                              (variable=="cesd_3cat")              ~ "Depressive symptoms",
                              (variable=="diab")                   ~ "Diabetes",
                              (variable=="hear_sr_2cat")           ~ "Hearing difficulty",
                              (variable=="hibp")                   ~ "Hypertension",
                              (variable=="income_hh_logc1")        ~ "Household income",
                              (variable=="actx_lt_fct")            ~ "Physical activity (light)",
                              (variable=="soc_iso_index_pro_intr") ~ "Social isolation index",
                              (variable=="stroke_ever")            ~ "Stroke status",
                              TRUE ~ NA_character_
                              )) %>%
  mutate(variable_sort = fct_reorder(as.factor(variable), pct_miss)) %>% 
  ggplot(aes(pct_miss, variable_sort, col=incar_miss)) +
  geom_line(aes(group=variable), color="black") +
  geom_point(aes(shape=incar_miss, size=incar_miss), alpha=.75) +
  theme(strip.background = element_rect(fill = "black", color = "black"),
        strip.text = element_text(colour = "white", face = "bold", size=14),
        panel.grid.major.x = element_line(linetype = "dotted", color="grey")) +
  labs(x="Percent (%) missing",
       y="") +
  facet_wrap(~year) +
  scale_color_manual(name="Incarceration Data",
                     values=c("red", "blue")) +
  scale_shape_manual(name="Incarceration Data",
                     values = c(15, 18)) +
  scale_size_manual(name="Incarceration Data",
                    values = c(2.5, 4))



# miss_by_year

design <- "
AACC##
AACC##
BBBBBB
BBBBBB
BBBBBB
BBBBBB
BBBBBB
BBBBBB
"

miss_plot <- miss_constant + miss_by_year + guide_area() +
  plot_layout(design = design, guides = "collect", tag_level = "new") +
  plot_annotation(tag_levels = "A",
                  tag_suffix = ".") &
  theme(axis.text = element_text(size = 12),
        legend.background = element_rect(fill = "grey90", colour = "black"),
        plot.tag = element_text(face = "bold", size = 16))

# miss_plot

ggsave("../output/figures/miss_plot_by_incar.tiff", height = 10, width = 8, dpi = 700)

  