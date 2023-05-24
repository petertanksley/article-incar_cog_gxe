source("0_packages.R")

hrs <- import("hrs_full_analytic.rds")

theme_set(theme_classic())

#=venn: race, sex, incar, apoe=================================================

p_load(ggalluvial)

hrs %>% 
  distinct(hhidpn, .keep_all=TRUE) %>% 
  # arrange(incar_ever) %>% 
  ggplot(aes(axis4=race_ethn, axis3=fct_rev(sex), axis1=fct_rev(incar_ever), axis2=fct_rev(apoe_info99_4ct))) +
  geom_alluvium(aes(fill=incar_ever, alpha=incar_ever)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme(legend.position = "bottom") +
  scale_fill_manual(name = "",
                    values = c("grey90", "red")) +
  scale_alpha_manual(values = c(1, .8)) +
  scale_x_discrete(limits = c("Incarcerated", "APOE-4 Count", "Sex", "Race/Ethnicity")) +
  guides(alpha="none")
  # coord_polar()

#=age x incarceration==========================================================

age_mean <- mean(hrs$age)

incar_age <- ggplot(hrs, aes(incar_ever, y=age, col=incar_ever)) +
  geom_point(position = position_jitternormal(sd_x = .05, seed = 973101), alpha=.25, size=1.5) +
  geom_hline(yintercept = age_mean, linetype="dashed") +
  geom_violin(alpha=.15, fill="grey", color="black", linewidth=.75) +
  geom_boxplot(width=.25, color="black", size=.5, outlier.colour = NA) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.ticks.x = element_blank()) +
  labs(x="",
       y="Years of Age") +
  guides(color="none") +
  scale_color_manual(values = c("grey", "red"))

ggsave("../output/figures/1_incar_age.png", height = 5, width = 7)


#=cognitive function by year, by cohort========================================
cog_apoe4 <- hrs %>% 
  mutate(study = fct_recode(study,
                            "AHEAD\n<1924" = "AHEAD",
                           "CODA\n1924-30" = "CODA",
                            "HRS\n1931-41" = "HRS",
                           "WB\n1942-47"   = "WB",
                           "EBB\n1948-53"  = "EBB",
                           "MBB\n1954-59"  = "MBB")) %>% 
  group_by(year, study, apoe_info99_4ct
  ) %>% 
  summarise(tot=n(),
            count=sum(cog_2cat_num)) %>%
  mutate(prop = count/tot) %>% 
  mutate(apoe_info99_4ct = fct_recode(apoe_info99_4ct,
                                      "Zero copies" = "zero copies",
                                      "One copy" = "one copy",
                                      "Two copies" = "two copies")) %>% 
  ggplot(aes(year, prop, fill=fct_rev(apoe_info99_4ct))) +
  stat_smooth(
    geom = 'area', method = 'loess', span = 1/3,
    alpha = .75) +
  theme(legend.position = c(.15,.2),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text.y.right = element_text(size = 14, angle = 0),
        legend.background = element_rect(fill = "grey95", color = "grey20"),
        panel.spacing.y = unit(2, "lines")) +
  labs(title = 'Percent of APEO-4 allele carriers with "impaired" congnitive status by study year and HRS cohort',
       x="Study year",
       y='Percent cognitively impaired') +
  scale_x_continuous(breaks = seq(1998, 2018, 4)) +
  scale_y_continuous(breaks = c(0, .5, 1),
                     labels = scales::percent_format(),
                     limits = c(0,1)) +
  scale_fill_viridis_d(name="APOE-4 allele count",
                    option = "A") +
  coord_cartesian(clip = "off", expand = 0) +
  facet_grid(study~.) 
  
cog_apoe4

ggsave("../output/figures/2_cogstat_apoe4.png", height = 7, width = 10)


#=contingency tables===========================================================

p_load(ggvenn,
       magick,
       plotly,
       vtree,
       webshot2)

# strat_race <- table(#hrs$cog_2cat,
#                     hrs$incar_ever,
#                     hrs$apoe_info99_4ct,
#                     hrs$race_ethn) %>% 
#   as.data.frame()
# 
# 
# strat_sex <- table(#hrs$cog_2cat,
#                    hrs$incar_ever,
#                    hrs$apoe_info99_4ct,
#                    hrs$sex) %>% 
#   as.data.frame()
# 
# strat_edu <- table(#hrs$cog_2cat,
#                    hrs$incar_ever,
#                    hrs$apoe_info99_4ct,
#                    hrs$edu) %>% 
#   as.data.frame() 

strat_none <- vtree(hrs, vars = "incar_ever apoe_info99_4ct",
      sameline = TRUE, showlegend = TRUE,
      labelvar = c(#cog_2cat="Cognitive Status",
                   incar_ever="Lifetime\nincarceration",
                   apoe_info99_4ct="APOE-4 genotype"))
strat_race <- vtree(hrs, vars = "incar_ever  apoe_info99_4ct race_ethn",
      sameline = TRUE, showlegend = TRUE,
      labelvar = c(#cog_2cat="Cognitive Status", 
                   incar_ever="Lifetime\nincarceration",
                   apoe_info99_4ct="APOE-4 genotype",
                   race_ethn="Race/ethnicity"))
strat_sex <- vtree(hrs, vars = "cog_2cat incar_ever apoe_info99_4ct sex",
      sameline = TRUE, showlegend = TRUE,
      labelvar = c(cog_2cat="Cognitive Status", 
                   incar_ever="Lifetime\nincarceration",
                   apoe_info99_4ct="APOE-4 genotype",
                   sex="Sex"))
strat_edu <- vtree(hrs, vars = "cog_2cat incar_ever apoe_info99_4ct edu",
      sameline = TRUE, showlegend = TRUE,
      labelvar = c(cog_2cat="Cognitive Status", 
                   incar_ever="Lifetime\nincarceration",
                   apoe_info99_4ct="APOE-4 genotype",
                   edu="Educational\nattainment"))


htmlwidgets::saveWidget(strat_none, "../output/figures/3_strat_none.html") 
webshot("../output/figures/3_strat_none.html", "../output/figures/3_strat_none.png")

htmlwidgets::saveWidget(strat_race, "../output/figures/3_strat_race.html") 
webshot("../output/figures/3_strat_race.html", "../output/figures/3_strat_race.png")

htmlwidgets::saveWidget(strat_sex, "../output/figures/3_strat_sex.html") 
webshot("../output/figures/3_strat_sex.html", "../output/figures/3_strat_sex.png")

htmlwidgets::saveWidget(strat_edu, "../output/figures/3_strat_edu.html") 
webshot("../output/figures/3_strat_edu.html", "../output/figures/3_strat_edu.png")

magick::image_read("../output/figures/3_strat_none.png") %>% image_trim() %>% image_write("../output/figures/3_strat_none.png")
magick::image_read("../output/figures/3_strat_race.png") %>% image_trim() %>% image_write("../output/figures/3_strat_race.png")
magick::image_read("../output/figures/3_strat_sex.png") %>% image_trim() %>% image_write("../output/figures/3_strat_sex.png")
magick::image_read("../output/figures/3_strat_edu.png") %>% image_trim() %>% image_write("../output/figures/3_strat_edu.png")
