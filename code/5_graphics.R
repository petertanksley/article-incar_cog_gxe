source("0_packages.R")

hrs <- import("hrs_analytic.rds")

theme_set(theme_classic())

#age x incarceration

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


#cognitive function by year, by cohort
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



