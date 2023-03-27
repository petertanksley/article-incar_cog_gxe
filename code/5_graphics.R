source("0_packages.R")

hrs <- import("hrs_analytic.rds")

theme_set(theme_classic())

#age x incarceration

age_mean <- mean(hrs$age)

ggplot(hrs, aes(incar_ever, y=age, col=incar_ever)) +
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


#cognitive function by year, by cohort
hrs %>% 
  group_by(year, study, apoe_info99_4ct
  ) %>% 
  summarise(tot=n(),
            count=sum(cog_2cat_num)) %>%
  mutate(prop = count/tot) %>% 
  mutate(apoe_info99_4ct = fct_recode(apoe_info99_4ct,
                                      "Zero copies" = "zero copies",
                                      "One copy" = "one copy",
                                      "Two copies" = "two copies")) %>% 
  ggplot(aes(year, apoe_info99_4ct)) +
  geom_density_ridges2(aes(fill=study, height=prop),
                       stat = "identity", 
                       # bandwidth=0.1,
                       scale=1,
                       alpha=.5) +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = .25)) +
  scale_fill_viridis_d(name="HRS Cohort",
                       option = "E") +
  scale_x_continuous(breaks = seq(1998, 2018, 4)) +
  coord_cartesian(clip = "off", expand = 0)




hrs %>% 
  group_by(year, study) %>% 
  summarize(tot=n(),
         count=sum(cog_2cat_num)) %>%
  mutate(prop = count/tot) %>% 
  mutate(apoe_info99_4ct = fct_recode(apoe_info99_4ct,
                                      "Zero copies" = "zero copies",
                                      "One copy" = "one copy",
                                      "Two copies" = "two copies")) %>% 
  ggplot(aes(year, study)) +
  geom_density_ridges2(aes(height=prop),
                       stat = "identity", 
                       # bandwidth=0.1,
                       scale=1,
                       alpha=.5) +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = .25)) +
  # scale_fill_viridis_d(name="HRS Cohort",
  #                      option = "E") +
  scale_x_continuous(breaks = seq(1998, 2018, 4)) +
  coord_cartesian(clip = "off", expand = 0) +
  facet_grid(~apoe_info99_4ct)
