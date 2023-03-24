source("0_packages.R")

hrs <- import("hrs_analytic.rds")

theme_set(theme_classic())

#age x incarceration

age_mean <- mean(hrs$age)

ggplot(hrs, aes(incar_ever, y=age, col=incar_ever)) +
  geom_point(position = position_jitternormal(sd_x = .05), alpha=.25, size=1.5) +
  geom_hline(yintercept = age_mean, linetype="dashed") +
  geom_violin(alpha=.15, fill="grey", color="black", linewidth=.75) +
  geom_boxplot(width=.25, color="black", size=.5, outlier.colour = NA) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  labs(x="",
       y="Years of Age") +
  guides(color="none") +
  scale_color_manual(values = c("grey", "red"))

#cognitive function X year

hrs %>% 
  group_by(year) %>% 
  summarise(tot = n(),
            count=sum(cog_2cat_num),
            age_mean = mean(age)) %>% 
  mutate(impaired_pct = (count/tot)) %>% 
  ggplot(aes(year, 1)) +
  geom_moon(aes(ratio=1, size=age_mean), key_glyph = draw_key_full_moon, fill="grey")+
  geom_moon(aes(ratio=impaired_pct, size=age_mean), color="red", key_glyph = draw_key_full_moon,right=TRUE)

risk <- glmer(cog_2cat_num ~ social_origins + sex + stroke_ever + apoe_info99_4ct + (1|hhidpn), 
              data = hrs,
              family = binomial(link = "logit")) 

hrs %>% 
  augment(risk)
  arrange(hhidpn, year, cog_2cat_num) %>% 
  ggplot(aes(year, hhidpn, fill=cog_2cat_num)) +
  geom_tile()


p_load(ggalluvial
       )
