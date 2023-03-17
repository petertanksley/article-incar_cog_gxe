source("0_packages.R")

hrs <- import("hrs_analytic.rds")

main_effects <- formula("as.numeric(cog_2cat) ~ incar_ever + scale(ad_pgs) + 
                   sex + age + social_origins + stroke_ever + apoe_info99_4ct + 
                   pc1_5a + pc1_5b + pc1_5c + pc1_5d + pc1_5e + 
                   pc6_10a + pc6_10b + pc6_10c + pc6_10d + pc6_10e + 
                   (1|hhidpn)")

interaction <- formula("as.numeric(cog_2cat) ~ incar_ever*scale(ad_pgs) +
                   sex + age + social_origins + stroke_ever + apoe_info99_4ct +
                   pc1_5a + pc1_5b + pc1_5c + pc1_5d + pc1_5e + 
                   pc6_10a + pc6_10b + pc6_10c + pc6_10d + pc6_10e + 
                   (1|hhidpn)")

m1 <- glmer(main_effects, data = hrs, family = poisson(link="log"))
m2 <- glmer(interaction, data = hrs, family = poisson(link="log"))

tidy(m1) %>% view()
tidy(m2) %>% view()
