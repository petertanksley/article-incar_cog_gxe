source("0_packages.R")

#color palettes




#=Data set up for survival analysis==========================================


if(!file.exists("hrs_surv_ind.rds") | !file.exists("hrs_surv_dep.rds")){
  hrs_full <- import("hrs_full_analytic.rds") 
  
  #make survival model recodes (age as time-scale approach)
  hrs_surv <- hrs_full %>% 
    group_by(hhidpn) %>% 
    fill(dod_yr, .direction="updown") %>% 
    ungroup() %>% 
    mutate(study_age = year-birthyr,
           firstiw_age = firstiw-birthyr,
           dod_age = as_numeric(dod_yr)-birthyr) %>% 
    group_by(hhidpn) %>% 
    mutate(cog_first = ifelse(cog_2cat_num==1, study_age, NA),
           cog_first = pmin(cog_first)) %>%
    fill(cog_first, .direction="updown") %>% 
    mutate(cog_ever = max(cog_2cat_num),
           cog_surv_age = ifelse(cog_ever>0, cog_first, max(study_age))) %>% 
    ungroup() 
  # filter(age>=50) #removed 2,032 rows (4%), 53,313 rows remaining
  
  
  #time-independent 
  hrs_surv_ind_raw <- hrs_surv %>% 
    distinct(hhidpn, .keep_all=TRUE) %>% #removed 64,219 rows (85%), 11,365 rows remaining
    select(hhidpn, study, 
           cog_surv_age, cog_first, cog_ever, 
           dod_yr, dod_age,
           race_ethn, sex, firstiw, firstiw_age, birthyr,
           apoe_info99_4ct,
           incar_ever, incar_time_3cat,
           edu,
           social_origins,
           smoke_first_iw,
           tbi_ever
    ) %>% 
    filter(cog_surv_age>firstiw_age) #removed 221 rows (2%), 11,144 rows remaining
  
  #format time-independent variables
  hrs_surv_ind <- tmerge(data1 = hrs_surv_ind_raw,
                         data2 = hrs_surv_ind_raw ,
                         id=hhidpn,
                         event=event(cog_surv_age, cog_ever))
  
  #time-dependent
  hrs_surv_dep_raw <- hrs_surv %>% 
    distinct(hhidpn, study_age, .keep_all=TRUE) %>% 
    select(hhidpn, study_age, 
           cog_2cat, cog_2cat_num,
           alc_daily_avg_logc1,
           bmi_combo,
           cesd_3cat,
           diab,
           hear_sr_2cat,
           hibp,
           income_hh_logc1,
           actx_lt_fct,
           # soc_iso_index_pro,
           stroke_ever)
  
  #merge
  hrs_surv_dep <- tmerge(data1 = hrs_surv_ind,
                         data2 = hrs_surv_dep_raw,
                         id=hhidpn,
                         alc   =tdc(study_age, alc_daily_avg_logc1),
                         bmi   =tdc(study_age, bmi_combo),
                         cesd  =tdc(study_age, cesd_3cat),
                         diab  =tdc(study_age, diab),
                         hear  =tdc(study_age, hear_sr_2cat),
                         hibp  =tdc(study_age, hibp),
                         income=tdc(study_age, income_hh_logc1),
                         active=tdc(study_age, actx_lt_fct),
                         # soc_iso=tdc(study_age, soc_iso_index_pro),
                         stroke=tdc(study_age, stroke_ever)
  ) %>% 
    filter(tstart>0) #removed 11,144 rows (17%), 55,994 rows remaining
  
  export(hrs_surv_dep, "hrs_surv_dep.rds")
  export(hrs_surv_ind, "hrs_surv_ind.rds")
} else {
  
  hrs_surv_ind<- import("hrs_surv_ind.rds")
  hrs_surv_dep<- import("hrs_surv_dep.rds")
}

#=Fit Cox model================================================================

#covariates lists

covars_min <- paste(c("factor(race_ethn)",
                      "factor(sex)",
                      "factor(edu)",
                      "strata(study)"),
                    collapse = " + ")

covars_full <- paste(c("factor(race_ethn)",
                       "factor(sex)",
                       "factor(edu)",
                       "factor(social_origins)",
                       "factor(tbi_ever)",
                       "scale(alc)",
                       "scale(bmi)",
                       "factor(smoke_first_iw)",
                       "factor(cesd)",
                       "factor(diab)",
                       "factor(hear)",
                       "factor(hibp)",
                       "scale(income)",
                       "factor(active)",
                       # "scale(soc_iso)",
                       "factor(stroke)",
                       "strata(study)"),
                     collapse = " + ")

f11 <- formula(glue("Surv(tstart, tstop, event) ~ factor(incar_ever) + {covars_min}"))
f12 <- formula(glue("Surv(tstart, tstop, event) ~ factor(apoe_info99_4ct) + {covars_min}"))
f21 <- formula(glue("Surv(tstart, tstop, event) ~ factor(incar_ever) + factor(apoe_info99_4ct) + {covars_min}"))
f22 <- formula(glue("Surv(tstart, tstop, event) ~ factor(incar_ever) + factor(apoe_info99_4ct) + {covars_full}"))
f31 <- formula(glue("Surv(tstart, tstop, event) ~ factor(incar_ever)*factor(apoe_info99_4ct) + {covars_min}"))
f32 <- formula(glue("Surv(tstart, tstop, event) ~ factor(incar_ever)*factor(apoe_info99_4ct) + {covars_full}"))

cox11 <- coxph(f11, data = hrs_surv_dep)
cox12 <- coxph(f12, data = hrs_surv_dep)
cox21 <- coxph(f21, data = hrs_surv_dep)
cox22 <- coxph(f22, data = hrs_surv_dep)
cox31 <- coxph(f31, data = hrs_surv_dep)
cox32 <- coxph(f32, data = hrs_surv_dep)

export(c("cox11", "cox12", "cox21", "cox22", "cox31", "cox32"), "../output/results/main_results_surv_models.rdata")

res_11 <- tidy(cox11, exponentiate = TRUE) %>% mutate(model = "11")
res_12 <- tidy(cox12, exponentiate = TRUE) %>% mutate(model = "12")
res_21 <- tidy(cox21, exponentiate = TRUE) %>% mutate(model = "21")
res_22 <- tidy(cox22, exponentiate = TRUE) %>% mutate(model = "22")
res_31 <- tidy(cox31, exponentiate = TRUE) %>% mutate(model = "31")
res_32 <- tidy(cox32, exponentiate = TRUE) %>% mutate(model = "32")

cox_all_res <- bind_rows(res_11,
                         res_12,
                         res_21,
                         res_22,
                         res_31,
                         res_32)


export(cox_all_res, "../output/results/main_results_surv.csv")

#=test of the proportionality assumption=====================================

#examine Schoenfeld residuals (looking for no time trends)
#visual examination will be pursued due to size of data (too easy to reject null)
cox.zph(cox11) %>% ggcoxzph()
cox.zph(cox12) %>% ggcoxzph()
cox.zph(cox21) %>% ggcoxzph()
cox.zph(cox22) %>% ggcoxzph()
cox.zph(cox31) %>% ggcoxzph()
cox.zph(cox32) %>% ggcoxzph()

#=test-stratified models=====================================
cox21 <- coxph(f21, data = hrs_surv_dep)


#=Fit and visualize basic survival curves======================================

#INCARCERATION
surv_ind_incar <- survfit(Surv(cog_surv_age, event) ~ incar_ever, data=hrs_surv_ind)

survplot1 <- ggsurvplot(surv_ind_incar,
                        conf.int = TRUE,
                        pval = TRUE, 
                        pval.method = TRUE,
                        risk.table = "nrisk_cumevents",
                        fontsize=6,
                        cumevents = TRUE,
                        xlim=c(50,100),
                        break.x.by=10,
                        # surv.median.line = "hv",
                        censor.shape=NA,
                        palette = c("darkblue", "darkred"),
                        cumcensor = TRUE) 
# survplot1

#median survival age================================#
closest<-function(var,val){
  var[which(abs(var-val)==min(abs(var-val)))] 
}

median_surv_nojail <- survplot1$data.survplot %>% 
  filter(incar_ever=="Not Incarcerated") %>% 
  filter(surv==closest(surv, .5)) %>% 
  pull(time)

median_surv_jail <- survplot1$data.survplot %>% 
  filter(incar_ever=="Incarcerated") %>% 
  filter(surv==closest(surv, .5)) %>% 
  pull(time)
#==================================================#

#statistic annotations
logrank_incar <- survdiff(Surv(cog_surv_age, event) ~ incar_ever, data=hrs_surv_ind) %>% 
  glance() %>% 
  pull(statistic)
logrank_incar_lab <- glue("Log-rank: *&chi;*<sup>2</sup> (1)={round(logrank_incar, 2)}, *P*<0.001")

cox_incar <- res_21 %>% 
  filter(term=="factor(incar_ever)Incarcerated") %>% 
  pull(estimate)
cox_incar_lab <- glue("Cox: HR={round(cox_incar, 2)}, *P*<0.001")

plot1 <- survplot1$plot +
  labs(y="Survival probability\n(no cognitive impairment)",
       x="Age",
       tag = "B.") +
  theme(legend.title = element_text(size = 22, face = "bold"),
        legend.text = element_text(size = 22),
        legend.direction = "vertical",
        legend.position = c(.85, .85),
        legend.key.size = unit(1, "cm"),
        axis.title.x.bottom = element_text(size=22, face = "bold"),
        axis.title.y.left = element_text(size=22, face = "bold", vjust = -15),
        axis.text.y.left = element_text(size = 18),
        axis.text.x.bottom = element_text(size=18)) +
  geom_segment(aes(x=45, xend=median_surv_nojail, y=.5, yend=.5), linewidth=1, linetype="dashed") +
  geom_segment(aes(x=median_surv_nojail, xend=median_surv_nojail, y=0, yend=.5),  linewidth=1, linetype="dashed") +
  geom_segment(aes(x=median_surv_jail, xend=median_surv_jail, y=0, yend=.5),  linewidth=1, linetype="dashed") +
  scale_y_continuous(expand = c(0,0)) +
  # annotate("text", x=60, y=.25, size=7, fontface="bold", label=expression(atop(textstyle("Log-rank"), paste(italic("p"), "<0.0001")))) +
  geom_richtext(aes(x=48, y=.1, label=logrank_incar_lab), size=7, hjust=0, label.color="white") +
  geom_richtext(aes(x=48, y=.05, label=cox_incar_lab), size=7, hjust=0, label.color="white") +
  scale_fill_manual(name="Lifetime incarceration",
                    values = c("darkblue", "darkred"),
                    labels = c("Never-incarcerated", "Incarcerated")) +
  scale_color_manual(name="Lifetime incarceration",
                     values = c("darkblue", "darkred"),
                     labels = c("Never-incarcerated", "Incarcerated")) 
# plot1

tab1 <- survplot1$table +
  labs(y="",
       x="") +
  scale_y_discrete(labels=c("Incarcerated", "Never-\nincarcerated")) +
  theme(axis.text.y.left = element_text(size = 22, face = "bold", hjust = .5),
        plot.title       = element_text(size = 22, face = "bold"),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.line.y.left = element_line(colour = NA),
        panel.background = element_rect(color = "black", linewidth = 1))



survplot_incar <- plot1 / tab1 + plot_layout(heights = c(3,1)) 
# survplot_incar
ggsave("../output/figures/survplot_incar.png", survplot_incar, width = 15, height = 8)


#APOE-4
surv_ind_apoe4 <- survfit(Surv(cog_surv_age, event) ~ apoe_info99_4ct, data=hrs_surv_ind)

survplot2 <- ggsurvplot(surv_ind_apoe4,
                        conf.int = TRUE,
                        pval = TRUE, 
                        pval.method = TRUE,
                        risk.table = "nrisk_cumevents",
                        fontsize=6,
                        cumevents = TRUE,
                        xlim=c(50,100),
                        break.x.by=10,
                        # surv.median.line = "hv",
                        censor.shape=NA,
                        palette = c("darkblue", "darkslateblue", "darkred"),
                        cumcensor = TRUE) 
# survplot2

#median survival age================================#
closest<-function(var,val){
  var[which(abs(var-val)==min(abs(var-val)))] 
}

median_surv_zero <- survplot2$data.survplot %>% 
  filter(apoe_info99_4ct=="zero copies") %>% 
  filter(surv==closest(surv, .5)) %>% 
  pull(time)

median_surv_one <- survplot2$data.survplot %>% 
  filter(apoe_info99_4ct=="one copy") %>% 
  filter(surv==closest(surv, .5)) %>% 
  pull(time)

median_surv_two <- survplot2$data.survplot %>% 
  filter(apoe_info99_4ct=="two copies") %>% 
  filter(surv==closest(surv, .5)) %>% 
  pull(time)
#==================================================#

#statistic annotations
logrank_apoe <- survdiff(Surv(cog_surv_age, event) ~ apoe_info99_4ct, data=hrs_surv_ind) %>% 
  glance() %>% 
  pull(statistic)
logrank_apoe_lab <- glue("Log-rank: *&chi;*<sup>2</sup> (2)={round(logrank_apoe, 2)}, *P*<0.001")

cox_apoe <- res_21 %>% 
  filter(grepl("apoe_info99_4ct", term)) %>% 
  select(estimate)
cox_apoe1_lab <- glue("Cox: one copy HR={round(cox_apoe[1,], 2)}, *P*<0.001")
cox_apoe2_lab <- glue("Cox: two copies HR={round(cox_apoe[2,], 2)}, *P*<0.001")

#plot
plot2 <- survplot2$plot +
  labs(y="Survival probability\n(no cognitive impairment)",
       x="Age",
       tag = "A.") +
  theme(legend.title = element_text(size = 22, face = "bold"),
        legend.text = element_text(size = 22),
        legend.direction = "vertical",
        legend.position = c(.85, .85),
        legend.key.size = unit(1, "cm"),
        axis.title.x.bottom = element_text(size=22, face = "bold"),
        axis.title.y.left = element_text(size=22, face = "bold", vjust = -15),
        axis.text.y.left   = element_text(size = 18),
        axis.text.x.bottom = element_text(size = 18)) +
  geom_segment(aes(x=45, xend=median_surv_zero, y=.5, yend=.5), linewidth=1, linetype="dashed") +
  geom_segment(aes(x=median_surv_zero, xend=median_surv_zero, y=0, yend=.5),  linewidth=1, linetype="dashed") +
  geom_segment(aes(x=median_surv_one, xend=median_surv_one, y=0, yend=.5),  linewidth=1, linetype="dashed") +
  geom_segment(aes(x=median_surv_two, xend=median_surv_two, y=0, yend=.5),  linewidth=1, linetype="dashed") +
  scale_y_continuous(expand = c(0,0)) +
  geom_richtext(aes(x=48, y=.15, label=logrank_apoe_lab), label.color="white", size=7, hjust=0) +
  geom_richtext(aes(x=48, y=.1,  label=cox_apoe1_lab),    label.color="white", size=7, hjust=0) +
  geom_richtext(aes(x=48, y=.05, label=cox_apoe2_lab),    label.color="white", size=7, hjust=0) +
  scale_fill_manual(name=expression(paste(bolditalic("APOE-\u03b54"), bold(" genotype"))),
                    values = c("darkblue", "darkslateblue", "darkred"),
                    labels = c("Zero copies", "One copy", "Two copies")) +
  scale_color_manual(name=expression(paste(bolditalic("APOE-\u03b54"), bold(" genotype"))),
                     values = c("darkblue",  "darkslateblue", "darkred"),
                     labels = c("Zero copies", "One copy", "Two copies"))
# plot2

tab2 <- survplot2$table +
  labs(y="",
       x="") +
  scale_y_discrete(labels=c("Two copies", "One copy", "Zero copies")) +
  theme(axis.title.y.left = element_blank(),
        axis.text.y.left  = element_text(size = 22, face = "bold", hjust = .5),
        plot.title        = element_text(size = 22, face = "bold"),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.line.y.left = element_line(colour = NA),
        panel.background = element_rect(color = "black", linewidth = 1))

survplot_apoe4 <- plot2 / tab2 + plot_layout(heights = c(3,1)) 
# survplot_apoe4
ggsave("../output/figures/survplot_apoe4.png", survplot_apoe4, width = 15, height = 8)

#=Combine plots========================================#

# design1="
# AAACCC
# AAACCC
# AAACCC
# AAACCC
# AAACCC
# BBBDDD
# "
# 
# combined1 <- plot1 + tab1 + plot2 + tab2 + 
#   plot_layout(design = design1, tag_level = "new") + 
#   plot_annotation(tag_levels = "A", tag_suffix=".") &
#   theme(plot.tag = element_text(size = 24, face = "bold"))
# combined1
# ggsave("../output/figures/survplot_combined1.png", combined1, width = 28, height =10)

design2="
AAA
AAA
AAA
AAA
AAA
BBB
CCC
CCC
CCC
CCC
CCC
DDD"

combined2 <- plot2 + tab2 + plot1 + tab1 + 
  plot_layout(design = design2, tag_level = "new") & 
  # plot_annotation(tag_levels = "A", tag_suffix=".") 
  theme(plot.tag = element_text(size = 24, face = "bold"))
# combined2
ggsave("../output/figures/survplot_combined2.tiff", combined2, width = 16, height =20, dpi = 700)






