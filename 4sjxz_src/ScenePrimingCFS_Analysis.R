# -------------------------------------------------------------------------------
# Statistical analysis of Baumann, L. & Valuch, C. (2022).
# Priming of Natural Scene Categorization during Continuous Flash Suppression.
# Manuscript submitted for publication.
#
# Correspondence: Christian Valuch, University of Vienna, Austria.
#                 christian.valuch@univie.ac.at
#
# Updated:        2022-03-10
#
# R version 4.1.2 (2021-11-01) Platform: x86_64-w64-mingw32/x64 (64-bit) 
# attached packages: patchwork_1.1.1; afex_1.0-1; lme4_1.1-28; Matrix_1.3-4; 
# multcomp_1.4-18; TH.data_1.1-0; MASS_7.3-54; survival_3.2-13 mvtnorm_1.1-3  
# effects_4.2-1; carData_3.0-5; forcats_0.5.1; stringr_1.4.0; dplyr_1.0.7; 
# purrr_0.3.4; readr_2.1.2; tidyr_1.2.0; tibble_3.1.6; ggplot2_3.3.5; 
# tidyverse_1.3.1 readxl_1.3.1  
# -------------------------------------------------------------------------------

rm(list=ls())

# load required packages
library(readxl)
library(tidyverse)
library(effects)
library(multcomp)
library(afex)
library(patchwork)

# functions for SEM calculation
sem<-function(x) {
  sd(x)/sqrt(length(x)) }
sem_lo<-function(x) {
  mean(x) - (sd(x)/sqrt(length(x)))*1 }
sem_hi<-function(x) {
  mean(x) + (sd(x)/sqrt(length(x)))*1 }

# load dataset 
data <- read.table(file="ScenePrimingCFS_Data.txt", sep = ";", dec=".")

# --------------------------------------
# Definition of variables in the dataset 
# --------------------------------------

# Exclude variables that are unused in the present analysis (prime and target
# filenames and inter-trial interval)
data <- data %>% dplyr::select(-prime_filename, -target_filename, -iti)
# participant ID
data$participant <- factor(data$participant)
# task: pri = speeded choice RT (priming) task; vis = prime discrimination task
data$task <- factor(data$task)
# prime_category: indoor vs. outdoor
data$prime_category <- factor(data$prime_category)
# prime_context: basic level category of the prime scene (e.g. playground, or bathroom)
data$prime_context <- factor(data$prime_context)
# target_category: indoor vs. outdoor
data$target_category <- factor(data$target_category)
# target_context: basic level category of the target scene (e.g. playground, or bathroom)
data$target_context <- factor(data$target_context)
# congruency: congruent vs. incongruent categories of prime and target
data$congruency <- factor(data$congruency)
# accuracy: correctness of response
data$accuracy <- factor(data$accuracy)
# soa: stimulus onset asynchrony of prime and target
data$soa <- factor(data$soa)
# mask_contrast: 20% vs. 100% (modulating prime visibility)
data$mask_contrast <- factor(data$mask_contrast)
data$mask_contrast <- relevel(data$mask_contrast, "20% (high prime visibility)")
# rt: response time in milliseconds
data$rt <- as.numeric(data$rt)
str(data)
# for analysis of prime discrimination / accuracy we also convert the accuracy variable
# to a numeric value (0 = wrong response, 1 = correct response)
data$acc <- 0
data$acc[data$accuracy=="correct"] <- 1

# -----------------------------------------
# Priming Task - Speeded choice RT analysis 
# -----------------------------------------
# We analyze RTs from trials with correct responses. We also exclude negative 
# RTs and outlier RTs (+/- 2SD around individual participant condition means 
# as upper and lower limit).

RTdata <- data %>% filter(task == "pri")
ntrials_priming <- nrow(RTdata)
RTdata <- RTdata %>% filter(accuracy == "correct")
ntrials_correct <- nrow(RTdata)

# how many trials have RTs below zero? 
ntrials_negativeRT <- nrow(RTdata %>% filter(rt <= 0))

# filter outlier RTs
RTdata <- RTdata %>% filter(rt > 0) %>%
  group_by(participant, congruency, soa, mask_contrast) %>% 
  mutate(avg_rt = mean(rt),
         sd_rt = sd(rt),
         lim_upper = avg_rt + 2*sd_rt,
         lim_lower = avg_rt - 2*sd_rt)
RTdata <- RTdata %>% filter(rt > lim_lower, rt < lim_upper)

# RT distribution for analysis
hist(RTdata$rt)
min(RTdata$rt)
max(RTdata$rt)
summary(RTdata$rt)

# log transform RTs for statistical analysis
RTdata$logRT <- log10(RTdata$rt)
hist(RTdata$logRT)

# fit a lienar mixed effects model with all theoretically relevant fixed effects 
# and # random intercepts for participant and target context
modelRT <- mixed(logRT ~ congruency*soa*mask_contrast + (1| participant) + (1|target_context),
                 data = RTdata, method = "S", type=3)
modelRT

# descriptive statistics of condition means (inverse transform to raw RTs) ...
# ... for the congruency effect
ph_congruency <- RTdata %>% group_by(participant, congruency) %>% 
  summarise( logRT = mean(logRT)) %>% 
  spread(congruency, logRT)
ph_congruency$congruent <- (10^ph_congruency$congruent)
ph_congruency$incongruent <- (10^ph_congruency$incongruent)
mean(ph_congruency$congruent)
mean(ph_congruency$incongruent)
# ... for the SOA effect
ph_soa <- RTdata %>% group_by(participant, soa) %>% 
  summarise( logRT = mean(logRT)) %>% 
  spread(soa, logRT)
ph_soa$`200 ms` <- (10^ph_soa$`200 ms`)
ph_soa$`400 ms` <- (10^ph_soa$`400 ms`)
mean(ph_soa$`200 ms`)
mean(ph_soa$`400 ms`)
# ... for the mask contrast effect
ph_mc <- RTdata %>% group_by(participant, mask_contrast) %>% 
  summarise( logRT = mean(logRT)) %>% 
  spread(mask_contrast, logRT)
ph_mc$`100% (low prime visibility)` <- (10^ph_mc$`100% (low prime visibility)`)
ph_mc$`20% (high prime visibility)` <- (10^ph_mc$`20% (high prime visibility)`)
mean(ph_mc$`100% (low prime visibility)`)
mean(ph_mc$`20% (high prime visibility)`)

# remove between-participant variance for plotting within-subject error bars 
# (method of Loftus & Masson, 1994) 
aRT <- RTdata %>% group_by(participant) %>% mutate( participant_mean = mean(logRT))
aRT$RTadj <- aRT$logRT + (mean(aRT$logRT) - aRT$participant_mean)

# Plot 1: RT results
summary_RT <- aRT %>% 
  group_by( congruency, soa, mask_contrast ) %>%
  summarise( RTmean = 10^(mean(RTadj)),
             RThi = 10^(sem_lo(RTadj)),
             RTlo = 10^(sem_hi(RTadj)))

col_congruence = c("#669cff", "#be0000")
summary_RT$mask_contrast2 <- "100%\n(low prime visibility)"
summary_RT$mask_contrast2[summary_RT$mask_contrast=="20% (high prime visibility)"] <- "20%\n(high prime visibility)"

P1 <- ggplot(summary_RT, aes(x=soa, y=RTmean)) +
  geom_line(stat="identity", alpha=0.75, size=0.8, position=position_dodge(0.05), aes(color=congruency, group=congruency)) +
  geom_errorbar(stat="identity", position=position_dodge(0.05), width=0, aes(ymin=RTlo, ymax=RThi, group=congruency, color=congruency)) +
  geom_point(stat="identity", size=2, color="black", position=position_dodge(0.05), aes(group=congruency, shape=congruency, fill=congruency)) +
  facet_grid(~ mask_contrast2) +
  scale_fill_manual(values=col_congruence) +
  scale_color_manual(values=col_congruence) +
  scale_shape_manual(values=c("triangle down filled", "triangle filled")) +
  theme_classic() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill=FALSE, color=NA),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        strip.text = element_text(size=10),
        plot.background = element_blank(),
        axis.ticks = element_line(size=0.4, color="black"),
        plot.margin = unit(c(0.25,1.15,0.25,0.25),"cm"),
        plot.title = element_text(hjust = 0.5, size=12),
        axis.text.x = element_blank(),
        axis.line.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(color="black")
        ) +
  ggtitle("---------   Mask Contrast   ---------") +
  xlab("Prime-Target SOA") +
  ylab("Mean RT (ms)") +
  labs(colour="Prime-Target Congruency",
       fill="Prime-Target Congruency",
       shape="Prime-Target Congruency")

# Plot 2: Distribution of individual congruency effects per condition
aRT2 <- RTdata %>% group_by(participant, congruency, soa, mask_contrast) %>% 
  summarise( logRT = mean(logRT))
aRT2 <- aRT2 %>% spread(congruency, logRT)
aRT2$congruent <- (10^aRT2$congruent)
aRT2$incongruent <- (10^aRT2$incongruent)
aRT2$CE <- aRT2$congruent - aRT2$incongruent

aRT2$mask_contrast2 <- "100%\n(low prime visibility)"
aRT2$mask_contrast2[aRT2$mask_contrast=="20% (high prime visibility)"] <- "20%\n(high prime visibility)"

# summarise congruency effects for Table 2
ce_summary <- aRT2 %>% 
  group_by( mask_contrast, soa ) %>%
  summarise( CE_mean = mean(CE),
             CE_sem = sem(CE))

pd <- position_jitter(0.05)
P2 <- ggplot(data=aRT2, aes(y=CE, x=soa)) +
  theme_classic() +
  geom_bar(stat="summary", width=0.25, fill=NA, color="black") +
  geom_errorbar(stat="summary", width=0, fill=NA, color="black") +
  geom_violin(color=NA, fill="#669cff", alpha=0.2) +
  geom_point(shape=16, size=1, position=pd, color="#669cff", alpha=0.8) +
  ylab("Congruency effect (ms)") + 
  facet_grid(~ mask_contrast2) +
  theme( strip.background = element_blank(),
         legend.title=element_blank(),
         axis.text = element_text(color="black"),
         axis.line.x = element_blank(),
         axis.ticks.x = element_blank(),
         strip.text=element_blank(),
         axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 10, l = 0))) +
  geom_hline(yintercept = 0) +
  xlab("--------   Prime-Target SOA    --------")

# save RT plot (Figure 1, panel A)
P1 / P2
ggsave("Fig1_A.png", dpi = 300, width = 18, height = 16, units = "cm")

# Compute directional contrasts to test for congruency effects in each condition
# (summarized in Table 2)
RTdata$conditions <- RTdata$mask_contrast:RTdata$soa:RTdata$congruency
rt_model <- lmer(data=RTdata, 
                 logRT ~ conditions + (1| participant) + (1|target_context))
# explicitly specify contrast weights
contr = rbind(
  "100%/200ms" = c(0,0, 0, 0, 1, -1, 0, 0),
  "100%/400ms" = c(0,0, 0, 0, 0, 0, 1, -1),
  "20%/200ms" = c(1, -1, 0, 0, 0, 0, 0, 0),
  "20%/400ms" = c(0, 0, 1, -1, 0, 0, 0, 0)
)
summary(glht(rt_model,linfct=mcp(conditions=contr), alternative = "less"),
        test=adjusted("none"))

# Plot 3: Correlation between individual congrueny effects and individual 
# prime discrimination performance (Fig. 1C)

# calculate individual prime discrimination performance per condition
vis_data <- data %>% filter(task =="vis" & rt >= 400) %>%
  group_by(participant, soa, mask_contrast) %>%
  summarise(sum_correct = sum(acc),
            n_trials = length(participant))

vis_data$PC <- (vis_data$sum_correct/vis_data$n_trials)*100

# calculate individual congruency effects per condition
ce_data <- RTdata %>% group_by(participant, congruency, soa, mask_contrast) %>% 
  summarise( logRT = mean(logRT))
ce_data <- ce_data %>% spread(congruency, logRT)
ce_data$congruent <- (10^ce_data$congruent)
ce_data$incongruent <- (10^ce_data$incongruent)
ce_data$CE <- ce_data$congruent - ce_data$incongruent

# join both effects into common data frame
CA <- left_join(vis_data, ce_data)

# shorten the factor levels name (for the plot)
CA$mc <- "20% contrast"
CA$mc[CA$mask_contrast=="100% (low prime visibility)"] <- "100% contrast"

P3 <- ggplot(CA, aes(x=PC, y=CE)) +
  geom_vline(xintercept=50) +
  geom_hline(yintercept=0) +
  geom_point(color="#669cff", size=2, alpha=0.8) +
  facet_grid(mc ~ soa) +
  theme_classic() +
  theme(panel.background = element_rect(fill=NA, color="black"),
        aspect.ratio=1,
        axis.text = element_text(color="black", size=12),
        axis.title = element_text(size=14),
        strip.background = element_rect(fill=FALSE, color=NA),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        strip.text = element_text(size=12),
        strip.text.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10)),
        strip.text.x = element_text(margin = margin(t = 0, r = 0, b = 10, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  xlab("Prime discrimination (% correct)") +
  ylab("Congruency effect (ms)") +
  coord_cartesian(ylim=c(-80, 80), xlim=c(30, 110)) 

# pearon correlations within each of these four conditions
ca_subset <- CA %>% filter(soa == "200 ms" & mc == "100% contrast")
cor.test(ca_subset$PC, ca_subset$CE)

ca_subset <- CA %>% filter(soa == "400 ms" & mc == "100% contrast")
cor.test(ca_subset$PC, ca_subset$CE)

ca_subset <- CA %>% filter(soa == "200 ms" & mc == "20% contrast")
cor.test(ca_subset$PC, ca_subset$CE)

ca_subset <- CA %>% filter(soa == "400 ms" & mc == "20% contrast")
cor.test(ca_subset$PC, ca_subset$CE)

# Plot 4: Average prime discrimination performance (Figure 1B)

# remove between-participant variance for plotting within-subject error bars 
# (method of Loftus & Masson, 1994) 
aVis <- CA %>% group_by(participant) %>% mutate( participant_mean = mean(PC))
aVis$PCadj <- aVis$PC + (mean(aVis$PC) - aVis$participant_mean)

summary_vis <- aVis %>% 
  group_by( soa, mask_contrast ) %>%
  summarise( PCmean = mean(PCadj),
             PChi = sem_lo(PCadj),
             PClo = sem_hi(PCadj))

col_visibility <- c("black", "gray50")
summary_vis$mask_contrast <- relevel(summary_vis$mask_contrast, ref="100% (low prime visibility)")

P4 <- ggplot(summary_vis, aes(x=soa, y=PCmean)) +
  geom_line(stat="identity", alpha=0.75, size=0.8, position=position_dodge(0.05), aes(color=mask_contrast, group=mask_contrast)) +
  geom_errorbar(stat="identity", position=position_dodge(0.05), width=0, aes(ymin=PChi, ymax=PClo, group=mask_contrast, color=mask_contrast)) +
  geom_point(stat="identity", size=2, color="black", position=position_dodge(0.05), aes(group=mask_contrast, shape=mask_contrast, fill=mask_contrast)) +
  scale_fill_manual(values=col_visibility) +
  scale_color_manual(values=col_visibility) +
  scale_shape_manual(values=c("triangle down filled", "triangle filled")) +
  theme_classic() +
  theme(aspect.ratio=3/5,
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "top",
        legend.direction = "vertical",
        strip.background = element_rect(fill=FALSE, color=NA),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12),
        strip.text = element_text(size=10),
        plot.background = element_blank(),
        axis.ticks = element_line(size=0.4, color="black"),
        plot.margin = unit(c(0.25,1.15,0.25,0.25),"cm"),
        plot.title = element_text(hjust = 0.5, size=12),
        axis.text = element_text(color="black", size=12),
        axis.title = element_text(size=14),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)) 
  ) + labs(colour="Mask Contrast",
         fill="Mask Contrast",
         shape="Mask Contrast") +
  xlab("Prime-Target SOA") +
  ylab("Prime discrimination\n(% correct)") +
  coord_cartesian(xlim=c(0.5,2.5), ylim=c(50, 85), expand=F)

# save panels B and C of Figure 1
P4 / P3
ggsave("Fig1_BC.png", dpi = 300, width = 12, height = 20, units = "cm")

# ----------------------------------
# Priming Trials - Accuracy analysis 
# ----------------------------------

data$acc <- 0
data$acc[data$accuracy=="correct"] <- 1

# for the accuracy analysis we exclude trials with negative RTs
data_er <- data %>% filter(task == "pri" & rt > 0)

# descriptive statistics of accuracy / errors
er_desc <- data_er %>% group_by(participant) %>%
  summarize(n_correct = sum(acc),
            n_trials = length(participant))

# compute percent correct
er_desc$pc <-(er_desc$n_correct/er_desc$n_trials)*100
summary(er_desc$pc)

# fit a GLMM model with all theoretically relevant fixed effects
modelER <- mixed(acc ~ congruency*soa*mask_contrast + (1| participant) + (1|target_context),
             family = binomial("logit"),
             data = data_er, method = "LRT", type =3)
summary(modelER)
anova(modelER)

# ---------------------------------------------
# Prime discrimination task - Accuracy analysis 
# ---------------------------------------------
# In line with instructions and feedback during the experiment, 
# we select only trials with RTs above 400 ms
acc_data <- data %>% filter(task =="vis" & rt >= 400)
acc_data$acc <- 0
acc_data$acc[acc_data$accuracy == "correct"] <- 1


# fit a GLMM model with all theoretically relevant effects
modelER <- mixed(acc ~ soa*mask_contrast + (1| participant) + (1|prime_context),
                 family = binomial("logit"),
                 data = acc_data, method = "LRT",
                 type=3)
modelER

# descriptive statistics of condition means ...
# ... for mask contrast
ph_mc <- acc_data %>% group_by(participant, mask_contrast) %>%
  summarize(pc = sum(acc)/length(participant)) %>% spread(mask_contrast, pc)
mean(ph_mc$`20% (high prime visibility)`)
mean(ph_mc$`100% (low prime visibility)`)

# ... for soa
ph_mc <- acc_data %>% group_by(participant, soa) %>%
  summarize(pc = sum(acc)/length(participant)) %>% spread(soa, pc)
mean(ph_mc$`200 ms`)
mean(ph_mc$`400 ms`)

# END OF ANALYSIS.