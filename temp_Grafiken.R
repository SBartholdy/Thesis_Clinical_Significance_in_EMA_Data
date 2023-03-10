
setwd(
  "C:/Users/steph/OneDrive/Desktop/Psychologie/Masterstudium Psychologie/3. Semester/1. Master-Konversatorium/Preprocessing und Berechnungen"
)

library(plyr)
library(dplyr)
library(tidyverse)
library(bootstrap)
library(lattice)
library(Rmisc)
library(devtools)
library(psych)
library(DescTools)
library(summarytools)
library(kableExtra)
library(lubridate)
library(timetk)
library(overlapping)
library(ggplot2)
library(gghalves)

set.seed(42)


load("cor_07_k25_30MZP_each/FB_30MZP_each.RData")
FB_30MZP_each = FB_30MZP_each %>%
  as_tibble()

load("cor_07_k25/FB_original.RData")
FB_original = FB_original %>% 
  as_tibble()

#FB_2MZP = FB_original %>% 
#  select(ID, PRE1_1, POST1_1) %>% 
#  rename(PRE = PRE1_1, POST = POST1_1)
#save(FB_2MZP, file = "cor_07_k25/FB_2MZP.RData")

load("cor_07_k25/FB_2MZP.RData")
FB_2MZP = FB_2MZP %>% 
  as_tibble()

pre_30mzp = c("PRE1_1","PRE1_2","PRE1_3","PRE1_4","PRE1_5",
              "PRE1_6","PRE1_7","PRE1_8","PRE1_9","PRE1_10",
              "PRE1_11","PRE1_12","PRE1_13","PRE1_14","PRE1_15",
              "PRE1_16","PRE1_17","PRE1_18","PRE1_19","PRE1_20",
              "PRE1_21","PRE1_22","PRE1_23","PRE1_24","PRE1_25",
              "PRE1_26","PRE1_27","PRE1_28","PRE1_29","PRE1_30")

post_30mzp = c("POST1_1","POST1_2","POST1_3","POST1_4","POST1_5",
               "POST1_6","POST1_7","POST1_8","POST1_9","POST1_10",
               "POST1_11","POST1_12","POST1_13","POST1_14","POST1_15",
               "POST1_16","POST1_17","POST1_18","POST1_19","POST1_20",
               "POST1_21","POST1_22","POST1_23","POST1_24","POST1_25",
               "POST1_26","POST1_27","POST1_28","POST1_29","POST1_30")

pre_5mzp = c("PRE1_1","PRE1_2","PRE1_3","PRE1_4","PRE1_5")
post_5mzp = c("POST1_1","POST1_2","POST1_3","POST1_4","POST1_5")

FB_original$PRE_Mean = apply(FB_original[pre_5mzp], 1, mean)
FB_original$POST_Mean = apply(FB_original[post_5mzp], 1, mean)
FB_original$MeanDiff = FB_original$PRE_Mean - FB_original$POST_Mean
FB_original$ind.pretestSD = apply(FB_original[pre_5mzp], 1, sd)
FB_original$ind.posttestSD = apply(FB_original[post_5mzp], 1, sd)

FB_2MZP$Diff = FB_2MZP$PRE - FB_2MZP$POST


##########


# optionaler Chunk: Ausschluss von (ca. 5350) Personen ohne Varianz in min. einem MZP-Intervall

# sd(c(1,1,1,1,2)) = 0.4472136 = min. SD bei 5 (nicht gleichen) MZP
# sd(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2)) = 0.1825742 = min. SD bei 30 (nicht gleichen) MZP

length(which(FB_original$ind.pretestSD <= 0.44))
length(which(FB_original$ind.posttestSD <= 0.44))
FB_original = FB_original %>% 
  filter(ind.pretestSD > 0.44 & ind.posttestSD > 0.44)

length(which(FB_30MZP_each$ind.pretestSD <= 0.18))
length(which(FB_30MZP_each$ind.posttestSD <= 0.18))
FB_30MZP_each = FB_30MZP_each %>% 
  filter(ind.pretestSD > 0.18 & ind.posttestSD > 0.18)

FB_original = FB_original %>% 
  filter(ID %in% intersect(intersect(FB_original$ID, FB_30MZP_each$ID), FB_2MZP$ID))

FB_30MZP_each = FB_30MZP_each %>% 
  filter(ID %in% intersect(intersect(FB_original$ID, FB_30MZP_each$ID), FB_2MZP$ID))

FB_2MZP = FB_2MZP %>% 
  filter(ID %in% intersect(intersect(FB_original$ID, FB_30MZP_each$ID), FB_2MZP$ID))


FB_original$ID = 1:nrow(FB_original)
FB_30MZP_each$ID = 1:nrow(FB_30MZP_each)
FB_2MZP$ID = 1:nrow(FB_2MZP)


##########  


### Original-Simulationsdaten (je 5 MZP)

# don´t run this section (code for extremely computation-intense plots that I already stored as .RData and .jpg)
# repeated-measures scatter-boxplot-violin-histograms for individual PRE and POST means
# from van Langen (2020) Open-visualizations tutorial for repeated measures in R

# FB_original
# converting my dataframes to use in the same ggplot structure:
Mean = c()

for (i in 1:nrow(FB_original)) {
  Mean = c(Mean, FB_original[i,"PRE_Mean"], FB_original[i,"POST_Mean"])
  message(i)
}

FB_original_ts = tibble(ID = factor(rep(1:nrow(FB_original), each = 2)),
                        Interval = c(rep(c(1,2), times = nrow(FB_original))),
                        Mean = as.numeric(Mean))

save(FB_original_ts, file = "Time Series Dataframes/k25_FB_original_ts.RData")

###

load("Time Series Dataframes/k25_FB_original_ts.RData")

# Repeated measures with box??? and violin plots
FB_original_ts$jit = jitter(FB_original_ts$Interval, amount = .09)

Pre_Post_Box_Violin = ggplot(data = FB_original_ts, aes(y = Mean)) +
  geom_point(data = FB_original_ts %>% filter(Interval == "1"), aes(x = jit), color = "dodgerblue", size = 1,
             alpha = .5) +
  geom_point(data = FB_original_ts %>% filter(Interval == "2"), aes(x = jit), color = "darkorange", size = 1,
             alpha = .5) +
  geom_line(aes(x = jit, group = ID), color = "lightgray", alpha = .05) +
  geom_half_boxplot(
    data = FB_original_ts %>% filter(Interval == "1"), aes(x = Interval, y = Mean), position = position_nudge(x = -.25),
    side = "r", outlier.shape = NA, center = TRUE, errorbar.draw = TRUE, width = .1,
    fill = "dodgerblue", alpha = .5) +
  geom_half_boxplot(
    data = FB_original_ts %>% filter(Interval == "2"), aes(x = Interval, y = Mean), position = position_nudge(x = .15),
    side = "r", outlier.shape = NA, center = TRUE, errorbar.draw = TRUE, width = .1,
    fill = "darkorange", alpha = .5) +
  geom_half_violin(
    data = FB_original_ts %>% filter(Interval == "1"), aes(x = Interval, y = Mean), position = position_nudge(x = 1.3),
    side = "r", fill = "dodgerblue", alpha = .5, trim = FALSE) +
  geom_half_violin(
    data = FB_original_ts %>% filter(Interval == "2"), aes(x = Interval, y = Mean), position = position_nudge(x = .3),
    side = "r", fill = "darkorange", alpha = .5, trim = FALSE) +
  scale_x_continuous(breaks = c(1,2), labels = c("PRE", "POST"), limits = c(0, 3)) +
  xlab("Interval") + ylab("Mean") +
  ggtitle("Paper-Pencil Data (5+5 Timepoints): Individual Pre-Post Means") +
  theme_classic()+
  coord_cartesian(ylim = c(0, 24))

ggsave("Time Series Dataframes/k25_FB_original_Pre-Post_Box_Violin.jpg", plot = Pre_Post_Box_Violin, width = 6, height = 4)
save(Pre_Post_Box_Violin, file = "Time Series Dataframes/k25_FB_original_Pre_Post_Box_Violin.RData")


# Repeated measures with box??? and violin plots and means + CIs
score_mean_1 = FB_original_ts %>% filter(Interval == "1") %>% summarise(mean(Mean)) %>% as.numeric()
score_mean_2 = FB_original_ts %>% filter(Interval == "2") %>% summarise(mean(Mean)) %>% as.numeric()
score_median1 = FB_original_ts %>% filter(Interval == "1") %>% summarise(median(Mean)) %>% as.numeric()
score_median2 = FB_original_ts %>% filter(Interval == "2") %>% summarise(median(Mean)) %>% as.numeric()
score_sd_1 = FB_original_ts %>% filter(Interval == "1") %>% summarise(sd(Mean)) %>% as.numeric()
score_sd_2 = FB_original_ts %>% filter(Interval == "2") %>% summarise(sd(Mean)) %>% as.numeric()
score_se_1 = score_sd_1/sqrt(nrow(FB_original))
score_se_2 = score_sd_2/sqrt(nrow(FB_original))
score_ci_1 = FB_original_ts %>% filter(Interval == "1") %>% pull(Mean) %>% CI(., ci = 0.95)
score_ci_2 = FB_original_ts %>% filter(Interval == "2") %>% pull(Mean) %>% CI(., ci = 0.95)
#Create data frame with 2 rows and 7 columns containing the descriptives
group = c("PRE", "POST")
N = c(nrow(FB_original), nrow(FB_original))
score_mean = c(score_mean_1, score_mean_2)
score_median = c(score_median1, score_median2)
sd = c(score_sd_1, score_sd_2)
se = c(score_se_1, score_se_2)
ci = c(as.numeric(score_ci_1[1] - score_ci_1[3]), as.numeric(score_ci_2[1] - score_ci_2[3]))
summary_df = data.frame(group, N, score_mean, score_median, sd, se, ci)

# FB_original_ts$jit = jitter(FB_original_ts$Interval, amount = .09)     #already created above
x_tick_means = c(.87, 2.13)

Pre_Post_Box_Violin_Mean_CI = ggplot(data = FB_original_ts, aes(y = Mean)) +
  geom_point(data = FB_original_ts %>% filter(Interval == "1"), aes(x = jit), color = "dodgerblue", size = 1,
             alpha = .6) +
  geom_point(data = FB_original_ts %>% filter(Interval == "2"), aes(x = jit), color = "darkorange", size = 1,
             alpha = .6) +
  geom_line(aes(x = jit, group = ID), color = "lightgray", alpha = .05) +
  geom_half_boxplot(
    data = FB_original_ts %>% filter(Interval == "1"), aes(x = Interval, y = Mean), position = position_nudge(x = -.28),
    side = "r", outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
    fill = "dodgerblue") +
  geom_half_boxplot(
    data = FB_original_ts %>% filter(Interval == "2"), aes(x = Interval, y = Mean), position = position_nudge(x = .18),
    side = "r", outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
    fill = "darkorange") +
  geom_half_violin(
    data = FB_original_ts %>% filter(Interval == "1"), aes(x = Interval, y = Mean), position = position_nudge(x = -.3),
    side = "l", fill = "dodgerblue") +
  geom_half_violin(
    data = FB_original_ts %>% filter(Interval == "2"),aes(x = Interval, y = Mean), position = position_nudge(x = .3),
    side = "r", fill = "darkorange") +
  geom_point(data = FB_original_ts %>% filter(Interval == "1"), aes(x = Interval, y = score_mean[1]),
             position = position_nudge(x = -.13), color = "dodgerblue", alpha = .6, size = 1.5) +
  geom_errorbar(data = FB_original_ts %>% filter(Interval == "1"), aes(x = Interval, y = score_mean[1],
                                                                       ymin = score_mean[1]-ci[1], ymax = score_mean[1]+ci[1]),
                position = position_nudge(-.13), color = "dodgerblue", width = 0.05, size = 0.4, alpha = .6) +
  geom_point(data = FB_original_ts %>% filter(Interval == "2"), aes(x = Interval, y = score_mean[2]),
             position = position_nudge(x = .13), color = "darkorange", alpha = .6, size = 1.5)+
  geom_errorbar(data = FB_original_ts %>% filter(Interval == "2"), aes(x = Interval, y = score_mean[2],
                                                                       ymin = score_mean[2]-ci[2], ymax = score_mean[2]+ci[2]), 
                position = position_nudge(.13), color = "darkorange", width = 0.05, size = 0.4, alpha = .6) +
  geom_line(data = summary_df, aes(x = x_tick_means, y = score_mean), color = "gray", size = 1) +
  scale_x_continuous(breaks = c(1,2), labels = c("PRE", "POST"), limits = c(0, 3)) +
  xlab("Interval") + ylab("Mean") +
  ggtitle("Paper-Pencil Data (5+5 Timepoints): Individual Pre-Post Means") +
  theme_classic()+
  coord_cartesian(ylim = c(0, 24))

ggsave("Time Series Dataframes/k25_FB_original_Pre-Post_Box_Violin_Mean+CI.jpg", plot = Pre_Post_Box_Violin_Mean_CI, width = 6, height = 4)
save(Pre_Post_Box_Violin_Mean_CI, file = "Time Series Dataframes/k25_FB_original_Pre_Post_Box_Violin_Mean_CI.RData")


##########
set.seed(42)

### Erweiterte Intervall-Daten (je 30 MZP)
  
# don´t run this section (code for extremely computation-intense plots that I already stored as .RData and .jpg)
# repeated-measures scatter-boxplot-violin-histograms for individual PRE and POST means
# from van Langen (2020) Open-visualizations tutorial for repeated measures in R

# FB_30MZP_each
# converting my dataframes to use in the same ggplot structure:
Mean = c()

for (i in 1:nrow(FB_30MZP_each)) {
  Mean = c(Mean, FB_30MZP_each[i,"PRE_Mean"], FB_30MZP_each[i,"POST_Mean"])
  message(i)
}

FB_30MZP_each_ts = tibble(ID = factor(rep(1:nrow(FB_30MZP_each), each = 2)),
                          Interval = c(rep(c(1,2), times = nrow(FB_30MZP_each))),
                          Mean = as.numeric(Mean))

save(FB_30MZP_each_ts, file = "Time Series Dataframes/k25_FB_30MZP_each_ts.RData")

###

load("Time Series Dataframes/k25_FB_30MZP_each_ts.RData")

# Repeated measures with box??? and violin plots
FB_30MZP_each_ts$jit = jitter(FB_30MZP_each_ts$Interval, amount = .09)

Pre_Post_Box_Violin = ggplot(data = FB_30MZP_each_ts, aes(y = Mean)) +
  geom_point(data = FB_30MZP_each_ts %>% filter(Interval == "1"), aes(x = jit), color = "dodgerblue", size = 1,
             alpha = .5) +
  geom_point(data = FB_30MZP_each_ts %>% filter(Interval == "2"), aes(x = jit), color = "darkorange", size = 1,
             alpha = .5) +
  geom_line(aes(x = jit, group = ID), color = "lightgray", alpha = .05) +
  geom_half_boxplot(
    data = FB_30MZP_each_ts %>% filter(Interval == "1"), aes(x = Interval, y = Mean), position = position_nudge(x = -.25),
    side = "r", outlier.shape = NA, center = TRUE, errorbar.draw = TRUE, width = .1,
    fill = "dodgerblue", alpha = .5) +
  geom_half_boxplot(
    data = FB_30MZP_each_ts %>% filter(Interval == "2"), aes(x = Interval, y = Mean), position = position_nudge(x = .15),
    side = "r", outlier.shape = NA, center = TRUE, errorbar.draw = TRUE, width = .1,
    fill = "darkorange", alpha = .5) +
  geom_half_violin(
    data = FB_30MZP_each_ts %>% filter(Interval == "1"), aes(x = Interval, y = Mean), position = position_nudge(x = 1.3),
    side = "r", fill = "dodgerblue", alpha = .5, trim = FALSE) +
  geom_half_violin(
    data = FB_30MZP_each_ts %>% filter(Interval == "2"), aes(x = Interval, y = Mean), position = position_nudge(x = .3),
    side = "r", fill = "darkorange", alpha = .5, trim = FALSE) +
  scale_x_continuous(breaks = c(1,2), labels = c("PRE", "POST"), limits = c(0, 3)) +
  xlab("Interval") + ylab("Mean") +
  ggtitle("Paper-Pencil Data (30+30 Timepoints): Individual Pre-Post Means") +
  theme_classic()+
  coord_cartesian(ylim = c(0, 24))

ggsave("Time Series Dataframes/k25_FB_30MZP_each_Pre-Post_Box_Violin.jpg", plot = Pre_Post_Box_Violin, width = 6, height = 4)
save(Pre_Post_Box_Violin, file = "Time Series Dataframes/k25_FB_30MZP_each_Pre_Post_Box_Violin.RData")


# Repeated measures with box??? and violin plots and means + CIs
score_mean_1 = FB_30MZP_each_ts %>% filter(Interval == "1") %>% summarise(mean(Mean)) %>% as.numeric()
score_mean_2 = FB_30MZP_each_ts %>% filter(Interval == "2") %>% summarise(mean(Mean)) %>% as.numeric()
score_median1 = FB_30MZP_each_ts %>% filter(Interval == "1") %>% summarise(median(Mean)) %>% as.numeric()
score_median2 = FB_30MZP_each_ts %>% filter(Interval == "2") %>% summarise(median(Mean)) %>% as.numeric()
score_sd_1 = FB_30MZP_each_ts %>% filter(Interval == "1") %>% summarise(sd(Mean)) %>% as.numeric()
score_sd_2 = FB_30MZP_each_ts %>% filter(Interval == "2") %>% summarise(sd(Mean)) %>% as.numeric()
score_se_1 = score_sd_1/sqrt(nrow(FB_30MZP_each))
score_se_2 = score_sd_2/sqrt(nrow(FB_30MZP_each))
score_ci_1 = FB_30MZP_each_ts %>% filter(Interval == "1") %>% pull(Mean) %>% CI(., ci = 0.95)
score_ci_2 = FB_30MZP_each_ts %>% filter(Interval == "2") %>% pull(Mean) %>% CI(., ci = 0.95)
#Create data frame with 2 rows and 7 columns containing the descriptives
group = c("PRE", "POST")
N = c(nrow(FB_30MZP_each), nrow(FB_30MZP_each))
score_mean = c(score_mean_1, score_mean_2)
score_median = c(score_median1, score_median2)
sd = c(score_sd_1, score_sd_2)
se = c(score_se_1, score_se_2)
ci = c((score_ci_1[1] - score_ci_1[3]), (score_ci_2[1] - score_ci_2[3]))
summary_df = data.frame(group, N, score_mean, score_median, sd, se, ci)

# FB_30MZP_each_ts$jit = jitter(FB_30MZP_each_ts$Interval, amount = .09)     #already created above
x_tick_means = c(.87, 2.13)

Pre_Post_Box_Violin_Mean_CI = ggplot(data = FB_30MZP_each_ts, aes(y = Mean)) +
  geom_point(data = FB_30MZP_each_ts %>% filter(Interval == "1"), aes(x = jit), color = "dodgerblue", size = 1,
             alpha = .6) +
  geom_point(data = FB_30MZP_each_ts %>% filter(Interval == "2"), aes(x = jit), color = "darkorange", size = 1,
             alpha = .6) +
  geom_line(aes(x = jit, group = ID), color = "lightgray", alpha = .05) +
  geom_half_boxplot(
    data = FB_30MZP_each_ts %>% filter(Interval == "1"), aes(x = Interval, y = Mean), position = position_nudge(x = -.28),
    side = "r", outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
    fill = "dodgerblue") +
  geom_half_boxplot(
    data = FB_30MZP_each_ts %>% filter(Interval == "2"), aes(x = Interval, y = Mean), position = position_nudge(x = .18),
    side = "r", outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
    fill = "darkorange") +
  geom_half_violin(
    data = FB_30MZP_each_ts %>% filter(Interval == "1"), aes(x = Interval, y = Mean), position = position_nudge(x = -.3),
    side = "l", fill = "dodgerblue") +
  geom_half_violin(
    data = FB_30MZP_each_ts %>% filter(Interval == "2"),aes(x = Interval, y = Mean), position = position_nudge(x = .3),
    side = "r", fill = "darkorange") +
  geom_point(data = FB_30MZP_each_ts %>% filter(Interval == "1"), aes(x = Interval, y = score_mean[1]),
             position = position_nudge(x = -.13), color = "dodgerblue", alpha = .6, size = 1.5) +
  geom_errorbar(data = FB_30MZP_each_ts %>% filter(Interval == "1"), aes(x = Interval, y = score_mean[1],
                                                                         ymin = score_mean[1]-ci[1], ymax = score_mean[1]+ci[1]),
                position = position_nudge(-.13), color = "dodgerblue", width = 0.05, size = 0.4, alpha = .6) +
  geom_point(data = FB_30MZP_each_ts %>% filter(Interval == "2"), aes(x = Interval, y = score_mean[2]),
             position = position_nudge(x = .13), color = "darkorange", alpha = .6, size = 1.5)+
  geom_errorbar(data = FB_30MZP_each_ts %>% filter(Interval == "2"), aes(x = Interval, y = score_mean[2],
                                                                         ymin = score_mean[2]-ci[2], ymax = score_mean[2]+ci[2]), 
                position = position_nudge(.13), color = "darkorange", width = 0.05, size = 0.4, alpha = .6) +
  geom_line(data = summary_df, aes(x = x_tick_means, y = score_mean), color = "gray", size = 1) +
  scale_x_continuous(breaks = c(1,2), labels = c("PRE", "POST"), limits = c(0, 3)) +
  xlab("Interval") + ylab("Mean") +
  ggtitle("Paper-Pencil Data (30+30 Timepoints): Individual Pre-Post Means") +
  theme_classic()+
  coord_cartesian(ylim = c(0, 24))

ggsave("Time Series Dataframes/k25_FB_30MZP_each_Pre-Post_Box_Violin_Mean+CI.jpg", plot = Pre_Post_Box_Violin_Mean_CI, width = 6, height = 4)
save(Pre_Post_Box_Violin_Mean_CI, file = "Time Series Dataframes/k25_FB_30MZP_each_Pre_Post_Box_Violin_Mean_CI.RData")


##########
set.seed(42)

### Stichprobe mit je 1 MZP
  
# don´t run this section (code for extremely computation-intense plots that I already stored as .RData and .jpg)
# repeated-measures scatter-boxplot-violin-histograms for individual PRE and POST means
# from van Langen (2020) Open-visualizations tutorial for repeated measures in R

# FB_2MZP
# converting my dataframes to use in the same ggplot structure:
Score = c()

for (i in 1:nrow(FB_2MZP)) {
  Score = c(Score, FB_2MZP[i,"PRE"], FB_2MZP[i,"POST"])
  message(i)
}

FB_2MZP_ts = tibble(ID = factor(rep(1:nrow(FB_2MZP), each = 2)),
                    Timepoint = c(rep(c(1,2), times = nrow(FB_2MZP))),
                    Score = as.numeric(Score))

save(FB_2MZP_ts, file = "Time Series Dataframes/k25_FB_2MZP_ts.RData")

###

load("Time Series Dataframes/k25_FB_2MZP_ts.RData")

# Repeated measures with box??? and violin plots
FB_2MZP_ts$jit = jitter(FB_2MZP_ts$Timepoint, amount = .09)

Pre_Post_Box_Violin = ggplot(data = FB_2MZP_ts, aes(y = Score)) +
  geom_point(data = FB_2MZP_ts %>% filter(Timepoint == "1"), aes(x = jit), color = "dodgerblue", size = 1,
             alpha = .5) +
  geom_point(data = FB_2MZP_ts %>% filter(Timepoint == "2"), aes(x = jit), color = "darkorange", size = 1,
             alpha = .5) +
  geom_line(aes(x = jit, group = ID), color = "lightgray", alpha = .05) +
  geom_half_boxplot(
    data = FB_2MZP_ts %>% filter(Timepoint == "1"), aes(x = Timepoint, y = Score), position = position_nudge(x = -.25),
    side = "r", outlier.shape = NA, center = TRUE, errorbar.draw = TRUE, width = .1,
    fill = "dodgerblue", alpha = .5) +
  geom_half_boxplot(
    data = FB_2MZP_ts %>% filter(Timepoint == "2"), aes(x = Timepoint, y = Score), position = position_nudge(x = .15),
    side = "r", outlier.shape = NA, center = TRUE, errorbar.draw = TRUE, width = .1,
    fill = "darkorange", alpha = .5) +
  geom_half_violin(
    data = FB_2MZP_ts %>% filter(Timepoint == "1"), aes(x = Timepoint, y = Score), position = position_nudge(x = 1.3),
    side = "r", fill = "dodgerblue", alpha = .5, trim = FALSE) +
  geom_half_violin(
    data = FB_2MZP_ts %>% filter(Timepoint == "2"), aes(x = Timepoint, y = Score), position = position_nudge(x = .3),
    side = "r", fill = "darkorange", alpha = .5, trim = FALSE) +
  scale_x_continuous(breaks = c(1,2), labels = c("PRE", "POST"), limits = c(0, 3)) +
  xlab("Timepoint") + ylab("Score") +
  ggtitle("Paper-Pencil Data (1+1 Timepoints): Individual Pre-Post Means") +
  theme_classic()+
  coord_cartesian(ylim = c(0, 24))

ggsave("Time Series Dataframes/k25_FB_2MZP_Pre-Post_Box_Violin.jpg", plot = Pre_Post_Box_Violin, width = 6, height = 4)
save(Pre_Post_Box_Violin, file = "Time Series Dataframes/k25_FB_2MZP_Pre_Post_Box_Violin.RData")


# Repeated measures with box??? and violin plots and means + CIs
score_mean_1 = FB_2MZP_ts %>% filter(Timepoint == "1") %>% summarise(mean(Score)) %>% as.numeric()
score_mean_2 = FB_2MZP_ts %>% filter(Timepoint == "2") %>% summarise(mean(Score)) %>% as.numeric()
score_median1 = FB_2MZP_ts %>% filter(Timepoint == "1") %>% summarise(median(Score)) %>% as.numeric()
score_median2 = FB_2MZP_ts %>% filter(Timepoint == "2") %>% summarise(median(Score)) %>% as.numeric()
score_sd_1 = FB_2MZP_ts %>% filter(Timepoint == "1") %>% summarise(sd(Score)) %>% as.numeric()
score_sd_2 = FB_2MZP_ts %>% filter(Timepoint == "2") %>% summarise(sd(Score)) %>% as.numeric()
score_se_1 = score_sd_1/sqrt(nrow(FB_2MZP))
score_se_2 = score_sd_2/sqrt(nrow(FB_2MZP))
score_ci_1 = FB_2MZP_ts %>% filter(Timepoint == "1") %>% pull(Score) %>% CI(., ci = 0.95)
score_ci_2 = FB_2MZP_ts %>% filter(Timepoint == "2") %>% pull(Score) %>% CI(., ci = 0.95)
#Create data frame with 2 rows and 7 columns containing the descriptives
group = c("PRE", "POST")
N = c(nrow(FB_2MZP), nrow(FB_2MZP))
score_mean = c(score_mean_1, score_mean_2)
score_median = c(score_median1, score_median2)
sd = c(score_sd_1, score_sd_2)
se = c(score_se_1, score_se_2)
ci = c((score_ci_1[1] - score_ci_1[3]), (score_ci_2[1] - score_ci_2[3]))
summary_df = data.frame(group, N, score_mean, score_median, sd, se, ci)

# FB_2MZP_ts$jit = jitter(FB_2MZP_ts$Timepoint, amount = .09)     #already created above
x_tick_means = c(.87, 2.13)

Pre_Post_Box_Violin_Mean_CI = ggplot(data = FB_2MZP_ts, aes(y = Score)) +
  geom_point(data = FB_2MZP_ts %>% filter(Timepoint == "1"), aes(x = jit), color = "dodgerblue", size = 1,
             alpha = .6) +
  geom_point(data = FB_2MZP_ts %>% filter(Timepoint == "2"), aes(x = jit), color = "darkorange", size = 1,
             alpha = .6) +
  geom_line(aes(x = jit, group = ID), color = "lightgray", alpha = .05) +
  geom_half_boxplot(
    data = FB_2MZP_ts %>% filter(Timepoint == "1"), aes(x = Timepoint, y = Score), position = position_nudge(x = -.28),
    side = "r", outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
    fill = "dodgerblue") +
  geom_half_boxplot(
    data = FB_2MZP_ts %>% filter(Timepoint == "2"), aes(x = Timepoint, y = Score), position = position_nudge(x = .18),
    side = "r", outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
    fill = "darkorange") +
  geom_half_violin(
    data = FB_2MZP_ts %>% filter(Timepoint == "1"), aes(x = Timepoint, y = Score), position = position_nudge(x = -.3),
    side = "l", fill = "dodgerblue") +
  geom_half_violin(
    data = FB_2MZP_ts %>% filter(Timepoint == "2"),aes(x = Timepoint, y = Score), position = position_nudge(x = .3),
    side = "r", fill = "darkorange") +
  geom_point(data = FB_2MZP_ts %>% filter(Timepoint == "1"), aes(x = Timepoint, y = score_mean[1]),
             position = position_nudge(x = -.13), color = "dodgerblue", alpha = .6, size = 1.5) +
  geom_errorbar(data = FB_2MZP_ts %>% filter(Timepoint == "1"), aes(x = Timepoint, y = score_mean[1],
                                                                    ymin = score_mean[1]-ci[1], ymax = score_mean[1]+ci[1]),
                position = position_nudge(-.13), color = "dodgerblue", width = 0.05, size = 0.4, alpha = .6) +
  geom_point(data = FB_2MZP_ts %>% filter(Timepoint == "2"), aes(x = Timepoint, y = score_mean[2]),
             position = position_nudge(x = .13), color = "darkorange", alpha = .6, size = 1.5)+
  geom_errorbar(data = FB_2MZP_ts %>% filter(Timepoint == "2"), aes(x = Timepoint, y = score_mean[2],
                                                                    ymin = score_mean[2]-ci[2], ymax = score_mean[2]+ci[2]), 
                position = position_nudge(.13), color = "darkorange", width = 0.05, size = 0.4, alpha = .6) +
  geom_line(data = summary_df, aes(x = x_tick_means, y = score_mean), color = "gray", size = 1) +
  scale_x_continuous(breaks = c(1,2), labels = c("PRE", "POST"), limits = c(0, 3)) +
  xlab("Timepoint") + ylab("Score") +
  ggtitle("Paper-Pencil Data (1+1 Timepoints): Individual Pre-Post Means") +
  theme_classic()+
  coord_cartesian(ylim = c(0, 24))

ggsave("Time Series Dataframes/k25_FB_2MZP_Pre-Post_Box_Violin_Mean+CI.jpg", plot = Pre_Post_Box_Violin_Mean_CI, width = 6, height = 4)
save(Pre_Post_Box_Violin_Mean_CI, file = "Time Series Dataframes/k25_FB_2MZP_Pre_Post_Box_Violin_Mean_CI.RData")

