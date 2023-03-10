
######################################################################################
###################### time series plots with timetk #################################

library(tidyverse)
library(lubridate)
library(timetk)
library(ggplot2)

# base:
#plot(as.numeric(FB_30MZP_each[1,c(pre_mzp,post_mzp)]), type = "l")

#seq(as.Date("2020-01-01"), length.out=60, by="1 day")

##### RMD for-loop
for (i in 1:27) {
  tsplot = x %>%
    filter(ID == i) %>%
    plot_time_series(MZP, Score,
                     .facet_ncol = 1,
                     .facet_scales = "fixed",
                     .y_intercept = 0,
                     .x_lab = "Date",
                     .y_lab = "PHQ-9 Score",
                     .title = paste0("'Time Series Plot for ID = ", i, "'"),
                     .interactive = TRUE,
                     .plotly_slider = FALSE,
                     .facet_collapse = FALSE
    )
  print(tsplot)
}
##### RMD

##### RMD single
x %>%
  filter(ID == 1) %>%
  plot_time_series(MZP, Score,
                   .facet_ncol = 1,
                   .facet_scales = "fixed",
                   .y_intercept = 0,
                   .x_lab = "Date",
                   .y_lab = "PHQ-9 Score",
                   .title = "Time Series Plot for ID = 1",
                   .interactive = TRUE,
                   .plotly_slider = FALSE,
                   .facet_collapse = FALSE
  )
#####RMD



######################################################################################
######################################################################################
######### repeated-measures scatter-boxplot-violin-histograms with gghalves ##########

library("plyr")
library("lattice")
library("ggplot2")
library("dplyr")
library("readr")
library("rmarkdown")
library("Rmisc")
library("devtools")
library("gghalves")

set.seed(42)

setwd(
  "C:/Users/steph/OneDrive/Desktop/Psychologie/Masterstudium Psychologie/3. Semester/1. Master-Konversatorium/Preprocessing und Berechnungen"
)

FB_original = read.delim("cor_07_k1/cor_07_dataset_k1.txt", row.names=NULL) %>% 
  as_tibble() %>% 
  select(PRE1_1:POST1_5)

load("cor_07_k1_30MZP_each/FB_30MZP_each.RData")
FB_30MZP_each = FB_30MZP_each %>%
  as_tibble()

load("cor_07_k1_30MZP_each/FB_random_2MZP.RData")
FB_random_2MZP = FB_random_2MZP %>%
  as_tibble()

FB_original$PRE_Mean = apply(FB_original[c("PRE1_1","PRE1_2","PRE1_3","PRE1_4","PRE1_5")], 1, mean)
FB_original$POST_Mean = apply(FB_original[c("POST1_1","POST1_2","POST1_3","POST1_4","POST1_5")], 1, mean)
FB_original$MeanDiff = FB_original$PRE_Mean - FB_original$POST_Mean
FB_original$ind.pretestSD = apply(FB_original[c("PRE1_1","PRE1_2","PRE1_3","PRE1_4","PRE1_5")], 1, sd)
FB_original$ind.posttestSD = apply(FB_original[c("POST1_1","POST1_2","POST1_3","POST1_4","POST1_5")], 1, sd)
FB_random_2MZP$Diff = FB_random_2MZP$PRE - FB_random_2MZP$POST


# converting my dataframes to use in the same ggplot structure:

# FB_original
Mean = c()

for (i in 1:nrow(FB_original)) {
  Mean = c(Mean, FB_original[i,"PRE_Mean"], FB_original[i,"POST_Mean"])
  message(i)
}

FB_original_ts = tibble(ID = factor(rep(1:nrow(FB_original), each = 2)),
                        Interval = c(rep(c(1,2), times = nrow(FB_original))),
                        Mean = as.numeric(Mean))

save(FB_original_ts, file = "Time Series Dataframes/FB_original_ts.RData")


# FB_30MZP_each
Mean = c()

for (i in 1:nrow(FB_30MZP_each)) {
  Mean = c(Mean, FB_30MZP_each[i,"PRE_Mean"], FB_30MZP_each[i,"POST_Mean"])
  message(i)
}

FB_30MZP_each_ts = tibble(ID = factor(rep(1:nrow(FB_30MZP_each), each = 2)),
                          Interval = c(rep(c(1,2), times = nrow(FB_30MZP_each))),
                          Mean = as.numeric(Mean))

save(FB_30MZP_each_ts, file = "Time Series Dataframes/FB_30MZP_each_ts.RData")


# FB_random_2MZP
Score = c()

for (i in 1:nrow(FB_random_2MZP)) {
  Score = c(Score, FB_random_2MZP[i,"PRE"], FB_random_2MZP[i,"POST"])
  message(i)
}

FB_random_2MZP_ts = tibble(ID = factor(rep(1:nrow(FB_random_2MZP), each = 2)),
                           Timepoint = c(rep(c(1,2), times = nrow(FB_random_2MZP))),
                           Score = as.numeric(Score))

save(FB_random_2MZP_ts, file = "Time Series Dataframes/FB_random_2MZP_ts.RData")




# cool jitter boxplot violin plots for individual PRE and POST means
# from van Langen (2020) Open-visualizations tutorial for repeated measures in R
load("Time Series Dataframes/FB_original_ts.RData")


# Repeated measures individual datapoints
Pre_Post_stacked = ggplot(data = FB_original_ts, aes(y = Mean)) +
  geom_point(aes(x = Interval), color = "dodgerblue", size = 1) +
  scale_x_continuous(breaks = c(1, 2), labels = c("PRE", "POST"), limits = c(0, 3)) +
  xlab("Interval") + ylab("Mean") +
  ggtitle("Paper-Pencil Data (5+5 Timepoints): Individual Pre-Post Means") +
  theme_classic() +
  coord_cartesian(ylim = c(0, 24))

ggsave("Time Series Dataframes/FB_original_Pre-Post_stacked.jpg", plot = Pre_Post_stacked, width = 6, height = 4)
save(Pre_Post_stacked, file = "Time Series Dataframes/FB_original_Pre_Post_stacked.RData")


# Repeated measures with jitter and connections
FB_original_ts$jit = jitter(FB_original_ts$Interval, amount = .15)

Pre_Post_Jitter = ggplot(data = FB_original_ts, aes(y = Mean)) +
  geom_point(data = FB_original_ts %>% filter(Interval == "1"), aes(x = jit), color = "dodgerblue", size = 1,
             alpha = .6) +
  geom_point(data = FB_original_ts %>% filter(Interval == "2"), aes(x = jit), color = "darkorange", size = 1,
             alpha = .6) +
  geom_line(aes(x = jit, group = ID), color = "lightgray", alpha = .05) +
  scale_x_continuous(breaks = c(1,2), labels = c("PRE", "POST"), limits = c(0, 3)) +
  xlab("Interval") + ylab("Mean") +
  ggtitle("Paper-Pencil Data (5+5 Timepoints): Individual Pre-Post Means") +
  theme_classic()+
  coord_cartesian(ylim = c(0, 24))

ggsave("Time Series Dataframes/FB_original_Pre-Post_Jitter.jpg", plot = Pre_Post_Jitter, width = 6, height = 4)
save(Pre_Post_Jitter, file = "Time Series Dataframes/FB_original_Pre_Post_Jitter.RData")


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

ggsave("Time Series Dataframes/FB_original_Pre-Post_Box_Violin.jpg", plot = Pre_Post_Box_Violin, width = 6, height = 4)
save(Pre_Post_Box_Violin, file = "Time Series Dataframes/FB_original_Pre_Post_Box_Violin.RData")


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

ggsave("Time Series Dataframes/FB_original_Pre-Post_Box_Violin_Mean+CI.jpg", plot = Pre_Post_Box_Violin_Mean_CI, width = 6, height = 4)
save(Pre_Post_Box_Violin_Mean_CI, file = "Time Series Dataframes/FB_original_Pre_Post_Box_Violin_Mean_CI.RData")


############################# FB_30MZP_each_ts
load("Time Series Dataframes/FB_30MZP_each_ts.RData")


# Repeated measures individual datapoints
Pre_Post_stacked = ggplot(data = FB_30MZP_each_ts, aes(y = Mean)) +
  geom_point(aes(x = Interval), color = "dodgerblue", size = 1) +
  scale_x_continuous(breaks = c(1, 2), labels = c("PRE", "POST"), limits = c(0, 3)) +
  xlab("Interval") + ylab("Mean") +
  ggtitle("Paper-Pencil Data (30+30 Timepoints): Individual Pre-Post Means") +
  theme_classic() +
  coord_cartesian(ylim = c(0, 24))

ggsave("Time Series Dataframes/FB_30MZP_each_Pre-Post_stacked.jpg", plot = Pre_Post_stacked, width = 6, height = 4)
save(Pre_Post_stacked, file = "Time Series Dataframes/FB_30MZP_each_Pre_Post_stacked.RData")


# Repeated measures with jitter and connections
FB_30MZP_each_ts$jit = jitter(FB_30MZP_each_ts$Interval, amount = .15)

Pre_Post_Jitter = ggplot(data = FB_30MZP_each_ts, aes(y = Mean)) +
  geom_point(data = FB_30MZP_each_ts %>% filter(Interval == "1"), aes(x = jit), color = "dodgerblue", size = 1,
             alpha = .6) +
  geom_point(data = FB_30MZP_each_ts %>% filter(Interval == "2"), aes(x = jit), color = "darkorange", size = 1,
             alpha = .6) +
  geom_line(aes(x = jit, group = ID), color = "lightgray", alpha = .05) +
  scale_x_continuous(breaks = c(1,2), labels = c("PRE", "POST"), limits = c(0, 3)) +
  xlab("Interval") + ylab("Mean") +
  ggtitle("Paper-Pencil Data (30+30 Timepoints): Individual Pre-Post Means") +
  theme_classic()+
  coord_cartesian(ylim = c(0, 24))

ggsave("Time Series Dataframes/FB_30MZP_each_Pre-Post_Jitter.jpg", plot = Pre_Post_Jitter, width = 6, height = 4)
save(Pre_Post_Jitter, file = "Time Series Dataframes/FB_30MZP_each_Pre_Post_Jitter.RData")


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

ggsave("Time Series Dataframes/FB_30MZP_each_Pre-Post_Box_Violin.jpg", plot = Pre_Post_Box_Violin, width = 6, height = 4)
save(Pre_Post_Box_Violin, file = "Time Series Dataframes/FB_30MZP_each_Pre_Post_Box_Violin.RData")


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

ggsave("Time Series Dataframes/FB_30MZP_each_Pre-Post_Box_Violin_Mean+CI.jpg", plot = Pre_Post_Box_Violin_Mean_CI, width = 6, height = 4)
save(Pre_Post_Box_Violin_Mean_CI, file = "Time Series Dataframes/FB_30MZP_each_Pre_Post_Box_Violin_Mean_CI.RData")


############################# FB_random_2MZP_ts
load("Time Series Dataframes/FB_random_2MZP_ts.RData")


# Repeated measures individual datapoints
Pre_Post_stacked = ggplot(data = FB_random_2MZP_ts, aes(y = Score)) +
  geom_point(aes(x = Timepoint), color = "dodgerblue", size = 1) +
  scale_x_continuous(breaks = c(1, 2), labels = c("PRE", "POST"), limits = c(0, 3)) +
  xlab("Timepoint") + ylab("Score") +
  ggtitle("Paper-Pencil Data (1+1 Timepoints): Individual Pre-Post Means") +
  theme_classic() +
  coord_cartesian(ylim = c(0, 24))

ggsave("Time Series Dataframes/FB_random_2MZP_Pre-Post_stacked.jpg", plot = Pre_Post_stacked, width = 6, height = 4)
save(Pre_Post_stacked, file = "Time Series Dataframes/FB_random_2MZP_Pre_Post_stacked.RData")


# Repeated measures with jitter and connections
FB_random_2MZP_ts$jit = jitter(FB_random_2MZP_ts$Timepoint, amount = .15)

Pre_Post_Jitter = ggplot(data = FB_random_2MZP_ts, aes(y = Score)) +
  geom_point(data = FB_random_2MZP_ts %>% filter(Timepoint == "1"), aes(x = jit), color = "dodgerblue", size = 1,
             alpha = .6) +
  geom_point(data = FB_random_2MZP_ts %>% filter(Timepoint == "2"), aes(x = jit), color = "darkorange", size = 1,
             alpha = .6) +
  geom_line(aes(x = jit, group = ID), color = "lightgray", alpha = .05) +
  scale_x_continuous(breaks = c(1,2), labels = c("PRE", "POST"), limits = c(0, 3)) +
  xlab("Timepoint") + ylab("Score") +
  ggtitle("Paper-Pencil Data (1+1 Timepoints): Individual Pre-Post Means") +
  theme_classic()+
  coord_cartesian(ylim = c(0, 24))

ggsave("Time Series Dataframes/FB_random_2MZP_Pre-Post_Jitter.jpg", plot = Pre_Post_Jitter, width = 6, height = 4)
save(Pre_Post_Jitter, file = "Time Series Dataframes/FB_random_2MZP_Pre_Post_Jitter.RData")


# Repeated measures with box??? and violin plots
FB_random_2MZP_ts$jit = jitter(FB_random_2MZP_ts$Timepoint, amount = .09)

Pre_Post_Box_Violin = ggplot(data = FB_random_2MZP_ts, aes(y = Score)) +
  geom_point(data = FB_random_2MZP_ts %>% filter(Timepoint == "1"), aes(x = jit), color = "dodgerblue", size = 1,
             alpha = .5) +
  geom_point(data = FB_random_2MZP_ts %>% filter(Timepoint == "2"), aes(x = jit), color = "darkorange", size = 1,
             alpha = .5) +
  geom_line(aes(x = jit, group = ID), color = "lightgray", alpha = .05) +
  geom_half_boxplot(
    data = FB_random_2MZP_ts %>% filter(Timepoint == "1"), aes(x = Timepoint, y = Score), position = position_nudge(x = -.25),
    side = "r", outlier.shape = NA, center = TRUE, errorbar.draw = TRUE, width = .1,
    fill = "dodgerblue", alpha = .5) +
  geom_half_boxplot(
    data = FB_random_2MZP_ts %>% filter(Timepoint == "2"), aes(x = Timepoint, y = Score), position = position_nudge(x = .15),
    side = "r", outlier.shape = NA, center = TRUE, errorbar.draw = TRUE, width = .1,
    fill = "darkorange", alpha = .5) +
  geom_half_violin(
    data = FB_random_2MZP_ts %>% filter(Timepoint == "1"), aes(x = Timepoint, y = Score), position = position_nudge(x = 1.3),
    side = "r", fill = "dodgerblue", alpha = .5, trim = FALSE) +
  geom_half_violin(
    data = FB_random_2MZP_ts %>% filter(Timepoint == "2"), aes(x = Timepoint, y = Score), position = position_nudge(x = .3),
    side = "r", fill = "darkorange", alpha = .5, trim = FALSE) +
  scale_x_continuous(breaks = c(1,2), labels = c("PRE", "POST"), limits = c(0, 3)) +
  xlab("Timepoint") + ylab("Score") +
  ggtitle("Paper-Pencil Data (1+1 Timepoints): Individual Pre-Post Means") +
  theme_classic()+
  coord_cartesian(ylim = c(0, 24))

ggsave("Time Series Dataframes/FB_random_2MZP_Pre-Post_Box_Violin.jpg", plot = Pre_Post_Box_Violin, width = 6, height = 4)
save(Pre_Post_Box_Violin, file = "Time Series Dataframes/FB_random_2MZP_Pre_Post_Box_Violin.RData")


# Repeated measures with box??? and violin plots and means + CIs
score_mean_1 = FB_random_2MZP_ts %>% filter(Timepoint == "1") %>% summarise(mean(Score)) %>% as.numeric()
score_mean_2 = FB_random_2MZP_ts %>% filter(Timepoint == "2") %>% summarise(mean(Score)) %>% as.numeric()
score_median1 = FB_random_2MZP_ts %>% filter(Timepoint == "1") %>% summarise(median(Score)) %>% as.numeric()
score_median2 = FB_random_2MZP_ts %>% filter(Timepoint == "2") %>% summarise(median(Score)) %>% as.numeric()
score_sd_1 = FB_random_2MZP_ts %>% filter(Timepoint == "1") %>% summarise(sd(Score)) %>% as.numeric()
score_sd_2 = FB_random_2MZP_ts %>% filter(Timepoint == "2") %>% summarise(sd(Score)) %>% as.numeric()
score_se_1 = score_sd_1/sqrt(nrow(FB_random_2MZP))
score_se_2 = score_sd_2/sqrt(nrow(FB_random_2MZP))
score_ci_1 = FB_random_2MZP_ts %>% filter(Timepoint == "1") %>% pull(Score) %>% CI(., ci = 0.95)
score_ci_2 = FB_random_2MZP_ts %>% filter(Timepoint == "2") %>% pull(Score) %>% CI(., ci = 0.95)
#Create data frame with 2 rows and 7 columns containing the descriptives
group = c("PRE", "POST")
N = c(nrow(FB_random_2MZP), nrow(FB_random_2MZP))
score_mean = c(score_mean_1, score_mean_2)
score_median = c(score_median1, score_median2)
sd = c(score_sd_1, score_sd_2)
se = c(score_se_1, score_se_2)
ci = c((score_ci_1[1] - score_ci_1[3]), (score_ci_2[1] - score_ci_2[3]))
summary_df = data.frame(group, N, score_mean, score_median, sd, se, ci)

# FB_random_2MZP_ts$jit = jitter(FB_random_2MZP_ts$Timepoint, amount = .09)     #already created above
x_tick_means = c(.87, 2.13)

Pre_Post_Box_Violin_Mean_CI = ggplot(data = FB_random_2MZP_ts, aes(y = Score)) +
  geom_point(data = FB_random_2MZP_ts %>% filter(Timepoint == "1"), aes(x = jit), color = "dodgerblue", size = 1,
             alpha = .6) +
  geom_point(data = FB_random_2MZP_ts %>% filter(Timepoint == "2"), aes(x = jit), color = "darkorange", size = 1,
             alpha = .6) +
  geom_line(aes(x = jit, group = ID), color = "lightgray", alpha = .05) +
  geom_half_boxplot(
    data = FB_random_2MZP_ts %>% filter(Timepoint == "1"), aes(x = Timepoint, y = Score), position = position_nudge(x = -.28),
    side = "r", outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
    fill = "dodgerblue") +
  geom_half_boxplot(
    data = FB_random_2MZP_ts %>% filter(Timepoint == "2"), aes(x = Timepoint, y = Score), position = position_nudge(x = .18),
    side = "r", outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
    fill = "darkorange") +
  geom_half_violin(
    data = FB_random_2MZP_ts %>% filter(Timepoint == "1"), aes(x = Timepoint, y = Score), position = position_nudge(x = -.3),
    side = "l", fill = "dodgerblue") +
  geom_half_violin(
    data = FB_random_2MZP_ts %>% filter(Timepoint == "2"),aes(x = Timepoint, y = Score), position = position_nudge(x = .3),
    side = "r", fill = "darkorange") +
  geom_point(data = FB_random_2MZP_ts %>% filter(Timepoint == "1"), aes(x = Timepoint, y = score_mean[1]),
             position = position_nudge(x = -.13), color = "dodgerblue", alpha = .6, size = 1.5) +
  geom_errorbar(data = FB_random_2MZP_ts %>% filter(Timepoint == "1"), aes(x = Timepoint, y = score_mean[1],
                                                                           ymin = score_mean[1]-ci[1], ymax = score_mean[1]+ci[1]),
                position = position_nudge(-.13), color = "dodgerblue", width = 0.05, size = 0.4, alpha = .6) +
  geom_point(data = FB_random_2MZP_ts %>% filter(Timepoint == "2"), aes(x = Timepoint, y = score_mean[2]),
             position = position_nudge(x = .13), color = "darkorange", alpha = .6, size = 1.5)+
  geom_errorbar(data = FB_random_2MZP_ts %>% filter(Timepoint == "2"), aes(x = Timepoint, y = score_mean[2],
                                                                           ymin = score_mean[2]-ci[2], ymax = score_mean[2]+ci[2]), 
                position = position_nudge(.13), color = "darkorange", width = 0.05, size = 0.4, alpha = .6) +
  geom_line(data = summary_df, aes(x = x_tick_means, y = score_mean), color = "gray", size = 1) +
  scale_x_continuous(breaks = c(1,2), labels = c("PRE", "POST"), limits = c(0, 3)) +
  xlab("Timepoint") + ylab("Score") +
  ggtitle("Paper-Pencil Data (1+1 Timepoints): Individual Pre-Post Means") +
  theme_classic()+
  coord_cartesian(ylim = c(0, 24))

ggsave("Time Series Dataframes/FB_random_2MZP_Pre-Post_Box_Violin_Mean+CI.jpg", plot = Pre_Post_Box_Violin_Mean_CI, width = 6, height = 4)
save(Pre_Post_Box_Violin_Mean_CI, file = "Time Series Dataframes/FB_random_2MZP_Pre_Post_Box_Violin_Mean_CI.RData")



############################# EMA Preprocessing

set.seed(42)

EMA_original = read.delim("cor_04_k1/cor_04_dataset_k1.txt", row.names=NULL) %>% 
  as_tibble() %>% 
  select(PRE1_1:POST1_5)

load("cor_04_k1_30MZP_each/EMA_30MZP_each.RData")
EMA_30MZP_each = EMA_30MZP_each %>%
  as_tibble()

load("cor_04_k1_30MZP_each/EMA_random_Days.RData")
EMA_random_Days = EMA_random_Days %>%
  as_tibble()

load("cor_04_k1_30MZP_each/EMA_random_Window.RData")
EMA_random_Window = EMA_random_Window %>%
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


pre_5mzp = c("PRE1","PRE2","PRE3","PRE4","PRE5")
post_5mzp = c("POST1","POST2","POST3","POST4","POST5")

EMA_original$PRE_Mean = apply(EMA_original[c("PRE1_1","PRE1_2","PRE1_3","PRE1_4","PRE1_5")], 1, mean)
EMA_original$POST_Mean = apply(EMA_original[c("POST1_1","POST1_2","POST1_3","POST1_4","POST1_5")], 1, mean)
EMA_original$MeanDiff = EMA_original$PRE_Mean - EMA_original$POST_Mean
EMA_original$ind.pretestSD = apply(EMA_original[c("PRE1_1","PRE1_2","PRE1_3","PRE1_4","PRE1_5")], 1, sd)
EMA_original$ind.posttestSD = apply(EMA_original[c("POST1_1","POST1_2","POST1_3","POST1_4","POST1_5")], 1, sd)

EMA_random_Window$PRE_Mean = apply(EMA_random_Window[pre_5mzp], 1, mean)
EMA_random_Window$POST_Mean = apply(EMA_random_Window[post_5mzp], 1, mean)
EMA_random_Window$MeanDiff = EMA_random_Window$PRE_Mean - EMA_random_Window$POST_Mean
EMA_random_Window$ind.pretestSD = apply(EMA_random_Window[pre_5mzp], 1, sd)
EMA_random_Window$ind.posttestSD = apply(EMA_random_Window[post_5mzp], 1, sd)

EMA_random_Days$PRE_Mean = apply(EMA_random_Days[pre_5mzp], 1, mean)
EMA_random_Days$POST_Mean = apply(EMA_random_Days[post_5mzp], 1, mean)
EMA_random_Days$MeanDiff = EMA_random_Days$PRE_Mean - EMA_random_Days$POST_Mean
EMA_random_Days$ind.pretestSD = apply(EMA_random_Days[pre_5mzp], 1, sd)
EMA_random_Days$ind.posttestSD = apply(EMA_random_Days[post_5mzp], 1, sd)


############################# EMA_original_ts

# EMA_original
# converting my dataframes to use in the same ggplot structure:
Mean = c()

for (i in 1:nrow(EMA_original)) {
  Mean = c(Mean, EMA_original[i,"PRE_Mean"], EMA_original[i,"POST_Mean"])
  message(i)
}

EMA_original_ts = tibble(ID = factor(rep(1:nrow(EMA_original), each = 2)),
                         Interval = c(rep(c(1,2), times = nrow(EMA_original))),
                         Mean = as.numeric(Mean))

save(EMA_original_ts, file = "Time Series Dataframes/EMA_original_ts.RData")

###

#load("Time Series Dataframes/EMA_original_ts.RData")


# Repeated measures individual datapoints
Pre_Post_stacked = ggplot(data = EMA_original_ts, aes(y = Mean)) +
  geom_point(aes(x = Interval), color = "dodgerblue", size = 1) +
  scale_x_continuous(breaks = c(1, 2), labels = c("PRE", "POST"), limits = c(0, 3)) +
  xlab("Interval") + ylab("Mean") +
  ggtitle("EMA Data (5+5 Timepoints): Individual Pre-Post Means") +
  theme_classic() +
  coord_cartesian(ylim = c(0, 24))

Pre_Post_stacked

ggsave("Time Series Dataframes/EMA_original_Pre-Post_stacked.jpg", plot = Pre_Post_stacked, width = 6, height = 4)
save(Pre_Post_stacked, file = "Time Series Dataframes/EMA_original_Pre_Post_stacked.RData")


# Repeated measures with jitter and connections
EMA_original_ts$jit = jitter(EMA_original_ts$Interval, amount = .15)

Pre_Post_Jitter = ggplot(data = EMA_original_ts, aes(y = Mean)) +
  geom_point(data = EMA_original_ts %>% filter(Interval == "1"), aes(x = jit), color = "dodgerblue", size = 1,
             alpha = .6) +
  geom_point(data = EMA_original_ts %>% filter(Interval == "2"), aes(x = jit), color = "darkorange", size = 1,
             alpha = .6) +
  geom_line(aes(x = jit, group = ID), color = "lightgray", alpha = .05) +
  scale_x_continuous(breaks = c(1,2), labels = c("PRE", "POST"), limits = c(0, 3)) +
  xlab("Interval") + ylab("Mean") +
  ggtitle("EMA Data (5+5 Timepoints): Individual Pre-Post Means") +
  theme_classic()+
  coord_cartesian(ylim = c(0, 24))

Pre_Post_Jitter

ggsave("Time Series Dataframes/EMA_original_Pre-Post_Jitter.jpg", plot = Pre_Post_Jitter, width = 6, height = 4)
save(Pre_Post_Jitter, file = "Time Series Dataframes/EMA_original_Pre_Post_Jitter.RData")


# Repeated measures with box??? and violin plots
EMA_original_ts$jit = jitter(EMA_original_ts$Interval, amount = .09)

Pre_Post_Box_Violin = ggplot(data = EMA_original_ts, aes(y = Mean)) +
  geom_point(data = EMA_original_ts %>% filter(Interval == "1"), aes(x = jit), color = "dodgerblue", size = 1,
             alpha = .5) +
  geom_point(data = EMA_original_ts %>% filter(Interval == "2"), aes(x = jit), color = "darkorange", size = 1,
             alpha = .5) +
  geom_line(aes(x = jit, group = ID), color = "lightgray", alpha = .05) +
  geom_half_boxplot(
    data = EMA_original_ts %>% filter(Interval == "1"), aes(x = Interval, y = Mean), position = position_nudge(x = -.25),
    side = "r", outlier.shape = NA, center = TRUE, errorbar.draw = TRUE, width = .1,
    fill = "dodgerblue", alpha = .5) +
  geom_half_boxplot(
    data = EMA_original_ts %>% filter(Interval == "2"), aes(x = Interval, y = Mean), position = position_nudge(x = .15),
    side = "r", outlier.shape = NA, center = TRUE, errorbar.draw = TRUE, width = .1,
    fill = "darkorange", alpha = .5) +
  geom_half_violin(
    data = EMA_original_ts %>% filter(Interval == "1"), aes(x = Interval, y = Mean), position = position_nudge(x = 1.3),
    side = "r", fill = "dodgerblue", alpha = .5, trim = FALSE) +
  geom_half_violin(
    data = EMA_original_ts %>% filter(Interval == "2"), aes(x = Interval, y = Mean), position = position_nudge(x = .3),
    side = "r", fill = "darkorange", alpha = .5, trim = FALSE) +
  scale_x_continuous(breaks = c(1,2), labels = c("PRE", "POST"), limits = c(0, 3)) +
  xlab("Interval") + ylab("Mean") +
  ggtitle("EMA Data (5+5 Timepoints): Individual Pre-Post Means") +
  theme_classic()+
  coord_cartesian(ylim = c(0, 24))

Pre_Post_Box_Violin

ggsave("Time Series Dataframes/EMA_original_Pre-Post_Box_Violin.jpg", plot = Pre_Post_Box_Violin, width = 6, height = 4)
save(Pre_Post_Box_Violin, file = "Time Series Dataframes/EMA_original_Pre_Post_Box_Violin.RData")


# Repeated measures with box??? and violin plots and means + CIs
score_mean_1 = EMA_original_ts %>% filter(Interval == "1") %>% summarise(mean(Mean)) %>% as.numeric()
score_mean_2 = EMA_original_ts %>% filter(Interval == "2") %>% summarise(mean(Mean)) %>% as.numeric()
score_median1 = EMA_original_ts %>% filter(Interval == "1") %>% summarise(median(Mean)) %>% as.numeric()
score_median2 = EMA_original_ts %>% filter(Interval == "2") %>% summarise(median(Mean)) %>% as.numeric()
score_sd_1 = EMA_original_ts %>% filter(Interval == "1") %>% summarise(sd(Mean)) %>% as.numeric()
score_sd_2 = EMA_original_ts %>% filter(Interval == "2") %>% summarise(sd(Mean)) %>% as.numeric()
score_se_1 = score_sd_1/sqrt(nrow(EMA_original))
score_se_2 = score_sd_2/sqrt(nrow(EMA_original))
score_ci_1 = EMA_original_ts %>% filter(Interval == "1") %>% pull(Mean) %>% CI(., ci = 0.95)
score_ci_2 = EMA_original_ts %>% filter(Interval == "2") %>% pull(Mean) %>% CI(., ci = 0.95)
#Create data frame with 2 rows and 7 columns containing the descriptives
group = c("PRE", "POST")
N = c(nrow(EMA_original), nrow(EMA_original))
score_mean = c(score_mean_1, score_mean_2)
score_median = c(score_median1, score_median2)
sd = c(score_sd_1, score_sd_2)
se = c(score_se_1, score_se_2)
ci = c(as.numeric(score_ci_1[1] - score_ci_1[3]), as.numeric(score_ci_2[1] - score_ci_2[3]))
summary_df = data.frame(group, N, score_mean, score_median, sd, se, ci)

# EMA_original_ts$jit = jitter(EMA_original_ts$Interval, amount = .09)     #already created above
x_tick_means = c(.87, 2.13)

Pre_Post_Box_Violin_Mean_CI = ggplot(data = EMA_original_ts, aes(y = Mean)) +
  geom_point(data = EMA_original_ts %>% filter(Interval == "1"), aes(x = jit), color = "dodgerblue", size = 1,
             alpha = .6) +
  geom_point(data = EMA_original_ts %>% filter(Interval == "2"), aes(x = jit), color = "darkorange", size = 1,
             alpha = .6) +
  geom_line(aes(x = jit, group = ID), color = "lightgray", alpha = .05) +
  geom_half_boxplot(
    data = EMA_original_ts %>% filter(Interval == "1"), aes(x = Interval, y = Mean), position = position_nudge(x = -.28),
    side = "r", outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
    fill = "dodgerblue") +
  geom_half_boxplot(
    data = EMA_original_ts %>% filter(Interval == "2"), aes(x = Interval, y = Mean), position = position_nudge(x = .18),
    side = "r", outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
    fill = "darkorange") +
  geom_half_violin(
    data = EMA_original_ts %>% filter(Interval == "1"), aes(x = Interval, y = Mean), position = position_nudge(x = -.3),
    side = "l", fill = "dodgerblue") +
  geom_half_violin(
    data = EMA_original_ts %>% filter(Interval == "2"),aes(x = Interval, y = Mean), position = position_nudge(x = .3),
    side = "r", fill = "darkorange") +
  geom_point(data = EMA_original_ts %>% filter(Interval == "1"), aes(x = Interval, y = score_mean[1]),
             position = position_nudge(x = -.13), color = "dodgerblue", alpha = .6, size = 1.5) +
  geom_errorbar(data = EMA_original_ts %>% filter(Interval == "1"), aes(x = Interval, y = score_mean[1],
                                                                        ymin = score_mean[1]-ci[1], ymax = score_mean[1]+ci[1]),
                position = position_nudge(-.13), color = "dodgerblue", width = 0.05, size = 0.4, alpha = .6) +
  geom_point(data = EMA_original_ts %>% filter(Interval == "2"), aes(x = Interval, y = score_mean[2]),
             position = position_nudge(x = .13), color = "darkorange", alpha = .6, size = 1.5)+
  geom_errorbar(data = EMA_original_ts %>% filter(Interval == "2"), aes(x = Interval, y = score_mean[2],
                                                                        ymin = score_mean[2]-ci[2], ymax = score_mean[2]+ci[2]), 
                position = position_nudge(.13), color = "darkorange", width = 0.05, size = 0.4, alpha = .6) +
  geom_line(data = summary_df, aes(x = x_tick_means, y = score_mean), color = "gray", size = 1) +
  scale_x_continuous(breaks = c(1,2), labels = c("PRE", "POST"), limits = c(0, 3)) +
  xlab("Interval") + ylab("Mean") +
  ggtitle("EMA Data (5+5 Timepoints): Individual Pre-Post Means") +
  theme_classic()+
  coord_cartesian(ylim = c(0, 24))

Pre_Post_Box_Violin_Mean_CI

ggsave("Time Series Dataframes/EMA_original_Pre-Post_Box_Violin_Mean+CI.jpg", plot = Pre_Post_Box_Violin_Mean_CI, width = 6, height = 4)
save(Pre_Post_Box_Violin_Mean_CI, file = "Time Series Dataframes/EMA_original_Pre_Post_Box_Violin_Mean_CI.RData")


############################# EMA_30MZP_each_ts

Mean = c()

for (i in 1:nrow(EMA_30MZP_each)) {
  Mean = c(Mean, EMA_30MZP_each[i,"PRE_Mean"], EMA_30MZP_each[i,"POST_Mean"])
  message(i)
}

EMA_30MZP_each_ts = tibble(ID = factor(rep(1:nrow(EMA_30MZP_each), each = 2)),
                           Interval = c(rep(c(1,2), times = nrow(EMA_30MZP_each))),
                           Mean = as.numeric(Mean))

save(EMA_30MZP_each_ts, file = "Time Series Dataframes/EMA_30MZP_each_ts.RData")

###

#load("Time Series Dataframes/EMA_30MZP_each_ts.RData")


# Repeated measures individual datapoints
Pre_Post_stacked = ggplot(data = EMA_30MZP_each_ts, aes(y = Mean)) +
  geom_point(aes(x = Interval), color = "dodgerblue", size = 1) +
  scale_x_continuous(breaks = c(1, 2), labels = c("PRE", "POST"), limits = c(0, 3)) +
  xlab("Interval") + ylab("Mean") +
  ggtitle("EMA Data (30+30 Timepoints): Individual Pre-Post Means") +
  theme_classic() +
  coord_cartesian(ylim = c(0, 24))

Pre_Post_stacked

ggsave("Time Series Dataframes/EMA_30MZP_each_Pre-Post_stacked.jpg", plot = Pre_Post_stacked, width = 6, height = 4)
save(Pre_Post_stacked, file = "Time Series Dataframes/EMA_30MZP_each_Pre_Post_stacked.RData")


# Repeated measures with jitter and connections
EMA_30MZP_each_ts$jit = jitter(EMA_30MZP_each_ts$Interval, amount = .15)

Pre_Post_Jitter = ggplot(data = EMA_30MZP_each_ts, aes(y = Mean)) +
  geom_point(data = EMA_30MZP_each_ts %>% filter(Interval == "1"), aes(x = jit), color = "dodgerblue", size = 1,
             alpha = .6) +
  geom_point(data = EMA_30MZP_each_ts %>% filter(Interval == "2"), aes(x = jit), color = "darkorange", size = 1,
             alpha = .6) +
  geom_line(aes(x = jit, group = ID), color = "lightgray", alpha = .05) +
  scale_x_continuous(breaks = c(1,2), labels = c("PRE", "POST"), limits = c(0, 3)) +
  xlab("Interval") + ylab("Mean") +
  ggtitle("EMA Data (30+30 Timepoints): Individual Pre-Post Means") +
  theme_classic()+
  coord_cartesian(ylim = c(0, 24))

Pre_Post_Jitter

ggsave("Time Series Dataframes/EMA_30MZP_each_Pre-Post_Jitter.jpg", plot = Pre_Post_Jitter, width = 6, height = 4)
save(Pre_Post_Jitter, file = "Time Series Dataframes/EMA_30MZP_each_Pre_Post_Jitter.RData")


# Repeated measures with box??? and violin plots
EMA_30MZP_each_ts$jit = jitter(EMA_30MZP_each_ts$Interval, amount = .09)

Pre_Post_Box_Violin = ggplot(data = EMA_30MZP_each_ts, aes(y = Mean)) +
  geom_point(data = EMA_30MZP_each_ts %>% filter(Interval == "1"), aes(x = jit), color = "dodgerblue", size = 1,
             alpha = .5) +
  geom_point(data = EMA_30MZP_each_ts %>% filter(Interval == "2"), aes(x = jit), color = "darkorange", size = 1,
             alpha = .5) +
  geom_line(aes(x = jit, group = ID), color = "lightgray", alpha = .05) +
  geom_half_boxplot(
    data = EMA_30MZP_each_ts %>% filter(Interval == "1"), aes(x = Interval, y = Mean), position = position_nudge(x = -.25),
    side = "r", outlier.shape = NA, center = TRUE, errorbar.draw = TRUE, width = .1,
    fill = "dodgerblue", alpha = .5) +
  geom_half_boxplot(
    data = EMA_30MZP_each_ts %>% filter(Interval == "2"), aes(x = Interval, y = Mean), position = position_nudge(x = .15),
    side = "r", outlier.shape = NA, center = TRUE, errorbar.draw = TRUE, width = .1,
    fill = "darkorange", alpha = .5) +
  geom_half_violin(
    data = EMA_30MZP_each_ts %>% filter(Interval == "1"), aes(x = Interval, y = Mean), position = position_nudge(x = 1.3),
    side = "r", fill = "dodgerblue", alpha = .5, trim = FALSE) +
  geom_half_violin(
    data = EMA_30MZP_each_ts %>% filter(Interval == "2"), aes(x = Interval, y = Mean), position = position_nudge(x = .3),
    side = "r", fill = "darkorange", alpha = .5, trim = FALSE) +
  scale_x_continuous(breaks = c(1,2), labels = c("PRE", "POST"), limits = c(0, 3)) +
  xlab("Interval") + ylab("Mean") +
  ggtitle("EMA Data (30+30 Timepoints): Individual Pre-Post Means") +
  theme_classic()+
  coord_cartesian(ylim = c(0, 24))

Pre_Post_Box_Violin

ggsave("Time Series Dataframes/EMA_30MZP_each_Pre-Post_Box_Violin.jpg", plot = Pre_Post_Box_Violin, width = 6, height = 4)
save(Pre_Post_Box_Violin, file = "Time Series Dataframes/EMA_30MZP_each_Pre_Post_Box_Violin.RData")


# Repeated measures with box??? and violin plots and means + CIs
score_mean_1 = EMA_30MZP_each_ts %>% filter(Interval == "1") %>% summarise(mean(Mean)) %>% as.numeric()
score_mean_2 = EMA_30MZP_each_ts %>% filter(Interval == "2") %>% summarise(mean(Mean)) %>% as.numeric()
score_median1 = EMA_30MZP_each_ts %>% filter(Interval == "1") %>% summarise(median(Mean)) %>% as.numeric()
score_median2 = EMA_30MZP_each_ts %>% filter(Interval == "2") %>% summarise(median(Mean)) %>% as.numeric()
score_sd_1 = EMA_30MZP_each_ts %>% filter(Interval == "1") %>% summarise(sd(Mean)) %>% as.numeric()
score_sd_2 = EMA_30MZP_each_ts %>% filter(Interval == "2") %>% summarise(sd(Mean)) %>% as.numeric()
score_se_1 = score_sd_1/sqrt(nrow(EMA_30MZP_each))
score_se_2 = score_sd_2/sqrt(nrow(EMA_30MZP_each))
score_ci_1 = EMA_30MZP_each_ts %>% filter(Interval == "1") %>% pull(Mean) %>% CI(., ci = 0.95)
score_ci_2 = EMA_30MZP_each_ts %>% filter(Interval == "2") %>% pull(Mean) %>% CI(., ci = 0.95)
#Create data frame with 2 rows and 7 columns containing the descriptives
group = c("PRE", "POST")
N = c(nrow(EMA_30MZP_each), nrow(EMA_30MZP_each))
score_mean = c(score_mean_1, score_mean_2)
score_median = c(score_median1, score_median2)
sd = c(score_sd_1, score_sd_2)
se = c(score_se_1, score_se_2)
ci = c(as.numeric(score_ci_1[1] - score_ci_1[3]), as.numeric(score_ci_2[1] - score_ci_2[3]))
summary_df = data.frame(group, N, score_mean, score_median, sd, se, ci)

# EMA_30MZP_each_ts$jit = jitter(EMA_30MZP_each_ts$Interval, amount = .09)     #already created above
x_tick_means = c(.87, 2.13)

Pre_Post_Box_Violin_Mean_CI = ggplot(data = EMA_30MZP_each_ts, aes(y = Mean)) +
  geom_point(data = EMA_30MZP_each_ts %>% filter(Interval == "1"), aes(x = jit), color = "dodgerblue", size = 1,
             alpha = .6) +
  geom_point(data = EMA_30MZP_each_ts %>% filter(Interval == "2"), aes(x = jit), color = "darkorange", size = 1,
             alpha = .6) +
  geom_line(aes(x = jit, group = ID), color = "lightgray", alpha = .05) +
  geom_half_boxplot(
    data = EMA_30MZP_each_ts %>% filter(Interval == "1"), aes(x = Interval, y = Mean), position = position_nudge(x = -.28),
    side = "r", outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
    fill = "dodgerblue") +
  geom_half_boxplot(
    data = EMA_30MZP_each_ts %>% filter(Interval == "2"), aes(x = Interval, y = Mean), position = position_nudge(x = .18),
    side = "r", outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
    fill = "darkorange") +
  geom_half_violin(
    data = EMA_30MZP_each_ts %>% filter(Interval == "1"), aes(x = Interval, y = Mean), position = position_nudge(x = -.3),
    side = "l", fill = "dodgerblue") +
  geom_half_violin(
    data = EMA_30MZP_each_ts %>% filter(Interval == "2"),aes(x = Interval, y = Mean), position = position_nudge(x = .3),
    side = "r", fill = "darkorange") +
  geom_point(data = EMA_30MZP_each_ts %>% filter(Interval == "1"), aes(x = Interval, y = score_mean[1]),
             position = position_nudge(x = -.13), color = "dodgerblue", alpha = .6, size = 1.5) +
  geom_errorbar(data = EMA_30MZP_each_ts %>% filter(Interval == "1"), aes(x = Interval, y = score_mean[1],
                                                                          ymin = score_mean[1]-ci[1], ymax = score_mean[1]+ci[1]),
                position = position_nudge(-.13), color = "dodgerblue", width = 0.05, size = 0.4, alpha = .6) +
  geom_point(data = EMA_30MZP_each_ts %>% filter(Interval == "2"), aes(x = Interval, y = score_mean[2]),
             position = position_nudge(x = .13), color = "darkorange", alpha = .6, size = 1.5)+
  geom_errorbar(data = EMA_30MZP_each_ts %>% filter(Interval == "2"), aes(x = Interval, y = score_mean[2],
                                                                          ymin = score_mean[2]-ci[2], ymax = score_mean[2]+ci[2]), 
                position = position_nudge(.13), color = "darkorange", width = 0.05, size = 0.4, alpha = .6) +
  geom_line(data = summary_df, aes(x = x_tick_means, y = score_mean), color = "gray", size = 1) +
  scale_x_continuous(breaks = c(1,2), labels = c("PRE", "POST"), limits = c(0, 3)) +
  xlab("Interval") + ylab("Mean") +
  ggtitle("EMA Data (30+30 Timepoints): Individual Pre-Post Means") +
  theme_classic()+
  coord_cartesian(ylim = c(0, 24))

Pre_Post_Box_Violin_Mean_CI

ggsave("Time Series Dataframes/EMA_30MZP_each_Pre-Post_Box_Violin_Mean+CI.jpg", plot = Pre_Post_Box_Violin_Mean_CI, width = 6, height = 4)
save(Pre_Post_Box_Violin_Mean_CI, file = "Time Series Dataframes/EMA_30MZP_each_Pre_Post_Box_Violin_Mean_CI.RData")


############################# EMA_random_Window_ts

Mean = c()

for (i in 1:nrow(EMA_random_Window)) {
  Mean = c(Mean, EMA_random_Window[i,"PRE_Mean"], EMA_random_Window[i,"POST_Mean"])
  message(i)
}

EMA_random_Window_ts = tibble(ID = factor(rep(1:nrow(EMA_random_Window), each = 2)),
                              Interval = c(rep(c(1,2), times = nrow(EMA_random_Window))),
                              Mean = as.numeric(Mean))

save(EMA_random_Window_ts, file = "Time Series Dataframes/EMA_random_Window_ts.RData")

###

#load("Time Series Dataframes/EMA_random_Window_ts.RData")


# Repeated measures individual datapoints
Pre_Post_stacked = ggplot(data = EMA_random_Window_ts, aes(y = Mean)) +
  geom_point(aes(x = Interval), color = "dodgerblue", size = 1) +
  scale_x_continuous(breaks = c(1, 2), labels = c("PRE", "POST"), limits = c(0, 3)) +
  xlab("Interval") + ylab("Mean") +
  ggtitle("EMA Data (5+5 Timepoint Random Windows): Individual Pre-Post Means") +
  theme_classic() +
  coord_cartesian(ylim = c(0, 24))

Pre_Post_stacked

ggsave("Time Series Dataframes/EMA_random_Window_Pre-Post_stacked.jpg", plot = Pre_Post_stacked, width = 6, height = 4)
save(Pre_Post_stacked, file = "Time Series Dataframes/EMA_random_Window_Pre_Post_stacked.RData")


# Repeated measures with jitter and connections
EMA_random_Window_ts$jit = jitter(EMA_random_Window_ts$Interval, amount = .15)

Pre_Post_Jitter = ggplot(data = EMA_random_Window_ts, aes(y = Mean)) +
  geom_point(data = EMA_random_Window_ts %>% filter(Interval == "1"), aes(x = jit), color = "dodgerblue", size = 1,
             alpha = .6) +
  geom_point(data = EMA_random_Window_ts %>% filter(Interval == "2"), aes(x = jit), color = "darkorange", size = 1,
             alpha = .6) +
  geom_line(aes(x = jit, group = ID), color = "lightgray", alpha = .05) +
  scale_x_continuous(breaks = c(1,2), labels = c("PRE", "POST"), limits = c(0, 3)) +
  xlab("Interval") + ylab("Mean") +
  ggtitle("EMA Data (5+5 Timepoint Random Windows): Individual Pre-Post Means") +
  theme_classic()+
  coord_cartesian(ylim = c(0, 24))

Pre_Post_Jitter

ggsave("Time Series Dataframes/EMA_random_Window_Pre-Post_Jitter.jpg", plot = Pre_Post_Jitter, width = 6, height = 4)
save(Pre_Post_Jitter, file = "Time Series Dataframes/EMA_random_Window_Pre_Post_Jitter.RData")


# Repeated measures with box??? and violin plots
EMA_random_Window_ts$jit = jitter(EMA_random_Window_ts$Interval, amount = .09)

Pre_Post_Box_Violin = ggplot(data = EMA_random_Window_ts, aes(y = Mean)) +
  geom_point(data = EMA_random_Window_ts %>% filter(Interval == "1"), aes(x = jit), color = "dodgerblue", size = 1,
             alpha = .5) +
  geom_point(data = EMA_random_Window_ts %>% filter(Interval == "2"), aes(x = jit), color = "darkorange", size = 1,
             alpha = .5) +
  geom_line(aes(x = jit, group = ID), color = "lightgray", alpha = .05) +
  geom_half_boxplot(
    data = EMA_random_Window_ts %>% filter(Interval == "1"), aes(x = Interval, y = Mean), position = position_nudge(x = -.25),
    side = "r", outlier.shape = NA, center = TRUE, errorbar.draw = TRUE, width = .1,
    fill = "dodgerblue", alpha = .5) +
  geom_half_boxplot(
    data = EMA_random_Window_ts %>% filter(Interval == "2"), aes(x = Interval, y = Mean), position = position_nudge(x = .15),
    side = "r", outlier.shape = NA, center = TRUE, errorbar.draw = TRUE, width = .1,
    fill = "darkorange", alpha = .5) +
  geom_half_violin(
    data = EMA_random_Window_ts %>% filter(Interval == "1"), aes(x = Interval, y = Mean), position = position_nudge(x = 1.3),
    side = "r", fill = "dodgerblue", alpha = .5, trim = FALSE) +
  geom_half_violin(
    data = EMA_random_Window_ts %>% filter(Interval == "2"), aes(x = Interval, y = Mean), position = position_nudge(x = .3),
    side = "r", fill = "darkorange", alpha = .5, trim = FALSE) +
  scale_x_continuous(breaks = c(1,2), labels = c("PRE", "POST"), limits = c(0, 3)) +
  xlab("Interval") + ylab("Mean") +
  ggtitle("EMA Data (5+5 Timepoint Random Windows): Individual Pre-Post Means") +
  theme_classic()+
  coord_cartesian(ylim = c(0, 24))

Pre_Post_Box_Violin

ggsave("Time Series Dataframes/EMA_random_Window_Pre-Post_Box_Violin.jpg", plot = Pre_Post_Box_Violin, width = 6, height = 4)
save(Pre_Post_Box_Violin, file = "Time Series Dataframes/EMA_random_Window_Pre_Post_Box_Violin.RData")


# Repeated measures with box??? and violin plots and means + CIs
score_mean_1 = EMA_random_Window_ts %>% filter(Interval == "1") %>% summarise(mean(Mean)) %>% as.numeric()
score_mean_2 = EMA_random_Window_ts %>% filter(Interval == "2") %>% summarise(mean(Mean)) %>% as.numeric()
score_median1 = EMA_random_Window_ts %>% filter(Interval == "1") %>% summarise(median(Mean)) %>% as.numeric()
score_median2 = EMA_random_Window_ts %>% filter(Interval == "2") %>% summarise(median(Mean)) %>% as.numeric()
score_sd_1 = EMA_random_Window_ts %>% filter(Interval == "1") %>% summarise(sd(Mean)) %>% as.numeric()
score_sd_2 = EMA_random_Window_ts %>% filter(Interval == "2") %>% summarise(sd(Mean)) %>% as.numeric()
score_se_1 = score_sd_1/sqrt(nrow(EMA_random_Window))
score_se_2 = score_sd_2/sqrt(nrow(EMA_random_Window))
score_ci_1 = EMA_random_Window_ts %>% filter(Interval == "1") %>% pull(Mean) %>% CI(., ci = 0.95)
score_ci_2 = EMA_random_Window_ts %>% filter(Interval == "2") %>% pull(Mean) %>% CI(., ci = 0.95)
#Create data frame with 2 rows and 7 columns containing the descriptives
group = c("PRE", "POST")
N = c(nrow(EMA_random_Window), nrow(EMA_random_Window))
score_mean = c(score_mean_1, score_mean_2)
score_median = c(score_median1, score_median2)
sd = c(score_sd_1, score_sd_2)
se = c(score_se_1, score_se_2)
ci = c(as.numeric(score_ci_1[1] - score_ci_1[3]), as.numeric(score_ci_2[1] - score_ci_2[3]))
summary_df = data.frame(group, N, score_mean, score_median, sd, se, ci)

# EMA_random_Window_ts$jit = jitter(EMA_random_Window_ts$Interval, amount = .09)     #already created above
x_tick_means = c(.87, 2.13)

Pre_Post_Box_Violin_Mean_CI = ggplot(data = EMA_random_Window_ts, aes(y = Mean)) +
  geom_point(data = EMA_random_Window_ts %>% filter(Interval == "1"), aes(x = jit), color = "dodgerblue", size = 1,
             alpha = .6) +
  geom_point(data = EMA_random_Window_ts %>% filter(Interval == "2"), aes(x = jit), color = "darkorange", size = 1,
             alpha = .6) +
  geom_line(aes(x = jit, group = ID), color = "lightgray", alpha = .05) +
  geom_half_boxplot(
    data = EMA_random_Window_ts %>% filter(Interval == "1"), aes(x = Interval, y = Mean), position = position_nudge(x = -.28),
    side = "r", outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
    fill = "dodgerblue") +
  geom_half_boxplot(
    data = EMA_random_Window_ts %>% filter(Interval == "2"), aes(x = Interval, y = Mean), position = position_nudge(x = .18),
    side = "r", outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
    fill = "darkorange") +
  geom_half_violin(
    data = EMA_random_Window_ts %>% filter(Interval == "1"), aes(x = Interval, y = Mean), position = position_nudge(x = -.3),
    side = "l", fill = "dodgerblue") +
  geom_half_violin(
    data = EMA_random_Window_ts %>% filter(Interval == "2"),aes(x = Interval, y = Mean), position = position_nudge(x = .3),
    side = "r", fill = "darkorange") +
  geom_point(data = EMA_random_Window_ts %>% filter(Interval == "1"), aes(x = Interval, y = score_mean[1]),
             position = position_nudge(x = -.13), color = "dodgerblue", alpha = .6, size = 1.5) +
  geom_errorbar(data = EMA_random_Window_ts %>% filter(Interval == "1"), aes(x = Interval, y = score_mean[1],
                                                                             ymin = score_mean[1]-ci[1], ymax = score_mean[1]+ci[1]),
                position = position_nudge(-.13), color = "dodgerblue", width = 0.05, size = 0.4, alpha = .6) +
  geom_point(data = EMA_random_Window_ts %>% filter(Interval == "2"), aes(x = Interval, y = score_mean[2]),
             position = position_nudge(x = .13), color = "darkorange", alpha = .6, size = 1.5)+
  geom_errorbar(data = EMA_random_Window_ts %>% filter(Interval == "2"), aes(x = Interval, y = score_mean[2],
                                                                             ymin = score_mean[2]-ci[2], ymax = score_mean[2]+ci[2]), 
                position = position_nudge(.13), color = "darkorange", width = 0.05, size = 0.4, alpha = .6) +
  geom_line(data = summary_df, aes(x = x_tick_means, y = score_mean), color = "gray", size = 1) +
  scale_x_continuous(breaks = c(1,2), labels = c("PRE", "POST"), limits = c(0, 3)) +
  xlab("Interval") + ylab("Mean") +
  ggtitle("EMA Data (5+5 Timepoint Random Windows): Individual Pre-Post Means") +
  theme_classic()+
  coord_cartesian(ylim = c(0, 24))

Pre_Post_Box_Violin_Mean_CI

ggsave("Time Series Dataframes/EMA_random_Window_Pre-Post_Box_Violin_Mean+CI.jpg", plot = Pre_Post_Box_Violin_Mean_CI, width = 6, height = 4)
save(Pre_Post_Box_Violin_Mean_CI, file = "Time Series Dataframes/EMA_random_Window_Pre_Post_Box_Violin_Mean_CI.RData")


############################# EMA_random_Days_ts

Mean = c()

for (i in 1:nrow(EMA_random_Days)) {
  Mean = c(Mean, EMA_random_Days[i,"PRE_Mean"], EMA_random_Days[i,"POST_Mean"])
  message(i)
}

EMA_random_Days_ts = tibble(ID = factor(rep(1:nrow(EMA_random_Days), each = 2)),
                            Interval = c(rep(c(1,2), times = nrow(EMA_random_Days))),
                            Mean = as.numeric(Mean))

save(EMA_random_Days_ts, file = "Time Series Dataframes/EMA_random_Days_ts.RData")

###

#load("Time Series Dataframes/EMA_random_Days_ts.RData")


# Repeated measures individual datapoints
Pre_Post_stacked = ggplot(data = EMA_random_Days_ts, aes(y = Mean)) +
  geom_point(aes(x = Interval), color = "dodgerblue", size = 1) +
  scale_x_continuous(breaks = c(1, 2), labels = c("PRE", "POST"), limits = c(0, 3)) +
  xlab("Interval") + ylab("Mean") +
  ggtitle("EMA Data (5+5 Timepoint Random Days): Individual Pre-Post Means") +
  theme_classic() +
  coord_cartesian(ylim = c(0, 24))

Pre_Post_stacked

ggsave("Time Series Dataframes/EMA_random_Days_Pre-Post_stacked.jpg", plot = Pre_Post_stacked, width = 6, height = 4)
save(Pre_Post_stacked, file = "Time Series Dataframes/EMA_random_Days_Pre_Post_stacked.RData")


# Repeated measures with jitter and connections
EMA_random_Days_ts$jit = jitter(EMA_random_Days_ts$Interval, amount = .15)

Pre_Post_Jitter = ggplot(data = EMA_random_Days_ts, aes(y = Mean)) +
  geom_point(data = EMA_random_Days_ts %>% filter(Interval == "1"), aes(x = jit), color = "dodgerblue", size = 1,
             alpha = .6) +
  geom_point(data = EMA_random_Days_ts %>% filter(Interval == "2"), aes(x = jit), color = "darkorange", size = 1,
             alpha = .6) +
  geom_line(aes(x = jit, group = ID), color = "lightgray", alpha = .05) +
  scale_x_continuous(breaks = c(1,2), labels = c("PRE", "POST"), limits = c(0, 3)) +
  xlab("Interval") + ylab("Mean") +
  ggtitle("EMA Data (5+5 Timepoint Random Days): Individual Pre-Post Means") +
  theme_classic()+
  coord_cartesian(ylim = c(0, 24))

Pre_Post_Jitter

ggsave("Time Series Dataframes/EMA_random_Days_Pre-Post_Jitter.jpg", plot = Pre_Post_Jitter, width = 6, height = 4)
save(Pre_Post_Jitter, file = "Time Series Dataframes/EMA_random_Days_Pre_Post_Jitter.RData")


# Repeated measures with box??? and violin plots
EMA_random_Days_ts$jit = jitter(EMA_random_Days_ts$Interval, amount = .09)

Pre_Post_Box_Violin = ggplot(data = EMA_random_Days_ts, aes(y = Mean)) +
  geom_point(data = EMA_random_Days_ts %>% filter(Interval == "1"), aes(x = jit), color = "dodgerblue", size = 1,
             alpha = .5) +
  geom_point(data = EMA_random_Days_ts %>% filter(Interval == "2"), aes(x = jit), color = "darkorange", size = 1,
             alpha = .5) +
  geom_line(aes(x = jit, group = ID), color = "lightgray", alpha = .05) +
  geom_half_boxplot(
    data = EMA_random_Days_ts %>% filter(Interval == "1"), aes(x = Interval, y = Mean), position = position_nudge(x = -.25),
    side = "r", outlier.shape = NA, center = TRUE, errorbar.draw = TRUE, width = .1,
    fill = "dodgerblue", alpha = .5) +
  geom_half_boxplot(
    data = EMA_random_Days_ts %>% filter(Interval == "2"), aes(x = Interval, y = Mean), position = position_nudge(x = .15),
    side = "r", outlier.shape = NA, center = TRUE, errorbar.draw = TRUE, width = .1,
    fill = "darkorange", alpha = .5) +
  geom_half_violin(
    data = EMA_random_Days_ts %>% filter(Interval == "1"), aes(x = Interval, y = Mean), position = position_nudge(x = 1.3),
    side = "r", fill = "dodgerblue", alpha = .5, trim = FALSE) +
  geom_half_violin(
    data = EMA_random_Days_ts %>% filter(Interval == "2"), aes(x = Interval, y = Mean), position = position_nudge(x = .3),
    side = "r", fill = "darkorange", alpha = .5, trim = FALSE) +
  scale_x_continuous(breaks = c(1,2), labels = c("PRE", "POST"), limits = c(0, 3)) +
  xlab("Interval") + ylab("Mean") +
  ggtitle("EMA Data (5+5 Timepoint Random Days): Individual Pre-Post Means") +
  theme_classic()+
  coord_cartesian(ylim = c(0, 24))

Pre_Post_Box_Violin

ggsave("Time Series Dataframes/EMA_random_Days_Pre-Post_Box_Violin.jpg", plot = Pre_Post_Box_Violin, width = 6, height = 4)
save(Pre_Post_Box_Violin, file = "Time Series Dataframes/EMA_random_Days_Pre_Post_Box_Violin.RData")


# Repeated measures with box??? and violin plots and means + CIs
score_mean_1 = EMA_random_Days_ts %>% filter(Interval == "1") %>% summarise(mean(Mean)) %>% as.numeric()
score_mean_2 = EMA_random_Days_ts %>% filter(Interval == "2") %>% summarise(mean(Mean)) %>% as.numeric()
score_median1 = EMA_random_Days_ts %>% filter(Interval == "1") %>% summarise(median(Mean)) %>% as.numeric()
score_median2 = EMA_random_Days_ts %>% filter(Interval == "2") %>% summarise(median(Mean)) %>% as.numeric()
score_sd_1 = EMA_random_Days_ts %>% filter(Interval == "1") %>% summarise(sd(Mean)) %>% as.numeric()
score_sd_2 = EMA_random_Days_ts %>% filter(Interval == "2") %>% summarise(sd(Mean)) %>% as.numeric()
score_se_1 = score_sd_1/sqrt(nrow(EMA_random_Days))
score_se_2 = score_sd_2/sqrt(nrow(EMA_random_Days))
score_ci_1 = EMA_random_Days_ts %>% filter(Interval == "1") %>% pull(Mean) %>% CI(., ci = 0.95)
score_ci_2 = EMA_random_Days_ts %>% filter(Interval == "2") %>% pull(Mean) %>% CI(., ci = 0.95)
#Create data frame with 2 rows and 7 columns containing the descriptives
group = c("PRE", "POST")
N = c(nrow(EMA_random_Days), nrow(EMA_random_Days))
score_mean = c(score_mean_1, score_mean_2)
score_median = c(score_median1, score_median2)
sd = c(score_sd_1, score_sd_2)
se = c(score_se_1, score_se_2)
ci = c(as.numeric(score_ci_1[1] - score_ci_1[3]), as.numeric(score_ci_2[1] - score_ci_2[3]))
summary_df = data.frame(group, N, score_mean, score_median, sd, se, ci)

# EMA_random_Days_ts$jit = jitter(EMA_random_Days_ts$Interval, amount = .09)     #already created above
x_tick_means = c(.87, 2.13)

Pre_Post_Box_Violin_Mean_CI = ggplot(data = EMA_random_Days_ts, aes(y = Mean)) +
  geom_point(data = EMA_random_Days_ts %>% filter(Interval == "1"), aes(x = jit), color = "dodgerblue", size = 1,
             alpha = .6) +
  geom_point(data = EMA_random_Days_ts %>% filter(Interval == "2"), aes(x = jit), color = "darkorange", size = 1,
             alpha = .6) +
  geom_line(aes(x = jit, group = ID), color = "lightgray", alpha = .05) +
  geom_half_boxplot(
    data = EMA_random_Days_ts %>% filter(Interval == "1"), aes(x = Interval, y = Mean), position = position_nudge(x = -.28),
    side = "r", outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
    fill = "dodgerblue") +
  geom_half_boxplot(
    data = EMA_random_Days_ts %>% filter(Interval == "2"), aes(x = Interval, y = Mean), position = position_nudge(x = .18),
    side = "r", outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
    fill = "darkorange") +
  geom_half_violin(
    data = EMA_random_Days_ts %>% filter(Interval == "1"), aes(x = Interval, y = Mean), position = position_nudge(x = -.3),
    side = "l", fill = "dodgerblue") +
  geom_half_violin(
    data = EMA_random_Days_ts %>% filter(Interval == "2"),aes(x = Interval, y = Mean), position = position_nudge(x = .3),
    side = "r", fill = "darkorange") +
  geom_point(data = EMA_random_Days_ts %>% filter(Interval == "1"), aes(x = Interval, y = score_mean[1]),
             position = position_nudge(x = -.13), color = "dodgerblue", alpha = .6, size = 1.5) +
  geom_errorbar(data = EMA_random_Days_ts %>% filter(Interval == "1"), aes(x = Interval, y = score_mean[1],
                                                                           ymin = score_mean[1]-ci[1], ymax = score_mean[1]+ci[1]),
                position = position_nudge(-.13), color = "dodgerblue", width = 0.05, size = 0.4, alpha = .6) +
  geom_point(data = EMA_random_Days_ts %>% filter(Interval == "2"), aes(x = Interval, y = score_mean[2]),
             position = position_nudge(x = .13), color = "darkorange", alpha = .6, size = 1.5)+
  geom_errorbar(data = EMA_random_Days_ts %>% filter(Interval == "2"), aes(x = Interval, y = score_mean[2],
                                                                           ymin = score_mean[2]-ci[2], ymax = score_mean[2]+ci[2]), 
                position = position_nudge(.13), color = "darkorange", width = 0.05, size = 0.4, alpha = .6) +
  geom_line(data = summary_df, aes(x = x_tick_means, y = score_mean), color = "gray", size = 1) +
  scale_x_continuous(breaks = c(1,2), labels = c("PRE", "POST"), limits = c(0, 3)) +
  xlab("Interval") + ylab("Mean") +
  ggtitle("EMA Data (5+5 Timepoint Random Days): Individual Pre-Post Means") +
  theme_classic()+
  coord_cartesian(ylim = c(0, 24))

Pre_Post_Box_Violin_Mean_CI

ggsave("Time Series Dataframes/EMA_random_Days_Pre-Post_Box_Violin_Mean+CI.jpg", plot = Pre_Post_Box_Violin_Mean_CI, width = 6, height = 4)
save(Pre_Post_Box_Violin_Mean_CI, file = "Time Series Dataframes/EMA_random_Days_Pre_Post_Box_Violin_Mean_CI.RData")


