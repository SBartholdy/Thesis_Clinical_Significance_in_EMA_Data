
#################################################################### andere Ansätze zum random sampling von MZP statt KNN Search
x <- rnorm(n, mean, sd); x <- x[x > lower.limit & x < upper.limit]

#MASS
MASS::mvrnorm(n = 1, mu, Sigma, tol = 1e-6, empirical = FALSE, EISPACK = FALSE)

#sample integers
sample(x=min:max, prob=dnorm(...))

#Three ways has worked for me:
# using sample() with rnorm():
sample(x=min:max, replace= TRUE, rnorm(n, mean))
# using the msm package and the rtnorm function:
rtnorm(n, mean, lower=min, upper=max)
# using the rnorm() and specifying the lower and upper limits, as Hugh has posted above:
sample <- rnorm(n, mean=mean); sample <- sample[x > min & x < max]
#################################################################### Ende andere Ansätze

pacman::p_load(dplyr, tidyverse, FNN, overlapping)

setwd(
  "C:/Users/steph/OneDrive/Desktop/Psychologie/Masterstudium Psychologie/3. Semester/1. Master-Konversatorium/Preprocessing und Berechnungen"
)


################################################################################# KNN Search mit k = 5 ####
####################################### . cor_07_k20: Erweiterung auf 30.30 durch KNN Search mit k = 5 ####

#### .. Öffnen des gesamten Datensets (N = 100.000) und Berechnung von MWs, SDs usw. ####
PP_5.5 = read.delim("cor_07_k20/cor_07_dataset_k20.txt", row.names=NULL) %>%
  select(PRE1_1:POST1_5) %>%
  add_column(., .before = "PRE1_1", ID = 1:nrow(.)) %>%
  as_tibble()

pre_5mzp = c("PRE1_1","PRE1_2","PRE1_3","PRE1_4","PRE1_5")
post_5mzp = c("POST1_1","POST1_2","POST1_3","POST1_4","POST1_5")

PP_5.5$PRE_Mean = apply(PP_5.5[pre_5mzp], 1, mean)
PP_5.5$POST_Mean = apply(PP_5.5[post_5mzp], 1, mean)
PP_5.5$MeanDiff = PP_5.5$PRE_Mean - PP_5.5$POST_Mean
PP_5.5$ind.pretestSD = apply(PP_5.5[pre_5mzp], 1, sd)
PP_5.5$ind.posttestSD = apply(PP_5.5[post_5mzp], 1, sd)

save(PP_5.5, file = "cor_07_k20/PP_5.5.RData")


#### .. KNN Search für PRE-Intervalle ####
pre_data = PP_5.5 %>%
  select(PRE_Mean, ind.pretestSD)

PP_PRE_KNN_df = get.knn(pre_data, k=5, algorithm = "kd_tree")
x = as_tibble(PP_PRE_KNN_df[[1]], .name_repair = "minimal")
colnames(x) = c("neighbor1", "neighbor2", "neighbor3", "neighbor4", "neighbor5")

y = as_tibble(PP_PRE_KNN_df[[2]], .name_repair = "minimal")
colnames(y) = c("distance1", "distance2", "distance3", "distance4", "distance5")

PP_PRE_KNN_df = bind_cols(x, y) %>%
  add_column(., .before = "neighbor1", ID = 1:nrow(.)) %>%
  filter(distance1 == 0 & distance2 == 0 & distance3 == 0 & distance4 == 0 & distance5 == 0)


#### .. KNN Search für POST-Intervalle ####
post_data = PP_5.5 %>%
  select(POST_Mean, ind.posttestSD)

PP_POST_KNN_df = get.knn(post_data, k=5, algorithm = "kd_tree")
x = as_tibble(PP_POST_KNN_df[[1]], .name_repair = "minimal")
colnames(x) = c("neighbor1", "neighbor2", "neighbor3", "neighbor4", "neighbor5")

y = as_tibble(PP_POST_KNN_df[[2]], .name_repair = "minimal")
colnames(y) = c("distance1", "distance2", "distance3", "distance4", "distance5")

PP_POST_KNN_df = bind_cols(x, y) %>%
  add_column(., .before = "neighbor1", ID = 1:nrow(.)) %>%
  filter(distance1 == 0 & distance2 == 0 & distance3 == 0 & distance4 == 0 & distance5 == 0)


##### .. Reduktion auf die wirklich einmaligen KNN-Kombinationen ####
# PRE
PP_PRE_KNN_df = PP_PRE_KNN_df %>%
  select(ID, neighbor1, neighbor2, neighbor3, neighbor4, neighbor5) %>%
  apply(., 1, sort) %>%
  t() %>%
  as_tibble() %>%
  arrange(., V1, V2, V3, V4, V5, V6) %>%
  distinct() %>%
  filter(V1 != V2 & V2 != V3 & V3 != V4 & V4 != V5 & V5 != V6) %>%
  group_by(V1) %>%
  filter(row_number() == 1) %>%
  ungroup()

colnames(PP_PRE_KNN_df) = c("ID1_PRE", "ID2_PRE", "ID3_PRE", "ID4_PRE", "ID5_PRE", "ID6_PRE")

# POST
PP_POST_KNN_df = PP_POST_KNN_df %>%
  select(ID, neighbor1, neighbor2, neighbor3, neighbor4, neighbor5) %>%
  apply(., 1, sort) %>%
  t() %>%
  as_tibble() %>%
  arrange(., V1, V2, V3, V4, V5, V6) %>%
  distinct() %>%
  filter(V1 != V2 & V2 != V3 & V3 != V4 & V4 != V5 & V5 != V6) %>%
  group_by(V1) %>%
  filter(row_number() == 1) %>%
  ungroup()

colnames(PP_POST_KNN_df) = c("ID1_POST", "ID2_POST", "ID3_POST", "ID4_POST", "ID5_POST", "ID6_POST")

# ID1-Filter (weil Pre- und Post-Intervalle anhand von "ID"=1:100000 zusammengesetzt werden sollen)
PP_matched_Pre_Post_IDs = inner_join(PP_PRE_KNN_df, PP_POST_KNN_df, by = c("ID1_PRE" = "ID1_POST"))
PP_matched_Pre_Post_IDs = PP_matched_Pre_Post_IDs %>%
  add_column(., .before = "ID2_POST", ID1_POST = PP_matched_Pre_Post_IDs$ID1_PRE)

save(PP_matched_Pre_Post_IDs, file = "cor_07_k20/PP_matched_Pre_Post_6_IDs.RData")


#### .. Zusammenführen matched nearest-neighbor IDs PRE und POST ####
load("cor_07_k20/PP_matched_Pre_Post_6_IDs.RData")
load("cor_07_k20/PP_5.5.RData")
PP_data = PP_5.5
PP_matched_Pre_Post_IDs = PP_matched_Pre_Post_IDs %>%
  as.data.frame()

PP_30.30 = data.frame(
  ID1_PRE = c(), ID2_PRE = c(), ID3_PRE = c(), ID4_PRE = c(), ID5_PRE = c(), ID6_PRE = c(),
  ID1_POST = c(), ID2_POST = c(), ID3_POST = c(), ID4_POST = c(), ID5_POST = c(), ID6_POST = c(),

  PRE1_1 = c(), PRE1_2 = c(), PRE1_3 = c(), PRE1_4 = c(), PRE1_5 = c(),
  PRE1_6 = c(), PRE1_7 = c(), PRE1_8 = c(), PRE1_9 = c(), PRE1_10 = c(),
  PRE1_11 = c(), PRE1_12 = c(), PRE1_13 = c(), PRE1_14 = c(), PRE1_15 = c(),
  PRE1_16 = c(), PRE1_17 = c(), PRE1_18 = c(), PRE1_19 = c(), PRE1_20 = c(),
  PRE1_21 = c(), PRE1_22 = c(), PRE1_23 = c(), PRE1_24 = c(), PRE1_25 = c(),
  PRE1_26 = c(), PRE1_27 = c(), PRE1_28 = c(), PRE1_29 = c(), PRE1_30 = c(),

  POST1_1 = c(), POST1_2 = c(), POST1_3 = c(), POST1_4 = c(), POST1_5 = c(),
  POST1_6 = c(), POST1_7 = c(), POST1_8 = c(), POST1_9 = c(), POST1_10 = c(),
  POST1_11 = c(), POST1_12 = c(), POST1_13 = c(), POST1_14 = c(), POST1_15 = c(),
  POST1_16 = c(), POST1_17 = c(), POST1_18 = c(), POST1_19 = c(), POST1_20 = c(),
  POST1_21 = c(), POST1_22 = c(), POST1_23 = c(), POST1_24 = c(), POST1_25 = c(),
  POST1_26 = c(), POST1_27 = c(), POST1_28 = c(), POST1_29 = c(), POST1_30 = c()
)

for (i in 1:length(PP_matched_Pre_Post_IDs$ID1_PRE)) {
  PP_30.30[i,"ID1_PRE"] = PP_matched_Pre_Post_IDs[i,"ID1_PRE"]
  PP_30.30[i,"ID2_PRE"] = PP_matched_Pre_Post_IDs[i,"ID2_PRE"]
  PP_30.30[i,"ID3_PRE"] = PP_matched_Pre_Post_IDs[i,"ID3_PRE"]
  PP_30.30[i,"ID4_PRE"] = PP_matched_Pre_Post_IDs[i,"ID4_PRE"]
  PP_30.30[i,"ID5_PRE"] = PP_matched_Pre_Post_IDs[i,"ID5_PRE"]
  PP_30.30[i,"ID6_PRE"] = PP_matched_Pre_Post_IDs[i,"ID6_PRE"]
  PP_30.30[i,"ID1_POST"] = PP_matched_Pre_Post_IDs[i,"ID1_POST"]
  PP_30.30[i,"ID2_POST"] = PP_matched_Pre_Post_IDs[i,"ID2_POST"]
  PP_30.30[i,"ID3_POST"] = PP_matched_Pre_Post_IDs[i,"ID3_POST"]
  PP_30.30[i,"ID4_POST"] = PP_matched_Pre_Post_IDs[i,"ID4_POST"]
  PP_30.30[i,"ID5_POST"] = PP_matched_Pre_Post_IDs[i,"ID5_POST"]
  PP_30.30[i,"ID6_POST"] = PP_matched_Pre_Post_IDs[i,"ID6_POST"]

  PP_30.30[i,"PRE1_1"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID1_PRE"],"PRE1_1"]
  PP_30.30[i,"PRE1_2"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID1_PRE"],"PRE1_2"]
  PP_30.30[i,"PRE1_3"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID1_PRE"],"PRE1_3"]
  PP_30.30[i,"PRE1_4"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID1_PRE"],"PRE1_4"]
  PP_30.30[i,"PRE1_5"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID1_PRE"],"PRE1_5"]

  PP_30.30[i,"PRE1_6"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID2_PRE"],"PRE1_1"]
  PP_30.30[i,"PRE1_7"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID2_PRE"],"PRE1_2"]
  PP_30.30[i,"PRE1_8"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID2_PRE"],"PRE1_3"]
  PP_30.30[i,"PRE1_9"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID2_PRE"],"PRE1_4"]
  PP_30.30[i,"PRE1_10"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID2_PRE"],"PRE1_5"]

  PP_30.30[i,"PRE1_11"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID3_PRE"],"PRE1_1"]
  PP_30.30[i,"PRE1_12"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID3_PRE"],"PRE1_2"]
  PP_30.30[i,"PRE1_13"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID3_PRE"],"PRE1_3"]
  PP_30.30[i,"PRE1_14"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID3_PRE"],"PRE1_4"]
  PP_30.30[i,"PRE1_15"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID3_PRE"],"PRE1_5"]

  PP_30.30[i,"PRE1_16"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID4_PRE"],"PRE1_1"]
  PP_30.30[i,"PRE1_17"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID4_PRE"],"PRE1_2"]
  PP_30.30[i,"PRE1_18"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID4_PRE"],"PRE1_3"]
  PP_30.30[i,"PRE1_19"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID4_PRE"],"PRE1_4"]
  PP_30.30[i,"PRE1_20"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID4_PRE"],"PRE1_5"]

  PP_30.30[i,"PRE1_21"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID5_PRE"],"PRE1_1"]
  PP_30.30[i,"PRE1_22"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID5_PRE"],"PRE1_2"]
  PP_30.30[i,"PRE1_23"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID5_PRE"],"PRE1_3"]
  PP_30.30[i,"PRE1_24"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID5_PRE"],"PRE1_4"]
  PP_30.30[i,"PRE1_25"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID5_PRE"],"PRE1_5"]

  PP_30.30[i,"PRE1_26"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID6_PRE"],"PRE1_1"]
  PP_30.30[i,"PRE1_27"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID6_PRE"],"PRE1_2"]
  PP_30.30[i,"PRE1_28"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID6_PRE"],"PRE1_3"]
  PP_30.30[i,"PRE1_29"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID6_PRE"],"PRE1_4"]
  PP_30.30[i,"PRE1_30"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID6_PRE"],"PRE1_5"]


  PP_30.30[i,"POST1_1"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID1_POST"],"POST1_1"]
  PP_30.30[i,"POST1_2"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID1_POST"],"POST1_2"]
  PP_30.30[i,"POST1_3"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID1_POST"],"POST1_3"]
  PP_30.30[i,"POST1_4"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID1_POST"],"POST1_4"]
  PP_30.30[i,"POST1_5"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID1_POST"],"POST1_5"]

  PP_30.30[i,"POST1_6"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID2_POST"],"POST1_1"]
  PP_30.30[i,"POST1_7"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID2_POST"],"POST1_2"]
  PP_30.30[i,"POST1_8"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID2_POST"],"POST1_3"]
  PP_30.30[i,"POST1_9"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID2_POST"],"POST1_4"]
  PP_30.30[i,"POST1_10"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID2_POST"],"POST1_5"]

  PP_30.30[i,"POST1_11"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID3_POST"],"POST1_1"]
  PP_30.30[i,"POST1_12"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID3_POST"],"POST1_2"]
  PP_30.30[i,"POST1_13"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID3_POST"],"POST1_3"]
  PP_30.30[i,"POST1_14"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID3_POST"],"POST1_4"]
  PP_30.30[i,"POST1_15"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID3_POST"],"POST1_5"]

  PP_30.30[i,"POST1_16"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID4_POST"],"POST1_1"]
  PP_30.30[i,"POST1_17"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID4_POST"],"POST1_2"]
  PP_30.30[i,"POST1_18"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID4_POST"],"POST1_3"]
  PP_30.30[i,"POST1_19"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID4_POST"],"POST1_4"]
  PP_30.30[i,"POST1_20"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID4_POST"],"POST1_5"]

  PP_30.30[i,"POST1_21"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID5_POST"],"POST1_1"]
  PP_30.30[i,"POST1_22"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID5_POST"],"POST1_2"]
  PP_30.30[i,"POST1_23"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID5_POST"],"POST1_3"]
  PP_30.30[i,"POST1_24"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID5_POST"],"POST1_4"]
  PP_30.30[i,"POST1_25"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID5_POST"],"POST1_5"]

  PP_30.30[i,"POST1_26"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID6_POST"],"POST1_1"]
  PP_30.30[i,"POST1_27"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID6_POST"],"POST1_2"]
  PP_30.30[i,"POST1_28"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID6_POST"],"POST1_3"]
  PP_30.30[i,"POST1_29"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID6_POST"],"POST1_4"]
  PP_30.30[i,"POST1_30"] = PP_data[PP_matched_Pre_Post_IDs[i,"ID6_POST"],"POST1_5"]
  message(i)
}

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

PP_30.30$PRE_Mean = apply(PP_30.30[pre_30mzp], 1, mean)
PP_30.30$POST_Mean = apply(PP_30.30[post_30mzp], 1, mean)
PP_30.30$MeanDiff = PP_30.30$PRE_Mean - PP_30.30$POST_Mean
PP_30.30$ind.pretestSD = apply(PP_30.30[pre_30mzp], 1, sd)
PP_30.30$ind.posttestSD = apply(PP_30.30[post_30mzp], 1, sd)

write.table(PP_30.30, file = "cor_07_k20/PP_30.30.txt")
save(PP_30.30, file = "cor_07_k20/PP_30.30.RData")


#### .. Cohen´s d und Pre-Post-Overlap-Plots ####
load("cor_07_k20/PP_5.5.RData")
load("cor_07_k20/PP_30.30.RData")

# PP_30.30
PP_30.30 = PP_30.30 %>%
  filter(ind.pretestSD != 0 & ind.posttestSD != 0)

d = round((mean(PP_30.30$PRE1_1) - mean(PP_30.30$POST1_1)) /
            sqrt(0.5 * (sd(PP_30.30$PRE1_1)^2 + sd(PP_30.30$POST1_1)^2)), digits = 2)

final.plot(list(PP_30.30_PRE_Mean = PP_30.30$PRE_Mean, PP_30.30_POST_Mean = PP_30.30$POST_Mean),
           overlap(list(PP_30.30_PRE_Mean = PP_30.30$PRE_Mean, PP_30.30_POST_Mean = PP_30.30$POST_Mean))$OV)

ggsave("cor_07_k20/Overlap_PP_30.30_d0.92.jpg", plot = last_plot(), width = 6, height = 4)

# PP_5.5
PP_5.5 = PP_5.5 %>%
  filter(ind.pretestSD != 0 & ind.posttestSD != 0 & ID %in% PP_30.30$ID1_PRE)

d = round((mean(PP_5.5$PRE1_1) - mean(PP_5.5$POST1_1)) /
            sqrt(0.5 * (sd(PP_5.5$PRE1_1)^2 + sd(PP_5.5$POST1_1)^2)), digits = 2)

final.plot(list(PP_5.5_PRE_Mean = PP_5.5$PRE_Mean, PP_5.5_POST_Mean = PP_5.5$POST_Mean),
           overlap(list(PP_5.5_PRE_Mean = PP_5.5$PRE_Mean, PP_5.5_POST_Mean = PP_5.5$POST_Mean))$OV)

ggsave("cor_07_k20/Overlap_PP_5.5_d0.92.jpg", plot = last_plot(), width = 6, height = 4)




####################################### . cor_04_k20: Erweiterung auf 30.30 durch KNN Search mit k = 5 ####

#### .. Öffnen des gesamten Datensets (N = 100.000) und Berechnung von MWs, SDs usw. ####
EMA_5.5 = read.delim("cor_04_k20/cor_04_dataset_k20.txt", row.names=NULL) %>%
  select(PRE1_1:POST1_5) %>%
  add_column(., .before = "PRE1_1", ID = 1:nrow(.)) %>%
  as_tibble()

pre_5mzp = c("PRE1_1","PRE1_2","PRE1_3","PRE1_4","PRE1_5")
post_5mzp = c("POST1_1","POST1_2","POST1_3","POST1_4","POST1_5")

EMA_5.5$PRE_Mean = apply(EMA_5.5[pre_5mzp], 1, mean)
EMA_5.5$POST_Mean = apply(EMA_5.5[post_5mzp], 1, mean)
EMA_5.5$MeanDiff = EMA_5.5$PRE_Mean - EMA_5.5$POST_Mean
EMA_5.5$ind.pretestSD = apply(EMA_5.5[pre_5mzp], 1, sd)
EMA_5.5$ind.posttestSD = apply(EMA_5.5[post_5mzp], 1, sd)

save(EMA_5.5, file = "cor_04_k20/EMA_5.5.RData")


#### .. KNN Search für PRE-Intervalle ####
pre_data = EMA_5.5 %>%
  select(PRE_Mean, ind.pretestSD)

EMA_PRE_KNN_df = get.knn(pre_data, k=5, algorithm = "kd_tree")
x = as_tibble(EMA_PRE_KNN_df[[1]], .name_repair = "minimal")
colnames(x) = c("neighbor1", "neighbor2", "neighbor3", "neighbor4", "neighbor5")

y = as_tibble(EMA_PRE_KNN_df[[2]], .name_repair = "minimal")
colnames(y) = c("distance1", "distance2", "distance3", "distance4", "distance5")

EMA_PRE_KNN_df = bind_cols(x, y) %>%
  add_column(., .before = "neighbor1", ID = 1:nrow(.)) %>%
  filter(distance1 == 0 & distance2 == 0 & distance3 == 0 & distance4 == 0 & distance5 == 0)


#### .. KNN Search für POST-Intervalle ####
post_data = EMA_5.5 %>%
  select(POST_Mean, ind.posttestSD)

EMA_POST_KNN_df = get.knn(post_data, k=5, algorithm = "kd_tree")
x = as_tibble(EMA_POST_KNN_df[[1]], .name_repair = "minimal")
colnames(x) = c("neighbor1", "neighbor2", "neighbor3", "neighbor4", "neighbor5")

y = as_tibble(EMA_POST_KNN_df[[2]], .name_repair = "minimal")
colnames(y) = c("distance1", "distance2", "distance3", "distance4", "distance5")

EMA_POST_KNN_df = bind_cols(x, y) %>%
  add_column(., .before = "neighbor1", ID = 1:nrow(.)) %>%
  filter(distance1 == 0 & distance2 == 0 & distance3 == 0 & distance4 == 0 & distance5 == 0)


##### .. Reduktion auf die wirklich einmaligen KNN-Kombinationen ####
# PRE
EMA_PRE_KNN_df = EMA_PRE_KNN_df %>%
  select(ID, neighbor1, neighbor2, neighbor3, neighbor4, neighbor5) %>%
  apply(., 1, sort) %>%
  t() %>%
  as_tibble() %>%
  arrange(., V1, V2, V3, V4, V5, V6) %>%
  distinct() %>%
  filter(V1 != V2 & V2 != V3 & V3 != V4 & V4 != V5 & V5 != V6) %>%
  group_by(V1) %>%
  filter(row_number() == 1) %>%
  ungroup()

colnames(EMA_PRE_KNN_df) = c("ID1_PRE", "ID2_PRE", "ID3_PRE", "ID4_PRE", "ID5_PRE", "ID6_PRE")

# POST
EMA_POST_KNN_df = EMA_POST_KNN_df %>%
  select(ID, neighbor1, neighbor2, neighbor3, neighbor4, neighbor5) %>%
  apply(., 1, sort) %>%
  t() %>%
  as_tibble() %>%
  arrange(., V1, V2, V3, V4, V5, V6) %>%
  distinct() %>%
  filter(V1 != V2 & V2 != V3 & V3 != V4 & V4 != V5 & V5 != V6) %>%
  group_by(V1) %>%
  filter(row_number() == 1) %>%
  ungroup()

colnames(EMA_POST_KNN_df) = c("ID1_POST", "ID2_POST", "ID3_POST", "ID4_POST", "ID5_POST", "ID6_POST")

# ID1-Filter (weil Pre- und Post-Intervalle anhand von "ID"=1:100000 zusammengesetzt werden sollen)
EMA_matched_Pre_Post_IDs = inner_join(EMA_PRE_KNN_df, EMA_POST_KNN_df, by = c("ID1_PRE" = "ID1_POST"))
EMA_matched_Pre_Post_IDs = EMA_matched_Pre_Post_IDs %>%
  add_column(., .before = "ID2_POST", ID1_POST = EMA_matched_Pre_Post_IDs$ID1_PRE)

save(EMA_matched_Pre_Post_IDs, file = "cor_04_k20/EMA_matched_Pre_Post_6_IDs.RData")


#### .. Zusammenführen matched nearest-neighbor IDs PRE und POST ####
load("cor_04_k20/EMA_matched_Pre_Post_6_IDs.RData")
load("cor_04_k20/EMA_5.5.RData")
EMA_data = EMA_5.5
EMA_matched_Pre_Post_IDs = EMA_matched_Pre_Post_IDs %>%
  as.data.frame()

EMA_30.30 = data.frame(
  ID1_PRE = c(), ID2_PRE = c(), ID3_PRE = c(), ID4_PRE = c(), ID5_PRE = c(), ID6_PRE = c(),
  ID1_POST = c(), ID2_POST = c(), ID3_POST = c(), ID4_POST = c(), ID5_POST = c(), ID6_POST = c(),

  PRE1_1 = c(), PRE1_2 = c(), PRE1_3 = c(), PRE1_4 = c(), PRE1_5 = c(),
  PRE1_6 = c(), PRE1_7 = c(), PRE1_8 = c(), PRE1_9 = c(), PRE1_10 = c(),
  PRE1_11 = c(), PRE1_12 = c(), PRE1_13 = c(), PRE1_14 = c(), PRE1_15 = c(),
  PRE1_16 = c(), PRE1_17 = c(), PRE1_18 = c(), PRE1_19 = c(), PRE1_20 = c(),
  PRE1_21 = c(), PRE1_22 = c(), PRE1_23 = c(), PRE1_24 = c(), PRE1_25 = c(),
  PRE1_26 = c(), PRE1_27 = c(), PRE1_28 = c(), PRE1_29 = c(), PRE1_30 = c(),

  POST1_1 = c(), POST1_2 = c(), POST1_3 = c(), POST1_4 = c(), POST1_5 = c(),
  POST1_6 = c(), POST1_7 = c(), POST1_8 = c(), POST1_9 = c(), POST1_10 = c(),
  POST1_11 = c(), POST1_12 = c(), POST1_13 = c(), POST1_14 = c(), POST1_15 = c(),
  POST1_16 = c(), POST1_17 = c(), POST1_18 = c(), POST1_19 = c(), POST1_20 = c(),
  POST1_21 = c(), POST1_22 = c(), POST1_23 = c(), POST1_24 = c(), POST1_25 = c(),
  POST1_26 = c(), POST1_27 = c(), POST1_28 = c(), POST1_29 = c(), POST1_30 = c()
)

for (i in 1:length(EMA_matched_Pre_Post_IDs$ID1_PRE)) {
  EMA_30.30[i,"ID1_PRE"] = EMA_matched_Pre_Post_IDs[i,"ID1_PRE"]
  EMA_30.30[i,"ID2_PRE"] = EMA_matched_Pre_Post_IDs[i,"ID2_PRE"]
  EMA_30.30[i,"ID3_PRE"] = EMA_matched_Pre_Post_IDs[i,"ID3_PRE"]
  EMA_30.30[i,"ID4_PRE"] = EMA_matched_Pre_Post_IDs[i,"ID4_PRE"]
  EMA_30.30[i,"ID5_PRE"] = EMA_matched_Pre_Post_IDs[i,"ID5_PRE"]
  EMA_30.30[i,"ID6_PRE"] = EMA_matched_Pre_Post_IDs[i,"ID6_PRE"]
  EMA_30.30[i,"ID1_POST"] = EMA_matched_Pre_Post_IDs[i,"ID1_POST"]
  EMA_30.30[i,"ID2_POST"] = EMA_matched_Pre_Post_IDs[i,"ID2_POST"]
  EMA_30.30[i,"ID3_POST"] = EMA_matched_Pre_Post_IDs[i,"ID3_POST"]
  EMA_30.30[i,"ID4_POST"] = EMA_matched_Pre_Post_IDs[i,"ID4_POST"]
  EMA_30.30[i,"ID5_POST"] = EMA_matched_Pre_Post_IDs[i,"ID5_POST"]
  EMA_30.30[i,"ID6_POST"] = EMA_matched_Pre_Post_IDs[i,"ID6_POST"]

  EMA_30.30[i,"PRE1_1"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID1_PRE"],"PRE1_1"]
  EMA_30.30[i,"PRE1_2"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID1_PRE"],"PRE1_2"]
  EMA_30.30[i,"PRE1_3"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID1_PRE"],"PRE1_3"]
  EMA_30.30[i,"PRE1_4"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID1_PRE"],"PRE1_4"]
  EMA_30.30[i,"PRE1_5"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID1_PRE"],"PRE1_5"]

  EMA_30.30[i,"PRE1_6"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID2_PRE"],"PRE1_1"]
  EMA_30.30[i,"PRE1_7"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID2_PRE"],"PRE1_2"]
  EMA_30.30[i,"PRE1_8"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID2_PRE"],"PRE1_3"]
  EMA_30.30[i,"PRE1_9"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID2_PRE"],"PRE1_4"]
  EMA_30.30[i,"PRE1_10"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID2_PRE"],"PRE1_5"]

  EMA_30.30[i,"PRE1_11"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID3_PRE"],"PRE1_1"]
  EMA_30.30[i,"PRE1_12"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID3_PRE"],"PRE1_2"]
  EMA_30.30[i,"PRE1_13"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID3_PRE"],"PRE1_3"]
  EMA_30.30[i,"PRE1_14"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID3_PRE"],"PRE1_4"]
  EMA_30.30[i,"PRE1_15"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID3_PRE"],"PRE1_5"]

  EMA_30.30[i,"PRE1_16"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID4_PRE"],"PRE1_1"]
  EMA_30.30[i,"PRE1_17"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID4_PRE"],"PRE1_2"]
  EMA_30.30[i,"PRE1_18"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID4_PRE"],"PRE1_3"]
  EMA_30.30[i,"PRE1_19"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID4_PRE"],"PRE1_4"]
  EMA_30.30[i,"PRE1_20"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID4_PRE"],"PRE1_5"]

  EMA_30.30[i,"PRE1_21"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID5_PRE"],"PRE1_1"]
  EMA_30.30[i,"PRE1_22"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID5_PRE"],"PRE1_2"]
  EMA_30.30[i,"PRE1_23"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID5_PRE"],"PRE1_3"]
  EMA_30.30[i,"PRE1_24"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID5_PRE"],"PRE1_4"]
  EMA_30.30[i,"PRE1_25"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID5_PRE"],"PRE1_5"]

  EMA_30.30[i,"PRE1_26"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID6_PRE"],"PRE1_1"]
  EMA_30.30[i,"PRE1_27"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID6_PRE"],"PRE1_2"]
  EMA_30.30[i,"PRE1_28"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID6_PRE"],"PRE1_3"]
  EMA_30.30[i,"PRE1_29"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID6_PRE"],"PRE1_4"]
  EMA_30.30[i,"PRE1_30"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID6_PRE"],"PRE1_5"]


  EMA_30.30[i,"POST1_1"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID1_POST"],"POST1_1"]
  EMA_30.30[i,"POST1_2"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID1_POST"],"POST1_2"]
  EMA_30.30[i,"POST1_3"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID1_POST"],"POST1_3"]
  EMA_30.30[i,"POST1_4"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID1_POST"],"POST1_4"]
  EMA_30.30[i,"POST1_5"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID1_POST"],"POST1_5"]

  EMA_30.30[i,"POST1_6"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID2_POST"],"POST1_1"]
  EMA_30.30[i,"POST1_7"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID2_POST"],"POST1_2"]
  EMA_30.30[i,"POST1_8"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID2_POST"],"POST1_3"]
  EMA_30.30[i,"POST1_9"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID2_POST"],"POST1_4"]
  EMA_30.30[i,"POST1_10"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID2_POST"],"POST1_5"]

  EMA_30.30[i,"POST1_11"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID3_POST"],"POST1_1"]
  EMA_30.30[i,"POST1_12"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID3_POST"],"POST1_2"]
  EMA_30.30[i,"POST1_13"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID3_POST"],"POST1_3"]
  EMA_30.30[i,"POST1_14"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID3_POST"],"POST1_4"]
  EMA_30.30[i,"POST1_15"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID3_POST"],"POST1_5"]

  EMA_30.30[i,"POST1_16"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID4_POST"],"POST1_1"]
  EMA_30.30[i,"POST1_17"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID4_POST"],"POST1_2"]
  EMA_30.30[i,"POST1_18"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID4_POST"],"POST1_3"]
  EMA_30.30[i,"POST1_19"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID4_POST"],"POST1_4"]
  EMA_30.30[i,"POST1_20"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID4_POST"],"POST1_5"]

  EMA_30.30[i,"POST1_21"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID5_POST"],"POST1_1"]
  EMA_30.30[i,"POST1_22"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID5_POST"],"POST1_2"]
  EMA_30.30[i,"POST1_23"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID5_POST"],"POST1_3"]
  EMA_30.30[i,"POST1_24"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID5_POST"],"POST1_4"]
  EMA_30.30[i,"POST1_25"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID5_POST"],"POST1_5"]

  EMA_30.30[i,"POST1_26"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID6_POST"],"POST1_1"]
  EMA_30.30[i,"POST1_27"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID6_POST"],"POST1_2"]
  EMA_30.30[i,"POST1_28"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID6_POST"],"POST1_3"]
  EMA_30.30[i,"POST1_29"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID6_POST"],"POST1_4"]
  EMA_30.30[i,"POST1_30"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID6_POST"],"POST1_5"]
  message(i)
}

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

EMA_30.30$PRE_Mean = apply(EMA_30.30[pre_30mzp], 1, mean)
EMA_30.30$POST_Mean = apply(EMA_30.30[post_30mzp], 1, mean)
EMA_30.30$MeanDiff = EMA_30.30$PRE_Mean - EMA_30.30$POST_Mean
EMA_30.30$ind.pretestSD = apply(EMA_30.30[pre_30mzp], 1, sd)
EMA_30.30$ind.posttestSD = apply(EMA_30.30[post_30mzp], 1, sd)

write.table(EMA_30.30, file = "cor_04_k20/EMA_30.30.txt")
save(EMA_30.30, file = "cor_04_k20/EMA_30.30.RData")


#### .. Cohen´s d und Pre-Post-Overlap-Plots ####
load("cor_04_k20/EMA_5.5.RData")
load("cor_04_k20/EMA_30.30.RData")

# EMA_30.30
EMA_30.30 = EMA_30.30 %>%
  filter(ind.pretestSD != 0 & ind.posttestSD != 0)

d = round((mean(EMA_30.30$PRE1_1) - mean(EMA_30.30$POST1_1)) /
            sqrt(0.5 * (sd(EMA_30.30$PRE1_1)^2 + sd(EMA_30.30$POST1_1)^2)), digits = 2)

final.plot(list(EMA_30.30_PRE_Mean = EMA_30.30$PRE_Mean, EMA_30.30_POST_Mean = EMA_30.30$POST_Mean),
           overlap(list(EMA_30.30_PRE_Mean = EMA_30.30$PRE_Mean, EMA_30.30_POST_Mean = EMA_30.30$POST_Mean))$OV)

ggsave("cor_04_k20/Overlap_EMA_30.30_d0.88.jpg", plot = last_plot(), width = 6, height = 4)

# EMA_5.5
EMA_5.5 = EMA_5.5 %>%
  filter(ind.pretestSD != 0 & ind.posttestSD != 0 & ID %in% EMA_30.30$ID1_PRE)

d = round((mean(EMA_5.5$PRE1_1) - mean(EMA_5.5$POST1_1)) /
            sqrt(0.5 * (sd(EMA_5.5$PRE1_1)^2 + sd(EMA_5.5$POST1_1)^2)), digits = 2)

final.plot(list(EMA_5.5_PRE_Mean = EMA_5.5$PRE_Mean, EMA_5.5_POST_Mean = EMA_5.5$POST_Mean),
           overlap(list(EMA_5.5_PRE_Mean = EMA_5.5$PRE_Mean, EMA_5.5_POST_Mean = EMA_5.5$POST_Mean))$OV)

ggsave("cor_04_k20/Overlap_EMA_5.5_d0.88.jpg", plot = last_plot(), width = 6, height = 4)


