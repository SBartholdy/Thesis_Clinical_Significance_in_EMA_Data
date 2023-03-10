############################################################ KNN-Matching-Ergebnisse aus SPSS (.xml) ####


# AUSPROBIEREN: kmeans_cluster(iris)
# https://bookdown.org/yihui/rmarkdown/shiny-widgets.html


# siehe KNN-Matching.sps und KNN-Matching.spv: In SPSS wurden mittels K-Nearest-Neighbor-Analyse "Personen"
# gesucht, die (1) im PRE-Intervall und (2) im POST-Intervall jeweils gleiche Mittelwerte und Standard-
# abweichungen haben. Um für EMA- und Fragebogen-Daten jeweils ein Datenset mit "Personen" mit je 30 Pre- 
# und 30 Post- Messzeitpunkten zusammenstellen zu können, wurden für jede "Person" in den Original-
# Simulationsdaten 5 perfekte Matches gesucht. Dazu wird hier der Output der KNN-Analysen aus SPSS (in Form 
# von .xml-Dokumenten) geöffnet und daraus werden die gematchten IDs extrahiert und in übersichtlichen 
# Tabellen gespeichert.
# Daraufhin werden diese ID-Tabellen (EMA_PRE_KNN_df, EMA_POST_KNN_df, FB_PRE_KNN_df, FB_POST_KNN_df) zum 
# Erstellen der erweiterten EMA- und FB-Datensets benutzt.


library(XML)
library(methods)
library(dplyr)
library(tidyverse)
library(overlapping)
library(haven)

setwd(
  "C:/Users/steph/OneDrive/Desktop/Psychologie/Masterstudium Psychologie/3. Semester/1. Master-Konversatorium/Preprocessing und Berechnungen"
)


############################################################
##### cor_07_k16 #####

# Öffnen des gesamten Datensets (N = 100.000) und Berechnung von MWs, SDs usw.
FB_original = read.delim("cor_07_k16/cor_07_dataset_k16.txt", row.names=NULL) %>% 
  select(PRE1_1:POST1_5) %>% 
  add_column(., .before = "PRE1_1", ID = 1:nrow(.)) %>% 
  as_tibble()

pre_5mzp = c("PRE1_1","PRE1_2","PRE1_3","PRE1_4","PRE1_5")
post_5mzp = c("POST1_1","POST1_2","POST1_3","POST1_4","POST1_5")

FB_original$PRE_Mean = apply(FB_original[pre_5mzp], 1, mean)
FB_original$POST_Mean = apply(FB_original[post_5mzp], 1, mean)
FB_original$MeanDiff = FB_original$PRE_Mean - FB_original$POST_Mean
FB_original$ind.pretestSD = apply(FB_original[pre_5mzp], 1, sd)
FB_original$ind.posttestSD = apply(FB_original[post_5mzp], 1, sd)

save(FB_original, file = "cor_07_k16/FB_original.RData")
write_sav(FB_original, "cor_07_k16/cor_07_k16.sav")

### ...
### nächster Schritt in SPSS: k-nearest-neighbor-Analyse zur Erweiterung auf 30+30 MZP pro Person
### ...

### Öffnen der XML-Dateien mit den matched cases aus SPSS
# FB: KNN-Matching_PRE_5NN
FB_PRE_KNN_list = xmlToList("cor_07_k16/KNN-Matching_PRE_5NN.xml")
FB_PRE_KNN = FB_PRE_KNN_list$"NearestNeighborModel"$Extension$SimpleTable
save(FB_PRE_KNN, file = "cor_07_k16_30MZP_each/FB_PRE_KNN.RData")

# FB: KNN-Matching_POST_5NN
FB_POST_KNN_list = xmlToList("cor_07_k16/KNN-Matching_POST_5NN.xml")
FB_POST_KNN = FB_POST_KNN_list$"NearestNeighborModel"$Extension$SimpleTable
save(FB_POST_KNN, file = "cor_07_k16_30MZP_each/FB_POST_KNN.RData")


############################################################ . FB: KNN-Matching ####
############################################################ .. PRE ####

load("cor_07_k16_30MZP_each/FB_PRE_KNN.RData")
FB_PRE_KNN = FB_PRE_KNN[- 1]

for (i in 1:length(FB_PRE_KNN)) {
  FB_PRE_KNN[[i]] = as.vector(unlist(strsplit(FB_PRE_KNN[[i]], ";")))
  message(i)
}

FB_PRE_KNN_df = t(as.data.frame(FB_PRE_KNN, row.names = NULL, optional = FALSE, fix.empty.names = TRUE,  stringsAsFactors = default.stringsAsFactors()))
colnames(FB_PRE_KNN_df) = FB_PRE_KNN_df[1,]
FB_PRE_KNN_df = FB_PRE_KNN_df[-1,]
FB_PRE_KNN_df = FB_PRE_KNN_df[-100001,]
FB_PRE_KNN_df = FB_PRE_KNN_df[,-2]

FB_PRE_KNN_df = as.data.frame(FB_PRE_KNN_df, row.names = c(1:100000),  stringsAsFactors = FALSE)

for (k in colnames(FB_PRE_KNN_df)) {
  FB_PRE_KNN_df[,k] = as.numeric(FB_PRE_KNN_df[,k])
  message(k)
}

FB_PRE_KNN_df = subset(FB_PRE_KNN_df, subset = FB_PRE_KNN_df$distance1 == 0 & FB_PRE_KNN_df$distance2 == 0 & FB_PRE_KNN_df$distance3 == 0 & FB_PRE_KNN_df$distance4 == 0 & FB_PRE_KNN_df$distance5 == 0)

save(FB_PRE_KNN_df, file = "cor_07_k16_30MZP_each/FB_PRE_KNN_df.RData")


############################################################ .. POST ####

load("cor_07_k16_30MZP_each/FB_POST_KNN.RData")
FB_POST_KNN = FB_POST_KNN[- 1]

for (i in 1:length(FB_POST_KNN)) {
  FB_POST_KNN[[i]] = as.vector(unlist(strsplit(FB_POST_KNN[[i]], ";")))
  message(i)
}

FB_POST_KNN_df = t(as.data.frame(FB_POST_KNN, row.names = NULL, optional = FALSE, fix.empty.names = TRUE,  stringsAsFactors = default.stringsAsFactors()))
colnames(FB_POST_KNN_df) = FB_POST_KNN_df[1,]
FB_POST_KNN_df = FB_POST_KNN_df[-1,]
FB_POST_KNN_df = FB_POST_KNN_df[-100001,]
FB_POST_KNN_df = FB_POST_KNN_df[,-2]

FB_POST_KNN_df = as.data.frame(FB_POST_KNN_df, row.names = c(1:100000),  stringsAsFactors = FALSE)

for (k in colnames(FB_POST_KNN_df)) {
  FB_POST_KNN_df[,k] = as.numeric(FB_POST_KNN_df[,k])
  message(k)
}

FB_POST_KNN_df = subset(FB_POST_KNN_df, subset = FB_POST_KNN_df$distance1 == 0 & FB_POST_KNN_df$distance2 == 0 & FB_POST_KNN_df$distance3 == 0 & FB_POST_KNN_df$distance4 == 0 & FB_POST_KNN_df$distance5 == 0)

save(FB_POST_KNN_df, file = "cor_07_k16_30MZP_each/FB_POST_KNN_df.RData")


############################################################ .. Zusammenführen matched IDs PRE und POST ####

load("cor_07_k16_30MZP_each/FB_PRE_KNN_df.RData")
load("cor_07_k16_30MZP_each/FB_POST_KNN_df.RData")


# um PRE- und POST-MZP matchen zu können, werden alle IDs/Zeilen gelöscht, die nicht pro Intervall 6 Matches haben
PrePost_inter = intersect(FB_PRE_KNN_df$ID, FB_POST_KNN_df$ID)
FB_PRE_KNN_df = subset(
  FB_PRE_KNN_df,
  subset = FB_PRE_KNN_df$ID %in% PrePost_inter,
  select = c(ID, neighbor1, neighbor2, neighbor3, neighbor4, neighbor5)
)

FB_POST_KNN_df = subset(
  FB_POST_KNN_df,
  subset = FB_POST_KNN_df$ID %in% PrePost_inter,
  select = c(ID, neighbor1, neighbor2, neighbor3, neighbor4, neighbor5)
)

# Test darauf, ob die IDs in der ersten Spalte von FB_PRE_KNN_df und FB_POST_KNN_df gleich sind
length(which(FB_PRE_KNN_df$ID != FB_POST_KNN_df$ID))

# IDs gleich, also können beide Tabellen zusammengesetzt werden
FB_matched_Pre_Post_IDs = cbind(FB_PRE_KNN_df, FB_POST_KNN_df)
colnames(FB_matched_Pre_Post_IDs) = c(
  "ID1_PRE",
  "ID2_PRE",
  "ID3_PRE",
  "ID4_PRE",
  "ID5_PRE",
  "ID6_PRE",
  "ID1_POST",
  "ID2_POST",
  "ID3_POST",
  "ID4_POST",
  "ID5_POST",
  "ID6_POST"
)

# über die Zeilen hinweg gleiche Match-Kombinationen entfernen, um keine gleichen MZP-Kombinationen
# in unterschiedlicher Reihenfolge zu bekommen
# strenger Weg: Nur diejenigen Intervall-Kombinationen werden behalten, deren IDs multipliziert 
# einen einzigartigen Wert ergeben.

FB_matched_Pre_Post_IDs$PreProd = apply(FB_matched_Pre_Post_IDs[,1:6], 1, prod)
FB_matched_Pre_Post_IDs$PostProd = apply(FB_matched_Pre_Post_IDs[,7:12], 1, prod)
#length(unique(FB_matched_Pre_Post_IDs$PreProd))
#length(unique(FB_matched_Pre_Post_IDs$PostProd))

FB_matched_Pre_Post_IDs = subset(FB_matched_Pre_Post_IDs,
                                 subset = !duplicated(FB_matched_Pre_Post_IDs$PreProd) & !duplicated(FB_matched_Pre_Post_IDs$PostProd),
                                 select = -c(PreProd, PostProd))

# vielleicht anderer Lösungsansatz?:
#FB_matched_Pre_Post_IDs$ID2_PRE[FB_matched_Pre_Post_IDs$ID2_PRE %in% FB_matched_Pre_Post_IDs$ID1_PRE] = NA
#FB_matched_Pre_Post_IDs$ID3_PRE[FB_matched_Pre_Post_IDs$ID3_PRE %in% FB_matched_Pre_Post_IDs$ID1_PRE] = NA
#...
#FB_matched_Pre_Post_IDs = subset(FB_matched_Pre_Post_IDs, subset = anyNA(FB_matched_Pre_Post_IDs) == 0)

save(FB_matched_Pre_Post_IDs, file = "cor_07_k16_30MZP_each/FB_matched_Pre_Post_IDs.RData")


############################################################ .. Zusammenführen PRE- und POST-MZP anhand matched IDs ####

load("cor_07_k16/FB_original.RData")
FB_data = FB_original

load("cor_07_k16_30MZP_each/FB_matched_Pre_Post_IDs.RData")

FB_30MZP_each = data.frame(
  ID1_PRE = c(),
  ID2_PRE = c(),
  ID3_PRE = c(),
  ID4_PRE = c(),
  ID5_PRE = c(),
  ID6_PRE = c(),
  ID1_POST = c(),
  ID2_POST = c(),
  ID3_POST = c(),
  ID4_POST = c(),
  ID5_POST = c(),
  ID6_POST = c(),
  
  PRE1_1 = c(),
  PRE1_2 = c(),
  PRE1_3 = c(),
  PRE1_4 = c(),
  PRE1_5 = c(),
  PRE1_6 = c(),
  PRE1_7 = c(),
  PRE1_8 = c(),
  PRE1_9 = c(),
  PRE1_10 = c(),
  PRE1_11 = c(),
  PRE1_12 = c(),
  PRE1_13 = c(),
  PRE1_14 = c(),
  PRE1_15 = c(),
  PRE1_16 = c(),
  PRE1_17 = c(),
  PRE1_18 = c(),
  PRE1_19 = c(),
  PRE1_20 = c(),
  PRE1_21 = c(),
  PRE1_22 = c(),
  PRE1_23 = c(),
  PRE1_24 = c(),
  PRE1_25 = c(),
  PRE1_26 = c(),
  PRE1_27 = c(),
  PRE1_28 = c(),
  PRE1_29 = c(),
  PRE1_30 = c(),
  
  POST1_1 = c(),
  POST1_2 = c(),
  POST1_3 = c(),
  POST1_4 = c(),
  POST1_5 = c(),
  POST1_6 = c(),
  POST1_7 = c(),
  POST1_8 = c(),
  POST1_9 = c(),
  POST1_10 = c(),
  POST1_11 = c(),
  POST1_12 = c(),
  POST1_13 = c(),
  POST1_14 = c(),
  POST1_15 = c(),
  POST1_16 = c(),
  POST1_17 = c(),
  POST1_18 = c(),
  POST1_19 = c(),
  POST1_20 = c(),
  POST1_21 = c(),
  POST1_22 = c(),
  POST1_23 = c(),
  POST1_24 = c(),
  POST1_25 = c(),
  POST1_26 = c(),
  POST1_27 = c(),
  POST1_28 = c(),
  POST1_29 = c(),
  POST1_30 = c()
)

for (i in 1:length(FB_matched_Pre_Post_IDs$ID1_PRE)) {
  FB_30MZP_each[i,"ID1_PRE"] = FB_matched_Pre_Post_IDs[i,"ID1_PRE"]
  FB_30MZP_each[i,"ID2_PRE"] = FB_matched_Pre_Post_IDs[i,"ID2_PRE"]
  FB_30MZP_each[i,"ID3_PRE"] = FB_matched_Pre_Post_IDs[i,"ID3_PRE"]
  FB_30MZP_each[i,"ID4_PRE"] = FB_matched_Pre_Post_IDs[i,"ID4_PRE"]
  FB_30MZP_each[i,"ID5_PRE"] = FB_matched_Pre_Post_IDs[i,"ID5_PRE"]
  FB_30MZP_each[i,"ID6_PRE"] = FB_matched_Pre_Post_IDs[i,"ID6_PRE"]
  FB_30MZP_each[i,"ID1_POST"] = FB_matched_Pre_Post_IDs[i,"ID1_POST"]
  FB_30MZP_each[i,"ID2_POST"] = FB_matched_Pre_Post_IDs[i,"ID2_POST"]
  FB_30MZP_each[i,"ID3_POST"] = FB_matched_Pre_Post_IDs[i,"ID3_POST"]
  FB_30MZP_each[i,"ID4_POST"] = FB_matched_Pre_Post_IDs[i,"ID4_POST"]
  FB_30MZP_each[i,"ID5_POST"] = FB_matched_Pre_Post_IDs[i,"ID5_POST"]
  FB_30MZP_each[i,"ID6_POST"] = FB_matched_Pre_Post_IDs[i,"ID6_POST"]
  
  FB_30MZP_each[i,"PRE1_1"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID1_PRE"],"PRE1_1"]
  FB_30MZP_each[i,"PRE1_2"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID1_PRE"],"PRE1_2"]
  FB_30MZP_each[i,"PRE1_3"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID1_PRE"],"PRE1_3"]
  FB_30MZP_each[i,"PRE1_4"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID1_PRE"],"PRE1_4"]
  FB_30MZP_each[i,"PRE1_5"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID1_PRE"],"PRE1_5"]
  
  FB_30MZP_each[i,"PRE1_6"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID2_PRE"],"PRE1_1"]
  FB_30MZP_each[i,"PRE1_7"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID2_PRE"],"PRE1_2"]
  FB_30MZP_each[i,"PRE1_8"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID2_PRE"],"PRE1_3"]
  FB_30MZP_each[i,"PRE1_9"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID2_PRE"],"PRE1_4"]
  FB_30MZP_each[i,"PRE1_10"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID2_PRE"],"PRE1_5"]
  
  FB_30MZP_each[i,"PRE1_11"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID3_PRE"],"PRE1_1"]
  FB_30MZP_each[i,"PRE1_12"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID3_PRE"],"PRE1_2"]
  FB_30MZP_each[i,"PRE1_13"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID3_PRE"],"PRE1_3"]
  FB_30MZP_each[i,"PRE1_14"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID3_PRE"],"PRE1_4"]
  FB_30MZP_each[i,"PRE1_15"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID3_PRE"],"PRE1_5"]
  
  FB_30MZP_each[i,"PRE1_16"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID4_PRE"],"PRE1_1"]
  FB_30MZP_each[i,"PRE1_17"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID4_PRE"],"PRE1_2"]
  FB_30MZP_each[i,"PRE1_18"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID4_PRE"],"PRE1_3"]
  FB_30MZP_each[i,"PRE1_19"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID4_PRE"],"PRE1_4"]
  FB_30MZP_each[i,"PRE1_20"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID4_PRE"],"PRE1_5"]
  
  FB_30MZP_each[i,"PRE1_21"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID5_PRE"],"PRE1_1"]
  FB_30MZP_each[i,"PRE1_22"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID5_PRE"],"PRE1_2"]
  FB_30MZP_each[i,"PRE1_23"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID5_PRE"],"PRE1_3"]
  FB_30MZP_each[i,"PRE1_24"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID5_PRE"],"PRE1_4"]
  FB_30MZP_each[i,"PRE1_25"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID5_PRE"],"PRE1_5"]
  
  FB_30MZP_each[i,"PRE1_26"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID6_PRE"],"PRE1_1"]
  FB_30MZP_each[i,"PRE1_27"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID6_PRE"],"PRE1_2"]
  FB_30MZP_each[i,"PRE1_28"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID6_PRE"],"PRE1_3"]
  FB_30MZP_each[i,"PRE1_29"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID6_PRE"],"PRE1_4"]
  FB_30MZP_each[i,"PRE1_30"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID6_PRE"],"PRE1_5"]
  
  
  FB_30MZP_each[i,"POST1_1"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID1_POST"],"POST1_1"]
  FB_30MZP_each[i,"POST1_2"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID1_POST"],"POST1_2"]
  FB_30MZP_each[i,"POST1_3"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID1_POST"],"POST1_3"]
  FB_30MZP_each[i,"POST1_4"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID1_POST"],"POST1_4"]
  FB_30MZP_each[i,"POST1_5"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID1_POST"],"POST1_5"]
  
  FB_30MZP_each[i,"POST1_6"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID2_POST"],"POST1_1"]
  FB_30MZP_each[i,"POST1_7"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID2_POST"],"POST1_2"]
  FB_30MZP_each[i,"POST1_8"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID2_POST"],"POST1_3"]
  FB_30MZP_each[i,"POST1_9"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID2_POST"],"POST1_4"]
  FB_30MZP_each[i,"POST1_10"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID2_POST"],"POST1_5"]
  
  FB_30MZP_each[i,"POST1_11"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID3_POST"],"POST1_1"]
  FB_30MZP_each[i,"POST1_12"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID3_POST"],"POST1_2"]
  FB_30MZP_each[i,"POST1_13"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID3_POST"],"POST1_3"]
  FB_30MZP_each[i,"POST1_14"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID3_POST"],"POST1_4"]
  FB_30MZP_each[i,"POST1_15"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID3_POST"],"POST1_5"]
  
  FB_30MZP_each[i,"POST1_16"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID4_POST"],"POST1_1"]
  FB_30MZP_each[i,"POST1_17"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID4_POST"],"POST1_2"]
  FB_30MZP_each[i,"POST1_18"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID4_POST"],"POST1_3"]
  FB_30MZP_each[i,"POST1_19"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID4_POST"],"POST1_4"]
  FB_30MZP_each[i,"POST1_20"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID4_POST"],"POST1_5"]
  
  FB_30MZP_each[i,"POST1_21"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID5_POST"],"POST1_1"]
  FB_30MZP_each[i,"POST1_22"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID5_POST"],"POST1_2"]
  FB_30MZP_each[i,"POST1_23"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID5_POST"],"POST1_3"]
  FB_30MZP_each[i,"POST1_24"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID5_POST"],"POST1_4"]
  FB_30MZP_each[i,"POST1_25"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID5_POST"],"POST1_5"]
  
  FB_30MZP_each[i,"POST1_26"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID6_POST"],"POST1_1"]
  FB_30MZP_each[i,"POST1_27"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID6_POST"],"POST1_2"]
  FB_30MZP_each[i,"POST1_28"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID6_POST"],"POST1_3"]
  FB_30MZP_each[i,"POST1_29"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID6_POST"],"POST1_4"]
  FB_30MZP_each[i,"POST1_30"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID6_POST"],"POST1_5"]
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

FB_30MZP_each$PRE_Mean = apply(FB_30MZP_each[pre_30mzp], 1, mean)
FB_30MZP_each$POST_Mean = apply(FB_30MZP_each[post_30mzp], 1, mean)
FB_30MZP_each$MeanDiff = FB_30MZP_each$PRE_Mean - FB_30MZP_each$POST_Mean
FB_30MZP_each$ind.pretestSD = apply(FB_30MZP_each[pre_30mzp], 1, sd)
FB_30MZP_each$ind.posttestSD = apply(FB_30MZP_each[post_30mzp], 1, sd)

FB_30MZP_each = FB_30MZP_each %>% 
  add_column(., .before = "ID1_PRE", ID = 1:nrow(.))

write.table(FB_30MZP_each, file = "cor_07_k16_30MZP_each/FB_30MZP_each.txt")
save(FB_30MZP_each, file = "cor_07_k16_30MZP_each/FB_30MZP_each.RData")



# Cohen´s d und Pre-Post-Overlap-Plot

FB_30MZP_each = FB_30MZP_each %>% 
  filter(ind.pretestSD > 0.44 & ind.posttestSD > 0.44)

FB_30MZP_each$ID = 1:nrow(FB_30MZP_each)

d = round((mean(FB_30MZP_each$PRE1_1) - mean(FB_30MZP_each$POST1_1)) / 
            sqrt(0.5 * (sd(FB_30MZP_each$PRE1_1)^2 + sd(FB_30MZP_each$POST1_1)^2)), digits = 2)

final.plot(list(FB_30MZP_PRE_Mean = FB_30MZP_each$PRE_Mean, FB_30MZP_POST_Mean = FB_30MZP_each$POST_Mean), 
           overlap(list(FB_30MZP_PRE_Mean = FB_30MZP_each$PRE_Mean, FB_30MZP_POST_Mean = FB_30MZP_each$POST_Mean))$OV)

ggsave("cor_07_k16_30MZP_each/Plot_cor_07_dataset_k16_d1.13.jpg", plot = last_plot(), width = 6, height = 4)


# gleiche Prozedur auch für das Datenset cor_07_k16 mit 5 MZP
load("cor_07_k16/FB_original.RData")
load("cor_07_k16_30MZP_each/FB_30MZP_each.RData")

FB_original = FB_original %>% 
  filter(ID %in% FB_30MZP_each$ID1_PRE)

save(FB_original, file = "cor_07_k16/FB_original.RData")


final.plot(list(FB_original_PRE_Mean = FB_original$PRE_Mean, FB_original_POST_Mean = FB_original$POST_Mean), 
           overlap(list(FB_original_PRE_Mean = FB_original$PRE_Mean, FB_original_POST_Mean = FB_original$POST_Mean))$OV)

ggsave("cor_07_k16/Plot_cor_07_dataset_k16_d1.13.jpg", plot = last_plot(), width = 6, height = 4)




############################################################
##### cor_07_k20 #####

# Öffnen des gesamten Datensets (N = 100.000) und Berechnung von MWs, SDs usw.
FB_original = read.delim("cor_07_k20/cor_07_dataset_k20.txt", row.names=NULL) %>% 
  select(PRE1_1:POST1_5) %>% 
  add_column(., .before = "PRE1_1", ID = 1:nrow(.)) %>% 
  as_tibble()

pre_5mzp = c("PRE1_1","PRE1_2","PRE1_3","PRE1_4","PRE1_5")
post_5mzp = c("POST1_1","POST1_2","POST1_3","POST1_4","POST1_5")

FB_original$PRE_Mean = apply(FB_original[pre_5mzp], 1, mean)
FB_original$POST_Mean = apply(FB_original[post_5mzp], 1, mean)
FB_original$MeanDiff = FB_original$PRE_Mean - FB_original$POST_Mean
FB_original$ind.pretestSD = apply(FB_original[pre_5mzp], 1, sd)
FB_original$ind.posttestSD = apply(FB_original[post_5mzp], 1, sd)

save(FB_original, file = "cor_07_k20/FB_original.RData")
write_sav(FB_original, "cor_07_k20/cor_07_k20.sav")

### ...
### nächster Schritt in SPSS: k-nearest-neighbor-Analyse zur Erweiterung auf 30+30 MZP pro Person
### ...

### Öffnen der XML-Dateien mit den matched cases aus SPSS
# FB: KNN-Matching_PRE_5NN
FB_PRE_KNN_list = xmlToList("cor_07_k20/KNN-Matching_PRE_5NN.xml")
FB_PRE_KNN = FB_PRE_KNN_list$"NearestNeighborModel"$Extension$SimpleTable
save(FB_PRE_KNN, file = "cor_07_k20_30MZP_each/FB_PRE_KNN.RData")

# FB: KNN-Matching_POST_5NN
FB_POST_KNN_list = xmlToList("cor_07_k20/KNN-Matching_POST_5NN.xml")
FB_POST_KNN = FB_POST_KNN_list$"NearestNeighborModel"$Extension$SimpleTable
save(FB_POST_KNN, file = "cor_07_k20_30MZP_each/FB_POST_KNN.RData")


############################################################ . FB: KNN-Matching ####
############################################################ .. PRE ####

load("cor_07_k20_30MZP_each/FB_PRE_KNN.RData")
FB_PRE_KNN = FB_PRE_KNN[- 1]

for (i in 1:length(FB_PRE_KNN)) {
  FB_PRE_KNN[[i]] = as.vector(unlist(strsplit(FB_PRE_KNN[[i]], ";")))
  message(i)
}

FB_PRE_KNN_df = t(as.data.frame(FB_PRE_KNN, row.names = NULL, optional = FALSE, fix.empty.names = TRUE,  stringsAsFactors = default.stringsAsFactors()))
colnames(FB_PRE_KNN_df) = FB_PRE_KNN_df[1,]
FB_PRE_KNN_df = FB_PRE_KNN_df[-1,]
FB_PRE_KNN_df = FB_PRE_KNN_df[-100001,]
FB_PRE_KNN_df = FB_PRE_KNN_df[,-2]

FB_PRE_KNN_df = as.data.frame(FB_PRE_KNN_df, row.names = c(1:100000),  stringsAsFactors = FALSE)

for (k in colnames(FB_PRE_KNN_df)) {
  FB_PRE_KNN_df[,k] = as.numeric(FB_PRE_KNN_df[,k])
  message(k)
}

FB_PRE_KNN_df = subset(FB_PRE_KNN_df, subset = FB_PRE_KNN_df$distance1 == 0 & FB_PRE_KNN_df$distance2 == 0 & FB_PRE_KNN_df$distance3 == 0 & FB_PRE_KNN_df$distance4 == 0 & FB_PRE_KNN_df$distance5 == 0)

save(FB_PRE_KNN_df, file = "cor_07_k20_30MZP_each/FB_PRE_KNN_df.RData")


############################################################ .. POST ####

load("cor_07_k20_30MZP_each/FB_POST_KNN.RData")
FB_POST_KNN = FB_POST_KNN[- 1]

for (i in 1:length(FB_POST_KNN)) {
  FB_POST_KNN[[i]] = as.vector(unlist(strsplit(FB_POST_KNN[[i]], ";")))
  message(i)
}

FB_POST_KNN_df = t(as.data.frame(FB_POST_KNN, row.names = NULL, optional = FALSE, fix.empty.names = TRUE,  stringsAsFactors = default.stringsAsFactors()))
colnames(FB_POST_KNN_df) = FB_POST_KNN_df[1,]
FB_POST_KNN_df = FB_POST_KNN_df[-1,]
FB_POST_KNN_df = FB_POST_KNN_df[-100001,]
FB_POST_KNN_df = FB_POST_KNN_df[,-2]

FB_POST_KNN_df = as.data.frame(FB_POST_KNN_df, row.names = c(1:100000),  stringsAsFactors = FALSE)

for (k in colnames(FB_POST_KNN_df)) {
  FB_POST_KNN_df[,k] = as.numeric(FB_POST_KNN_df[,k])
  message(k)
}

FB_POST_KNN_df = subset(FB_POST_KNN_df, subset = FB_POST_KNN_df$distance1 == 0 & FB_POST_KNN_df$distance2 == 0 & FB_POST_KNN_df$distance3 == 0 & FB_POST_KNN_df$distance4 == 0 & FB_POST_KNN_df$distance5 == 0)

save(FB_POST_KNN_df, file = "cor_07_k20_30MZP_each/FB_POST_KNN_df.RData")


############################################################ .. Zusammenführen matched IDs PRE und POST ####

load("cor_07_k20_30MZP_each/FB_PRE_KNN_df.RData")
load("cor_07_k20_30MZP_each/FB_POST_KNN_df.RData")


# um PRE- und POST-MZP matchen zu können, werden alle IDs/Zeilen gelöscht, die nicht pro Intervall 6 Matches haben
PrePost_inter = intersect(FB_PRE_KNN_df$ID, FB_POST_KNN_df$ID)
FB_PRE_KNN_df = subset(
  FB_PRE_KNN_df,
  subset = FB_PRE_KNN_df$ID %in% PrePost_inter,
  select = c(ID, neighbor1, neighbor2, neighbor3, neighbor4, neighbor5)
)

FB_POST_KNN_df = subset(
  FB_POST_KNN_df,
  subset = FB_POST_KNN_df$ID %in% PrePost_inter,
  select = c(ID, neighbor1, neighbor2, neighbor3, neighbor4, neighbor5)
)

# Test darauf, ob die IDs in der ersten Spalte von FB_PRE_KNN_df und FB_POST_KNN_df gleich sind
length(which(FB_PRE_KNN_df$ID != FB_POST_KNN_df$ID))

# IDs gleich, also können beide Tabellen zusammengesetzt werden
FB_matched_Pre_Post_IDs = cbind(FB_PRE_KNN_df, FB_POST_KNN_df)
colnames(FB_matched_Pre_Post_IDs) = c(
  "ID1_PRE",
  "ID2_PRE",
  "ID3_PRE",
  "ID4_PRE",
  "ID5_PRE",
  "ID6_PRE",
  "ID1_POST",
  "ID2_POST",
  "ID3_POST",
  "ID4_POST",
  "ID5_POST",
  "ID6_POST"
)

# über die Zeilen hinweg gleiche Match-Kombinationen entfernen, um keine gleichen MZP-Kombinationen
# in unterschiedlicher Reihenfolge zu bekommen
# strenger Weg: Nur diejenigen Intervall-Kombinationen werden behalten, deren IDs multipliziert 
# einen einzigartigen Wert ergeben.

FB_matched_Pre_Post_IDs$PreProd = apply(FB_matched_Pre_Post_IDs[,1:6], 1, prod)
FB_matched_Pre_Post_IDs$PostProd = apply(FB_matched_Pre_Post_IDs[,7:12], 1, prod)
#length(unique(FB_matched_Pre_Post_IDs$PreProd))
#length(unique(FB_matched_Pre_Post_IDs$PostProd))

FB_matched_Pre_Post_IDs = subset(FB_matched_Pre_Post_IDs,
                                 subset = !duplicated(FB_matched_Pre_Post_IDs$PreProd) & !duplicated(FB_matched_Pre_Post_IDs$PostProd),
                                 select = -c(PreProd, PostProd))

# vielleicht anderer Lösungsansatz?:
#FB_matched_Pre_Post_IDs$ID2_PRE[FB_matched_Pre_Post_IDs$ID2_PRE %in% FB_matched_Pre_Post_IDs$ID1_PRE] = NA
#FB_matched_Pre_Post_IDs$ID3_PRE[FB_matched_Pre_Post_IDs$ID3_PRE %in% FB_matched_Pre_Post_IDs$ID1_PRE] = NA
#...
#FB_matched_Pre_Post_IDs = subset(FB_matched_Pre_Post_IDs, subset = anyNA(FB_matched_Pre_Post_IDs) == 0)

save(FB_matched_Pre_Post_IDs, file = "cor_07_k20_30MZP_each/FB_matched_Pre_Post_IDs.RData")


############################################################ .. Zusammenführen PRE- und POST-MZP anhand matched IDs ####

load("cor_07_k20/FB_original.RData")
FB_data = FB_original

load("cor_07_k20_30MZP_each/FB_matched_Pre_Post_IDs.RData")

FB_30MZP_each = data.frame(
  ID1_PRE = c(),
  ID2_PRE = c(),
  ID3_PRE = c(),
  ID4_PRE = c(),
  ID5_PRE = c(),
  ID6_PRE = c(),
  ID1_POST = c(),
  ID2_POST = c(),
  ID3_POST = c(),
  ID4_POST = c(),
  ID5_POST = c(),
  ID6_POST = c(),
  
  PRE1_1 = c(),
  PRE1_2 = c(),
  PRE1_3 = c(),
  PRE1_4 = c(),
  PRE1_5 = c(),
  PRE1_6 = c(),
  PRE1_7 = c(),
  PRE1_8 = c(),
  PRE1_9 = c(),
  PRE1_10 = c(),
  PRE1_11 = c(),
  PRE1_12 = c(),
  PRE1_13 = c(),
  PRE1_14 = c(),
  PRE1_15 = c(),
  PRE1_16 = c(),
  PRE1_17 = c(),
  PRE1_18 = c(),
  PRE1_19 = c(),
  PRE1_20 = c(),
  PRE1_21 = c(),
  PRE1_22 = c(),
  PRE1_23 = c(),
  PRE1_24 = c(),
  PRE1_25 = c(),
  PRE1_26 = c(),
  PRE1_27 = c(),
  PRE1_28 = c(),
  PRE1_29 = c(),
  PRE1_30 = c(),
  
  POST1_1 = c(),
  POST1_2 = c(),
  POST1_3 = c(),
  POST1_4 = c(),
  POST1_5 = c(),
  POST1_6 = c(),
  POST1_7 = c(),
  POST1_8 = c(),
  POST1_9 = c(),
  POST1_10 = c(),
  POST1_11 = c(),
  POST1_12 = c(),
  POST1_13 = c(),
  POST1_14 = c(),
  POST1_15 = c(),
  POST1_16 = c(),
  POST1_17 = c(),
  POST1_18 = c(),
  POST1_19 = c(),
  POST1_20 = c(),
  POST1_21 = c(),
  POST1_22 = c(),
  POST1_23 = c(),
  POST1_24 = c(),
  POST1_25 = c(),
  POST1_26 = c(),
  POST1_27 = c(),
  POST1_28 = c(),
  POST1_29 = c(),
  POST1_30 = c()
)

for (i in 1:length(FB_matched_Pre_Post_IDs$ID1_PRE)) {
  FB_30MZP_each[i,"ID1_PRE"] = FB_matched_Pre_Post_IDs[i,"ID1_PRE"]
  FB_30MZP_each[i,"ID2_PRE"] = FB_matched_Pre_Post_IDs[i,"ID2_PRE"]
  FB_30MZP_each[i,"ID3_PRE"] = FB_matched_Pre_Post_IDs[i,"ID3_PRE"]
  FB_30MZP_each[i,"ID4_PRE"] = FB_matched_Pre_Post_IDs[i,"ID4_PRE"]
  FB_30MZP_each[i,"ID5_PRE"] = FB_matched_Pre_Post_IDs[i,"ID5_PRE"]
  FB_30MZP_each[i,"ID6_PRE"] = FB_matched_Pre_Post_IDs[i,"ID6_PRE"]
  FB_30MZP_each[i,"ID1_POST"] = FB_matched_Pre_Post_IDs[i,"ID1_POST"]
  FB_30MZP_each[i,"ID2_POST"] = FB_matched_Pre_Post_IDs[i,"ID2_POST"]
  FB_30MZP_each[i,"ID3_POST"] = FB_matched_Pre_Post_IDs[i,"ID3_POST"]
  FB_30MZP_each[i,"ID4_POST"] = FB_matched_Pre_Post_IDs[i,"ID4_POST"]
  FB_30MZP_each[i,"ID5_POST"] = FB_matched_Pre_Post_IDs[i,"ID5_POST"]
  FB_30MZP_each[i,"ID6_POST"] = FB_matched_Pre_Post_IDs[i,"ID6_POST"]
  
  FB_30MZP_each[i,"PRE1_1"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID1_PRE"],"PRE1_1"]
  FB_30MZP_each[i,"PRE1_2"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID1_PRE"],"PRE1_2"]
  FB_30MZP_each[i,"PRE1_3"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID1_PRE"],"PRE1_3"]
  FB_30MZP_each[i,"PRE1_4"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID1_PRE"],"PRE1_4"]
  FB_30MZP_each[i,"PRE1_5"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID1_PRE"],"PRE1_5"]
  
  FB_30MZP_each[i,"PRE1_6"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID2_PRE"],"PRE1_1"]
  FB_30MZP_each[i,"PRE1_7"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID2_PRE"],"PRE1_2"]
  FB_30MZP_each[i,"PRE1_8"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID2_PRE"],"PRE1_3"]
  FB_30MZP_each[i,"PRE1_9"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID2_PRE"],"PRE1_4"]
  FB_30MZP_each[i,"PRE1_10"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID2_PRE"],"PRE1_5"]
  
  FB_30MZP_each[i,"PRE1_11"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID3_PRE"],"PRE1_1"]
  FB_30MZP_each[i,"PRE1_12"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID3_PRE"],"PRE1_2"]
  FB_30MZP_each[i,"PRE1_13"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID3_PRE"],"PRE1_3"]
  FB_30MZP_each[i,"PRE1_14"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID3_PRE"],"PRE1_4"]
  FB_30MZP_each[i,"PRE1_15"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID3_PRE"],"PRE1_5"]
  
  FB_30MZP_each[i,"PRE1_16"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID4_PRE"],"PRE1_1"]
  FB_30MZP_each[i,"PRE1_17"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID4_PRE"],"PRE1_2"]
  FB_30MZP_each[i,"PRE1_18"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID4_PRE"],"PRE1_3"]
  FB_30MZP_each[i,"PRE1_19"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID4_PRE"],"PRE1_4"]
  FB_30MZP_each[i,"PRE1_20"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID4_PRE"],"PRE1_5"]
  
  FB_30MZP_each[i,"PRE1_21"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID5_PRE"],"PRE1_1"]
  FB_30MZP_each[i,"PRE1_22"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID5_PRE"],"PRE1_2"]
  FB_30MZP_each[i,"PRE1_23"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID5_PRE"],"PRE1_3"]
  FB_30MZP_each[i,"PRE1_24"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID5_PRE"],"PRE1_4"]
  FB_30MZP_each[i,"PRE1_25"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID5_PRE"],"PRE1_5"]
  
  FB_30MZP_each[i,"PRE1_26"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID6_PRE"],"PRE1_1"]
  FB_30MZP_each[i,"PRE1_27"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID6_PRE"],"PRE1_2"]
  FB_30MZP_each[i,"PRE1_28"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID6_PRE"],"PRE1_3"]
  FB_30MZP_each[i,"PRE1_29"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID6_PRE"],"PRE1_4"]
  FB_30MZP_each[i,"PRE1_30"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID6_PRE"],"PRE1_5"]
  
  
  FB_30MZP_each[i,"POST1_1"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID1_POST"],"POST1_1"]
  FB_30MZP_each[i,"POST1_2"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID1_POST"],"POST1_2"]
  FB_30MZP_each[i,"POST1_3"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID1_POST"],"POST1_3"]
  FB_30MZP_each[i,"POST1_4"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID1_POST"],"POST1_4"]
  FB_30MZP_each[i,"POST1_5"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID1_POST"],"POST1_5"]
  
  FB_30MZP_each[i,"POST1_6"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID2_POST"],"POST1_1"]
  FB_30MZP_each[i,"POST1_7"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID2_POST"],"POST1_2"]
  FB_30MZP_each[i,"POST1_8"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID2_POST"],"POST1_3"]
  FB_30MZP_each[i,"POST1_9"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID2_POST"],"POST1_4"]
  FB_30MZP_each[i,"POST1_10"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID2_POST"],"POST1_5"]
  
  FB_30MZP_each[i,"POST1_11"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID3_POST"],"POST1_1"]
  FB_30MZP_each[i,"POST1_12"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID3_POST"],"POST1_2"]
  FB_30MZP_each[i,"POST1_13"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID3_POST"],"POST1_3"]
  FB_30MZP_each[i,"POST1_14"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID3_POST"],"POST1_4"]
  FB_30MZP_each[i,"POST1_15"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID3_POST"],"POST1_5"]
  
  FB_30MZP_each[i,"POST1_16"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID4_POST"],"POST1_1"]
  FB_30MZP_each[i,"POST1_17"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID4_POST"],"POST1_2"]
  FB_30MZP_each[i,"POST1_18"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID4_POST"],"POST1_3"]
  FB_30MZP_each[i,"POST1_19"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID4_POST"],"POST1_4"]
  FB_30MZP_each[i,"POST1_20"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID4_POST"],"POST1_5"]
  
  FB_30MZP_each[i,"POST1_21"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID5_POST"],"POST1_1"]
  FB_30MZP_each[i,"POST1_22"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID5_POST"],"POST1_2"]
  FB_30MZP_each[i,"POST1_23"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID5_POST"],"POST1_3"]
  FB_30MZP_each[i,"POST1_24"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID5_POST"],"POST1_4"]
  FB_30MZP_each[i,"POST1_25"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID5_POST"],"POST1_5"]
  
  FB_30MZP_each[i,"POST1_26"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID6_POST"],"POST1_1"]
  FB_30MZP_each[i,"POST1_27"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID6_POST"],"POST1_2"]
  FB_30MZP_each[i,"POST1_28"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID6_POST"],"POST1_3"]
  FB_30MZP_each[i,"POST1_29"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID6_POST"],"POST1_4"]
  FB_30MZP_each[i,"POST1_30"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID6_POST"],"POST1_5"]
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

FB_30MZP_each$PRE_Mean = apply(FB_30MZP_each[pre_30mzp], 1, mean)
FB_30MZP_each$POST_Mean = apply(FB_30MZP_each[post_30mzp], 1, mean)
FB_30MZP_each$MeanDiff = FB_30MZP_each$PRE_Mean - FB_30MZP_each$POST_Mean
FB_30MZP_each$ind.pretestSD = apply(FB_30MZP_each[pre_30mzp], 1, sd)
FB_30MZP_each$ind.posttestSD = apply(FB_30MZP_each[post_30mzp], 1, sd)

FB_30MZP_each = FB_30MZP_each %>% 
  add_column(., .before = "ID1_PRE", ID = 1:nrow(.))

write.table(FB_30MZP_each, file = "cor_07_k20_30MZP_each/FB_30MZP_each.txt")
save(FB_30MZP_each, file = "cor_07_k20_30MZP_each/FB_30MZP_each.RData")



# Cohen´s d und Pre-Post-Overlap-Plot

FB_30MZP_each = FB_30MZP_each %>% 
  filter(ind.pretestSD > 0.44 & ind.posttestSD > 0.44)

FB_30MZP_each$ID = 1:nrow(FB_30MZP_each)

d = round((mean(FB_30MZP_each$PRE1_1) - mean(FB_30MZP_each$POST1_1)) / 
            sqrt(0.5 * (sd(FB_30MZP_each$PRE1_1)^2 + sd(FB_30MZP_each$POST1_1)^2)), digits = 2)

final.plot(list(FB_30MZP_PRE_Mean = FB_30MZP_each$PRE_Mean, FB_30MZP_POST_Mean = FB_30MZP_each$POST_Mean), 
           overlap(list(FB_30MZP_PRE_Mean = FB_30MZP_each$PRE_Mean, FB_30MZP_POST_Mean = FB_30MZP_each$POST_Mean))$OV)

ggsave("cor_07_k20_30MZP_each/Plot_cor_07_dataset_k20_d0.99.jpg", plot = last_plot(), width = 6, height = 4)


# gleiche Prozedur auch für das Datenset cor_07_k20 mit 5 MZP
load("cor_07_k20/FB_original.RData")
load("cor_07_k20_30MZP_each/FB_30MZP_each.RData")

FB_original = FB_original %>% 
  filter(ID %in% FB_30MZP_each$ID1_PRE)

save(FB_original, file = "cor_07_k20/FB_original.RData")


final.plot(list(FB_original_PRE_Mean = FB_original$PRE_Mean, FB_original_POST_Mean = FB_original$POST_Mean), 
           overlap(list(FB_original_PRE_Mean = FB_original$PRE_Mean, FB_original_POST_Mean = FB_original$POST_Mean))$OV)

ggsave("cor_07_k20/Plot_cor_07_dataset_k20_d0.99.jpg", plot = last_plot(), width = 6, height = 4)




############################################################
##### cor_07_k25 #####

# Öffnen des gesamten Datensets (N = 100.000) und Berechnung von MWs, SDs usw.
FB_original = read.delim("cor_07_k25/cor_07_dataset_k25.txt", row.names=NULL) %>% 
  select(PRE1_1:POST1_5) %>% 
  add_column(., .before = "PRE1_1", ID = 1:nrow(.)) %>% 
  as_tibble()

pre_5mzp = c("PRE1_1","PRE1_2","PRE1_3","PRE1_4","PRE1_5")
post_5mzp = c("POST1_1","POST1_2","POST1_3","POST1_4","POST1_5")

FB_original$PRE_Mean = apply(FB_original[pre_5mzp], 1, mean)
FB_original$POST_Mean = apply(FB_original[post_5mzp], 1, mean)
FB_original$MeanDiff = FB_original$PRE_Mean - FB_original$POST_Mean
FB_original$ind.pretestSD = apply(FB_original[pre_5mzp], 1, sd)
FB_original$ind.posttestSD = apply(FB_original[post_5mzp], 1, sd)

save(FB_original, file = "cor_07_k25/FB_original.RData")
write_sav(FB_original, "cor_07_k25/cor_07_k25.sav")

### ...
### nächster Schritt in SPSS: k-nearest-neighbor-Analyse zur Erweiterung auf 30+30 MZP pro Person
### ...

### Öffnen der XML-Dateien mit den matched cases aus SPSS
# FB: KNN-Matching_PRE_5NN
FB_PRE_KNN_list = xmlToList("cor_07_k25/KNN-Matching_PRE_5NN.xml")
FB_PRE_KNN = FB_PRE_KNN_list$"NearestNeighborModel"$Extension$SimpleTable
save(FB_PRE_KNN, file = "cor_07_k25_30MZP_each/FB_PRE_KNN.RData")

# FB: KNN-Matching_POST_5NN
FB_POST_KNN_list = xmlToList("cor_07_k25/KNN-Matching_POST_5NN.xml")
FB_POST_KNN = FB_POST_KNN_list$"NearestNeighborModel"$Extension$SimpleTable
save(FB_POST_KNN, file = "cor_07_k25_30MZP_each/FB_POST_KNN.RData")


############################################################ . FB: KNN-Matching ####
############################################################ .. PRE ####

load("cor_07_k25_30MZP_each/FB_PRE_KNN.RData")
FB_PRE_KNN = FB_PRE_KNN[- 1]

for (i in 1:length(FB_PRE_KNN)) {
  FB_PRE_KNN[[i]] = as.vector(unlist(strsplit(FB_PRE_KNN[[i]], ";")))
  message(i)
}

FB_PRE_KNN_df = t(as.data.frame(FB_PRE_KNN, row.names = NULL, optional = FALSE, fix.empty.names = TRUE,  stringsAsFactors = default.stringsAsFactors()))
colnames(FB_PRE_KNN_df) = FB_PRE_KNN_df[1,]
FB_PRE_KNN_df = FB_PRE_KNN_df[-1,]
FB_PRE_KNN_df = FB_PRE_KNN_df[-100001,]
FB_PRE_KNN_df = FB_PRE_KNN_df[,-2]

FB_PRE_KNN_df = as.data.frame(FB_PRE_KNN_df, row.names = c(1:100000),  stringsAsFactors = FALSE)

for (k in colnames(FB_PRE_KNN_df)) {
  FB_PRE_KNN_df[,k] = as.numeric(FB_PRE_KNN_df[,k])
  message(k)
}

FB_PRE_KNN_df = subset(FB_PRE_KNN_df, subset = FB_PRE_KNN_df$distance1 == 0 & FB_PRE_KNN_df$distance2 == 0 & FB_PRE_KNN_df$distance3 == 0 & FB_PRE_KNN_df$distance4 == 0 & FB_PRE_KNN_df$distance5 == 0)

save(FB_PRE_KNN_df, file = "cor_07_k25_30MZP_each/FB_PRE_KNN_df.RData")


############################################################ .. POST ####

load("cor_07_k25_30MZP_each/FB_POST_KNN.RData")
FB_POST_KNN = FB_POST_KNN[- 1]

for (i in 1:length(FB_POST_KNN)) {
  FB_POST_KNN[[i]] = as.vector(unlist(strsplit(FB_POST_KNN[[i]], ";")))
  message(i)
}

FB_POST_KNN_df = t(as.data.frame(FB_POST_KNN, row.names = NULL, optional = FALSE, fix.empty.names = TRUE,  stringsAsFactors = default.stringsAsFactors()))
colnames(FB_POST_KNN_df) = FB_POST_KNN_df[1,]
FB_POST_KNN_df = FB_POST_KNN_df[-1,]
FB_POST_KNN_df = FB_POST_KNN_df[-100001,]
FB_POST_KNN_df = FB_POST_KNN_df[,-2]

FB_POST_KNN_df = as.data.frame(FB_POST_KNN_df, row.names = c(1:100000),  stringsAsFactors = FALSE)

for (k in colnames(FB_POST_KNN_df)) {
  FB_POST_KNN_df[,k] = as.numeric(FB_POST_KNN_df[,k])
  message(k)
}

FB_POST_KNN_df = subset(FB_POST_KNN_df, subset = FB_POST_KNN_df$distance1 == 0 & FB_POST_KNN_df$distance2 == 0 & FB_POST_KNN_df$distance3 == 0 & FB_POST_KNN_df$distance4 == 0 & FB_POST_KNN_df$distance5 == 0)

save(FB_POST_KNN_df, file = "cor_07_k25_30MZP_each/FB_POST_KNN_df.RData")


############################################################ .. Zusammenführen matched IDs PRE und POST ####

load("cor_07_k25_30MZP_each/FB_PRE_KNN_df.RData")
load("cor_07_k25_30MZP_each/FB_POST_KNN_df.RData")


# um PRE- und POST-MZP matchen zu können, werden alle IDs/Zeilen gelöscht, die nicht pro Intervall 6 Matches haben
PrePost_inter = intersect(FB_PRE_KNN_df$ID, FB_POST_KNN_df$ID)
FB_PRE_KNN_df = subset(
  FB_PRE_KNN_df,
  subset = FB_PRE_KNN_df$ID %in% PrePost_inter,
  select = c(ID, neighbor1, neighbor2, neighbor3, neighbor4, neighbor5)
)

FB_POST_KNN_df = subset(
  FB_POST_KNN_df,
  subset = FB_POST_KNN_df$ID %in% PrePost_inter,
  select = c(ID, neighbor1, neighbor2, neighbor3, neighbor4, neighbor5)
)

# Test darauf, ob die IDs in der ersten Spalte von FB_PRE_KNN_df und FB_POST_KNN_df gleich sind
length(which(FB_PRE_KNN_df$ID != FB_POST_KNN_df$ID))

# IDs gleich, also können beide Tabellen zusammengesetzt werden
FB_matched_Pre_Post_IDs = cbind(FB_PRE_KNN_df, FB_POST_KNN_df)
colnames(FB_matched_Pre_Post_IDs) = c(
  "ID1_PRE",
  "ID2_PRE",
  "ID3_PRE",
  "ID4_PRE",
  "ID5_PRE",
  "ID6_PRE",
  "ID1_POST",
  "ID2_POST",
  "ID3_POST",
  "ID4_POST",
  "ID5_POST",
  "ID6_POST"
)

# über die Zeilen hinweg gleiche Match-Kombinationen entfernen, um keine gleichen MZP-Kombinationen
# in unterschiedlicher Reihenfolge zu bekommen
# strenger Weg: Nur diejenigen Intervall-Kombinationen werden behalten, deren IDs multipliziert 
# einen einzigartigen Wert ergeben.

FB_matched_Pre_Post_IDs$PreProd = apply(FB_matched_Pre_Post_IDs[,1:6], 1, prod)
FB_matched_Pre_Post_IDs$PostProd = apply(FB_matched_Pre_Post_IDs[,7:12], 1, prod)
#length(unique(FB_matched_Pre_Post_IDs$PreProd))
#length(unique(FB_matched_Pre_Post_IDs$PostProd))

FB_matched_Pre_Post_IDs = subset(FB_matched_Pre_Post_IDs,
                                 subset = !duplicated(FB_matched_Pre_Post_IDs$PreProd) & !duplicated(FB_matched_Pre_Post_IDs$PostProd),
                                 select = -c(PreProd, PostProd))

# vielleicht anderer Lösungsansatz?:
#FB_matched_Pre_Post_IDs$ID2_PRE[FB_matched_Pre_Post_IDs$ID2_PRE %in% FB_matched_Pre_Post_IDs$ID1_PRE] = NA
#FB_matched_Pre_Post_IDs$ID3_PRE[FB_matched_Pre_Post_IDs$ID3_PRE %in% FB_matched_Pre_Post_IDs$ID1_PRE] = NA
#...
#FB_matched_Pre_Post_IDs = subset(FB_matched_Pre_Post_IDs, subset = anyNA(FB_matched_Pre_Post_IDs) == 0)

save(FB_matched_Pre_Post_IDs, file = "cor_07_k25_30MZP_each/FB_matched_Pre_Post_IDs.RData")


############################################################ .. Zusammenführen PRE- und POST-MZP anhand matched IDs ####

load("cor_07_k25/FB_original.RData")
FB_data = FB_original

load("cor_07_k25_30MZP_each/FB_matched_Pre_Post_IDs.RData")

FB_30MZP_each = data.frame(
  ID1_PRE = c(),
  ID2_PRE = c(),
  ID3_PRE = c(),
  ID4_PRE = c(),
  ID5_PRE = c(),
  ID6_PRE = c(),
  ID1_POST = c(),
  ID2_POST = c(),
  ID3_POST = c(),
  ID4_POST = c(),
  ID5_POST = c(),
  ID6_POST = c(),
  
  PRE1_1 = c(),
  PRE1_2 = c(),
  PRE1_3 = c(),
  PRE1_4 = c(),
  PRE1_5 = c(),
  PRE1_6 = c(),
  PRE1_7 = c(),
  PRE1_8 = c(),
  PRE1_9 = c(),
  PRE1_10 = c(),
  PRE1_11 = c(),
  PRE1_12 = c(),
  PRE1_13 = c(),
  PRE1_14 = c(),
  PRE1_15 = c(),
  PRE1_16 = c(),
  PRE1_17 = c(),
  PRE1_18 = c(),
  PRE1_19 = c(),
  PRE1_20 = c(),
  PRE1_21 = c(),
  PRE1_22 = c(),
  PRE1_23 = c(),
  PRE1_24 = c(),
  PRE1_25 = c(),
  PRE1_26 = c(),
  PRE1_27 = c(),
  PRE1_28 = c(),
  PRE1_29 = c(),
  PRE1_30 = c(),
  
  POST1_1 = c(),
  POST1_2 = c(),
  POST1_3 = c(),
  POST1_4 = c(),
  POST1_5 = c(),
  POST1_6 = c(),
  POST1_7 = c(),
  POST1_8 = c(),
  POST1_9 = c(),
  POST1_10 = c(),
  POST1_11 = c(),
  POST1_12 = c(),
  POST1_13 = c(),
  POST1_14 = c(),
  POST1_15 = c(),
  POST1_16 = c(),
  POST1_17 = c(),
  POST1_18 = c(),
  POST1_19 = c(),
  POST1_20 = c(),
  POST1_21 = c(),
  POST1_22 = c(),
  POST1_23 = c(),
  POST1_24 = c(),
  POST1_25 = c(),
  POST1_26 = c(),
  POST1_27 = c(),
  POST1_28 = c(),
  POST1_29 = c(),
  POST1_30 = c()
)

for (i in 1:length(FB_matched_Pre_Post_IDs$ID1_PRE)) {
  FB_30MZP_each[i,"ID1_PRE"] = FB_matched_Pre_Post_IDs[i,"ID1_PRE"]
  FB_30MZP_each[i,"ID2_PRE"] = FB_matched_Pre_Post_IDs[i,"ID2_PRE"]
  FB_30MZP_each[i,"ID3_PRE"] = FB_matched_Pre_Post_IDs[i,"ID3_PRE"]
  FB_30MZP_each[i,"ID4_PRE"] = FB_matched_Pre_Post_IDs[i,"ID4_PRE"]
  FB_30MZP_each[i,"ID5_PRE"] = FB_matched_Pre_Post_IDs[i,"ID5_PRE"]
  FB_30MZP_each[i,"ID6_PRE"] = FB_matched_Pre_Post_IDs[i,"ID6_PRE"]
  FB_30MZP_each[i,"ID1_POST"] = FB_matched_Pre_Post_IDs[i,"ID1_POST"]
  FB_30MZP_each[i,"ID2_POST"] = FB_matched_Pre_Post_IDs[i,"ID2_POST"]
  FB_30MZP_each[i,"ID3_POST"] = FB_matched_Pre_Post_IDs[i,"ID3_POST"]
  FB_30MZP_each[i,"ID4_POST"] = FB_matched_Pre_Post_IDs[i,"ID4_POST"]
  FB_30MZP_each[i,"ID5_POST"] = FB_matched_Pre_Post_IDs[i,"ID5_POST"]
  FB_30MZP_each[i,"ID6_POST"] = FB_matched_Pre_Post_IDs[i,"ID6_POST"]
  
  FB_30MZP_each[i,"PRE1_1"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID1_PRE"],"PRE1_1"]
  FB_30MZP_each[i,"PRE1_2"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID1_PRE"],"PRE1_2"]
  FB_30MZP_each[i,"PRE1_3"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID1_PRE"],"PRE1_3"]
  FB_30MZP_each[i,"PRE1_4"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID1_PRE"],"PRE1_4"]
  FB_30MZP_each[i,"PRE1_5"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID1_PRE"],"PRE1_5"]
  
  FB_30MZP_each[i,"PRE1_6"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID2_PRE"],"PRE1_1"]
  FB_30MZP_each[i,"PRE1_7"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID2_PRE"],"PRE1_2"]
  FB_30MZP_each[i,"PRE1_8"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID2_PRE"],"PRE1_3"]
  FB_30MZP_each[i,"PRE1_9"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID2_PRE"],"PRE1_4"]
  FB_30MZP_each[i,"PRE1_10"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID2_PRE"],"PRE1_5"]
  
  FB_30MZP_each[i,"PRE1_11"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID3_PRE"],"PRE1_1"]
  FB_30MZP_each[i,"PRE1_12"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID3_PRE"],"PRE1_2"]
  FB_30MZP_each[i,"PRE1_13"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID3_PRE"],"PRE1_3"]
  FB_30MZP_each[i,"PRE1_14"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID3_PRE"],"PRE1_4"]
  FB_30MZP_each[i,"PRE1_15"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID3_PRE"],"PRE1_5"]
  
  FB_30MZP_each[i,"PRE1_16"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID4_PRE"],"PRE1_1"]
  FB_30MZP_each[i,"PRE1_17"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID4_PRE"],"PRE1_2"]
  FB_30MZP_each[i,"PRE1_18"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID4_PRE"],"PRE1_3"]
  FB_30MZP_each[i,"PRE1_19"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID4_PRE"],"PRE1_4"]
  FB_30MZP_each[i,"PRE1_20"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID4_PRE"],"PRE1_5"]
  
  FB_30MZP_each[i,"PRE1_21"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID5_PRE"],"PRE1_1"]
  FB_30MZP_each[i,"PRE1_22"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID5_PRE"],"PRE1_2"]
  FB_30MZP_each[i,"PRE1_23"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID5_PRE"],"PRE1_3"]
  FB_30MZP_each[i,"PRE1_24"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID5_PRE"],"PRE1_4"]
  FB_30MZP_each[i,"PRE1_25"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID5_PRE"],"PRE1_5"]
  
  FB_30MZP_each[i,"PRE1_26"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID6_PRE"],"PRE1_1"]
  FB_30MZP_each[i,"PRE1_27"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID6_PRE"],"PRE1_2"]
  FB_30MZP_each[i,"PRE1_28"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID6_PRE"],"PRE1_3"]
  FB_30MZP_each[i,"PRE1_29"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID6_PRE"],"PRE1_4"]
  FB_30MZP_each[i,"PRE1_30"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID6_PRE"],"PRE1_5"]
  
  
  FB_30MZP_each[i,"POST1_1"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID1_POST"],"POST1_1"]
  FB_30MZP_each[i,"POST1_2"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID1_POST"],"POST1_2"]
  FB_30MZP_each[i,"POST1_3"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID1_POST"],"POST1_3"]
  FB_30MZP_each[i,"POST1_4"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID1_POST"],"POST1_4"]
  FB_30MZP_each[i,"POST1_5"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID1_POST"],"POST1_5"]
  
  FB_30MZP_each[i,"POST1_6"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID2_POST"],"POST1_1"]
  FB_30MZP_each[i,"POST1_7"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID2_POST"],"POST1_2"]
  FB_30MZP_each[i,"POST1_8"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID2_POST"],"POST1_3"]
  FB_30MZP_each[i,"POST1_9"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID2_POST"],"POST1_4"]
  FB_30MZP_each[i,"POST1_10"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID2_POST"],"POST1_5"]
  
  FB_30MZP_each[i,"POST1_11"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID3_POST"],"POST1_1"]
  FB_30MZP_each[i,"POST1_12"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID3_POST"],"POST1_2"]
  FB_30MZP_each[i,"POST1_13"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID3_POST"],"POST1_3"]
  FB_30MZP_each[i,"POST1_14"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID3_POST"],"POST1_4"]
  FB_30MZP_each[i,"POST1_15"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID3_POST"],"POST1_5"]
  
  FB_30MZP_each[i,"POST1_16"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID4_POST"],"POST1_1"]
  FB_30MZP_each[i,"POST1_17"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID4_POST"],"POST1_2"]
  FB_30MZP_each[i,"POST1_18"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID4_POST"],"POST1_3"]
  FB_30MZP_each[i,"POST1_19"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID4_POST"],"POST1_4"]
  FB_30MZP_each[i,"POST1_20"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID4_POST"],"POST1_5"]
  
  FB_30MZP_each[i,"POST1_21"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID5_POST"],"POST1_1"]
  FB_30MZP_each[i,"POST1_22"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID5_POST"],"POST1_2"]
  FB_30MZP_each[i,"POST1_23"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID5_POST"],"POST1_3"]
  FB_30MZP_each[i,"POST1_24"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID5_POST"],"POST1_4"]
  FB_30MZP_each[i,"POST1_25"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID5_POST"],"POST1_5"]
  
  FB_30MZP_each[i,"POST1_26"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID6_POST"],"POST1_1"]
  FB_30MZP_each[i,"POST1_27"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID6_POST"],"POST1_2"]
  FB_30MZP_each[i,"POST1_28"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID6_POST"],"POST1_3"]
  FB_30MZP_each[i,"POST1_29"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID6_POST"],"POST1_4"]
  FB_30MZP_each[i,"POST1_30"] = FB_data[FB_matched_Pre_Post_IDs[i,"ID6_POST"],"POST1_5"]
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

FB_30MZP_each$PRE_Mean = apply(FB_30MZP_each[pre_30mzp], 1, mean)
FB_30MZP_each$POST_Mean = apply(FB_30MZP_each[post_30mzp], 1, mean)
FB_30MZP_each$MeanDiff = FB_30MZP_each$PRE_Mean - FB_30MZP_each$POST_Mean
FB_30MZP_each$ind.pretestSD = apply(FB_30MZP_each[pre_30mzp], 1, sd)
FB_30MZP_each$ind.posttestSD = apply(FB_30MZP_each[post_30mzp], 1, sd)

FB_30MZP_each = FB_30MZP_each %>% 
  add_column(., .before = "ID1_PRE", ID = 1:nrow(.))

write.table(FB_30MZP_each, file = "cor_07_k25_30MZP_each/FB_30MZP_each.txt")
save(FB_30MZP_each, file = "cor_07_k25_30MZP_each/FB_30MZP_each.RData")



############################################ Test auf Gleichheit der IDs und MZP zwischen Datensets

load("cor_07_k1/FB_original.RData")
load("cor_07_k1/FB_2MZP.RData")
load("cor_07_k1_30MZP_each/FB_30MZP_each.RData")

length(which(FB_original$ID == FB_30MZP_each$ID))
length(which(FB_original$ID == FB_2MZP$ID))

length(which(FB_original$PRE1_1 == FB_30MZP_each$PRE1_1))
length(which(FB_original$PRE1_2 == FB_30MZP_each$PRE1_2))
length(which(FB_original$PRE1_3 == FB_30MZP_each$PRE1_3))
length(which(FB_original$PRE1_4 == FB_30MZP_each$PRE1_4))
length(which(FB_original$PRE1_5 == FB_30MZP_each$PRE1_5))

length(which(FB_original$POST1_1 == FB_30MZP_each$POST1_1))
length(which(FB_original$POST1_2 == FB_30MZP_each$POST1_2))
length(which(FB_original$POST1_3 == FB_30MZP_each$POST1_3))
length(which(FB_original$POST1_4 == FB_30MZP_each$POST1_4))
length(which(FB_original$POST1_5 == FB_30MZP_each$POST1_5))

length(which(FB_original$PRE1_1 == FB_2MZP$PRE))
length(which(FB_original$POST1_1 == FB_2MZP$POST))

FB_original$ID = 1:nrow(FB_original)
FB_2MZP$ID = 1:nrow(FB_2MZP)

save(FB_original, file = "cor_07_k1/FB_original.RData")
save(FB_2MZP, file = "cor_07_k1/FB_2MZP.RData")
save(FB_30MZP_each, file = "cor_07_k1_30MZP_each/FB_30MZP_each.RData")



############################################ Cohen´s d und Pre-Post-Overlap-Plot

FB_30MZP_each = FB_30MZP_each %>% 
  filter(ind.pretestSD > 0.44 & ind.posttestSD > 0.44)

FB_30MZP_each$ID = 1:nrow(FB_30MZP_each)

d = round((mean(FB_30MZP_each$PRE1_1) - mean(FB_30MZP_each$POST1_1)) / 
            sqrt(0.5 * (sd(FB_30MZP_each$PRE1_1)^2 + sd(FB_30MZP_each$POST1_1)^2)), digits = 2)

final.plot(list(FB_30MZP_PRE_Mean = FB_30MZP_each$PRE_Mean, FB_30MZP_POST_Mean = FB_30MZP_each$POST_Mean), 
           overlap(list(FB_30MZP_PRE_Mean = FB_30MZP_each$PRE_Mean, FB_30MZP_POST_Mean = FB_30MZP_each$POST_Mean))$OV)

ggsave("cor_07_k25_30MZP_each/Plot_cor_07_dataset_k25_d0.9.jpg", plot = last_plot(), width = 6, height = 4)


# gleiche Prozedur auch für das Datenset cor_07_k25 mit 5 MZP
load("cor_07_k25/FB_original.RData")
load("cor_07_k25_30MZP_each/FB_30MZP_each.RData")

FB_original = FB_original %>% 
  filter(ID %in% FB_30MZP_each$ID1_PRE)

save(FB_original, file = "cor_07_k25/FB_original.RData")


final.plot(list(FB_original_PRE_Mean = FB_original$PRE_Mean, FB_original_POST_Mean = FB_original$POST_Mean), 
           overlap(list(FB_original_PRE_Mean = FB_original$PRE_Mean, FB_original_POST_Mean = FB_original$POST_Mean))$OV)

ggsave("cor_07_k25/Plot_cor_07_dataset_k25_d0.9.jpg", plot = last_plot(), width = 6, height = 4)




############################################################
##### cor_04_k20 #####

# Öffnen des gesamten Datensets (N = 100.000) und Berechnung von MWs, SDs usw.
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
write_sav(EMA_5.5, "cor_04_k20/cor_04_k20.sav")

### ...
### nächster Schritt in SPSS: k-nearest-neighbor-Analyse zur Erweiterung auf 30+30 MZP pro Person
### ...

### Öffnen der XML-Dateien mit den matched cases aus SPSS
# EMA: KNN-Matching_PRE_5NN
EMA_PRE_KNN_list = xmlToList("cor_04_k20/KNN-Matching_PRE_5NN.xml")
EMA_PRE_KNN = EMA_PRE_KNN_list$"NearestNeighborModel"$Extension$SimpleTable
save(EMA_PRE_KNN, file = "cor_04_k20/EMA_PRE_KNN.RData")

# EMA: KNN-Matching_POST_5NN
EMA_POST_KNN_list = xmlToList("cor_04_k20/KNN-Matching_POST_5NN.xml")
EMA_POST_KNN = EMA_POST_KNN_list$"NearestNeighborModel"$Extension$SimpleTable
save(EMA_POST_KNN, file = "cor_04_k20/EMA_POST_KNN.RData")


############################################################ . EMA: KNN-Matching ####
############################################################ .. PRE ####

load("cor_04_k20/EMA_PRE_KNN.RData")
EMA_PRE_KNN = EMA_PRE_KNN[- 1]

for (i in 1:length(EMA_PRE_KNN)) {
  EMA_PRE_KNN[[i]] = as.vector(unlist(strsplit(EMA_PRE_KNN[[i]], ";")))
  message(i)
}

EMA_PRE_KNN_df = t(as.data.frame(EMA_PRE_KNN, row.names = NULL, optional = FALSE, fix.empty.names = TRUE,  stringsAsFactors = default.stringsAsFactors()))
colnames(EMA_PRE_KNN_df) = EMA_PRE_KNN_df[1,]
EMA_PRE_KNN_df = EMA_PRE_KNN_df[-1,]
EMA_PRE_KNN_df = EMA_PRE_KNN_df[-100001,]
EMA_PRE_KNN_df = EMA_PRE_KNN_df[,-2]

EMA_PRE_KNN_df = as.data.frame(EMA_PRE_KNN_df, row.names = c(1:100000),  stringsAsFactors = FALSE)

for (k in colnames(EMA_PRE_KNN_df)) {
  EMA_PRE_KNN_df[,k] = as.numeric(EMA_PRE_KNN_df[,k])
  message(k)
}

EMA_PRE_KNN_df = subset(EMA_PRE_KNN_df, subset = EMA_PRE_KNN_df$distance1 == 0 & EMA_PRE_KNN_df$distance2 == 0 & EMA_PRE_KNN_df$distance3 == 0 & EMA_PRE_KNN_df$distance4 == 0 & EMA_PRE_KNN_df$distance5 == 0)

save(EMA_PRE_KNN_df, file = "cor_04_k20/EMA_PRE_KNN_df.RData")


############################################################ .. POST ####

load("cor_04_k20/EMA_POST_KNN.RData")
EMA_POST_KNN = EMA_POST_KNN[- 1]

for (i in 1:length(EMA_POST_KNN)) {
  EMA_POST_KNN[[i]] = as.vector(unlist(strsplit(EMA_POST_KNN[[i]], ";")))
  message(i)
}

EMA_POST_KNN_df = t(as.data.frame(EMA_POST_KNN, row.names = NULL, optional = FALSE, fix.empty.names = TRUE,  stringsAsFactors = default.stringsAsFactors()))
colnames(EMA_POST_KNN_df) = EMA_POST_KNN_df[1,]
EMA_POST_KNN_df = EMA_POST_KNN_df[-1,]
EMA_POST_KNN_df = EMA_POST_KNN_df[-100001,]
EMA_POST_KNN_df = EMA_POST_KNN_df[,-2]

EMA_POST_KNN_df = as.data.frame(EMA_POST_KNN_df, row.names = c(1:100000),  stringsAsFactors = FALSE)

for (k in colnames(EMA_POST_KNN_df)) {
  EMA_POST_KNN_df[,k] = as.numeric(EMA_POST_KNN_df[,k])
  message(k)
}

EMA_POST_KNN_df = subset(EMA_POST_KNN_df, subset = EMA_POST_KNN_df$distance1 == 0 & EMA_POST_KNN_df$distance2 == 0 & EMA_POST_KNN_df$distance3 == 0 & EMA_POST_KNN_df$distance4 == 0 & EMA_POST_KNN_df$distance5 == 0)

save(EMA_POST_KNN_df, file = "cor_04_k20/EMA_POST_KNN_df.RData")


############################################################ .. Zusammenführen matched IDs PRE und POST ####

load("cor_04_k20/EMA_PRE_KNN_df.RData")
load("cor_04_k20/EMA_POST_KNN_df.RData")


# um PRE- und POST-MZP matchen zu können, werden alle IDs/Zeilen gelöscht, die nicht pro Intervall 6 Matches haben
PrePost_inter = intersect(EMA_PRE_KNN_df$ID, EMA_POST_KNN_df$ID)
EMA_PRE_KNN_df = subset(
  EMA_PRE_KNN_df,
  subset = EMA_PRE_KNN_df$ID %in% PrePost_inter,
  select = c(ID, neighbor1, neighbor2, neighbor3, neighbor4, neighbor5)
)

EMA_POST_KNN_df = subset(
  EMA_POST_KNN_df,
  subset = EMA_POST_KNN_df$ID %in% PrePost_inter,
  select = c(ID, neighbor1, neighbor2, neighbor3, neighbor4, neighbor5)
)

# Test darauf, ob die IDs in der ersten Spalte von EMA_PRE_KNN_df und EMA_POST_KNN_df gleich sind
length(which(EMA_PRE_KNN_df$ID != EMA_POST_KNN_df$ID))

# IDs gleich, also können beide Tabellen zusammengesetzt werden
EMA_matched_Pre_Post_IDs = cbind(EMA_PRE_KNN_df, EMA_POST_KNN_df)
colnames(EMA_matched_Pre_Post_IDs) = c(
  "ID1_PRE",
  "ID2_PRE",
  "ID3_PRE",
  "ID4_PRE",
  "ID5_PRE",
  "ID6_PRE",
  "ID1_POST",
  "ID2_POST",
  "ID3_POST",
  "ID4_POST",
  "ID5_POST",
  "ID6_POST"
)

# über die Zeilen hinweg gleiche Match-Kombinationen entfernen, um keine gleichen MZP-Kombinationen
# in unterschiedlicher Reihenfolge zu bekommen
# strenger Weg: Nur diejenigen Intervall-Kombinationen werden behalten, deren IDs multipliziert 
# einen einzigartigen Wert ergeben.

EMA_matched_Pre_Post_IDs$PreProd = apply(EMA_matched_Pre_Post_IDs[,1:6], 1, prod)
EMA_matched_Pre_Post_IDs$PostProd = apply(EMA_matched_Pre_Post_IDs[,7:12], 1, prod)
#length(unique(EMA_matched_Pre_Post_IDs$PreProd))
#length(unique(EMA_matched_Pre_Post_IDs$PostProd))

EMA_matched_Pre_Post_IDs = subset(EMA_matched_Pre_Post_IDs,
                                 subset = !duplicated(EMA_matched_Pre_Post_IDs$PreProd) & !duplicated(EMA_matched_Pre_Post_IDs$PostProd),
                                 select = -c(PreProd, PostProd))

# vielleicht anderer Lösungsansatz?:
#EMA_matched_Pre_Post_IDs$ID2_PRE[EMA_matched_Pre_Post_IDs$ID2_PRE %in% EMA_matched_Pre_Post_IDs$ID1_PRE] = NA
#EMA_matched_Pre_Post_IDs$ID3_PRE[EMA_matched_Pre_Post_IDs$ID3_PRE %in% EMA_matched_Pre_Post_IDs$ID1_PRE] = NA
#...
#EMA_matched_Pre_Post_IDs = subset(EMA_matched_Pre_Post_IDs, subset = anyNA(EMA_matched_Pre_Post_IDs) == 0)

save(EMA_matched_Pre_Post_IDs, file = "cor_04_k20/EMA_matched_Pre_Post_IDs.RData")


############################################################ .. Zusammenführen PRE- und POST-MZP anhand matched IDs ####

load("cor_04_k20/EMA_5.5.RData")
EMA_data = EMA_5.5

load("cor_04_k20/EMA_matched_Pre_Post_IDs.RData")

EMA_30.30 = data.frame(
  ID1_PRE = c(),
  ID2_PRE = c(),
  ID3_PRE = c(),
  ID4_PRE = c(),
  ID5_PRE = c(),
  ID6_PRE = c(),
  ID1_POST = c(),
  ID2_POST = c(),
  ID3_POST = c(),
  ID4_POST = c(),
  ID5_POST = c(),
  ID6_POST = c(),
  
  PRE1_1 = c(),
  PRE1_2 = c(),
  PRE1_3 = c(),
  PRE1_4 = c(),
  PRE1_5 = c(),
  PRE1_6 = c(),
  PRE1_7 = c(),
  PRE1_8 = c(),
  PRE1_9 = c(),
  PRE1_10 = c(),
  PRE1_11 = c(),
  PRE1_12 = c(),
  PRE1_13 = c(),
  PRE1_14 = c(),
  PRE1_15 = c(),
  PRE1_16 = c(),
  PRE1_17 = c(),
  PRE1_18 = c(),
  PRE1_19 = c(),
  PRE1_20 = c(),
  PRE1_21 = c(),
  PRE1_22 = c(),
  PRE1_23 = c(),
  PRE1_24 = c(),
  PRE1_25 = c(),
  PRE1_26 = c(),
  PRE1_27 = c(),
  PRE1_28 = c(),
  PRE1_29 = c(),
  PRE1_30 = c(),
  
  POST1_1 = c(),
  POST1_2 = c(),
  POST1_3 = c(),
  POST1_4 = c(),
  POST1_5 = c(),
  POST1_6 = c(),
  POST1_7 = c(),
  POST1_8 = c(),
  POST1_9 = c(),
  POST1_10 = c(),
  POST1_11 = c(),
  POST1_12 = c(),
  POST1_13 = c(),
  POST1_14 = c(),
  POST1_15 = c(),
  POST1_16 = c(),
  POST1_17 = c(),
  POST1_18 = c(),
  POST1_19 = c(),
  POST1_20 = c(),
  POST1_21 = c(),
  POST1_22 = c(),
  POST1_23 = c(),
  POST1_24 = c(),
  POST1_25 = c(),
  POST1_26 = c(),
  POST1_27 = c(),
  POST1_28 = c(),
  POST1_29 = c(),
  POST1_30 = c()
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

EMA_30.30 = EMA_30.30 %>% 
  add_column(., .before = "ID1_PRE", ID = 1:nrow(.))

write.table(EMA_30.30, file = "cor_04_k20/EMA_30.30.txt")
save(EMA_30.30, file = "cor_04_k20/EMA_30.30.RData")



# Cohen´s d und Pre-Post-Overlap-Plot

EMA_30.30 = EMA_30.30 %>% 
  filter(ind.pretestSD > 0.44 & ind.posttestSD > 0.44)

EMA_30.30$ID = 1:nrow(EMA_30.30)

d = round((mean(EMA_30.30$PRE1_1) - mean(EMA_30.30$POST1_1)) / 
            sqrt(0.5 * (sd(EMA_30.30$PRE1_1)^2 + sd(EMA_30.30$POST1_1)^2)), digits = 2)

final.plot(list(EMA_30.30_PRE_Mean = EMA_30.30$PRE_Mean, EMA_30.30_POST_Mean = EMA_30.30$POST_Mean), 
           overlap(list(EMA_30.30_PRE_Mean = EMA_30.30$PRE_Mean, EMA_30.30_POST_Mean = EMA_30.30$POST_Mean))$OV)

ggsave("cor_04_k20/Plot_cor_04_dataset_k20_d0.99.jpg", plot = last_plot(), width = 6, height = 4)


# gleiche Prozedur auch für das Datenset cor_04_k20 mit 5 MZP
load("cor_04_k20/EMA_5.5.RData")
load("cor_04_k20/EMA_30.30.RData")

EMA_5.5 = EMA_5.5 %>% 
  filter(ID %in% EMA_30.30$ID1_PRE)

save(EMA_5.5, file = "cor_04_k20/EMA_5.5.RData")


final.plot(list(EMA_5.5_PRE_Mean = EMA_5.5$PRE_Mean, EMA_5.5_POST_Mean = EMA_5.5$POST_Mean), 
           overlap(list(EMA_5.5_PRE_Mean = EMA_5.5$PRE_Mean, EMA_5.5_POST_Mean = EMA_5.5$POST_Mean))$OV)

ggsave("cor_04_k20/Plot_cor_04_dataset_k20_d0.99.jpg", plot = last_plot(), width = 6, height = 4)


