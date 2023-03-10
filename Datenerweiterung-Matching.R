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

# EMA: KNN-Matching_PRE_5NN
EMA_PRE_KNN_list = xmlToList("C:/Users/steph/OneDrive/Desktop/Psychologie/Masterstudium Psychologie/3. Semester/1. Master-Konversatorium/Preprocessing und Berechnungen/cor_04_k1_30MZP_each/KNN-Matching_PRE_5NN.xml")
EMA_PRE_KNN = EMA_PRE_KNN_list$"NearestNeighborModel"$Extension$SimpleTable
save(EMA_PRE_KNN, file = "cor_04_k1_30MZP_each/EMA_PRE_KNN.RData")

# EMA: KNN-Matching_POST_5NN
EMA_POST_KNN_list = xmlToList("C:/Users/steph/OneDrive/Desktop/Psychologie/Masterstudium Psychologie/3. Semester/1. Master-Konversatorium/Preprocessing und Berechnungen/cor_04_k1_30MZP_each/KNN-Matching_POST_5NN.xml")
EMA_POST_KNN = EMA_POST_KNN_list$"NearestNeighborModel"$Extension$SimpleTable
save(EMA_POST_KNN, file = "cor_04_k1_30MZP_each/EMA_POST_KNN.RData")

# FB: KNN-Matching_PRE_5NN
FB_PRE_KNN_list = xmlToList("C:/Users/steph/OneDrive/Desktop/Psychologie/Masterstudium Psychologie/3. Semester/1. Master-Konversatorium/Preprocessing und Berechnungen/cor_07_k1_30MZP_each/KNN-Matching_PRE_5NN.xml")
FB_PRE_KNN = FB_PRE_KNN_list$"NearestNeighborModel"$Extension$SimpleTable
save(FB_PRE_KNN, file = "cor_07_k1_30MZP_each/FB_PRE_KNN.RData")

# FB: KNN-Matching_POST_5NN
FB_POST_KNN_list = xmlToList("C:/Users/steph/OneDrive/Desktop/Psychologie/Masterstudium Psychologie/3. Semester/1. Master-Konversatorium/Preprocessing und Berechnungen/cor_07_k1_30MZP_each/KNN-Matching_POST_5NN.xml")
FB_POST_KNN = FB_POST_KNN_list$"NearestNeighborModel"$Extension$SimpleTable
save(FB_POST_KNN, file = "cor_07_k1_30MZP_each/FB_POST_KNN.RData")


############################################################ . EMA: KNN-Matching ####

setwd(
  "C:/Users/steph/OneDrive/Desktop/Psychologie/Masterstudium Psychologie/3. Semester/1. Master-Konversatorium/Preprocessing und Berechnungen"
)


############################################################ .. PRE ####

load("cor_04_k1_30MZP_each/EMA_PRE_KNN.RData")
EMA_PRE_KNN = EMA_PRE_KNN[- 1]

for (i in 1:length(EMA_PRE_KNN)) {
  EMA_PRE_KNN[[i]] = as.vector(unlist(strsplit(EMA_PRE_KNN[[i]], ";")))
}

EMA_PRE_KNN_df = t(as.data.frame(EMA_PRE_KNN, row.names = NULL, optional = FALSE, fix.empty.names = TRUE,  stringsAsFactors = default.stringsAsFactors()))
colnames(EMA_PRE_KNN_df) = EMA_PRE_KNN_df[1,]
EMA_PRE_KNN_df = EMA_PRE_KNN_df[-1,]
EMA_PRE_KNN_df = EMA_PRE_KNN_df[-100001,]
EMA_PRE_KNN_df = EMA_PRE_KNN_df[,-2]

EMA_PRE_KNN_df = as.data.frame(EMA_PRE_KNN_df, row.names = c(1:100000),  stringsAsFactors = FALSE)

for (k in colnames(EMA_PRE_KNN_df)) {
  EMA_PRE_KNN_df[,k] = as.numeric(EMA_PRE_KNN_df[,k])
}

EMA_PRE_KNN_df = subset(EMA_PRE_KNN_df, subset = EMA_PRE_KNN_df$distance1 == 0 & EMA_PRE_KNN_df$distance2 == 0 & EMA_PRE_KNN_df$distance3 == 0 & EMA_PRE_KNN_df$distance4 == 0 & EMA_PRE_KNN_df$distance5 == 0)

save(EMA_PRE_KNN_df, file = "cor_04_k1_30MZP_each/EMA_PRE_KNN_df.RData")


############################################################ .. POST ####

load("cor_04_k1_30MZP_each/EMA_POST_KNN.RData")
EMA_POST_KNN = EMA_POST_KNN[- 1]

for (i in 1:length(EMA_POST_KNN)) {
  EMA_POST_KNN[[i]] = as.vector(unlist(strsplit(EMA_POST_KNN[[i]], ";")))
}

EMA_POST_KNN_df = t(as.data.frame(EMA_POST_KNN, row.names = NULL, optional = FALSE, fix.empty.names = TRUE,  stringsAsFactors = default.stringsAsFactors()))
colnames(EMA_POST_KNN_df) = EMA_POST_KNN_df[1,]
EMA_POST_KNN_df = EMA_POST_KNN_df[-1,]
EMA_POST_KNN_df = EMA_POST_KNN_df[-100001,]
EMA_POST_KNN_df = EMA_POST_KNN_df[,-2]

EMA_POST_KNN_df = as.data.frame(EMA_POST_KNN_df, row.names = c(1:100000),  stringsAsFactors = FALSE)

for (k in colnames(EMA_POST_KNN_df)) {
  EMA_POST_KNN_df[,k] = as.numeric(EMA_POST_KNN_df[,k])
}

EMA_POST_KNN_df = subset(EMA_POST_KNN_df, subset = EMA_POST_KNN_df$distance1 == 0 & EMA_POST_KNN_df$distance2 == 0 & EMA_POST_KNN_df$distance3 == 0 & EMA_POST_KNN_df$distance4 == 0 & EMA_POST_KNN_df$distance5 == 0)

save(EMA_POST_KNN_df, file = "cor_04_k1_30MZP_each/EMA_POST_KNN_df.RData")


############################################################ .. Zusammenführen matched IDs PRE und POST ####

load("cor_04_k1_30MZP_each/EMA_PRE_KNN_df.RData")
load("cor_04_k1_30MZP_each/EMA_POST_KNN_df.RData")


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

save(EMA_matched_Pre_Post_IDs, file = "cor_04_k1_30MZP_each/EMA_matched_Pre_Post_IDs.RData")


############################################################ .. Zusammenführen PRE- und POST-MZP anhand matched IDs ####

setwd(
  "C:/Users/steph/OneDrive/Desktop/Psychologie/Masterstudium Psychologie/3. Semester/1. Master-Konversatorium/Preprocessing und Berechnungen"
)

EMA_data = read.table(file = "cor_04_k1/cor_04_dataset_k1.txt", header = TRUE, sep = "")
EMA_data = subset(EMA_data, select = c(PRE1_1, PRE1_2, PRE1_3, PRE1_4, PRE1_5, POST1_1, POST1_2, POST1_3, POST1_4, POST1_5))
EMA_data = cbind(ID = 1:100000, EMA_data)

load("cor_04_k1_30MZP_each/EMA_matched_Pre_Post_IDs.RData")

EMA_30MZP_each = data.frame(
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
  EMA_30MZP_each[i,"ID1_PRE"] = EMA_matched_Pre_Post_IDs[i,"ID1_PRE"]
  EMA_30MZP_each[i,"ID2_PRE"] = EMA_matched_Pre_Post_IDs[i,"ID2_PRE"]
  EMA_30MZP_each[i,"ID3_PRE"] = EMA_matched_Pre_Post_IDs[i,"ID3_PRE"]
  EMA_30MZP_each[i,"ID4_PRE"] = EMA_matched_Pre_Post_IDs[i,"ID4_PRE"]
  EMA_30MZP_each[i,"ID5_PRE"] = EMA_matched_Pre_Post_IDs[i,"ID5_PRE"]
  EMA_30MZP_each[i,"ID6_PRE"] = EMA_matched_Pre_Post_IDs[i,"ID6_PRE"]
  EMA_30MZP_each[i,"ID1_POST"] = EMA_matched_Pre_Post_IDs[i,"ID1_POST"]
  EMA_30MZP_each[i,"ID2_POST"] = EMA_matched_Pre_Post_IDs[i,"ID2_POST"]
  EMA_30MZP_each[i,"ID3_POST"] = EMA_matched_Pre_Post_IDs[i,"ID3_POST"]
  EMA_30MZP_each[i,"ID4_POST"] = EMA_matched_Pre_Post_IDs[i,"ID4_POST"]
  EMA_30MZP_each[i,"ID5_POST"] = EMA_matched_Pre_Post_IDs[i,"ID5_POST"]
  EMA_30MZP_each[i,"ID6_POST"] = EMA_matched_Pre_Post_IDs[i,"ID6_POST"]
  
  EMA_30MZP_each[i,"PRE1_1"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID1_PRE"],"PRE1_1"]
  EMA_30MZP_each[i,"PRE1_2"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID1_PRE"],"PRE1_2"]
  EMA_30MZP_each[i,"PRE1_3"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID1_PRE"],"PRE1_3"]
  EMA_30MZP_each[i,"PRE1_4"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID1_PRE"],"PRE1_4"]
  EMA_30MZP_each[i,"PRE1_5"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID1_PRE"],"PRE1_5"]
  
  EMA_30MZP_each[i,"PRE1_6"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID2_PRE"],"PRE1_1"]
  EMA_30MZP_each[i,"PRE1_7"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID2_PRE"],"PRE1_2"]
  EMA_30MZP_each[i,"PRE1_8"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID2_PRE"],"PRE1_3"]
  EMA_30MZP_each[i,"PRE1_9"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID2_PRE"],"PRE1_4"]
  EMA_30MZP_each[i,"PRE1_10"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID2_PRE"],"PRE1_5"]
  
  EMA_30MZP_each[i,"PRE1_11"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID3_PRE"],"PRE1_1"]
  EMA_30MZP_each[i,"PRE1_12"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID3_PRE"],"PRE1_2"]
  EMA_30MZP_each[i,"PRE1_13"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID3_PRE"],"PRE1_3"]
  EMA_30MZP_each[i,"PRE1_14"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID3_PRE"],"PRE1_4"]
  EMA_30MZP_each[i,"PRE1_15"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID3_PRE"],"PRE1_5"]
  
  EMA_30MZP_each[i,"PRE1_16"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID4_PRE"],"PRE1_1"]
  EMA_30MZP_each[i,"PRE1_17"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID4_PRE"],"PRE1_2"]
  EMA_30MZP_each[i,"PRE1_18"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID4_PRE"],"PRE1_3"]
  EMA_30MZP_each[i,"PRE1_19"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID4_PRE"],"PRE1_4"]
  EMA_30MZP_each[i,"PRE1_20"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID4_PRE"],"PRE1_5"]
  
  EMA_30MZP_each[i,"PRE1_21"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID5_PRE"],"PRE1_1"]
  EMA_30MZP_each[i,"PRE1_22"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID5_PRE"],"PRE1_2"]
  EMA_30MZP_each[i,"PRE1_23"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID5_PRE"],"PRE1_3"]
  EMA_30MZP_each[i,"PRE1_24"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID5_PRE"],"PRE1_4"]
  EMA_30MZP_each[i,"PRE1_25"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID5_PRE"],"PRE1_5"]
  
  EMA_30MZP_each[i,"PRE1_26"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID6_PRE"],"PRE1_1"]
  EMA_30MZP_each[i,"PRE1_27"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID6_PRE"],"PRE1_2"]
  EMA_30MZP_each[i,"PRE1_28"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID6_PRE"],"PRE1_3"]
  EMA_30MZP_each[i,"PRE1_29"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID6_PRE"],"PRE1_4"]
  EMA_30MZP_each[i,"PRE1_30"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID6_PRE"],"PRE1_5"]
  
  
  EMA_30MZP_each[i,"POST1_1"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID1_POST"],"POST1_1"]
  EMA_30MZP_each[i,"POST1_2"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID1_POST"],"POST1_2"]
  EMA_30MZP_each[i,"POST1_3"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID1_POST"],"POST1_3"]
  EMA_30MZP_each[i,"POST1_4"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID1_POST"],"POST1_4"]
  EMA_30MZP_each[i,"POST1_5"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID1_POST"],"POST1_5"]
  
  EMA_30MZP_each[i,"POST1_6"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID2_POST"],"POST1_1"]
  EMA_30MZP_each[i,"POST1_7"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID2_POST"],"POST1_2"]
  EMA_30MZP_each[i,"POST1_8"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID2_POST"],"POST1_3"]
  EMA_30MZP_each[i,"POST1_9"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID2_POST"],"POST1_4"]
  EMA_30MZP_each[i,"POST1_10"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID2_POST"],"POST1_5"]
  
  EMA_30MZP_each[i,"POST1_11"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID3_POST"],"POST1_1"]
  EMA_30MZP_each[i,"POST1_12"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID3_POST"],"POST1_2"]
  EMA_30MZP_each[i,"POST1_13"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID3_POST"],"POST1_3"]
  EMA_30MZP_each[i,"POST1_14"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID3_POST"],"POST1_4"]
  EMA_30MZP_each[i,"POST1_15"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID3_POST"],"POST1_5"]
  
  EMA_30MZP_each[i,"POST1_16"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID4_POST"],"POST1_1"]
  EMA_30MZP_each[i,"POST1_17"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID4_POST"],"POST1_2"]
  EMA_30MZP_each[i,"POST1_18"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID4_POST"],"POST1_3"]
  EMA_30MZP_each[i,"POST1_19"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID4_POST"],"POST1_4"]
  EMA_30MZP_each[i,"POST1_20"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID4_POST"],"POST1_5"]
  
  EMA_30MZP_each[i,"POST1_21"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID5_POST"],"POST1_1"]
  EMA_30MZP_each[i,"POST1_22"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID5_POST"],"POST1_2"]
  EMA_30MZP_each[i,"POST1_23"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID5_POST"],"POST1_3"]
  EMA_30MZP_each[i,"POST1_24"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID5_POST"],"POST1_4"]
  EMA_30MZP_each[i,"POST1_25"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID5_POST"],"POST1_5"]
  
  EMA_30MZP_each[i,"POST1_26"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID6_POST"],"POST1_1"]
  EMA_30MZP_each[i,"POST1_27"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID6_POST"],"POST1_2"]
  EMA_30MZP_each[i,"POST1_28"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID6_POST"],"POST1_3"]
  EMA_30MZP_each[i,"POST1_29"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID6_POST"],"POST1_4"]
  EMA_30MZP_each[i,"POST1_30"] = EMA_data[EMA_matched_Pre_Post_IDs[i,"ID6_POST"],"POST1_5"]
  message(i)
}

EMA_30MZP_each$PRE_Mean = apply(EMA_30MZP_each[,13:42], 1, mean)
EMA_30MZP_each$POST_Mean = apply(EMA_30MZP_each[,43:72], 1, mean)
EMA_30MZP_each$MeanDiff = EMA_30MZP_each$PRE_Mean - EMA_30MZP_each$POST_Mean
EMA_30MZP_each$ind.pretestSD = apply(EMA_30MZP_each[,13:42], 1, sd)
EMA_30MZP_each$ind.posttestSD = apply(EMA_30MZP_each[,43:72], 1, sd)

EMA_30MZP_each = cbind("ID" = as.numeric(c(1:length(EMA_30MZP_each$ID1_PRE))), EMA_30MZP_each)

write.table(EMA_30MZP_each, file = "cor_04_k1_30MZP_each/EMA_30MZP_each.txt")
save(EMA_30MZP_each, file = "cor_04_k1_30MZP_each/EMA_30MZP_each.RData")



############################################################ . FB: KNN-Matching ####

setwd(
  "C:/Users/steph/OneDrive/Desktop/Psychologie/Masterstudium Psychologie/3. Semester/1. Master-Konversatorium/Preprocessing und Berechnungen"
)


############################################################ .. PRE ####

load("cor_07_k1_30MZP_each/FB_PRE_KNN.RData")
FB_PRE_KNN = FB_PRE_KNN[- 1]

for (i in 1:length(FB_PRE_KNN)) {
  FB_PRE_KNN[[i]] = as.vector(unlist(strsplit(FB_PRE_KNN[[i]], ";")))
}

FB_PRE_KNN_df = t(as.data.frame(FB_PRE_KNN, row.names = NULL, optional = FALSE, fix.empty.names = TRUE,  stringsAsFactors = default.stringsAsFactors()))
colnames(FB_PRE_KNN_df) = FB_PRE_KNN_df[1,]
FB_PRE_KNN_df = FB_PRE_KNN_df[-1,]
FB_PRE_KNN_df = FB_PRE_KNN_df[-100001,]
FB_PRE_KNN_df = FB_PRE_KNN_df[,-2]

FB_PRE_KNN_df = as.data.frame(FB_PRE_KNN_df, row.names = c(1:100000),  stringsAsFactors = FALSE)

for (k in colnames(FB_PRE_KNN_df)) {
  FB_PRE_KNN_df[,k] = as.numeric(FB_PRE_KNN_df[,k])
}

FB_PRE_KNN_df = subset(FB_PRE_KNN_df, subset = FB_PRE_KNN_df$distance1 == 0 & FB_PRE_KNN_df$distance2 == 0 & FB_PRE_KNN_df$distance3 == 0 & FB_PRE_KNN_df$distance4 == 0 & FB_PRE_KNN_df$distance5 == 0)

save(FB_PRE_KNN_df, file = "cor_07_k1_30MZP_each/FB_PRE_KNN_df.RData")


############################################################ .. POST ####

load("cor_07_k1_30MZP_each/FB_POST_KNN.RData")
FB_POST_KNN = FB_POST_KNN[- 1]

for (i in 1:length(FB_POST_KNN)) {
  FB_POST_KNN[[i]] = as.vector(unlist(strsplit(FB_POST_KNN[[i]], ";")))
}

FB_POST_KNN_df = t(as.data.frame(FB_POST_KNN, row.names = NULL, optional = FALSE, fix.empty.names = TRUE,  stringsAsFactors = default.stringsAsFactors()))
colnames(FB_POST_KNN_df) = FB_POST_KNN_df[1,]
FB_POST_KNN_df = FB_POST_KNN_df[-1,]
FB_POST_KNN_df = FB_POST_KNN_df[-100001,]
FB_POST_KNN_df = FB_POST_KNN_df[,-2]

FB_POST_KNN_df = as.data.frame(FB_POST_KNN_df, row.names = c(1:100000),  stringsAsFactors = FALSE)

for (k in colnames(FB_POST_KNN_df)) {
  FB_POST_KNN_df[,k] = as.numeric(FB_POST_KNN_df[,k])
}

FB_POST_KNN_df = subset(FB_POST_KNN_df, subset = FB_POST_KNN_df$distance1 == 0 & FB_POST_KNN_df$distance2 == 0 & FB_POST_KNN_df$distance3 == 0 & FB_POST_KNN_df$distance4 == 0 & FB_POST_KNN_df$distance5 == 0)

save(FB_POST_KNN_df, file = "cor_07_k1_30MZP_each/FB_POST_KNN_df.RData")


############################################################ .. Zusammenführen matched IDs PRE und POST ####

load("cor_07_k1_30MZP_each/FB_PRE_KNN_df.RData")
load("cor_07_k1_30MZP_each/FB_POST_KNN_df.RData")


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

save(FB_matched_Pre_Post_IDs, file = "cor_07_k1_30MZP_each/FB_matched_Pre_Post_IDs.RData")


############################################################ .. Zusammenführen PRE- und POST-MZP anhand matched IDs ####

setwd(
  "C:/Users/steph/OneDrive/Desktop/Psychologie/Masterstudium Psychologie/3. Semester/1. Master-Konversatorium/Preprocessing und Berechnungen"
)

FB_data = read.table(file = "cor_07_k1/cor_07_dataset_k1.txt", header = TRUE, sep = "")
FB_data = subset(FB_data, select = c(PRE1_1, PRE1_2, PRE1_3, PRE1_4, PRE1_5, POST1_1, POST1_2, POST1_3, POST1_4, POST1_5))
FB_data = cbind(ID = 1:100000, FB_data)

load("cor_07_k1_30MZP_each/FB_matched_Pre_Post_IDs.RData")

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

FB_30MZP_each$PRE_Mean = apply(FB_30MZP_each[,13:42], 1, mean)
FB_30MZP_each$POST_Mean = apply(FB_30MZP_each[,43:72], 1, mean)
FB_30MZP_each$MeanDiff = FB_30MZP_each$PRE_Mean - FB_30MZP_each$POST_Mean
FB_30MZP_each$ind.pretestSD = apply(FB_30MZP_each[,13:42], 1, sd)
FB_30MZP_each$ind.posttestSD = apply(FB_30MZP_each[,43:72], 1, sd)

FB_30MZP_each = cbind("ID" = as.numeric(c(1:length(FB_30MZP_each$ID1_PRE))), FB_30MZP_each)

write.table(FB_30MZP_each, file = "cor_07_k1_30MZP_each/FB_30MZP_each.txt")
save(FB_30MZP_each, file = "cor_07_k1_30MZP_each/FB_30MZP_each.RData")

