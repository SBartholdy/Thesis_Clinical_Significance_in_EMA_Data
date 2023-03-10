#siehe R-Code zur Datenerzeugung


#manuelle Erzeugung von csv- und xlsx-Dateien aus dataset_k....RData-Dateien
#f?r automatische Erzeugung siehe unten

library(openxlsx)
library(haven)
library(foreign)


load(
  "C:/Users/steph/OneDrive/Desktop/Psychologie/Masterstudium Psychologie/3. Semester/1. Master-Konversatorium/Daten von Manuelas Simulation/Simulierte Daten und Ergebnisse der Tests als RData/cor_07/dataset5/dataset_k1.RData"
)

View(dataset)


df <- dataset[,1]
df <- as.data.frame(df)


for(i in 2:1000){
  dff <- dataset[,i]
  dff <- as.data.frame(dff)
  df <- rbind(df,dff)
}

View(df)


write.csv(df, "C:/Users/steph/OneDrive/Desktop/cor_04_dataset5_k1.csv", col.names = TRUE)

write.xlsx(df, "C:/Users/steph/OneDrive/Desktop/cor_04_dataset5_k1.xlsx", col.names = TRUE)

write_sav(df, "C:/Users/steph/OneDrive/Desktop/Psychologie/Masterstudium Psychologie/3. Semester/1. Master-Konversatorium/Preprocessing und Berechnungen/cor_07_k1/cor_07_k1.sav")


# Erzeugung von cor_07_dataset_k16.sav aus cor_07_dataset_k16.txt
setwd("C:/Users/steph/OneDrive/Desktop/Psychologie/Masterstudium Psychologie/3. Semester/1. Master-Konversatorium/Daten von Manuelas Simulation/Simulierte Daten und Ergebnisse der Tests als RData/cor_07/dataset5")
PP_5.5 = read.delim("cor_07_dataset_k16.txt", row.names=NULL) %>%
  select(PRE1_1:POST1_5) %>%
  add_column(., .before = "PRE1_1", ID = 1:nrow(.)) %>%
  as_tibble()

pre_5mzp = c("PRE1_1","PRE1_2","PRE1_3","PRE1_4","PRE1_5")
post_5mzp = c("POST1_1","POST1_2","POST1_3","POST1_4","POST1_5")

PP_original$PRE_Mean = apply(PP_original[pre_5mzp], 1, mean)
PP_original$POST_Mean = apply(PP_original[post_5mzp], 1, mean)
PP_original$MeanDiff = PP_original$PRE_Mean - PP_original$POST_Mean
PP_original$ind.pretestSD = apply(PP_original[pre_5mzp], 1, sd)
PP_original$ind.posttestSD = apply(PP_original[post_5mzp], 1, sd)

write_sav(PP_original, "C:/Users/steph/OneDrive/Desktop/Psychologie/Masterstudium Psychologie/3. Semester/1. Master-Konversatorium/Preprocessing und Berechnungen/cor_07_k16/cor_07_k16.sav")


###################
#automatische Erzeugung von txt-Dateien aus dataset_k....RData-Dateien
#Code aus datenerzeugung_cor_04.R
#analoger Code f?r cor_04 und cor_07


for(k in 1:62){
  setwd("C:/Users/steph/OneDrive/Desktop/Psychologie/Masterstudium Psychologie/3. Semester/1. Master-Konversatorium/Daten von Manuelas Simulation/Simulierte Daten und Ergebnisse der Tests als RData/cor_07/dataset5")
  print(k)

  load(file = paste0("dataset_k",k,".RData"))

  df <- dataset[,1]
  df <- as.data.frame(df)

  for(i in 2:1000){
    dff <- dataset[,i]
    dff <- as.data.frame(dff)
    df <- rbind(df,dff)
  }

  #setwd("C:/Users/b1015748/Desktop/Power/Daten excel")

  write.table(df, paste0("cor_07_dataset_k",k,".txt"), sep="\t",row.names=FALSE)

  #library(haven)
  #write_sav(df, paste0("delta_", round(Delta[k],3),".sav"))
}



###################
#Tabelle mit Datensets (cor_04/dataset5/dataset_k...) und deren Power


load(
  "C:/Users/steph/OneDrive/Desktop/Psychologie/Masterstudium Psychologie/3. Semester/1. Master-Konversatorium/Daten von Manuelas Simulation/Simulierte Daten und Ergebnisse der Tests als RData/cor_04/power.RData"
)

k = 1:62

k_vector = paste0("dataset_k",k)

powertable_cor04 = data.frame(k_vector, power)

View(powertable_cor04)

save(powertable_cor04, file = "C:/Users/steph/OneDrive/Desktop/Psychologie/Masterstudium Psychologie/3. Semester/1. Master-Konversatorium/Daten von Manuelas Simulation/Simulierte Daten und Ergebnisse der Tests als RData/cor_04/powertable_cor04.RData")



###################
#Berechnung von Mittelwerts-Unterschieden, Cohen?s d und Korrelationen f?r alle FB-Datensets


PP_datasets = data.frame(
  Dataset = c(),
  MW_PRE1 = c(),
  MW_POST1 = c(),
  MeanDiff_PRE1_POST1 = c(),
  SD_PRE1 = c(),
  SD_POST1 = c(),
  Cohens_d_PRE1_POST1 = c(),
  Cor_PRE1_POST1 = c(),
  Cor_pairwise_PRE1_PRE2 = c(),
  Cor_pairwise_PRE2_PRE3 = c(),
  Cor_pairwise_PRE3_PRE4 = c(),
  Cor_pairwise_PRE4_PRE5 = c(),
  Cor_pairwise_POST1_POST2 = c(),
  Cor_pairwise_POST2_POST3 = c(),
  Cor_pairwise_POST3_POST4 = c(),
  Cor_pairwise_POST4_POST5 = c()
)


for(k in 1:62){
  setwd("C:/Users/steph/OneDrive/Desktop/Psychologie/Masterstudium Psychologie/3. Semester/1. Master-Konversatorium/Daten von Manuelas Simulation/Simulierte Daten und Ergebnisse der Tests als RData/cor_07/dataset5")
  print(k)

  load(file = paste0("dataset_k",k,".RData"))

  df <- dataset[,1]
  df <- as.data.frame(df)

  for(i in 2:1000){
    dff <- dataset[,i]
    dff <- as.data.frame(dff)
    df <- rbind(df,dff)
  }

  PP_datasets[k,"Dataset"] = paste0("cor_07_dataset_k",k)

  PP_datasets[k,"MW_PRE1"] = mean(df$PRE1_1)
  PP_datasets[k,"MW_POST1"] = mean(df$POST1_1)
  PP_datasets[k,"MeanDiff_PRE1_POST1"] = mean(df$PRE1_1) - mean(df$POST1_1)
  PP_datasets[k,"SD_PRE1"] = sd(df$PRE1_1)
  PP_datasets[k,"SD_POST1"] = sd(df$POST1_1)
  PP_datasets[k,"Cohens_d_PRE1_POST1"] = (mean(df$PRE1_1) - mean(df$POST1_1)) / sqrt(0.5 * (sd(df$PRE1_1)^2 + sd(df$POST1_1)^2))

  PP_datasets[k,"Cor_PRE1_POST1"] = cor(df$PRE1_1,df$POST1_1)

  PP_datasets[k,"Cor_pairwise_PRE1_PRE2"] = cor(df$PRE1_1,df$PRE1_2)
  PP_datasets[k,"Cor_pairwise_PRE2_PRE3"] = cor(df$PRE1_2,df$PRE1_3)
  PP_datasets[k,"Cor_pairwise_PRE3_PRE4"] = cor(df$PRE1_3,df$PRE1_4)
  PP_datasets[k,"Cor_pairwise_PRE4_PRE5"] = cor(df$PRE1_4,df$PRE1_5)

  PP_datasets[k,"Cor_pairwise_POST1_POST2"] = cor(df$POST1_1,df$POST1_2)
  PP_datasets[k,"Cor_pairwise_POST2_POST3"] = cor(df$POST1_2,df$POST1_3)
  PP_datasets[k,"Cor_pairwise_POST3_POST4"] = cor(df$POST1_3,df$POST1_4)
  PP_datasets[k,"Cor_pairwise_POST4_POST5"] = cor(df$POST1_4,df$POST1_5)
}

save(PP_datasets, file = "C:/Users/steph/OneDrive/Desktop/Psychologie/Masterstudium Psychologie/3. Semester/1. Master-Konversatorium/Preprocessing und Berechnungen/PP_Datasets.RData")
View(PP_datasets)



#Berechnung von Mittelwerts-Unterschieden, Cohen?s d und Korrelationen f?r alle EMA-Datensets


EMA_datasets = data.frame(
  Dataset = c(),
  MW_PRE1 = c(),
  MW_POST1 = c(),
  MeanDiff_PRE1_POST1 = c(),
  SD_PRE1 = c(),
  SD_POST1 = c(),
  Cohens_d_PRE1_POST1 = c(),
  Cor_PRE1_POST1 = c(),
  Cor_pairwise_PRE1_PRE2 = c(),
  Cor_pairwise_PRE2_PRE3 = c(),
  Cor_pairwise_PRE3_PRE4 = c(),
  Cor_pairwise_PRE4_PRE5 = c(),
  Cor_pairwise_POST1_POST2 = c(),
  Cor_pairwise_POST2_POST3 = c(),
  Cor_pairwise_POST3_POST4 = c(),
  Cor_pairwise_POST4_POST5 = c()
)


for(k in 1:62){
  setwd("C:/Users/steph/OneDrive/Desktop/Psychologie/Masterstudium Psychologie/3. Semester/1. Master-Konversatorium/Daten von Manuelas Simulation/Simulierte Daten und Ergebnisse der Tests als RData/cor_04/dataset5")
  print(k)

  load(file = paste0("dataset_k",k,".RData"))

  df <- dataset[,1]
  df <- as.data.frame(df)

  for(i in 2:1000){
    dff <- dataset[,i]
    dff <- as.data.frame(dff)
    df <- rbind(df,dff)
  }

  EMA_datasets[k,"Dataset"] = paste0("cor_04_dataset_k",k)

  EMA_datasets[k,"MW_PRE1"] = mean(df$PRE1_1)
  EMA_datasets[k,"MW_POST1"] = mean(df$POST1_1)
  EMA_datasets[k,"MeanDiff_PRE1_POST1"] = mean(df$PRE1_1) - mean(df$POST1_1)
  EMA_datasets[k,"SD_PRE1"] = sd(df$PRE1_1)
  EMA_datasets[k,"SD_POST1"] = sd(df$POST1_1)
  EMA_datasets[k,"Cohens_d_PRE1_POST1"] = (mean(df$PRE1_1) - mean(df$POST1_1)) / sqrt(0.5 * (sd(df$PRE1_1)^2 + sd(df$POST1_1)^2))

  EMA_datasets[k,"Cor_PRE1_POST1"] = cor(df$PRE1_1,df$POST1_1)

  EMA_datasets[k,"Cor_pairwise_PRE1_PRE2"] = cor(df$PRE1_1,df$PRE1_2)
  EMA_datasets[k,"Cor_pairwise_PRE2_PRE3"] = cor(df$PRE1_2,df$PRE1_3)
  EMA_datasets[k,"Cor_pairwise_PRE3_PRE4"] = cor(df$PRE1_3,df$PRE1_4)
  EMA_datasets[k,"Cor_pairwise_PRE4_PRE5"] = cor(df$PRE1_4,df$PRE1_5)

  EMA_datasets[k,"Cor_pairwise_POST1_POST2"] = cor(df$POST1_1,df$POST1_2)
  EMA_datasets[k,"Cor_pairwise_POST2_POST3"] = cor(df$POST1_2,df$POST1_3)
  EMA_datasets[k,"Cor_pairwise_POST3_POST4"] = cor(df$POST1_3,df$POST1_4)
  EMA_datasets[k,"Cor_pairwise_POST4_POST5"] = cor(df$POST1_4,df$POST1_5)
}


save(EMA_datasets, file = "C:/Users/steph/OneDrive/Desktop/Psychologie/Masterstudium Psychologie/3. Semester/1. Master-Konversatorium/Preprocessing und Berechnungen/EMA_Datasets.RData")
View(EMA_datasets)



#Zusammenf?gen der FB- und EMA-Datenset-Eigenschaften

SimDatasets = rbind(PP_datasets, EMA_datasets)
View(SimDatasets)

save(SimDatasets, file = "C:/Users/steph/OneDrive/Desktop/Psychologie/Masterstudium Psychologie/3. Semester/1. Master-Konversatorium/Preprocessing und Berechnungen/PP_EMA_Datasets.RData")

write.table(SimDatasets, "C:/Users/steph/OneDrive/Desktop/Psychologie/Masterstudium Psychologie/3. Semester/1. Master-Konversatorium/Preprocessing und Berechnungen/PP_EMA_Datasets.txt")

