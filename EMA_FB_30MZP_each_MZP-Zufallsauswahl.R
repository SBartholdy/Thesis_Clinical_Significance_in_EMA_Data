
setwd(
  "C:/Users/steph/OneDrive/Desktop/Psychologie/Masterstudium Psychologie/3. Semester/1. Master-Konversatorium/Preprocessing und Berechnungen"
)

load("cor_04_k1_30MZP_each/EMA_30MZP_each.RData")
load("cor_07_k1_30MZP_each/FB_30MZP_each.RData")


pre_mzp = c("PRE1_1","PRE1_2","PRE1_3","PRE1_4","PRE1_5",
            "PRE1_6","PRE1_7","PRE1_8","PRE1_9","PRE1_10",
            "PRE1_11","PRE1_12","PRE1_13","PRE1_14","PRE1_15",
            "PRE1_16","PRE1_17","PRE1_18","PRE1_19","PRE1_20",
            "PRE1_21","PRE1_22","PRE1_23","PRE1_24","PRE1_25",
            "PRE1_26","PRE1_27","PRE1_28","PRE1_29","PRE1_30")

post_mzp = c("POST1_1","POST1_2","POST1_3","POST1_4","POST1_5",
             "POST1_6","POST1_7","POST1_8","POST1_9","POST1_10",
             "POST1_11","POST1_12","POST1_13","POST1_14","POST1_15",
             "POST1_16","POST1_17","POST1_18","POST1_19","POST1_20",
             "POST1_21","POST1_22","POST1_23","POST1_24","POST1_25",
             "POST1_26","POST1_27","POST1_28","POST1_29","POST1_30")


############################################################ FB: Zufalls-Datenset mit je 1 MZP PRE und POST ####
# Für jede Person werden aus den 30-MZP-Intervallen zufällig je ein Pre- und ein Post-MZP ausgewählt.
# Dadurch bekommt jede Person eine zufällige Kombination zweier FB-MZP.

set.seed(42)

FB_random_2MZP = data.frame(ID = c(), Pre_MZP = c(), Post_MZP = c(), PRE = c(), POST = c())

for (i in FB_30MZP_each$ID) {
  a = sample(pre_mzp, size = 1)
  b = sample(post_mzp, size = 1)
  
  FB_random_2MZP[i,"ID"] = i
  FB_random_2MZP[i,"Pre_MZP"] = a
  FB_random_2MZP[i,"Post_MZP"] = b
  FB_random_2MZP[i,"PRE"] = FB_30MZP_each[i,a]
  FB_random_2MZP[i,"POST"] = FB_30MZP_each[i,b]
  message(i)
}

save(FB_random_2MZP, file = "cor_07_k1_30MZP_each/FB_random_2MZP.RData")



############################################################ EMA: Zufallsauswahl von je 5 MZP PRE und POST ####
# . je ein zufälliges Intervall von 5 aufeinanderfolgenden MZP ####

set.seed(42)

EMA_random_Window = data.frame(
  ID = c(),
  Pre_MZP1 = c(),
  Pre_MZP2 = c(),
  Pre_MZP3 = c(),
  Pre_MZP4 = c(),
  Pre_MZP5 = c(),
  Post_MZP1 = c(),
  Post_MZP2 = c(),
  Post_MZP3 = c(),
  Post_MZP4 = c(),
  Post_MZP5 = c(),
  PRE1 = c(),
  PRE2 = c(),
  PRE3 = c(),
  PRE4 = c(),
  PRE5 = c(),
  POST1 = c(),
  POST2 = c(),
  POST3 = c(),
  POST4 = c(),
  POST5 = c()
)

for (i in EMA_30MZP_each$ID) {
  a = sample(1:26, 1)
  EMA_rand_pre_Window = pre_mzp[seq(from = a, to = a+4)]
  b = sample(1:26, 1)
  EMA_rand_post_Window = post_mzp[seq(from = b, to = b+4)]
  
  EMA_random_Window[i,"ID"] = i
  EMA_random_Window[i,"Pre_MZP1"] = EMA_rand_pre_Window[1]
  EMA_random_Window[i,"Pre_MZP2"] = EMA_rand_pre_Window[2]
  EMA_random_Window[i,"Pre_MZP3"] = EMA_rand_pre_Window[3]
  EMA_random_Window[i,"Pre_MZP4"] = EMA_rand_pre_Window[4]
  EMA_random_Window[i,"Pre_MZP5"] = EMA_rand_pre_Window[5]
  
  EMA_random_Window[i,"Post_MZP1"] = EMA_rand_post_Window[1]
  EMA_random_Window[i,"Post_MZP2"] = EMA_rand_post_Window[2]
  EMA_random_Window[i,"Post_MZP3"] = EMA_rand_post_Window[3]
  EMA_random_Window[i,"Post_MZP4"] = EMA_rand_post_Window[4]
  EMA_random_Window[i,"Post_MZP5"] = EMA_rand_post_Window[5]
  
  EMA_random_Window[i,"PRE1"] = EMA_30MZP_each[i,EMA_rand_pre_Window[1]]
  EMA_random_Window[i,"PRE2"] = EMA_30MZP_each[i,EMA_rand_pre_Window[2]]
  EMA_random_Window[i,"PRE3"] = EMA_30MZP_each[i,EMA_rand_pre_Window[3]]
  EMA_random_Window[i,"PRE4"] = EMA_30MZP_each[i,EMA_rand_pre_Window[4]]
  EMA_random_Window[i,"PRE5"] = EMA_30MZP_each[i,EMA_rand_pre_Window[5]]
  
  EMA_random_Window[i,"POST1"] = EMA_30MZP_each[i,EMA_rand_post_Window[1]]
  EMA_random_Window[i,"POST2"] = EMA_30MZP_each[i,EMA_rand_post_Window[2]]
  EMA_random_Window[i,"POST3"] = EMA_30MZP_each[i,EMA_rand_post_Window[3]]
  EMA_random_Window[i,"POST4"] = EMA_30MZP_each[i,EMA_rand_post_Window[4]]
  EMA_random_Window[i,"POST5"] = EMA_30MZP_each[i,EMA_rand_post_Window[5]]
  message(i)
}

save(EMA_random_Window, file = "cor_04_k1_30MZP_each/EMA_random_Window.RData")


# . je 5 zufällig ausgewählte (nicht zwingend aufeinanderfolgende) MZP ####

set.seed(42)

EMA_random_Days = data.frame(
  ID = c(),
  Pre_MZP1 = c(),
  Pre_MZP2 = c(),
  Pre_MZP3 = c(),
  Pre_MZP4 = c(),
  Pre_MZP5 = c(),
  Post_MZP1 = c(),
  Post_MZP2 = c(),
  Post_MZP3 = c(),
  Post_MZP4 = c(),
  Post_MZP5 = c(),
  PRE1 = c(),
  PRE2 = c(),
  PRE3 = c(),
  PRE4 = c(),
  PRE5 = c(),
  POST1 = c(),
  POST2 = c(),
  POST3 = c(),
  POST4 = c(),
  POST5 = c()
)

for (i in EMA_30MZP_each$ID) {
  EMA_rand_pre_Days = pre_mzp[sort(sample(1:30, 5))]
  EMA_rand_post_Days = post_mzp[sort(sample(1:30, 5))]
  
  EMA_random_Days[i,"ID"] = i
  EMA_random_Days[i,"Pre_MZP1"] = EMA_rand_pre_Days[1]
  EMA_random_Days[i,"Pre_MZP2"] = EMA_rand_pre_Days[2]
  EMA_random_Days[i,"Pre_MZP3"] = EMA_rand_pre_Days[3]
  EMA_random_Days[i,"Pre_MZP4"] = EMA_rand_pre_Days[4]
  EMA_random_Days[i,"Pre_MZP5"] = EMA_rand_pre_Days[5]
  
  EMA_random_Days[i,"Post_MZP1"] = EMA_rand_post_Days[1]
  EMA_random_Days[i,"Post_MZP2"] = EMA_rand_post_Days[2]
  EMA_random_Days[i,"Post_MZP3"] = EMA_rand_post_Days[3]
  EMA_random_Days[i,"Post_MZP4"] = EMA_rand_post_Days[4]
  EMA_random_Days[i,"Post_MZP5"] = EMA_rand_post_Days[5]
  
  EMA_random_Days[i,"PRE1"] = EMA_30MZP_each[i,EMA_rand_pre_Days[1]]
  EMA_random_Days[i,"PRE2"] = EMA_30MZP_each[i,EMA_rand_pre_Days[2]]
  EMA_random_Days[i,"PRE3"] = EMA_30MZP_each[i,EMA_rand_pre_Days[3]]
  EMA_random_Days[i,"PRE4"] = EMA_30MZP_each[i,EMA_rand_pre_Days[4]]
  EMA_random_Days[i,"PRE5"] = EMA_30MZP_each[i,EMA_rand_pre_Days[5]]
  
  EMA_random_Days[i,"POST1"] = EMA_30MZP_each[i,EMA_rand_post_Days[1]]
  EMA_random_Days[i,"POST2"] = EMA_30MZP_each[i,EMA_rand_post_Days[2]]
  EMA_random_Days[i,"POST3"] = EMA_30MZP_each[i,EMA_rand_post_Days[3]]
  EMA_random_Days[i,"POST4"] = EMA_30MZP_each[i,EMA_rand_post_Days[4]]
  EMA_random_Days[i,"POST5"] = EMA_30MZP_each[i,EMA_rand_post_Days[5]]
  message(i)
}

save(EMA_random_Days, file = "cor_04_k1_30MZP_each/EMA_random_Days.RData")


