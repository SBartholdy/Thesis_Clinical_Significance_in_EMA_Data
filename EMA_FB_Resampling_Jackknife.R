
############################################################ Jackknife-Methode zum Resampling von MZP ####
# Statt wenige zufällige MZP-Kombinationen zu ziehen und diese dann mit den "wahren" Schätzwerten und Klassifikationen
# (= berechnet anhand der gesamten Intervalle mit je 30 MZP) zu vergleichen, sollen die empirische Verteilung der 
# Parameter und somit der Schätzfehler über Resampling-Methoden wie Jackknife-Verfahren und Bootstrapping berechnet werden.

library(bootstrap)

#jackknife(as.numeric(FB_30MZP_each[1,pre_30mzp]), mean)


ID1 = data.frame(PRE = as.numeric(FB_30MZP_each[1,pre_30mzp]), POST = as.numeric(FB_30MZP_each[1,post_30mzp]))
#
n = 30
theta = function(x, ID1) {cor(ID1[x,1], ID1[x,2])}
jackknife(1:n, theta, ID1)
#

########################################
FB_30MZP_each$RCI_ind_pooledSD = (FB_30MZP_each$POST_Mean - FB_30MZP_each$PRE_Mean) / sqrt((FB_30MZP_each$ind.pretestSD ^ 2 + FB_30MZP_each$ind.posttestSD ^ 2) * (1 - 0.974))
# (mean(ID1[,2]) - mean(ID1[,1])) / sqrt((sd(ID1[,1])^ 2 + sd(ID1[,2])^ 2) * (1 - 0.9))

n = 30

RCI_ind_pooledSD = function(x, ID_df) {(mean(ID_df[x,2]) - mean(ID_df[x,1])) / 
    sqrt((sd(ID_df[x,1])^ 2 + sd(ID_df[x,2])^ 2) * (1 - 0.974))}
jackknife(1:n, RCI_ind_pooledSD, ID1)


# für normales Tibble-Format:
test_df = FB_30MZP_each %>% 
  select(ID, PRE1_1:POST1_30) %>% 
  filter(ID %in% 1:100)

n = 30

RCI_ind_pooledSD = function(x, df) {(mean(df[x,2]) - mean(df[x,1])) /
    sqrt((sd(ID_df[x,1])^ 2 + sd(df[x,2])^ 2) * (1 - 0.974))} ######## ?

for (i in 1:100) {
  FB_30MZP_each[i,"RCI_ind_pooledSD_jse"] = jackknife(1:n, RCI_ind_pooledSD, test_df)$jack.se ######## ?
  FB_30MZP_each[i,"RCI_ind_pooledSD_jbias"] = jackknife(1:n, RCI_ind_pooledSD, test_df)$jack.bias ######## ?
}



######################################## funktioniert:
# für transponiertes Tibble-Format:
n = 30

RCI_ind_pooledSD = function(x, ID_df) {(mean(ID_df[x,2]) - mean(ID_df[x,1])) / 
    sqrt((sd(ID_df[x,1])^ 2 + sd(ID_df[x,2])^ 2) * (1 - FB_30MZP_each_Alpha))}

for (i in 1:nrow(FB_30MZP_each)) {
  df = data.frame(PRE = as.numeric(FB_30MZP_each[i,pre_30mzp]), POST = as.numeric(FB_30MZP_each[i,post_30mzp]))
  
  FB_30MZP_each[i,"RCI_ind_pooledSD_jse"] = jackknife(1:n, RCI_ind_pooledSD, df)$jack.se
  FB_30MZP_each[i,"RCI_ind_pooledSD_jbias"] = jackknife(1:n, RCI_ind_pooledSD, df)$jack.bias
  message(i)
}

#ID1 = data.frame(PRE = as.numeric(FB_30MZP_each[1,pre_30mzp]), POST = as.numeric(FB_30MZP_each[1,post_30mzp]))
#RCI_ind_pooledSD = function(x, ID_df) {(mean(ID_df[x,2]) - mean(ID_df[x,1])) / 
#    sqrt((sd(ID_df[x,1])^ 2 + sd(ID_df[x,2])^ 2) * (1 - 0.974))}
#jackknife(1:n, RCI_ind_pooledSD, ID1)

########################################

# library(resample)
# jackknife()

# -> resample-package {resample}
