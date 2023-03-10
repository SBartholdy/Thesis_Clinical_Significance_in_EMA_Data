
library(dplyr)


#BaselineKorr = tibble(Cohen_d = as.factor(c("d0.9", "d0.99", "d1.13", "d1.96")),
#                      MeanPC_30 = c(0.378, 0.356, 0.338, 0.251),
#                      RCI_JT_1 = c(-0.46, -0.465, -0.475, -0.543),
#                      RCI_ind_preSD_30 = c(-0.454, -0.448, -0.444, -0.439),
#                      RCI_ind_pooledSD_30 = c(-0.437, -0.427, -0.416, -0.363))

BaselineKorr = matrix(c(0.378, -0.46, -0.454, -0.437, 0.356, -0.465, -0.448, -0.427, 0.338, -0.475, -0.444, -0.416, 0.251, -0.543, -0.439, -0.363), ncol = 4, nrow = 4, dimnames = list(c("MeanPC_PreMean", "RCI_JT_PRE", "RCI_ind_preSD_PreMean", "RCI_ind_pooledSD_PreMean"), c("d0.9", "d0.99", "d1.13", "d1.96")), byrow = TRUE)
BaselineKorr = matrix(c(0.378, 0.46, 0.454, 0.437, 0.356, 0.465, 0.448, 0.427, 0.338, 0.475, 0.444, 0.416, 0.251, 0.543, 0.439, 0.363), ncol = 4, nrow = 4, dimnames = list(c("MeanPC_PreMean", "RCI_JT_PRE", "RCI_ind_preSD_PreMean", "RCI_ind_pooledSD_PreMean"), c("d0.9", "d0.99", "d1.13", "d1.96")), byrow = TRUE)

dotchart(BaselineKorr, main = "Abs. Baseline-Correlation of Classification Methods", pch = 21)


################################################################ Specificity-Vergleich zwischen Gesamtstichproben, n50 und n100
library(ggplot2)

### EMA
ntotal = c(0.930, 0.785, 0.875, 0.651, 1, 0.864, 0.756, 0.841, 0.639, 0.929, 0.813, 0.686, 0.786, 0.588, 0.891)
n50 = c(0.927, 0.707, 0.854, 0.634, 1, 0.854, 0.707, 0.805, 0.610, 0.878, 0.756, 0.585, 0.707, 0.561, 0.854)
n100 = c(0.951, 0.765, 0.889, 0.667, 1, 0.864, 0.704, 0.852, 0.654, 0.889, 0.827, 0.630, 0.790, 0.543, 0.889)

EMA_Spec_df = tibble(Var = rep(1:15, times = 3),
                     N = rep(c("ntotal", "n50", "n100"), each = 15),
                     Spec = c(ntotal, n50, n100))

EMA_Spec_Comp = EMA_Spec_df %>%
  ggplot(aes(x = Var, y = Spec, fill = N)) +
  geom_col(position = "dodge")

sum(sqrt((ntotal - n50)^2)) #0.595
sum(sqrt((ntotal - n100)^2)) #0.31
sum(sqrt((n100 - n50)^2)) #0.517

### PP
ntotal = c(0.863, 0.415, 0.504, 0.635, 0.310, 1, 0.863, 0.378, 0.468, 0.282, 1, 0.707, 0.592, 0.443, 0.846)
n50 = c(0.846, 0.385, 0.410, 0.487, 0.256, 1, 0.846, 0.359, 0.410, 0.256, 1, 0.692, 0.538, 0.410, 0.821)
n100 = c(0.859, 0.397, 0.487, 0.577, 0.333, 1, 0.859, 0.372, 0.449, 0.308, 1, 0.718, 0.615, 0.423, 0.872)

PP_Spec_df = tibble(Var = rep(1:15, times = 3),
                    N = rep(c("ntotal", "n50", "n100"), each = 15),
                    Spec = c(ntotal, n50, n100))

PP_Spec_Comp = PP_Spec_df %>%
  ggplot(aes(x = Var, y = Spec, fill = N)) +
  geom_col(position = "dodge")

sum(sqrt((ntotal - n50)^2)) #0.59
sum(sqrt((ntotal - n100)^2)) #0.255
sum(sqrt((n100 - n50)^2)) #0.553



