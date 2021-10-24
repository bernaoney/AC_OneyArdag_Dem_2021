## ----setup, include=FALSE---------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------

library(haven)
library(tidyverse)
w2 <- read_stata("ABII_English.dta") %>% zap_labels() %>% 
  select(country,
         q2011,q2014,q2016, q2017,
         eg513,t513,
         q512,
         q5161:q5164,
         q5211,q5214,q5215,
         q60103:q60104,
         q6076,
         q6061:q6064,
         q101,
         q511,q303,q404) %>% filter(country %in% c("5", "21"))

library(psych)
describe(w2[,2:26])

library(naniar)
w2 <- w2 %>% replace_with_na_at(.vars = c(colnames(w2[, 2:26])), condition = ~.x == 96)
w2 <- w2 %>% replace_with_na_at(.vars = c(colnames(w2[, 2:26])), condition = ~.x == 98)
w2 <- w2 %>% replace_with_na_at(.vars = c(colnames(w2[, 2:26])), condition = ~.x == 99)

w2 <- w2 %>% replace_with_na_at(.vars = c("q2011","q2014","q2016", "q2017",
                                          "q5161", "q5162", "q5163", "q5164",
                                          "q5211","q5214","q5215",
                                          "q60103", "q60104",
                                          "q6076",
                                          "q6061", "q6062", "q6063", "q6064",
                                          "q101",
                                          "q303","q404"), condition = ~.x == 0)

w2 <- w2 %>% replace_with_na_at(.vars = c("q2011","q2014","q2016", "q2017",
                                          "q5161", "q5162", "q5163", "q5164",
                                          "q5211","q5214","q5215",
                                          "q60103", "q60104",
                                          "q6076",
                                          "q6061", "q6062", "q6063", "q6064",
                                          "q101",
                                          "q303","q404"), condition = ~.x == 8)

w2 <- w2 %>% replace_with_na_at(.vars = c("q2011","q2014","q2016", "q2017",
                                          "q5161", "q5162", "q5163", "q5164",
                                          "q5211","q5214","q5215",
                                          "q60103", "q60104",
                                          "q6076",
                                          "q6061", "q6062", "q6063", "q6064",
                                          "q101",
                                          "q303","q404"), condition = ~.x == 9)
describe(w2[,2:26])

eg_q513 <- w2 %>% select(country, eg513) %>% filter(country %in% c("5"))
eg_q513 <- eg_q513 %>% rename(q513 = eg513)
tu_q513 <- w2 %>% select(country, t513) %>% filter(country %in% c("21"))
tu_q513 <- tu_q513 %>% rename(q513 = t513)
q513_vec <- bind_rows(eg_q513,tu_q513)
w2$q513 <- q513_vec$q513

w2$wave <- "W2"

colnames(w2)

rm(list=setdiff(ls(), "w2"))

w2 <- w2 %>% select(country, wave, q2011, q2014, q2016, q2017, q512, q5161, q5162, q5163, q5164, q5211, q5214, q5215,  
                    q60103, q60104, q6076, q6061, q6062, q6063, q6064, q101, q511, q303, q404, q513)

describe(w2[,3:26])
w2$country <- factor(w2$country, levels = c(5,21), labels = c("Egypt","Tunisia"))

vis_miss(w2[,3:26])

save(w2, file = "w2.RData")



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------

w3 <- read_stata("ABIII_English.dta") %>% zap_labels() %>%
  select(country,
         q2011,q2014,q2016, q2017,
         q513,
         q512,
         q5161:q5164,
         q5211,q5214,q5215,
         q6013:q6014,
         q6076,
         q6061:q6064,
         q101,
         q511,q303,q404) %>% filter(country %in% c("5", "21"))

describe(w3[,2:25])

w3 <- w3 %>% replace_with_na_at(.vars = c(colnames(w3[, 2:25])), condition = ~.x == 96)
w3 <- w3 %>% replace_with_na_at(.vars = c(colnames(w3[, 2:25])), condition = ~.x == 98)
w3 <- w3 %>% replace_with_na_at(.vars = c(colnames(w3[, 2:25])), condition = ~.x == 99)

w3 <- w3 %>% replace_with_na_at(.vars = c("q2011","q2014","q2016", "q2017",
                                          "q5161", "q5162","q5163","q5164",
                                          "q5211","q5214","q5215",
                                          "q6013","q6014",
                                          "q6076",
                                          "q6061", "q6062","q6063","q6064",
                                          "q101",
                                          "q303","q404"), condition = ~.x == 0)

w3 <- w3 %>% replace_with_na_at(.vars = c("q2011","q2014","q2016", "q2017",
                                          "q5161", "q5162","q5163","q5164",
                                          "q5211","q5214","q5215",
                                          "q6013","q6014",
                                          "q6076",
                                          "q6061", "q6062","q6063","q6064",
                                          "q101",
                                          "q303","q404"), condition = ~.x == 8)

w3 <- w3 %>% replace_with_na_at(.vars = c("q2011","q2014","q2016", "q2017",
                                          "q5161", "q5162","q5163","q5164",
                                          "q5211","q5214","q5215",
                                          "q6013","q6014",
                                          "q6076",
                                          "q6061", "q6062","q6063","q6064",
                                          "q101",
                                          "q303","q404"), condition = ~.x == 9)

describe(w3[,2:25])

w3$country <- factor(w3$country, levels = c(5,21), labels = c("Egypt","Tunisia"))
w3$wave <- "W3"
w3 <- w3 %>% select(1,26,2:25)

vis_miss(w3[,3:25])

save(w3, file = "w3.RData")



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------

w5 <- read_stata("AB-WaveV-EN.dta") %>% zap_labels() %>%
  select(country,
         Q201A_1, Q201A_7, Q201A_42, Q201B_6,
         Q513,
         Q512,
         Q516_1:Q516_4,
         Q521_1,Q521_4,Q521_5,
         Q601_3:Q601_4,
         Q607_6,
         Q606_1:Q606_4,
         Q101,
         Q511,Q303A,Q404) %>% filter(country %in% c("5", "21"))

describe(w5[,2:25])

w5 <- w5 %>% replace_with_na_at(.vars = c(colnames(w5[, 2:25])), condition = ~.x == 96)
w5 <- w5 %>% replace_with_na_at(.vars = c(colnames(w5[, 2:25])), condition = ~.x == 98)
w5 <- w5 %>% replace_with_na_at(.vars = c(colnames(w5[, 2:25])), condition = ~.x == 99)

w5 <- w5 %>% replace_with_na_at(.vars = c("Q201A_1", "Q201A_42", "Q201B_6", "Q201A_7",
                                          "Q516_1", "Q516_2", "Q516_3", "Q516_4",
                                          "Q521_1","Q521_4", "Q521_5",
                                          "Q601_3", "Q601_4",
                                          "Q607_6",
                                          "Q606_1", "Q606_2", "Q606_3", "Q606_4",
                                          "Q101",
                                          "Q303A", "Q404"), condition = ~.x == 0)

w5 <- w5 %>% replace_with_na_at(.vars = c("Q201A_1", "Q201A_42", "Q201B_6", "Q201A_7",
                                          "Q516_1", "Q516_2", "Q516_3", "Q516_4",
                                          "Q521_1","Q521_4", "Q521_5",
                                          "Q601_3", "Q601_4",
                                          "Q607_6",
                                          "Q606_1", "Q606_2", "Q606_3", "Q606_4",
                                          "Q101",
                                          "Q303A", "Q404"), condition = ~.x == 8)

w5 <- w5 %>% replace_with_na_at(.vars = c("Q201A_1", "Q201A_42", "Q201B_6", "Q201A_7",
                                          "Q516_1", "Q516_2", "Q516_3", "Q516_4",
                                          "Q521_1","Q521_4", "Q521_5",
                                          "Q601_3", "Q601_4",
                                          "Q607_6",
                                          "Q606_1", "Q606_2", "Q606_3", "Q606_4",
                                          "Q101",
                                          "Q303A", "Q404"), condition = ~.x == 9)

describe(w5[,2:25])

w5$country <- factor(w5$country, levels = c(5,21), labels = c("Egypt","Tunisia"))
w5$wave <- "w5"
w5 <- w5 %>% select(1,26,2:25)

vis_miss(w5[,3:25])

w5_e <- w5 %>% filter(country%in%"Egypt")
w5_t <- w5 %>% filter(country%in%"Tunisia")

w5 <- bind_rows(w5_e,w5_t)

rm(w5_e,w5_t)

save(w5, file = "w5.RData")



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------

colnames(w2)
w2 <- w2 %>% rename(Tgo = q2011, Tpo = q2014, Taf = q2016,  Tcv = q2017,
                    Dsu = q512, Dec = q5161, Dfp = q5162, Dne = q5163, Dbe = q5164,
                    Fex = q5211,  Fde = q5214, Fcs = q5215,
                    GeL = q60103, GeE = q60104, GeI = q6076,  
                    Irl = q6061,  Icb = q6062,  Iip = q6063, Ise = q6064,
                    Eco = q101, Dem = q511, Efa = q303, Int = q404, Sre = q513)

colnames(w3)
w3 <- w3 %>% rename(Tgo = q2011, Tpo = q2014, Taf = q2016,  Tcv = q2017,
                    Dsu = q512, Dec = q5161, Dfp = q5162, Dne = q5163, Dbe = q5164,
                    Fex = q5211,  Fde = q5214, Fcs = q5215,
                    GeL = q6013, GeE = q6014, GeI = q6076,  
                    Irl = q6061,  Icb = q6062,  Iip = q6063, Ise = q6064,
                    Eco = q101, Dem = q511, Efa = q303, Int = q404, Sre = q513)

colnames(w5)
w5 <- w5 %>% rename(Tgo = Q201A_1, Tpo = Q201A_42, Taf = Q201B_6,  Tcv = Q201A_7,
                    Dsu = Q512, Dec = Q516_1, Dfp = Q516_2, Dne = Q516_3, Dbe = Q516_4,
                    Fex = Q521_1,  Fde = Q521_4, Fcs = Q521_5,
                    GeL = Q601_3, GeE = Q601_4, GeI = Q607_6,  
                    Irl = Q606_1,  Icb = Q606_2,  Iip = Q606_3, Ise = Q606_4,
                    Eco = Q101, Dem = Q511, Efa = Q303A, Int = Q404, Sre = Q513)

colnames(w2)
colnames(w3)
colnames(w5)

w2 <- w2 %>% select(country,wave,
                    Tgo, Tpo, Taf, Tcv, Sre,
                    Dsu, Dec, Dfp, Dne, Dbe,
                    Fex, Fde, Fcs,
                    GeL, GeE, GeI,
                    Irl, Icb, Iip, Ise,
                    Eco, Dem, Int, Efa)
w3 <- w3 %>% select(country,wave,
                    Tgo, Tpo, Taf, Tcv, Sre,
                    Dsu, Dec, Dfp, Dne, Dbe,
                    Fex, Fde, Fcs,
                    GeL, GeE, GeI,
                    Irl, Icb, Iip, Ise,
                    Eco, Dem, Int, Efa)
w5 <- w5 %>% select(country,wave,
                    Tgo, Tpo, Taf, Tcv, Sre,
                    Dsu, Dec, Dfp, Dne, Dbe,
                    Fex, Fde, Fcs,
                    GeL, GeE, GeI,
                    Irl, Icb, Iip, Ise,
                    Eco, Dem, Int, Efa)

save(w2, file = "w2.Rdata")
save(w3, file = "w3.Rdata")
save(w5, file = "w5.RData")



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------

colnames(w2)
describe(w2[,3:26])
w2 <- w2 %>% mutate_at(c("Tgo","Tpo","Taf","Tcv",
                         "Fex","Fde","Fcs",
                         "Dec","Dfp","Dne","Dbe",
                         "GeL","GeE","GeI",
                         "Irl","Icb","Iip","Ise",
                         "Eco","Int","Efa"),
                       funs(dplyr::recode(., `4`=1, `3`=2, `2`=3, `1`=4, .default = NaN)))

colnames(w3)
describe(w3[,3:26])
w3 <- w3 %>% mutate_at(c("Tgo","Tpo","Taf","Tcv",
                         "Fex","Fde","Fcs",
                         "Dec","Dfp","Dne","Dbe",
                         "GeL","GeE","GeI",
                         "Irl","Icb","Iip","Ise",
                         "Eco","Int","Efa"),
                       funs(dplyr::recode(., `4`=1, `3`=2, `2`=3, `1`=4, .default = NaN)))

colnames(w5)
describe(w5[,3:26])
w5 <- w5 %>% mutate_at(c("Tgo","Tpo","Taf","Tcv",
                         "Fex","Fde","Fcs",
                         "Dec","Dfp","Dne","Dbe",
                         "GeL","GeE","GeI",
                         "Irl","Icb","Iip","Ise",
                         "Eco","Int"),
                       funs(dplyr::recode(., `4`=1, `3`=2, `2`=3, `1`=4, .default = NaN)))
w5 <- w5 %>% mutate_at(c("Efa"), funs(dplyr::recode(., `3`=1, `2`=2, `1`=3, .default = NaN)))



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------

w2_e <- w2 %>% filter(country%in%"Egypt")
w2_t <- w2 %>% filter(country%in%"Tunisia")

w3_e <- w3 %>% filter(country%in%"Egypt")
w3_t <- w3 %>% filter(country%in%"Tunisia")

w5_e <- w5 %>% filter(country%in%"Egypt")
w5_t <- w5 %>% filter(country%in%"Tunisia")

library(mice)
w2_e_miss <- mice(w2_e [, c(3:26)], m = 10, maxit = 100, method = "pmm", seed = 123)
w2_e_miss_imp <- mice::complete(w2_e_miss,10)
w2_e[,3:26] <- w2_e_miss_imp
describe(w2_e[,3:26])

w2_t_miss <- mice(w2_t [, c(3:26)], m = 10, maxit = 100, method = "pmm", seed = 123)
w2_t_miss_imp <- mice::complete(w2_t_miss,10)
w2_t[,3:26] <- w2_t_miss_imp
describe(w2_t[,3:26])

w3_e_miss <- mice(w3_e [, c(3:26)], m = 10, maxit = 100, method = "pmm", seed = 123)
w3_e_miss_imp <- mice::complete(w3_e_miss,10)
w3_e[,3:26] <- w3_e_miss_imp
describe(w3_e[,3:26])

w3_t_miss <- mice(w3_t [, c(3:26)], m = 10, maxit = 100, method = "pmm", seed = 123)
w3_t_miss_imp <- mice::complete(w3_t_miss,10)
w3_t[,3:26] <- w3_t_miss_imp
describe(w3_t[,3:26])

w5_e_miss <- mice(w5_e [, c(3:26)], m = 10, maxit = 100, method = "pmm", seed = 123)
w5_e_miss_imp <- mice::complete(w5_e_miss,10)
w5_e[,3:26] <- w5_e_miss_imp
describe(w5_e[,3:26])

w5_t_miss <- mice(w5_t [, c(3:26)], m = 10, maxit = 100, method = "pmm", seed = 123)
w5_t_miss_imp <- mice::complete(w5_t_miss,10)
w5_t[,3:26] <- w5_t_miss_imp
describe(w5_t[,3:26])



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------

w2 <- bind_rows(w2_e, w2_t)
w3 <- bind_rows(w3_e, w3_t)
w5 <- bind_rows(w5_e, w5_t)

describe(w2[,3:26])
describe(w3[,3:26])
describe(w5[,3:26])

save(w2, file = "w2.Rdata")
save(w3, file = "w3.Rdata")
save(w5, file = "w5.RData")

data <- bind_rows(w2,w3,w5)
data$wave <- as.factor(data$wave)
save(data, file = "data.Rdata")

rm(list = ls(all.names = T))



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------

load("data.RData")

w2_e <- data %>% filter(country%in%"Egypt" & wave%in%"W2") 
w2_t <- data %>% filter(country%in%"Tunisia" & wave%in%"W2")
describe(w2_e[,3:26])
describe(w2_t[,3:26])



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------

library(lavaan)
cfa_3d <- "
TPI =~ Tgo + Tpo + Taf + Tcv
Dde =~ Dec + Dfp + Dne
Fre =~ Fex + Fde + Fcs
"
w2_e_tdl_cfa_fit <- cfa(model = cfa_3d, data = w2_e, estimator = "mlr", mimic = "mplus",
                        std.ov = T, std.lv = T)
summary(w2_e_tdl_cfa_fit, fit.measures=T, standardized=T, rsquare=T)

w2_t_tdl_cfa_fit <- cfa(model = cfa_3d, data = w2_t, estimator = "mlr", mimic = "mplus",
                        std.ov = T, std.lv = T)
summary(w2_t_tdl_cfa_fit, fit.measures=T, standardized=T, rsquare=T)

library(semPlot)
par(mfrow=c(1,2))
semPaths(w2_e_tdl_cfa_fit, "mod", "std", intercepts = F, edge.label.cex = 1.2, rotation = 4)
title(sub = "W2 Egypt 3D CFA")
semPaths(w2_t_tdl_cfa_fit, "mod", "std", intercepts = F, edge.label.cex = 1.2, rotation = 4)
title(sub = "W2 Tunisia 3D CFA")
dev.off()

library(semTools)
round(reliability(w2_e_tdl_cfa_fit),2)
round(reliability(w2_t_tdl_cfa_fit),2)

# all OK



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------

w2_e <- w2_e %>% mutate(TPI = lavPredict(w2_e_tdl_cfa_fit, type = "lv", method = "EBM", label = T, fsm = T)[, "TPI"],
                        Dde = lavPredict(w2_e_tdl_cfa_fit, type = "lv", method = "EBM", label = T, fsm = T)[, "Dde"],
                        Fre = lavPredict(w2_e_tdl_cfa_fit, type = "lv", method = "EBM", label = T, fsm = T)[, "Fre"])
colnames(w2_e)
w2_t <- w2_t %>% mutate(TPI = lavPredict(w2_t_tdl_cfa_fit, type = "lv", method = "EBM", label = T, fsm = T)[, "TPI"],
                        Dde = lavPredict(w2_t_tdl_cfa_fit, type = "lv", method = "EBM", label = T, fsm = T)[, "Dde"],
                        Fre = lavPredict(w2_t_tdl_cfa_fit, type = "lv", method = "EBM", label = T, fsm = T)[, "Fre"])
colnames(w2_t)
w2_e$TPI <- scales::rescale(w2_e$TPI, to = c(0,1), from = range(w2_e$TPI, na.rm = F, finite = T))
w2_e$Dde <- scales::rescale(w2_e$Dde, to = c(0,1), from = range(w2_e$Dde, na.rm = F, finite = T))
w2_e$Fre <- scales::rescale(w2_e$Fre, to = c(0,1), from = range(w2_e$Fre, na.rm = F, finite = T))

describe(w2_e[,27:29])

w2_t$TPI <- scales::rescale(w2_t$TPI, to = c(0,1), from = range(w2_t$TPI, na.rm = F, finite = T))
w2_t$Dde <- scales::rescale(w2_t$Dde, to = c(0,1), from = range(w2_t$Dde, na.rm = F, finite = T))
w2_t$Fre <- scales::rescale(w2_t$Fre, to = c(0,1), from = range(w2_t$Fre, na.rm = F, finite = T))
describe(w2_t[,27:29])

w2 <- bind_rows(w2_e,w2_t)
save(w2, file = "w2.RData")

gdata::keep(list = c("cfa_3d", "data"), sure = T)



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------
w3_e <- data %>% filter(country%in%"Egypt" & wave%in%"W3") 
w3_t <- data %>% filter(country%in%"Tunisia"& wave%in%"W3")
w3_e_tdl_cfa_fit <- cfa(model = cfa_3d, data = w3_e, estimator = "mlr", mimic = "mplus",
                        std.ov = T, std.lv = T)
summary(w3_e_tdl_cfa_fit, fit.measures=T, standardized=T, rsquare=T)

w3_t_tdl_cfa_fit <- cfa(model = cfa_3d, data = w3_t, estimator = "mlr", mimic = "mplus",
                        std.ov = T, std.lv = T)
summary(w3_t_tdl_cfa_fit, fit.measures=T, standardized=T, rsquare=T)
par(mfrow=c(1,2))
semPaths(w3_e_tdl_cfa_fit, "mod", "std", intercepts = F, edge.label.cex = 1.2, rotation = 4)
title(sub = "W3 Egypt 3D CFA")
semPaths(w3_t_tdl_cfa_fit, "mod", "std", intercepts = F, edge.label.cex = 1.2, rotation = 4)
title(sub = "W3 Tunisia 3D CFA")
dev.off()

round(reliability(w3_e_tdl_cfa_fit),2)
round(reliability(w3_t_tdl_cfa_fit),2)

w3_e <- w3_e %>% mutate(TPI = lavPredict(w3_e_tdl_cfa_fit, type = "lv", method = "EBM", label = T, fsm = T)[, "TPI"],
                        Dde = lavPredict(w3_e_tdl_cfa_fit, type = "lv", method = "EBM", label = T, fsm = T)[, "Dde"],
                        Fre = lavPredict(w3_e_tdl_cfa_fit, type = "lv", method = "EBM", label = T, fsm = T)[, "Fre"])
w3_t <- w3_t %>% mutate(TPI = lavPredict(w3_t_tdl_cfa_fit, type = "lv", method = "EBM", label = T, fsm = T)[, "TPI"],
                        Dde = lavPredict(w3_t_tdl_cfa_fit, type = "lv", method = "EBM", label = T, fsm = T)[, "Dde"],
                        Fre = lavPredict(w3_t_tdl_cfa_fit, type = "lv", method = "EBM", label = T, fsm = T)[, "Fre"])
w3_e$TPI <- scales::rescale(w3_e$TPI, to = c(0,1), from = range(w3_e$TPI, na.rm = F, finite = T))
w3_e$Dde <- scales::rescale(w3_e$Dde, to = c(0,1), from = range(w3_e$Dde, na.rm = F, finite = T))
w3_e$Fre <- scales::rescale(w3_e$Fre, to = c(0,1), from = range(w3_e$Fre, na.rm = F, finite = T))
w3_t$TPI <- scales::rescale(w3_t$TPI, to = c(0,1), from = range(w3_t$TPI, na.rm = F, finite = T))
w3_t$Dde <- scales::rescale(w3_t$Dde, to = c(0,1), from = range(w3_t$Dde, na.rm = F, finite = T))
w3_t$Fre <- scales::rescale(w3_t$Fre, to = c(0,1), from = range(w3_t$Fre, na.rm = F, finite = T))
w3 <- bind_rows(w3_e,w3_t)
save(w3, file = "w3.RData")

gdata::keep(list = c("cfa_3d", "data"), sure = T)

w5_e <- data %>% filter(country%in%"Egypt" & wave%in%"w5")
w5_t <- data %>% filter(country%in%"Tunisia" & wave%in%"w5")
w5_e_tdl_cfa_fit <- cfa(model = cfa_3d, data = w5_e, estimator = "mlr", mimic = "mplus",
                        std.ov = T, std.lv = T)
summary(w5_e_tdl_cfa_fit, fit.measures=T, standardized=T, rsquare=T)

w5_t_tdl_cfa_fit <- cfa(model = cfa_3d, data = w5_t, estimator = "mlr", mimic = "mplus",
                        std.ov = T, std.lv = T)
summary(w5_t_tdl_cfa_fit, fit.measures=T, standardized=T, rsquare=T)
par(mfrow=c(1,2))
semPaths(w5_e_tdl_cfa_fit, "mod", "std", intercepts = F, edge.label.cex = 1.2, rotation = 4)
title(sub = "w5 Egypt 3D CFA")
semPaths(w5_t_tdl_cfa_fit, "mod", "std", intercepts = F, edge.label.cex = 1.2, rotation = 4)
title(sub = "w5 Tunisia 3D CFA")
dev.off()
round(reliability(w5_e_tdl_cfa_fit),2)
round(reliability(w5_t_tdl_cfa_fit),2)

w5_e <- w5_e %>% mutate(TPI = lavPredict(w5_e_tdl_cfa_fit, type = "lv", method = "EBM", label = T, fsm = T)[, "TPI"],
                        Dde = lavPredict(w5_e_tdl_cfa_fit, type = "lv", method = "EBM", label = T, fsm = T)[, "Dde"],
                        Fre = lavPredict(w5_e_tdl_cfa_fit, type = "lv", method = "EBM", label = T, fsm = T)[, "Fre"])
w5_t <- w5_t %>% mutate(TPI = lavPredict(w5_t_tdl_cfa_fit, type = "lv", method = "EBM", label = T, fsm = T)[, "TPI"],
                        Dde = lavPredict(w5_t_tdl_cfa_fit, type = "lv", method = "EBM", label = T, fsm = T)[, "Dde"],
                        Fre = lavPredict(w5_t_tdl_cfa_fit, type = "lv", method = "EBM", label = T, fsm = T)[, "Fre"])
w5_e$TPI <- scales::rescale(w5_e$TPI, to = c(0,1), from = range(w5_e$TPI, na.rm = F, finite = T))
w5_e$Dde <- scales::rescale(w5_e$Dde, to = c(0,1), from = range(w5_e$Dde, na.rm = F, finite = T))
w5_e$Fre <- scales::rescale(w5_e$Fre, to = c(0,1), from = range(w5_e$Fre, na.rm = F, finite = T))
w5_t$TPI <- scales::rescale(w5_t$TPI, to = c(0,1), from = range(w5_t$TPI, na.rm = F, finite = T))
w5_t$Dde <- scales::rescale(w5_t$Dde, to = c(0,1), from = range(w5_t$Dde, na.rm = F, finite = T))
w5_t$Fre <- scales::rescale(w5_t$Fre, to = c(0,1), from = range(w5_t$Fre, na.rm = F, finite = T))
w5 <- bind_rows(w5_e,w5_t)
save(w5, file = "w5.RData")

colnames(w5)

rm(list = ls(all.names = T))



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------

load("w2.RData")
load("w3.RData")
load("w5.RData")

data <- bind_rows(w2,w3,w5)
describe(data[,3:29])
save(data, file = "data.RData")

rm(list = ls(all.names = T))



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------

load("data.Rdata")
colnames(data)

data <- data %>% select(country, wave, TPI, Dde, Fre, Dbe, Dsu, Sre, Iip, Ise, Eco, Dem, Int)

nodevar_names <- paste(names(data[,3:13]), collapse = "+")
splitvars_names_time <- "country+wave"
nwt_cw_f <- as.formula(paste(c(nodevar_names, splitvars_names_time), collapse = "~"))

library(networktree)
nwt_cw <- networktree(nwt_cw_f, data = data, method = "ctree", model = "correlation", transform = "glasso")
# color
plot(nwt_cw, type = "glasso", layout = "circle", maximum = 1, edge.labels = T, edge.label.cex = 1, theme = "colorblind")
# black & white
plot(nwt_cw, type = "glasso", layout = "circle", maximum = 1, edge.labels = T, edge.label.cex = 1, theme = "gray")

# the attitude networks across differences waves and countries are all significantly different

rm(list = ls(all.names = T))



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------

load("data.Rdata")

library(parallel)
cl <- makeCluster(7)

library(bnlearn)



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------

w2_e <- data %>% filter(country%in%"Egypt" & wave%in%"W2") %>%
  select(country, wave, TPI, Dde, Fre, Dbe, Dsu, Sre, Iip, Ise, Eco, Dem, Int)

set.seed(21)
bootnet_w2_e <- boot.strength(data = w2_e[,3:13], R = 1000, algorithm = "tabu", cluster = cl)
head(bootnet_w2_e)

avgnet_w2_e <- averaged.network(bootnet_w2_e, threshold = 0.85)
estrength_w2_e <- arc.strength(avgnet_w2_e, w2_e[,3:13], "bic-g")
strength.plot(avgnet_w2_e, estrength_w2_e, shape = "ellipse", main = "deneme")

subedge_w2_e <- head(bootnet_w2_e[bootnet_w2_e$strength > 0.95, ])
subedge_w2_e

boottab_w2_e <- bootnet_w2_e[bootnet_w2_e$strength > 0.85 & bootnet_w2_e$direction > 0.5, ]
astr_w2_e <- boottab_w2_e
astr_w2_e$strength <- astr_w2_e$direction 
strength.plot(avgnet_w2_e, astr_w2_e, shape = "ellipse", main = "Bayesian Network of Egypt W2")

graphviz.plot(avgnet_w2_e, layout = "fdp", main = "Bayesian Network of Egypt W2")



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------

w2_t <- data %>% filter(country%in%"Tunisia" & wave%in%"W2") %>% 
  select(country, wave, TPI, Dde, Fre, Dbe, Dsu, Sre, Iip, Ise, Eco, Dem, Int)

set.seed(22)
bootnet_w2_t <- boot.strength(data = w2_t[,3:13], R = 1000, algorithm = "tabu", cluster = cl)
head(bootnet_w2_t)

avgnet_w2_t <- averaged.network(bootnet_w2_t, threshold = 0.85)
estrength_w2_t <- arc.strength(avgnet_w2_t, w2_t[,3:13], "bic-g")
strength.plot(avgnet_w2_t, estrength_w2_t, shape = "ellipse")

subedge_w2_t <- head(bootnet_w2_t[bootnet_w2_t$strength > 0.95, ])
subedge_w2_t

boottab_w2_t <- bootnet_w2_t[bootnet_w2_t$strength > 0.85 & bootnet_w2_t$direction > 0.5, ]
astr_w2_t <- boottab_w2_t
astr_w2_t$strength <- astr_w2_t$direction 
strength.plot(avgnet_w2_t, astr_w2_t, shape = "ellipse", main = "Bayesian Network of Egypt W2")

graphviz.plot(avgnet_w2_t, layout = "fdp", main = "Bayesian Network of Tunisia W2")

par(mfrow = c(1,2))
set.seed(123)
graphviz.plot(avgnet_w2_e, layout = "fdp", main = "Bayesian Network of Egypt W2")
set.seed(321)
graphviz.plot(avgnet_w2_t, layout = "fdp", main = "Bayesian Network of Tunisia W2")
dev.off()




## ---------------------------------------------------------------------------------------------------------------------------------------------------------------
par(mfrow = c(1,2))
strength.plot(avgnet_w2_e, astr_w2_e, shape = "ellipse", main = "Bayesian Network of Egypt W2")
strength.plot(avgnet_w2_t, astr_w2_t, shape = "ellipse", main = "Bayesian Network of Tunisia W2")
dev.off()


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------

actors <- c("TPI", "Dde", "Fre", "Dbe", "Dsu", "Sre", "Iip", "Ise", "Eco", "Dem", "Int")
relations_in_w2_e <- astr_w2_e[,1:3]
relations_in_w2_e$strength <- round(relations_in_w2_e$strength,1)*10
relations_in_w2_e
relations_in_w2_t <- astr_w2_t[,1:3]
relations_in_w2_t$strength <- round(relations_in_w2_t$strength,1)*10
relations_in_w2_t

library(igraph)
g_w2_e <- graph_from_data_frame(relations_in_w2_e[,1:2], directed = T, vertices=actors)
sort(degree(g_w2_e, mode = "in"), decreasing = T)
# Eco Dde Sre Ise Dem Int TPI Fre Dbe Dsu Iip 
#  3   2   2   2   2   2   1   1   0   0   0 
sort(degree(g_w2_e, mode = "out"), decreasing = T)
# Dbe Iip TPI Dde Dsu Int Dem Fre Sre Ise Eco 
#  3   3   2   2   2   2   1   0   0   0   0 
E(g_w2_e)$weight <- relations_in_w2_e$strength
par(oma=c(1,1,1,1),
    mar=c(1,1,1,1))
set.seed(321)
plot(g_w2_e,
     vertex.size = degree(g_w2_e, mode = "in"),
     edge.width = log(E(g_w2_e)$weight),
     edge.arrow.size = 0.25, main = "W2 Egypt Incoming Centrality")

g_w2_t <- graph_from_data_frame(relations_in_w2_t[,1:2], directed = T, vertices=actors)
sort(degree(g_w2_t, mode = "in"), decreasing = T)
# Iip Fre TPI Dbe Eco Dem Int Dde Dsu Sre Ise 
#  3   2   1   1   1   1   1   0   0   0   0
sort(degree(g_w2_t, mode = "out"), decreasing = T)
# TPI Fre Ise Dem Dde Dsu Dbe Sre Iip Eco Int 
#  2   2   2   2   1   1   0   0   0   0   0
E(g_w2_t)$weight <- relations_in_w2_t$strength

coords <- layout_(g_w2_e, with_kk()) ## get coordinates to apply to all the following graphs

par(mfrow=c(2,2),
    oma=c(.1,.1,2,.1),
    mar=c(.1,.1,2,.1))
set.seed(123456)
plot(g_w2_e, layout = coords,
     vertex.size = degree(g_w2_e, mode = "out")*5,
     vertex.label.cex	= 1.2,
     vertex.color = adjustcolor("gray", alpha.f = .5),
     vertex.label.color = adjustcolor("black", alpha.f = .5),
     edge.width = log(E(g_w2_e)$weight),
     edge.color = "black",
     edge.arrow.size = 0.25, main = "W2 Egypt Causes (Outgoing Centrality)")
plot(g_w2_t, layout = coords,
     vertex.size = degree(g_w2_t, mode = "out")*5,
     vertex.label.cex	= 1.2,
     vertex.color = adjustcolor("gray", alpha.f = .5),
     vertex.label.color = adjustcolor("black", alpha.f = .5),
     edge.width = log(E(g_w2_t)$weight),
     edge.color = "black",
     edge.arrow.size = 0.25, main = "W2 Tunisia Causes (Outgoing Centrality)")
plot(g_w2_e, layout = coords,
     vertex.size = degree(g_w2_e, mode = "in")*5,
     vertex.label.cex	= 1.2,
     vertex.color = adjustcolor("gray", alpha.f = .5),
     vertex.label.color = adjustcolor("black", alpha.f = .5),
     edge.width = log(E(g_w2_e)$weight),
     edge.color = "black",
     edge.arrow.size = 0.25,
     main = "W2 Egypt Effects (Incoming Centrality)")
plot(g_w2_t, layout = coords,
     vertex.size = degree(g_w2_t, mode = "in")*5,
     vertex.label.cex	= 1.2,
     vertex.color = adjustcolor("gray", alpha.f = .5),
     vertex.label.color = adjustcolor("black", alpha.f = .5),
     edge.width = log(E(g_w2_t)$weight),
     edge.color = "black",
     edge.arrow.size = 0.25,
     main = "W2 Tunisia Effects (Incoming Centrality)")
dev.off()

gdata::keep(list = c("cl", "data", "actors", "coords"), sure = T)



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------

w3_e <- data %>% filter(country%in%"Egypt" & wave%in%"W3") %>%
  select(country, wave, TPI, Dde, Fre, Dbe, Dsu, Sre, Iip, Ise, Eco, Dem, Int)

set.seed(31)
bootnet_w3_e <- boot.strength(data = w3_e[,3:13], R = 1000, algorithm = "tabu", cluster = cl)
head(bootnet_w3_e)

avgnet_w3_e <- averaged.network(bootnet_w3_e, threshold = 0.85)
estrength_w3_e <- arc.strength(avgnet_w3_e, w3_e[,3:13], "bic-g")
strength.plot(avgnet_w3_e, estrength_w3_e, shape = "ellipse")

subedge_w3_e <- head(bootnet_w3_e[bootnet_w3_e$strength > 0.95, ])
subedge_w3_e

boottab_w3_e <- bootnet_w3_e[bootnet_w3_e$strength > 0.85 & bootnet_w3_e$direction > 0.5, ]
astr_w3_e <- boottab_w3_e
astr_w3_e$strength <- astr_w3_e$direction 
strength.plot(avgnet_w3_e, astr_w3_e, shape = "ellipse", main = "Bayesian Network of Egypt w3")

graphviz.plot(avgnet_w3_e, layout = "fdp", main = "Bayesian Network of Egypt w3")



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------

w3_t <- data %>% filter(country%in%"Tunisia" & wave%in%"W3") %>% 
  select(country, wave, TPI, Dde, Fre, Dbe, Dsu, Sre, Iip, Ise, Eco, Dem, Int)

set.seed(32)
bootnet_w3_t <- boot.strength(data = w3_t[,3:13], R = 1000, algorithm = "tabu", cluster = cl)
head(bootnet_w3_t)

avgnet_w3_t <- averaged.network(bootnet_w3_t, threshold = 0.85)
estrength_w3_t <- arc.strength(avgnet_w3_t, w3_t[,3:13], "bic-g")
strength.plot(avgnet_w3_t, estrength_w3_t, shape = "ellipse")

subedge_w3_t <- head(bootnet_w3_t[bootnet_w3_t$strength > 0.95, ])
subedge_w3_t

boottab_w3_t <- bootnet_w3_t[bootnet_w3_t$strength > 0.85 & bootnet_w3_t$direction > 0.5, ]
astr_w3_t <- boottab_w3_t
astr_w3_t$strength <- astr_w3_t$direction 
strength.plot(avgnet_w3_t, astr_w3_t, shape = "ellipse", main = "Bayesian Network of Egypt w3")

graphviz.plot(avgnet_w3_t, layout = "fdp", main = "Bayesian Network of Tunisia w3")

par(mfrow = c(1,2))
set.seed(123)
graphviz.plot(avgnet_w3_e, layout = "fdp", main = "Bayesian Network of Egypt w3")
set.seed(321)
graphviz.plot(avgnet_w3_t, layout = "fdp", main = "Bayesian Network of Tunisia w3")
dev.off()



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------

par(mfrow = c(1,2))
strength.plot(avgnet_w3_e, astr_w3_e, shape = "ellipse", main = "Bayesian Network of Egypt w3")
strength.plot(avgnet_w3_t, astr_w3_t, shape = "ellipse", main = "Bayesian Network of Tunisia w3")
dev.off()



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------

relations_in_w3_e <- astr_w3_e[,1:3]
relations_in_w3_e$strength <- round(relations_in_w3_e$strength,1)*10
relations_in_w3_e
relations_in_w3_t <- astr_w3_t[,1:3]
relations_in_w3_t$strength <- round(relations_in_w3_t$strength,1)*10
relations_in_w3_t

g_w3_e <- graph_from_data_frame(relations_in_w3_e[,1:2], directed = T, vertices=actors)
sort(degree(g_w3_e, mode = "in"), decreasing = T)
# Fre Ise Dsu Sre Iip Eco Dde Dem Int TPI Dbe 
# 3   3   2   2   2   2   1   1   1   0   0  
sort(degree(g_w3_e, mode = "out"), decreasing = T)
# TPI Sre Dem Dde Dbe Fre Dsu Ise Eco Iip Int 
# 3   3   3   2   2   1   1   1   1   0   0
E(g_w3_e)$weight <- relations_in_w3_e$strength

g_w3_t <- graph_from_data_frame(relations_in_w3_t[,1:2], directed = T, vertices=actors)
sort(degree(g_w3_t, mode = "in"), decreasing = T)
# Dsu Dem Ise Dbe Eco TPI Fre Sre Iip Dde Int 
# 4   4   3   2   2   1   1   1   1   0   0 
sort(degree(g_w3_t, mode = "out"), decreasing = T)
# TPI Dde Sre Fre Dbe Iip Ise Eco Dem Int Dsu 
# 3   3   3   2   2   2   1   1   1   1   0
E(g_w3_t)$weight <- relations_in_w3_t$strength

par(mfrow=c(2,2),
    oma=c(.1,.1,2,.1),
    mar=c(.1,.1,2,.1))
set.seed(123456)
plot(g_w3_e, layout = coords,
     vertex.size = degree(g_w3_e, mode = "out")*5,
     vertex.label.cex	= 1.2,
     vertex.color = adjustcolor("gray", alpha.f = .5),
     vertex.label.color = adjustcolor("black", alpha.f = .5),
     edge.width = log(E(g_w3_e)$weight),
     edge.color = "black",
     edge.arrow.size = 0.25, main = "W3 Egypt Causes (Outgoing Centrality)")
plot(g_w3_t, layout = coords,
     vertex.size = degree(g_w3_t, mode = "out")*5,
     vertex.label.cex	= 1.2,
     vertex.color = adjustcolor("gray", alpha.f = .5),
     vertex.label.color = adjustcolor("black", alpha.f = .5),
     edge.width = log(E(g_w3_t)$weight),
     edge.color = "black",
     edge.arrow.size = 0.25, main = "W3 Tunisia Causes (Outgoing Centrality)")
plot(g_w3_e, layout = coords,
     vertex.size = degree(g_w3_e, mode = "in")*5,
     vertex.label.cex	= 1.2,
     vertex.color = adjustcolor("gray", alpha.f = .5),
     vertex.label.color = adjustcolor("black", alpha.f = .5),
     edge.width = log(E(g_w3_e)$weight),
     edge.color = "black",
     edge.arrow.size = 0.25,
     main = "W3 Egypt Effects (Incoming Centrality)")
plot(g_w3_t, layout = coords,
     vertex.size = degree(g_w3_t, mode = "in")*5,
     vertex.label.cex	= 1.2,
     vertex.color = adjustcolor("gray", alpha.f = .5),
     vertex.label.color = adjustcolor("black", alpha.f = .5),
     edge.width = log(E(g_w3_t)$weight),
     edge.color = "black",
     edge.arrow.size = 0.25,
     main = "W3 Tunisia Effects (Incoming Centrality)")
dev.off()

gdata::keep(list = c("cl", "data", "actors", "coords"), sure = T)



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------

w5_e <- data %>% filter(country%in%"Egypt" & wave%in%"w5") %>%
  select(country, wave, TPI, Dde, Fre, Dbe, Dsu, Sre, Iip, Ise, Eco, Dem, Int)

set.seed(51)
bootnet_w5_e <- boot.strength(data = w5_e[,3:13], R = 1000, algorithm = "tabu", cluster = cl)
head(bootnet_w5_e)

avgnet_w5_e <- averaged.network(bootnet_w5_e, threshold = 0.85)
estrength_w5_e <- arc.strength(avgnet_w5_e, w5_e[,3:13], "bic-g")
strength.plot(avgnet_w5_e, estrength_w5_e, shape = "ellipse")

subedge_w5_e <- head(bootnet_w5_e[bootnet_w5_e$strength > 0.95, ])
subedge_w5_e

boottab_w5_e <- bootnet_w5_e[bootnet_w5_e$strength > 0.85 & bootnet_w5_e$direction > 0.5, ]
astr_w5_e <- boottab_w5_e
astr_w5_e$strength <- astr_w5_e$direction 
strength.plot(avgnet_w5_e, astr_w5_e, shape = "ellipse", main = "Bayesian Network of Egypt w5")

graphviz.plot(avgnet_w5_e, layout = "fdp", main = "Bayesian Network of Egypt w5")



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------

w5_t <- data %>% filter(country%in%"Tunisia" & wave%in%"w5") %>% 
  select(country, wave, TPI, Dde, Fre, Dbe, Dsu, Sre, Iip, Ise, Eco, Dem, Int)

set.seed(52)
bootnet_w5_t <- boot.strength(data = w5_t[,3:13], R = 1000, algorithm = "tabu", cluster = cl)
head(bootnet_w5_t)

avgnet_w5_t <- averaged.network(bootnet_w5_t, threshold = 0.85)
estrength_w5_t <- arc.strength(avgnet_w5_t, w5_t[,3:13], "bic-g")
strength.plot(avgnet_w5_t, estrength_w5_t, shape = "ellipse")

subedge_w5_t <- head(bootnet_w5_t[bootnet_w5_t$strength > 0.95, ])
subedge_w5_t

boottab_w5_t <- bootnet_w5_t[bootnet_w5_t$strength > 0.85 & bootnet_w5_t$direction > 0.5, ]
astr_w5_t <- boottab_w5_t
astr_w5_t$strength <- astr_w5_t$direction 
strength.plot(avgnet_w5_t, astr_w5_t, shape = "ellipse", main = "Bayesian Network of Egypt w5")

graphviz.plot(avgnet_w5_t, layout = "fdp", main = "Bayesian Network of Tunisia w5")

par(mfrow = c(1,2))
set.seed(123)
graphviz.plot(avgnet_w5_e, layout = "fdp", main = "Bayesian Network of Egypt w5")
set.seed(321)
graphviz.plot(avgnet_w5_t, layout = "fdp", main = "Bayesian Network of Tunisia w5")
dev.off()



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------

par(mfrow = c(1,2))
strength.plot(avgnet_w5_e, astr_w5_e, shape = "ellipse", main = "Bayesian Network of Egypt w5")
strength.plot(avgnet_w5_t, astr_w5_t, shape = "ellipse", main = "Bayesian Network of Tunisia w5")
dev.off()



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------

relations_in_w5_e <- astr_w5_e[,1:3]
relations_in_w5_e$strength <- round(relations_in_w5_e$strength,1)*10
relations_in_w5_e
relations_in_w5_t <- astr_w5_t[,1:3]
relations_in_w5_t$strength <- round(relations_in_w5_t$strength,1)*10
relations_in_w5_t

g_w5_e <- graph_from_data_frame(relations_in_w5_e[,1:2], directed = T, vertices=actors)
sort(degree(g_w5_e, mode = "in"), decreasing = T)
# Eco Dem Int Dsu Sre Iip Fre Dbe TPI Ise Dde 
# 6   5   4   3   3   3   2   2   1   1   0 
sort(degree(g_w5_e, mode = "out"), decreasing = T)
# TPI Dde Ise Dsu Fre Dbe Sre Dem Int Iip Eco 
#  7   5   5   4   3   2   2   1   1   0   0 
E(g_w5_e)$weight <- relations_in_w5_e$strength

g_w5_t <- graph_from_data_frame(relations_in_w5_t[,1:2], directed = T, vertices=actors)
sort(degree(g_w5_t, mode = "in"), decreasing = T)
# Dbe Dem Dsu Ise Eco Dde Fre Sre Int TPI Iip 
#  3   3   2   2   2   1   1   1   1   0   0 
sort(degree(g_w5_t, mode = "out"), decreasing = T)
# TPI Fre Sre Dsu Dde Iip Ise Dem Dbe Eco Int
# 4   3   3   2   1   1   1   1   0   0   0
E(g_w5_t)$weight <- relations_in_w5_t$strength

par(mfrow=c(2,2),
    oma=c(.1,.1,2,.1),
    mar=c(.1,.1,2,.1))
set.seed(123456)
plot(g_w5_e, layout = coords,
     vertex.size = degree(g_w5_e, mode = "out")*5,
     vertex.label.cex	= 1.2,
     vertex.color = adjustcolor("gray", alpha.f = .5),
     vertex.label.color = adjustcolor("black", alpha.f = .5),
     edge.width = log(E(g_w5_e)$weight),
     edge.color = "black",
     edge.arrow.size = 0.25, main = "W5 Egypt Causes (Outgoing Centrality)")
plot(g_w5_t, layout = coords,
     vertex.size = degree(g_w5_t, mode = "out")*5,
     vertex.label.cex	= 1.2,
     vertex.color = adjustcolor("gray", alpha.f = .5),
     vertex.label.color = adjustcolor("black", alpha.f = .5),
     edge.width = log(E(g_w5_t)$weight),
     edge.color = "black",
     edge.arrow.size = 0.25, main = "W5 Tunisia Causes (Outgoing Centrality)")
plot(g_w5_e, layout = coords,
     vertex.size = degree(g_w5_e, mode = "in")*5,
     vertex.label.cex	= 1.2,
     vertex.color = adjustcolor("gray", alpha.f = .5),
     vertex.label.color = adjustcolor("black", alpha.f = .5),
     edge.width = log(E(g_w5_e)$weight),
     edge.color = "black",
     edge.arrow.size = 0.25,
     main = "W5 Egypt Effects (Incoming Centrality)")
plot(g_w5_t, layout = coords,
     vertex.size = degree(g_w5_t, mode = "in")*5,
     vertex.label.cex	= 1.2,
     vertex.color = adjustcolor("gray", alpha.f = .5),
     vertex.label.color = adjustcolor("black", alpha.f = .5),
     edge.width = log(E(g_w5_t)$weight),
     edge.color = "black",
     edge.arrow.size = 0.25,
     main = "W5 Tunisia Effects (Incoming Centrality)")
dev.off()

stopCluster(cl)



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------

gdata::keep(data, sure = T)

w2_e <- data %>% filter(country%in%"Egypt" & wave%in%"W2") %>% 
  select(TPI, Dde, Fre, Dbe, Dsu, Sre, Iip, Ise, Eco, Dem, Int)
w2_e_ds <- describe(w2_e) %>% select(mean:median, min:max, skew:se)
w2_e_ds <- round(w2_e_ds, 2)

w2_t <- data %>% filter(country%in%"Tunisia" & wave%in%"W2") %>% 
  select(TPI, Dde, Fre, Dbe, Dsu, Sre, Iip, Ise, Eco, Dem, Int)
w2_t_ds <- describe(w2_t) %>% select(mean:median, min:max, skew:se)
w2_t_ds <- round(w2_t_ds, 2)

w3_e <- data %>% filter(country%in%"Egypt" & wave%in%"W3") %>% 
  select(TPI, Dde, Fre, Dbe, Dsu, Sre, Iip, Ise, Eco, Dem, Int)
w3_e_ds <- describe(w3_e) %>% select(mean:median, min:max, skew:se)
w3_e_ds <- round(w3_e_ds, 2)

w3_t <- data %>% filter(country%in%"Tunisia" & wave%in%"W3") %>% 
  select(TPI, Dde, Fre, Dbe, Dsu, Sre, Iip, Ise, Eco, Dem, Int)
w3_t_ds <- describe(w3_t) %>% select(mean:median, min:max, skew:se)
w3_t_ds <- round(w3_t_ds, 2)

w5_e <- data %>% filter(country%in%"Egypt" & wave%in%"w5") %>% 
  select(TPI, Dde, Fre, Dbe, Dsu, Sre, Iip, Ise, Eco, Dem, Int)
w5_e_ds <- describe(w5_e) %>% select(mean:median, min:max, skew:se)
w5_e_ds <- round(w5_e_ds, 2)

w5_t <- data %>% filter(country%in%"Tunisia" & wave%in%"w5") %>% 
  select(TPI, Dde, Fre, Dbe, Dsu, Sre, Iip, Ise, Eco, Dem, Int)
w5_t_ds <- describe(w5_t) %>% select(mean:median, min:max, skew:se)
w5_t_ds <- round(w5_t_ds, 2)

rio::export(list(w2_e_ds = w2_e_ds, w2_t_ds = w2_t_ds,
                 w3_e_ds = w3_e_ds, w3_t_ds = w3_t_ds,
                 w5_e_ds = w5_e_ds, w5_t_ds = w5_t_ds), file = "desc_stats.xlsx")



