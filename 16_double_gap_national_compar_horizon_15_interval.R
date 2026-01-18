setwd("~/Library/CloudStorage/Dropbox/Todos/Gap_modeling/code/accuracy_comparison/FTS_model")
setwd("/Users/mq20178348/Dropbox/Todos/Gap_modeling/code/accuracy_comparison/FTS_model")
source("double_gap_aux.R")

### interval forecasts

## ARIMA

# alpha = 0.2

subnational_double_gap_score_M_ARIMA_PI_80 = subnational_double_gap_score_F_ARIMA_PI_80 = array(NA, dim = c(47, 15, 3), 
                                                                                                dimnames = list(1:47, 1:15, c("ECP", "CPD", "score")))
for(iwj in 1:47)
{
    for(iwk in 1:15)
    {
        dum = double_gap_fore_fun_conformal(prefecture_index = iwj,
                                            national_F_CDF = Japan_female_pop_CDF,
                                            PDF_M_holdout = male_prefecture_dx[[iwj]], 
                                            PDF_F_holdout = female_prefecture_dx[[iwj]],
                                            horizon = iwk, fore_method = "arima", length_test_data = 15,
                                            alpha = 0.2)
        subnational_double_gap_score_M_ARIMA_PI_80[iwj,iwk,] = dum$int_score_M
        subnational_double_gap_score_F_ARIMA_PI_80[iwj,iwk,] = dum$int_score_F
        rm(dum); rm(iwk)
    }
    print(iwj); rm(iwj)
}

subnational_double_gap_score_M_ARIMA_PI_80_mean = round(apply(subnational_double_gap_score_M_ARIMA_PI_80, c(2, 3), mean), 4) # 
subnational_double_gap_score_F_ARIMA_PI_80_mean = round(apply(subnational_double_gap_score_F_ARIMA_PI_80, c(2, 3), mean), 4) # 

# alpha = 0.05

subnational_double_gap_score_M_ARIMA_PI_95 = subnational_double_gap_score_F_ARIMA_PI_95 = array(NA, dim = c(47, 15, 3), 
                                                                                                dimnames = list(1:47, 1:15, c("ECP", "CPD", "score")))
for(iwj in 1:47)
{
    for(iwk in 1:15)
    {
        dum = double_gap_fore_fun_conformal(prefecture_index = iwj,
                                            national_F_CDF = Japan_female_pop_CDF,
                                            PDF_M_holdout = male_prefecture_dx[[iwj]], 
                                            PDF_F_holdout = female_prefecture_dx[[iwj]],
                                            horizon = iwk, fore_method = "arima", length_test_data = 15,
                                            alpha = 0.05)
        subnational_double_gap_score_M_ARIMA_PI_95[iwj,iwk,] = dum$int_score_M
        subnational_double_gap_score_F_ARIMA_PI_95[iwj,iwk,] = dum$int_score_F
        rm(dum); rm(iwk)
    }
    print(iwj); rm(iwj)
}

subnational_double_gap_score_M_ARIMA_PI_95_mean = round(apply(subnational_double_gap_score_M_ARIMA_PI_95, c(2, 3), mean), 4) # 
subnational_double_gap_score_F_ARIMA_PI_95_mean = round(apply(subnational_double_gap_score_F_ARIMA_PI_95, c(2, 3), mean), 4) # 

## ETS

# alpha = 0.2

subnational_double_gap_score_M_ETS_PI_80 = subnational_double_gap_score_F_ETS_PI_80 = array(NA, dim = c(47, 15, 3), 
                                                                                                dimnames = list(1:47, 1:15, c("ECP", "CPD", "score")))
for(iwj in 1:47)
{
    for(iwk in 1:15)
    {
        dum = double_gap_fore_fun_conformal(prefecture_index = iwj,
                                            national_F_CDF = Japan_female_pop_CDF,
                                            PDF_M_holdout = male_prefecture_dx[[iwj]], 
                                            PDF_F_holdout = female_prefecture_dx[[iwj]],
                                            horizon = iwk, fore_method = "ets", length_test_data = 15,
                                            alpha = 0.2)
        subnational_double_gap_score_M_ETS_PI_80[iwj,iwk,] = dum$int_score_M
        subnational_double_gap_score_F_ETS_PI_80[iwj,iwk,] = dum$int_score_F
        rm(dum); rm(iwk)
    }
    print(iwj); rm(iwj)
}

subnational_double_gap_score_M_ETS_PI_80_mean = round(apply(subnational_double_gap_score_M_ETS_PI_80, c(2, 3), mean), 4) # 
subnational_double_gap_score_F_ETS_PI_80_mean = round(apply(subnational_double_gap_score_F_ETS_PI_80, c(2, 3), mean), 4) # 

# alpha = 0.05

subnational_double_gap_score_M_ETS_PI_95 = subnational_double_gap_score_F_ETS_PI_95 = array(NA, dim = c(47, 15, 3), 
                                                                                            dimnames = list(1:47, 1:15, c("ECP", "CPD", "score")))
for(iwj in 1:47)
{
    for(iwk in 1:15)
    {
        dum = double_gap_fore_fun_conformal(prefecture_index = iwj,
                                            national_F_CDF = Japan_female_pop_CDF,
                                            PDF_M_holdout = male_prefecture_dx[[iwj]], 
                                            PDF_F_holdout = female_prefecture_dx[[iwj]],
                                            horizon = iwk, fore_method = "ets", length_test_data = 15,
                                            alpha = 0.05)
        subnational_double_gap_score_M_ETS_PI_95[iwj,iwk,] = dum$int_score_M
        subnational_double_gap_score_F_ETS_PI_95[iwj,iwk,] = dum$int_score_F
        rm(dum); rm(iwk)
    }
    print(iwj); rm(iwj)
}

subnational_double_gap_score_M_ETS_PI_95_mean = round(apply(subnational_double_gap_score_M_ETS_PI_95, c(2, 3), mean), 4) # 
subnational_double_gap_score_F_ETS_PI_95_mean = round(apply(subnational_double_gap_score_F_ETS_PI_95, c(2, 3), mean), 4) # 

