####################
# compute forecasts
####################

# ARIMA

subnational_region_gap_fore_F = subnational_region_gap_fore_M = list()
for(iwj in 1:15)
{
    dum = region_gap_fore_fun(s_M_CDF = subnational_male_prefecture_dx_CDF, 
                              s_F_CDF = subnational_female_prefecture_dx_CDF,
                              N_M_CDF = Japan_male_pop_CDF, 
                              N_F_CDF = Japan_female_pop_CDF, 
                              fh = iwj, fmethod = "arima", length_test_data = 15)
    subnational_region_gap_fore_F[[iwj]] = dum$female_fore
    subnational_region_gap_fore_M[[iwj]] = dum$male_fore
    print(iwj); rm(iwj)
}

# ETS

subnational_region_gap_fore_F_ETS = subnational_region_gap_fore_M_ETS = list()
for(iwj in 1:15)
{
    dum = region_gap_fore_fun(s_M_CDF = subnational_male_prefecture_dx_CDF, 
                              s_F_CDF = subnational_female_prefecture_dx_CDF,
                              N_M_CDF = Japan_male_pop_CDF, 
                              N_F_CDF = Japan_female_pop_CDF, 
                              fh = iwj, fmethod = "ets", length_test_data = 15)
    subnational_region_gap_fore_F_ETS[[iwj]] = dum$female_fore
    subnational_region_gap_fore_M_ETS[[iwj]] = dum$male_fore
    print(iwj); rm(iwj)
}

############# 
# subnational
############# 

# ARIMA

subnational_gap_fore_male = subnational_gap_fore_female = array(NA, dim = c(47, 15, 2))
for(iwj in 1:47)
{
    for(iwk in 1:15)
    {
        dum = region_gap_fore_fun_fore(forecast_M = matrix(subnational_region_gap_fore_M[[iwk]][,,iwj], 111, ),
                                       forecast_F = matrix(subnational_region_gap_fore_F[[iwk]][,,iwj], 111, ),
                                       PDF_M_holdout = male_prefecture_dx[[iwj]], 
                                       PDF_F_holdout = female_prefecture_dx[[iwj]], 
                                       horizon = iwk, length_test_data = 15)
        subnational_gap_fore_male[iwj,iwk,] = dum$err_M
        subnational_gap_fore_female[iwj,iwk,] = dum$err_F
        rm(iwk); rm(dum)
    }
    print(iwj); rm(iwj)
}

subnational_gap_fore_male_mean = apply(subnational_gap_fore_male, c(2, 3), mean)
subnational_gap_fore_female_mean = apply(subnational_gap_fore_female, c(2, 3), mean)

rownames(subnational_gap_fore_male_mean) = rownames(subnational_gap_fore_female_mean) = 1:15
colnames(subnational_gap_fore_male_mean) = colnames(subnational_gap_fore_female_mean) = c("KLD", "JSD")

# ETS

subnational_gap_fore_male_ETS = subnational_gap_fore_female_ETS = array(NA, dim = c(47, 15, 2))
for(iwj in 1:47)
{
    for(iwk in 1:15)
    {
        dum = region_gap_fore_fun_fore(forecast_M = matrix(subnational_region_gap_fore_M_ETS[[iwk]][,,iwj], 111, ),
                                       forecast_F = matrix(subnational_region_gap_fore_F_ETS[[iwk]][,,iwj], 111, ),
                                       PDF_M_holdout = male_prefecture_dx[[iwj]], 
                                       PDF_F_holdout = female_prefecture_dx[[iwj]], 
                                       horizon = iwk, length_test_data = 15)
        subnational_gap_fore_male_ETS[iwj,iwk,] = dum$err_M
        subnational_gap_fore_female_ETS[iwj,iwk,] = dum$err_F
        rm(iwk); rm(dum)
    }
    print(iwj); rm(iwj)
}

subnational_gap_fore_male_ETS_mean = apply(subnational_gap_fore_male_ETS, c(2, 3), mean)
subnational_gap_fore_female_ETS_mean = apply(subnational_gap_fore_female_ETS, c(2, 3), mean)

rownames(subnational_gap_fore_male_ETS_mean) = rownames(subnational_gap_fore_female_ETS_mean) = 1:15
colnames(subnational_gap_fore_male_ETS_mean) = colnames(subnational_gap_fore_female_ETS_mean) = c("KLD", "JSD")

