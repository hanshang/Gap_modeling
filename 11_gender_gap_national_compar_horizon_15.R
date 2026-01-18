###################
# F & M (national)
###################

## point forecasts

# ARIMA

national_gender_gap_KLD_JSD_M_ARIMA = national_gender_gap_KLD_JSD_F_ARIMA = matrix(NA, 15, 2)
for(iwk in 1:15)
{
    dum = gender_gap_fore_fun(CDF_M = Japan_male_pop_CDF, CDF_F = Japan_female_pop_CDF, 
                              PDF_M_holdout = Japan_male_pop, PDF_F_holdout = Japan_female_pop, 
                              horizon = iwk, fore_method = "arima", length_test_data = 15)
    national_gender_gap_KLD_JSD_M_ARIMA[iwk,] = dum$err_M
    national_gender_gap_KLD_JSD_F_ARIMA[iwk,] = dum$err_F
    print(iwk); rm(iwk); rm(dum)
}

# ETS

national_gender_gap_KLD_JSD_M_ETS = national_gender_gap_KLD_JSD_F_ETS = matrix(NA, 15, 2)
for(iwk in 1:15)
{
    dum = gender_gap_fore_fun(CDF_M = Japan_male_pop_CDF, CDF_F = Japan_female_pop_CDF, 
                              PDF_M_holdout = Japan_male_pop,   PDF_F_holdout = Japan_female_pop, 
                              horizon = iwk, fore_method = "ets", length_test_data = 15)
    national_gender_gap_KLD_JSD_M_ETS[iwk,] = dum$err_M
    national_gender_gap_KLD_JSD_F_ETS[iwk,] = dum$err_F
    print(iwk); rm(iwk); rm(dum)
}
rownames(national_gender_gap_KLD_JSD_M_ARIMA) = rownames(national_gender_gap_KLD_JSD_F_ARIMA) = 
rownames(national_gender_gap_KLD_JSD_M_ETS)   = rownames(national_gender_gap_KLD_JSD_F_ETS)   = 1:15

colnames(national_gender_gap_KLD_JSD_M_ARIMA) = colnames(national_gender_gap_KLD_JSD_F_ARIMA) = 
colnames(national_gender_gap_KLD_JSD_M_ETS)   = colnames(national_gender_gap_KLD_JSD_F_ETS)   = c("KLD", "JSD")

national_gender_gap_KLD_JSD_all = cbind(national_gender_gap_KLD_JSD_F_ARIMA, national_gender_gap_KLD_JSD_M_ARIMA, 
                                        national_gender_gap_KLD_JSD_F_ETS,   national_gender_gap_KLD_JSD_M_ETS)
colnames(national_gender_gap_KLD_JSD_all) = c("KLD (F + ARIMA)", "JSD (F + ARIMA)", "KLD (M + ARIMA)", "JSD (M + ARIMA)",
                                              "KLD (F + ETS)", "JSD (F + ETS)", "KLD (M + ETS)", "JSD (M + ETS)")

##############
# subnational
##############

## point forecasts

# ARIMA

subnational_gender_gap_KLD_JSD_M_ARIMA = subnational_gender_gap_KLD_JSD_F_ARIMA = array(NA, dim = c(47, 15, 2), 
                                                                  dimnames = list(1:47, 1:15, c("KLD", "JSD")))
for(iwj in 1:47)
{
    for(iwk in 1:15)
    {
        dum = gender_gap_fore_fun(CDF_M = subnational_male_prefecture_dx_CDF[[iwj]], 
                                  CDF_F = subnational_female_prefecture_dx_CDF[[iwj]],
                                  PDF_M_holdout = male_prefecture_dx[[iwj]], 
                                  PDF_F_holdout = female_prefecture_dx[[iwj]],
                                  horizon = iwk, fore_method = "arima", length_test_data = 15)
        subnational_gender_gap_KLD_JSD_M_ARIMA[iwj,iwk,] = dum$err_M
        subnational_gender_gap_KLD_JSD_F_ARIMA[iwj,iwk,] = dum$err_F
        rm(dum)
    }
    print(iwj); rm(iwj)
}
subnational_gender_gap_KLD_JSD_M_ARIMA_mean = apply(subnational_gender_gap_KLD_JSD_M_ARIMA, c(2, 3), mean)
subnational_gender_gap_KLD_JSD_F_ARIMA_mean = apply(subnational_gender_gap_KLD_JSD_F_ARIMA, c(2, 3), mean)

# ETS

subnational_gender_gap_KLD_JSD_M_ETS = subnational_gender_gap_KLD_JSD_F_ETS = array(NA, dim = c(47, 15, 2), 
                                                                      dimnames = list(1:47, 1:15, c("KLD", "JSD")))
for(iwj in 1:47)
{
    for(iwk in 1:15)
    {
        dum = gender_gap_fore_fun(CDF_M = subnational_male_prefecture_dx_CDF[[iwj]], 
                                  CDF_F = subnational_female_prefecture_dx_CDF[[iwj]],
                                  PDF_M_holdout = male_prefecture_dx[[iwj]], 
                                  PDF_F_holdout = female_prefecture_dx[[iwj]],
                                  horizon = iwk, fore_method = "ets", length_test_data = 15)
        subnational_gender_gap_KLD_JSD_M_ETS[iwj,iwk,] = dum$err_M
        subnational_gender_gap_KLD_JSD_F_ETS[iwj,iwk,] = dum$err_F
        rm(dum)
    }
    print(iwj); rm(iwj)
}
subnational_gender_gap_KLD_JSD_M_ETS_mean = apply(subnational_gender_gap_KLD_JSD_M_ETS, c(2, 3), mean)
subnational_gender_gap_KLD_JSD_F_ETS_mean = apply(subnational_gender_gap_KLD_JSD_F_ETS, c(2, 3), mean)

##########
# summary
##########

## point forecasts

# mean

subnational_gender_fore_mean_horizon = rbind(colMeans(subnational_gender_gap_KLD_JSD_F_ARIMA_PI_80_mean),
                                             colMeans(subnational_gender_gap_KLD_JSD_M_ARIMA_PI_80_mean),
                                             
                                             colMeans(subnational_gender_gap_KLD_JSD_F_ETS_PI_80_mean),
                                             colMeans(subnational_gender_gap_KLD_JSD_M_ETS_PI_80_mean))

# median

subnational_gender_fore_median_horizon = rbind(apply(subnational_gender_gap_KLD_JSD_F_ARIMA_mean, 2, median),
                                               apply(subnational_gender_gap_KLD_JSD_M_ARIMA_mean, 2, median),
                                               
                                               apply(subnational_gender_gap_KLD_JSD_F_ETS_mean, 2, median),
                                               apply(subnational_gender_gap_KLD_JSD_M_ETS_mean, 2, median))

rownames(subnational_gender_fore_mean_horizon) =
rownames(subnational_gender_fore_median_horizon) = c("F + ARIMA", "M + ARIMA", "F + ETS", "M + ETS")

