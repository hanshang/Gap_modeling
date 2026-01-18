### interval forecasts

## ARIMA

# alpha = 0.2

national_gender_gap_score_M_ARIMA_PI_80 = national_gender_gap_score_F_ARIMA_PI_80 = matrix(NA, 15, 3)
for(iwk in 1:15)
{
    dum = gender_gap_fore_fun_conformal(CDF_M = Japan_male_pop_CDF, CDF_F = Japan_female_pop_CDF, 
                                        PDF_M_holdout = Japan_male_pop,   PDF_F_holdout = Japan_female_pop, 
                                        horizon = iwk, fore_method = "arima", length_test_data = 15, 
                                        alpha = 0.2)
    national_gender_gap_score_M_ARIMA_PI_80[iwk,] = dum$int_score_M
    national_gender_gap_score_F_ARIMA_PI_80[iwk,] = dum$int_score_F
    print(iwk); rm(iwk); rm(dum)
}
rownames(national_gender_gap_score_M_ARIMA_PI_80) = rownames(national_gender_gap_score_F_ARIMA_PI_80) = 1:15
colnames(national_gender_gap_score_M_ARIMA_PI_80) = colnames(national_gender_gap_score_F_ARIMA_PI_80) = c("ECP", "CPD", "score")

national_gender_gap_score_M_ARIMA_PI_80_mean = round(colMeans(national_gender_gap_score_M_ARIMA_PI_80), 4)
national_gender_gap_score_F_ARIMA_PI_80_mean = round(colMeans(national_gender_gap_score_F_ARIMA_PI_80), 4)

# alpha = 0.05

national_gender_gap_score_M_ARIMA_PI_95 = national_gender_gap_score_F_ARIMA_PI_95 = matrix(NA, 15, 3)
for(iwk in 1:15)
{
    dum = gender_gap_fore_fun_conformal(CDF_M = Japan_male_pop_CDF, CDF_F = Japan_female_pop_CDF, 
                                        PDF_M_holdout = Japan_male_pop,   PDF_F_holdout = Japan_female_pop, 
                                        horizon = iwk, fore_method = "arima", length_test_data = 15, 
                                        alpha = 0.05)
    national_gender_gap_score_M_ARIMA_PI_95[iwk,] = dum$int_score_M
    national_gender_gap_score_F_ARIMA_PI_95[iwk,] = dum$int_score_F
    print(iwk); rm(iwk); rm(dum)
}
rownames(national_gender_gap_score_M_ARIMA_PI_95) = rownames(national_gender_gap_score_F_ARIMA_PI_95) = 1:15
colnames(national_gender_gap_score_M_ARIMA_PI_95) = colnames(national_gender_gap_score_F_ARIMA_PI_95) = c("ECP", "CPD", "score")

national_gender_gap_score_M_ARIMA_PI_95_mean = round(colMeans(national_gender_gap_score_M_ARIMA_PI_95), 4)
national_gender_gap_score_F_ARIMA_PI_95_mean = round(colMeans(national_gender_gap_score_F_ARIMA_PI_95), 4)

## ETS

# alpha = 0.2

national_gender_gap_score_M_ETS_PI_80 = national_gender_gap_score_F_ETS_PI_80 = matrix(NA, 15, 3)
for(iwk in 1:15)
{
    dum = gender_gap_fore_fun_conformal(CDF_M = Japan_male_pop_CDF, CDF_F = Japan_female_pop_CDF, 
                                        PDF_M_holdout = Japan_male_pop,   PDF_F_holdout = Japan_female_pop, 
                                        horizon = iwk, fore_method = "ets", length_test_data = 15, 
                                        alpha = 0.2)
    national_gender_gap_score_M_ETS_PI_80[iwk,] = dum$int_score_M
    national_gender_gap_score_F_ETS_PI_80[iwk,] = dum$int_score_F
    print(iwk); rm(iwk); rm(dum)
}

national_gender_gap_score_M_ETS_PI_80_mean = round(colMeans(national_gender_gap_score_M_ETS_PI_80), 4)
national_gender_gap_score_F_ETS_PI_80_mean = round(colMeans(national_gender_gap_score_F_ETS_PI_80), 4)

# alpha = 0.05

national_gender_gap_score_M_ETS_PI_95 = national_gender_gap_score_F_ETS_PI_95 = matrix(NA, 15, 3)
for(iwk in 1:15)
{
    dum = gender_gap_fore_fun_conformal(CDF_M = Japan_male_pop_CDF, CDF_F = Japan_female_pop_CDF, 
                                        PDF_M_holdout = Japan_male_pop,   PDF_F_holdout = Japan_female_pop, 
                                        horizon = iwk, fore_method = "ets", length_test_data = 15, 
                                        alpha = 0.05)
    national_gender_gap_score_M_ETS_PI_95[iwk,] = dum$int_score_M
    national_gender_gap_score_F_ETS_PI_95[iwk,] = dum$int_score_F
    print(iwk); rm(iwk); rm(dum)
}
rownames(national_gender_gap_score_M_ETS_PI_95) = rownames(national_gender_gap_score_F_ETS_PI_95) = 1:15
colnames(national_gender_gap_score_M_ETS_PI_95) = colnames(national_gender_gap_score_F_ETS_PI_95) = c("ECP", "CPD", "score")

national_gender_gap_score_M_ETS_PI_95_mean = round(colMeans(national_gender_gap_score_M_ETS_PI_95), 4)
national_gender_gap_score_F_ETS_PI_95_mean = round(colMeans(national_gender_gap_score_F_ETS_PI_95), 4)

##############
# subnational
##############

### interval forecasts

## ARIMA

# alpha = 0.2

subnational_gender_gap_score_M_ARIMA_PI_80 = subnational_gender_gap_score_F_ARIMA_PI_80 = array(NA, dim = c(47, 15, 3), 
                                                                  dimnames = list(1:47, 1:15, c("ECP", "CPD", "score")))
for(iwj in 1:47)
{
    for(iwk in 1:15)
    {
        dum = gender_gap_fore_fun_conformal(CDF_M = subnational_male_prefecture_dx_CDF[[iwj]], 
                                            CDF_F = subnational_female_prefecture_dx_CDF[[iwj]],
                                            PDF_M_holdout = male_prefecture_dx[[iwj]], 
                                            PDF_F_holdout = female_prefecture_dx[[iwj]],
                                            horizon = iwk, fore_method = "arima", length_test_data = 15,
                                            alpha = 0.2)
        subnational_gender_gap_score_M_ARIMA_PI_80[iwj,iwk,] = dum$int_score_M
        subnational_gender_gap_score_F_ARIMA_PI_80[iwj,iwk,] = dum$int_score_F
        rm(dum)
    }
    print(iwj); rm(iwj)
}
subnational_gender_gap_score_M_ARIMA_PI_80_mean = apply(subnational_gender_gap_score_M_ARIMA_PI_80, c(2, 3), mean)
subnational_gender_gap_score_F_ARIMA_PI_80_mean = apply(subnational_gender_gap_score_F_ARIMA_PI_80, c(2, 3), mean)

# alpha = 0.05

subnational_gender_gap_score_M_ARIMA_PI_95 = subnational_gender_gap_score_F_ARIMA_PI_95 = array(NA, dim = c(47, 15, 3), 
                                                                    dimnames = list(1:47, 1:15, c("ECP", "CPD", "score")))
for(iwj in 1:47)
{
    for(iwk in 1:15)
    {
        dum = gender_gap_fore_fun_conformal(CDF_M = subnational_male_prefecture_dx_CDF[[iwj]], 
                                            CDF_F = subnational_female_prefecture_dx_CDF[[iwj]],
                                            PDF_M_holdout = male_prefecture_dx[[iwj]], 
                                            PDF_F_holdout = female_prefecture_dx[[iwj]],
                                            horizon = iwk, fore_method = "arima", length_test_data = 15,
                                            alpha = 0.05)
        subnational_gender_gap_score_M_ARIMA_PI_95[iwj,iwk,] = dum$int_score_M
        subnational_gender_gap_score_F_ARIMA_PI_95[iwj,iwk,] = dum$int_score_F
        rm(dum)
    }
    print(iwj); rm(iwj)
}
subnational_gender_gap_score_M_ARIMA_PI_95_mean = apply(subnational_gender_gap_score_M_ARIMA_PI_95, c(2, 3), mean)
subnational_gender_gap_score_F_ARIMA_PI_95_mean = apply(subnational_gender_gap_score_F_ARIMA_PI_95, c(2, 3), mean)

## ETS

# alpha = 0.2

subnational_gender_gap_score_M_ETS_PI_80 = subnational_gender_gap_score_F_ETS_PI_80 = array(NA, dim = c(47, 15, 3), 
                                                                                                dimnames = list(1:47, 1:15, c("ECP", "CPD", "score")))
for(iwj in 1:47)
{
    for(iwk in 1:15)
    {
        dum = gender_gap_fore_fun_conformal(CDF_M = subnational_male_prefecture_dx_CDF[[iwj]], 
                                            CDF_F = subnational_female_prefecture_dx_CDF[[iwj]],
                                            PDF_M_holdout = male_prefecture_dx[[iwj]], 
                                            PDF_F_holdout = female_prefecture_dx[[iwj]],
                                            horizon = iwk, fore_method = "ets", length_test_data = 15,
                                            alpha = 0.2)
        subnational_gender_gap_score_M_ETS_PI_80[iwj,iwk,] = dum$int_score_M
        subnational_gender_gap_score_F_ETS_PI_80[iwj,iwk,] = dum$int_score_F
        rm(dum); rm(iwk)
    }
    print(iwj); rm(iwj)
}
subnational_gender_gap_score_M_ETS_PI_80_mean = apply(subnational_gender_gap_score_M_ETS_PI_80, c(2, 3), mean)
subnational_gender_gap_score_F_ETS_PI_80_mean = apply(subnational_gender_gap_score_F_ETS_PI_80, c(2, 3), mean)

# alpha = 0.05

subnational_gender_gap_score_M_ETS_PI_95 = subnational_gender_gap_score_F_ETS_PI_95 = array(NA, dim = c(47, 15, 3), 
                                                                                                dimnames = list(1:47, 1:15, c("ECP", "CPD", "score")))
for(iwj in 1:47)
{
    for(iwk in 1:15)
    {
        dum = gender_gap_fore_fun_conformal(CDF_M = subnational_male_prefecture_dx_CDF[[iwj]], 
                                            CDF_F = subnational_female_prefecture_dx_CDF[[iwj]],
                                            PDF_M_holdout = male_prefecture_dx[[iwj]], 
                                            PDF_F_holdout = female_prefecture_dx[[iwj]],
                                            horizon = iwk, fore_method = "ets", length_test_data = 15,
                                            alpha = 0.05)
        subnational_gender_gap_score_M_ETS_PI_95[iwj,iwk,] = dum$int_score_M
        subnational_gender_gap_score_F_ETS_PI_95[iwj,iwk,] = dum$int_score_F
        rm(dum); rm(iwk)
    }
    print(iwj); rm(iwj)
}
subnational_gender_gap_score_M_ETS_PI_95_mean = apply(subnational_gender_gap_score_M_ETS_PI_95, c(2, 3), mean)
subnational_gender_gap_score_F_ETS_PI_95_mean = apply(subnational_gender_gap_score_F_ETS_PI_95, c(2, 3), mean)

##########
# summary
##########

## interval forecasts

# alpha = 0.2

subnational_gender_fore_mean_horizon_int_0.2 = rbind(colMeans(subnational_gender_gap_score_F_ARIMA_PI_80_mean),
                                                     colMeans(subnational_gender_gap_score_M_ARIMA_PI_80_mean),
                                                    
                                                     colMeans(subnational_gender_gap_score_F_ETS_PI_80_mean),
                                                     colMeans(subnational_gender_gap_score_M_ETS_PI_80_mean))

# alpha = 0.05

subnational_gender_fore_mean_horizon_int_0.05 = rbind(colMeans(subnational_gender_gap_score_F_ARIMA_PI_95_mean),
                                                      colMeans(subnational_gender_gap_score_M_ARIMA_PI_95_mean),
                                                      
                                                      colMeans(subnational_gender_gap_score_F_ETS_PI_95_mean),
                                                      colMeans(subnational_gender_gap_score_M_ETS_PI_95_mean))

rownames(subnational_gender_fore_mean_horizon_int_0.2) = 
rownames(subnational_gender_fore_mean_horizon_int_0.05) = c("F + ARIMA", "M + ARIMA", "F + ETS", "M + ETS")

