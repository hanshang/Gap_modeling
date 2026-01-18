# fmethod = "ARIMA"

subnational_double_gap_fore_male_arima = subnational_double_gap_fore_female_arima = list()
for(iwj in 1:15)
{
    dum = double_gap_fore_fun(N_F_CDF = Japan_female_pop_CDF, 
                              subnational_female_region_gap = subnational_region_F_diff_CDF,
                              subnational_gender_gap = subnational_gender_diff_CDF, 
                              fh = iwj, fmethod = "arima", length_test_data = 15)
    subnational_double_gap_fore_male_arima[[iwj]] = dum$male_fore
    subnational_double_gap_fore_female_arima[[iwj]] = dum$female_fore
    print(iwj); rm(iwj); rm(dum)
}

# fmethod = "ETS"

subnational_double_gap_fore_male_ets = subnational_double_gap_fore_female_ets = list()
for(iwj in 1:15)
{
    dum = double_gap_fore_fun(N_F_CDF = Japan_female_pop_CDF, 
                              subnational_female_region_gap = subnational_region_F_diff_CDF,
                              subnational_gender_gap = subnational_gender_diff_CDF, 
                              fh = iwj, fmethod = "ets", length_test_data = 15)
    subnational_double_gap_fore_male_ets[[iwj]] = dum$male_fore
    subnational_double_gap_fore_female_ets[[iwj]] = dum$female_fore
    print(iwj); rm(iwj); rm(dum)
}

##########################
## evaluation of accuracy
##########################

# ARIMA

double_gap_err_M = double_gap_err_F = array(NA, dim = c(15, 47, 2), dimnames = list(1:15, 1:47, c("KLD", "JSD")))
for(iwj in 1:15)
{
    dum = eval_double_gap_fun(PDF_M_fore = subnational_double_gap_fore_male_arima, 
                              PDF_F_fore = subnational_double_gap_fore_female_arima,
                              PDF_M_holdout = male_prefecture_dx, 
                              PDF_F_holdout = female_prefecture_dx, 
                              horizon = iwj, length_test_data = 15)
    double_gap_err_M[iwj,,] = dum$err_M
    double_gap_err_F[iwj,,] = dum$err_F
    print(iwj); rm(iwj); rm(dum)
}

# ETS

double_gap_err_M_ets = double_gap_err_F_ets = array(NA, dim = c(15, 47, 2), dimnames = list(1:15, 1:47, c("KLD", "JSD")))
for(iwj in 1:15)
{
    dum = eval_double_gap_fun(PDF_M_fore = subnational_double_gap_fore_male_ets, 
                              PDF_F_fore = subnational_double_gap_fore_female_ets,
                              PDF_M_holdout = male_prefecture_dx, 
                              PDF_F_holdout = female_prefecture_dx, 
                              horizon = iwj, length_test_data = 15)
    double_gap_err_M_ets[iwj,,] = dum$err_M
    double_gap_err_F_ets[iwj,,] = dum$err_F
    print(iwj); rm(iwj); rm(dum)
}

double_gap_err_F_mean = apply(double_gap_err_F, c(1, 3), mean)
double_gap_err_M_mean = apply(double_gap_err_M, c(1, 3), mean)

double_gap_err_F_ets_mean = apply(double_gap_err_F_ets, c(1, 3), mean)
double_gap_err_M_ets_mean = apply(double_gap_err_M_ets, c(1, 3), mean)

rownames(double_gap_err_F_mean) = rownames(double_gap_err_M_mean) = 
rownames(double_gap_err_F_ets_mean) = rownames(double_gap_err_M_ets_mean) = 1:15
colnames(double_gap_err_F_mean) = colnames(double_gap_err_M_mean) = 
colnames(double_gap_err_F_ets_mean) = colnames(double_gap_err_M_ets_mean) = c("KLD", "JSD")

