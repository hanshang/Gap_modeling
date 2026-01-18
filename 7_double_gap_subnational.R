#########################
# modeling the region gap
#########################

# forecast female national data
# to forecast CDF of a series, take logit transformation with the last row (age), inverse logit forecasts back

Japan_female_pop_CDF_forecast <- rbind(invlogit(forecast(ftsm(fts(ages[1:110], logit(Japan_female_pop_CDF[1:110,]))), h = 20, method = "ets")$mean$y), rep(1, 20))
Japan_female_pop_CDF_forecast_ARIMA <- rbind(invlogit(forecast(ftsm(fts(ages[1:110], logit(Japan_female_pop_CDF[1:110,]))), h = 20, method = "arima")$mean$y), rep(1, 20))

# ETS

savefig("Fig_8a", width = 12, height = 10, toplines = 0.8, type = "png")
plot(fts(ages, Japan_female_pop_CDF), xlab = "Age", ylab = "CDF", 
     main = "Japanese national females (2024-2043)", col = gray(.7))
lines(fts(ages, Japan_female_pop_CDF_forecast))
dev.off()

# ARIMA

savefig("Fig_8a_ARIMA", width = 12, height = 10, toplines = 0.8, type = "png")
plot(fts(ages, Japan_female_pop_CDF), xlab = "Age", ylab = "Cumulative relative life-table death count", 
     main = "Japanese national females (2024-2043)", col = gray(.7))
lines(fts(ages, Japan_female_pop_CDF_forecast_ARIMA))
dev.off()

##########################################
## forecast region gap for 20-years-ahead
##########################################

# ETS

subnational_region_F_diff_CDF_forecast = array(NA, dim = c(111, 20, 47), dimnames = list(ages, 1:20, state)) 
for(ik in 1:47)
{
    # take Fisher Z transformation
  
    fts_object <- fts(ages, FisherZ(subnational_region_F_diff_CDF[[ik]]))
    
    # take inverse Fisher Z transformation
    
    subnational_region_F_diff_CDF_forecast[,,ik] = FisherZInv(forecast(ftsm(fts_object), h = 20, method = "ets")$mean$y)
    print(ik); rm(ik)
}

# ARIMA

subnational_region_F_diff_CDF_forecast_ARIMA = array(NA, dim = c(111, 20, 47), dimnames = list(ages, 1:20, state)) 
for(ik in 1:47)
{
    # take Fisher Z transformation
    
    fts_object <- fts(ages, FisherZ(subnational_region_F_diff_CDF[[ik]]))
    
    # take inverse Fisher Z transformation
    
    subnational_region_F_diff_CDF_forecast_ARIMA[,,ik] = FisherZInv(forecast(ftsm(fts_object), h = 20, method = "arima")$mean$y)
    print(ik); rm(ik)
}

#####################################
## forecast subnational female CDFs
#####################################

# ETS

subnational_F_CDF_forecast = array(NA, dim = c(111, 20, 47))
for(ik in 1:47)
{
    subnational_F_CDF_forecast[,,ik] = subnational_region_F_diff_CDF_forecast[,,ik] + Japan_female_pop_CDF_forecast
    rm(ik)
}

savefig("Fig_8b", width = 12, height = 10, toplines = 0.8, type = "png")
plot(fts(ages, subnational_female_prefecture_dx_CDF[[47]]), xlab = "Age", ylab = "CDF", 
         main = "Okinawa females (2024-2043)", col = gray(.7))
lines(fts(ages, subnational_F_CDF_forecast[,,47]))
dev.off()

# ARIMA

subnational_F_CDF_forecast_ARIMA = array(NA, dim = c(111, 20, 47))
for(ik in 1:47)
{
    subnational_F_CDF_forecast_ARIMA[,,ik] = subnational_region_F_diff_CDF_forecast_ARIMA[,,ik] + Japan_female_pop_CDF_forecast_ARIMA
    rm(ik)
}

savefig("Fig_8b_ARIMA", width = 12, height = 10, toplines = 0.8, type = "png")
plot(fts(ages, subnational_female_prefecture_dx_CDF[[47]]), xlab = "Age", ylab = "", 
     main = "Okinawa females (2024-2043)", col = gray(.7))
lines(fts(ages, subnational_F_CDF_forecast_ARIMA[,,47]))
dev.off()

##########################################
## forecast gender gap for 20-years-ahead
##########################################

# ETS

subnational_gender_diff_CDF_forecast = array(NA, dim = c(111, 20, 47), dimnames = list(ages, 1:20, state))
for(ik in 1:length(subnational_gender_diff_CDF))
{
    # take Fisher Z transformation
  
    fts_object <- fts(ages, FisherZ(subnational_gender_diff_CDF[[ik]]))
    
    # take inverse Fisher Z transformation
    
    subnational_gender_diff_CDF_forecast[,,ik] = FisherZInv(forecast(ftsm(fts_object), h = 20, method = "ets")$mean$y)
    print(ik); rm(ik)
}

savefig("Fig_8c", width = 12, height = 10, toplines = 0.8, type = "png")
plot(fts(ages, subnational_gender_diff_CDF[[47]]), xlab = "Age", ylab = "Gender gap", 
         main = "20-years-ahead forecasts (Okinawa)", col = gray(.7))
lines(fts(ages, subnational_gender_diff_CDF_forecast[,,47]))
dev.off()

# ARIMA

subnational_gender_diff_CDF_forecast_ARIMA = array(NA, dim = c(111, 20, 47), dimnames = list(ages, 1:20, state))
for(ik in 1:length(subnational_gender_diff_CDF))
{
    # take Fisher Z transformation
    
    fts_object <- fts(ages, FisherZ(subnational_gender_diff_CDF[[ik]]))
    
    # take inverse Fisher Z transformation
    
    subnational_gender_diff_CDF_forecast_ARIMA[,,ik] = FisherZInv(forecast(ftsm(fts_object), h = 20, method = "arima")$mean$y)
    print(ik); rm(ik)
}

savefig("Fig_8c_ARIMA", width = 12, height = 10, toplines = 0.8, type = "png")
plot(fts(ages, subnational_gender_diff_CDF[[47]]), xlab = "Age", ylab = "Gender gap", 
     main = "20-years-ahead forecasts (Okinawa)", col = gray(.7))
lines(fts(ages, subnational_gender_diff_CDF_forecast_ARIMA[,,47]))
dev.off()

#########################################################################
## forecast subnational male CDFs = gender gap + subnational female CDFs
#########################################################################

# ETS

subnational_M_CDF_forecast = array(NA, dim = c(111, 20, 47), dimnames = list(ages, 1:20, state))
for(ik in 1:47)
{
    subnational_M_CDF_forecast[,,ik] = subnational_gender_diff_CDF_forecast[,,ik] + subnational_F_CDF_forecast[,,ik]
    print(ik); rm(ik)
}

savefig("Fig_8d", width = 12, height = 10, toplines = 0.8, type = "png")
plot(fts(ages, subnational_male_prefecture_dx_CDF[[47]]), xlab = "Age", ylab = "CDF", 
     main = "20-years-ahead forecasts of Okinawa males", col = gray(.7))
lines(fts(ages, subnational_M_CDF_forecast[,,47]))
dev.off()

# ARIMA

subnational_M_CDF_forecast_ARIMA = array(NA, dim = c(111, 20, 47), dimnames = list(ages, 1:20, state))
for(ik in 1:47)
{
    subnational_M_CDF_forecast_ARIMA[,,ik] = subnational_gender_diff_CDF_forecast_ARIMA[,,ik] + subnational_F_CDF_forecast_ARIMA[,,ik]
    print(ik); rm(ik)
}

savefig("Fig_8d_ARIMA", width = 12, height = 10, toplines = 0.8, type = "png")
plot(fts(ages, subnational_male_prefecture_dx_CDF[[47]]), xlab = "Age", ylab = "", 
     main = "20-years-ahead forecasts of Okinawa males", col = gray(.7))
lines(fts(ages, subnational_M_CDF_forecast_ARIMA[,,47]))
dev.off()

#####################
## from CDFs to PDFs
#####################

# ETS

double_gap_subnational_female_PDF_forecast = double_gap_subnational_male_PDF_forecast = array(NA, dim = c(111, 20, 47), dimnames = list(ages, 1:20, state))
for(ik in 1:47)
{
    for(ij in 1:20)
    {
        double_gap_subnational_female_PDF_forecast[,ij,ik] = c(subnational_F_CDF_forecast[1,ij,ik], diff(subnational_F_CDF_forecast[,ij,ik])) * 10^5
        double_gap_subnational_male_PDF_forecast[,ij,ik]   = c(subnational_M_CDF_forecast[1,ij,ik], diff(subnational_M_CDF_forecast[,ij,ik])) * 10^5
        rm(ij)
    }
    rm(ik)
}

savefig("Fig_8e", width = 12, height = 10, toplines = 0.8, type = "png")
plot(fts(ages, t(female_prefecture_dx[[47]] * 10^5)),
     ylim = c(0, 5000), xlab = "Age", ylab = "Life-table death counts", col = gray(.7))
lines(fts(ages, double_gap_subnational_female_PDF_forecast[,,47]))
dev.off()

savefig("Fig_8f", width = 12, height = 10, toplines = 0.8, type = "png")
plot(fts(ages, t(male_prefecture_dx[[47]] * 10^5)), xlab = "Age", ylab = "Life-table death counts", 
     col = gray(.7), ylim = c(0, 5000))
lines(fts(ages, double_gap_subnational_male_PDF_forecast[,,47]), xlab = "Age", ylab = "Life-table death counts")
dev.off()

# ARIMA

double_gap_subnational_female_PDF_forecast_ARIMA = double_gap_subnational_male_PDF_forecast_ARIMA = array(NA, dim = c(111, 20, 47), dimnames = list(ages, 1:20, state))
for(ik in 1:47)
{
    for(ij in 1:20)
    {
        double_gap_subnational_female_PDF_forecast_ARIMA[,ij,ik] = c(subnational_F_CDF_forecast_ARIMA[1,ij,ik], diff(subnational_F_CDF_forecast_ARIMA[,ij,ik])) * 10^5
        double_gap_subnational_male_PDF_forecast_ARIMA[,ij,ik]   = c(subnational_M_CDF_forecast_ARIMA[1,ij,ik], diff(subnational_M_CDF_forecast_ARIMA[,ij,ik])) * 10^5
        rm(ij)
    }
    rm(ik)
}

###############
# save figures
###############

savefig("Fig_8e_ARIMA", width = 12, height = 10, toplines = 0.8, type = "png")
plot(fts(ages, t(female_prefecture_dx[[47]] * 10^5)), xlab = "Age", ylab = "Life-table death counts", 
     col = gray(.7), ylim = c(0, 5000))
lines(fts(ages, double_gap_subnational_female_PDF_forecast_ARIMA[,,47]))
dev.off()

savefig("Fig_8f_ARIMA", width = 12, height = 10, toplines = 0.8, type = "png")
plot(fts(ages, t(male_prefecture_dx[[47]] * 10^5)), xlab = "Age", ylab = "Life-table death counts", 
     col = gray(.7), ylim = c(0, 5000))
lines(fts(ages,double_gap_subnational_male_PDF_forecast_ARIMA[,,47]))
dev.off()

