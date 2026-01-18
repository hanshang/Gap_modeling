########
# plots
########

savefig("Fig_5a", width = 16, height = 10, toplines = 1.5, pointsize = 4, type = "png")
filled.contour(years, 1:47, subnational_region_M_diff_CDF_integral_measure_mat, ylim = c(47, 1), 
               main = "Integral measure of the regional heterogeneity \n Japanese subnational male data (1947-2023)", 
               xlab = "Year", ylab = "Prefecture")
dev.off()

savefig("Fig_5b", width = 16, height = 10, toplines = 1.5, pointsize = 4, type = "png")
filled.contour(years, 1:47, subnational_region_F_diff_CDF_integral_measure_mat, ylim = c(47, 1), 
               main = "Integral measure of the regional heterogeneity \n Japanese subnational female data (1947-2023)", 
               xlab = "Year", ylab = "")
dev.off()

##############################################
# forecasting regional gap for 20-years-ahead
##############################################

## forecast female and male CDF series (take logit transformation)

# ETS

Japan_female_pop_CDF_forecast = rbind(invlogit(forecast(ftsm(fts(ages[1:110], logit(Japan_female_pop_CDF[1:110,]))), h = 20, method = "ets")$mean$y), rep(1, 20))
Japan_male_pop_CDF_forecast   = rbind(invlogit(forecast(ftsm(fts(ages[1:110], logit(Japan_male_pop_CDF[1:110,]))), h = 20, method = "ets")$mean$y), rep(1, 20))

# ARIMA

Japan_female_pop_CDF_forecast_ARIMA = rbind(invlogit(forecast(ftsm(fts(ages[1:110], logit(Japan_female_pop_CDF[1:110,]))), h = 20, method = "arima")$mean$y), rep(1, 20))
Japan_male_pop_CDF_forecast_ARIMA   = rbind(invlogit(forecast(ftsm(fts(ages[1:110], logit(Japan_male_pop_CDF[1:110,]))), h = 20, method = "arima")$mean$y), rep(1, 20))

## take the Fisher Z transformation

# ETS & ARIMA

subnational_region_F_diff_CDF_forecast = subnational_region_M_diff_CDF_forecast = array(NA, dim = c(111, 20, 47))
for(ik in 1:47)
{
    subnational_region_F_diff_CDF_forecast[,,ik] = FisherZInv(forecast(ftsm(fts(ages, FisherZ(subnational_region_F_diff_CDF[[ik]]))), h = 20, method = "ets")$mean$y)
    subnational_region_M_diff_CDF_forecast[,,ik] = FisherZInv(forecast(ftsm(fts(ages, FisherZ(subnational_region_M_diff_CDF[[ik]]))), h = 20, method = "ets")$mean$y)
    print(ik); rm(ik)
}

# ARIMA

subnational_region_F_diff_CDF_forecast_ARIMA = subnational_region_M_diff_CDF_forecast_ARIMA = array(NA, dim = c(111, 20, 47))
for(ik in 1:47)
{
    subnational_region_F_diff_CDF_forecast_ARIMA[,,ik] = FisherZInv(forecast(ftsm(fts(ages, FisherZ(subnational_region_F_diff_CDF[[ik]]))), h = 20, method = "arima")$mean$y)
    subnational_region_M_diff_CDF_forecast_ARIMA[,,ik] = FisherZInv(forecast(ftsm(fts(ages, FisherZ(subnational_region_M_diff_CDF[[ik]]))), h = 20, method = "arima")$mean$y)
    print(ik); rm(ik)
}

## take the inverse Fisher Z transformation

# ETS

subnational_female_pop_CDF_forecast = subnational_male_pop_CDF_forecast = array(NA, dim = c(111, 20, 47))
for(ik in 1:47)
{
    subnational_female_pop_CDF_forecast[,,ik] = subnational_region_F_diff_CDF_forecast[,,ik] + Japan_female_pop_CDF_forecast
    subnational_male_pop_CDF_forecast[,,ik]   = subnational_region_M_diff_CDF_forecast[,,ik] + Japan_male_pop_CDF_forecast
    print(ik); rm(ik)
}
colnames(subnational_female_pop_CDF_forecast) = colnames(subnational_male_pop_CDF_forecast) = 1:20
rownames(subnational_female_pop_CDF_forecast) = rownames(subnational_male_pop_CDF_forecast) = ages

# ARIMA

subnational_female_pop_CDF_forecast_ARIMA = subnational_male_pop_CDF_forecast_ARIMA = array(NA, dim = c(111, 20, 47))
for(ik in 1:47)
{
    subnational_female_pop_CDF_forecast_ARIMA[,,ik] = subnational_region_F_diff_CDF_forecast_ARIMA[,,ik] + Japan_female_pop_CDF_forecast_ARIMA
    subnational_male_pop_CDF_forecast_ARIMA[,,ik]   = subnational_region_M_diff_CDF_forecast_ARIMA[,,ik] + Japan_male_pop_CDF_forecast_ARIMA
    print(ik); rm(ik)
}
colnames(subnational_female_pop_CDF_forecast_ARIMA) = colnames(subnational_male_pop_CDF_forecast_ARIMA) = 1:20
rownames(subnational_female_pop_CDF_forecast_ARIMA) = rownames(subnational_male_pop_CDF_forecast_ARIMA) = ages

########
# plots
########

# ETS

savefig("Fig_7a", width = 12, height = 10, toplines = 0.8, type = "png")
plot(fts(ages, subnational_region_F_diff_CDF[[47]]), xlab = "Age", ylab = "Region gap", 
     main = "20-years-ahead forecasts", col = gray(.7), ylim = c(-0.15, 0.15))
lines(fts(ages, FisherZInv(subnational_region_F_diff_CDF_forecast[,,47])))
dev.off()

savefig("Fig_7b", width = 12, height = 10, toplines = 0.8, type = "png")
plot(fts(ages, subnational_female_pop_CDF_forecast[,,47]), xlab = "Age", 
     ylab = "CDF", main = "Okinawa female and male")  
lines(fts(ages, subnational_male_pop_CDF_forecast[,,47]))
arrows(95, 0.45, 91, 0.55)
text(97, 0.43, "Female")
arrows(74, 0.46, 81, 0.54)
text(74, 0.44, "Male")
dev.off()

# ARIMA

savefig("Fig_7a_ARIMA", width = 12, height = 10, toplines = 0.8, type = "png")
plot(fts(ages, subnational_region_F_diff_CDF[[47]]), xlab = "Age", ylab = "Region gap", 
     main = "20-years-ahead forecasts", col = gray(.7), ylim = c(-0.15, 0.07))
lines(fts(ages, FisherZInv(subnational_region_F_diff_CDF_forecast_ARIMA[,,47])))
dev.off()

savefig("Fig_7b_ARIMA", width = 12, height = 10, toplines = 0.8, type = "png")
plot(fts(ages, subnational_female_pop_CDF_forecast_ARIMA[,,47]), xlab = "Age", 
     ylab = "Cumulative relative life-table death count", main = "Okinawa female and male")  
lines(fts(ages, subnational_male_pop_CDF_forecast_ARIMA[,,47]))
arrows(95, 0.45, 91, 0.55)
text(97, 0.43, "Female")
arrows(74, 0.46, 81, 0.54)
text(74, 0.44, "Male")
dev.off()

###########################################################
# from CDF to PDF for national life-table death count data
###########################################################

# ETS

subnational_female_pop_PDF_forecast = subnational_male_pop_PDF_forecast = array(NA, dim = c(111, 20, 47))
for(ik in 1:47)
{
    for(ij in 1:20)
    {
        subnational_female_pop_PDF_forecast[,ij,ik] = c(subnational_female_pop_CDF_forecast[1,ij,ik], diff(subnational_female_pop_CDF_forecast[,ij,ik])) * 10^5
        subnational_male_pop_PDF_forecast[,ij,ik]   = c(subnational_male_pop_CDF_forecast[1,ij,ik],   diff(subnational_male_pop_CDF_forecast[,ij,ik])) * 10^5
    }
    rm(ik)    
}
rownames(subnational_female_pop_PDF_forecast) = rownames(subnational_male_pop_PDF_forecast) = ages
colnames(subnational_female_pop_PDF_forecast) = colnames(subnational_male_pop_PDF_forecast) = 1:20

savefig("Fig_7c", width = 12, height = 10, toplines = 0.8, type = "png")
plot(fts(ages, t(female_prefecture_dx[[47]] * 10^5)), xlab = "Age", ylab = "Life-table death counts", 
     col = gray(.7), ylim = c(0, 5000), main = "Okinawa female data")
lines(fts(ages, subnational_female_pop_PDF_forecast[,,47]))
dev.off()

savefig("Fig_7d", width = 12, height = 10, toplines = 0.8, type = "png")
plot(fts(ages, t(male_prefecture_dx[[47]] * 10^5)), xlab = "Age", ylab = "Life-table death counts", 
     col = gray(.7), ylim = c(0, 4000), main = "Okinawa male data")
lines(fts(ages, subnational_male_pop_PDF_forecast[,,47]))
dev.off()

# ARIMA

subnational_female_pop_PDF_forecast_ARIMA = subnational_male_pop_PDF_forecast_ARIMA = array(NA, dim = c(111, 20, 47))
for(ik in 1:47)
{
    for(ij in 1:20)
    {
        subnational_female_pop_PDF_forecast_ARIMA[,ij,ik] = c(subnational_female_pop_CDF_forecast_ARIMA[1,ij,ik], diff(subnational_female_pop_CDF_forecast_ARIMA[,ij,ik])) * 10^5
        subnational_male_pop_PDF_forecast_ARIMA[,ij,ik]   = c(subnational_male_pop_CDF_forecast_ARIMA[1,ij,ik],   diff(subnational_male_pop_CDF_forecast_ARIMA[,ij,ik])) * 10^5
    }
    rm(ik)    
}
rownames(subnational_female_pop_PDF_forecast_ARIMA) = rownames(subnational_male_pop_PDF_forecast_ARIMA) = ages
colnames(subnational_female_pop_PDF_forecast_ARIMA) = colnames(subnational_male_pop_PDF_forecast_ARIMA) = 1:20

savefig("Fig_7c_ARIMA", width = 12, height = 10, toplines = 0.8, type = "png")
plot(fts(ages, t(female_prefecture_dx[[47]] * 10^5)), xlab = "Age", ylab = "Life-table death counts", 
     col = gray(.7), ylim = c(0, 5000), main = "Okinawa female data")
lines(fts(ages, subnational_female_pop_PDF_forecast_ARIMA[,,47]))
dev.off()

savefig("Fig_7d_ARIMA", width = 12, height = 10, toplines = 0.8, type = "png")
plot(fts(ages, t(male_prefecture_dx[[47]] * 10^5)), xlab = "Age", ylab = "", 
     col = gray(.7), ylim = c(0, 4000), main = "Okinawa male data")
lines(fts(ages, subnational_male_pop_PDF_forecast_ARIMA[,,47]))
dev.off()

