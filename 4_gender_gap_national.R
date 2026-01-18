##############################################
# difference between CDF_M and CDF_F in Japan
##############################################

# national (CDF_M - CDF_F)

Japan_gender_diff_national_CDF = Japan_male_pop_CDF - Japan_female_pop_CDF

# Wasserstein 1 and 2

Japan_gender_wasserstein1d_p1 = Japan_gender_wasserstein1d_p2 = vector("numeric", 77)
for(ik in 1:77)
{
    Japan_gender_wasserstein1d_p1[ik] = wasserstein1d(Japan_male_pop[ik,], Japan_female_pop[ik,], p = 1)
    Japan_gender_wasserstein1d_p2[ik] = wasserstein1d(Japan_male_pop[ik,], Japan_female_pop[ik,], p = 2)
    rm(ik)
}

plot(1947:2023, Japan_gender_wasserstein1d_p1, xlab = "Year", ylab = "Wasserstein distance", 
     ylim = c(0, max(Japan_gender_wasserstein1d_p2)), type = "l")
lines(1947:2023, Japan_gender_wasserstein1d_p2, xlab = "Year", ylab = "Wasserstein distance",
     ylim = c(0, max(Japan_gender_wasserstein1d_p2)), col = 2, lty = 2)
legend("topleft", c("Wasserstein distance of order 1", "Wasserstein distance of order 2"), col = 1:2,
       lty = 1:2)
    
# integral measure

Japan_gender_diff_national_CDF_integral_measure = apply(Japan_gender_diff_national_CDF, 2, sum)

savefig("Fig_3a", width = 12, height = 10, toplines = 0.8, type = "png")
plot(fts(ages, Japan_gender_diff_national_CDF), xlab = "Age", ylab = "Gender differences in CDFs (M - F)")
dev.off()

savefig("Fig_3b", width = 12, height = 10, toplines = 0.8, type = "png")
plot(years, Japan_gender_diff_national_CDF_integral_measure, xlab = "Year", 
     ylab = "Integral measure of differences in CDFs", type = "l")
dev.off()

#################
# model CDFs gap
#################

# Fisher Z transformation of gap (-1,1) to (-infty, infty)

Japan_gender_gap_national_CDF_fisherz = FisherZ(Japan_gender_diff_national_CDF)
plot(fts(ages, Japan_gender_gap_national_CDF_fisherz), xlab = "Age", ylab = "Gender map")

# forecast gender gap via FTS

Japan_gender_gap_national_CDF_fisherz_fts <- fts(ages, Japan_gender_gap_national_CDF_fisherz)
Japan_gender_gap_national_CDF_fisherz_fts_forecast <- forecast(ftsm(Japan_gender_gap_national_CDF_fisherz_fts), h = 20, method = "ets")$mean$y
Japan_gender_gap_national_CDF_fisherz_fts_forecast_ARIMA <- forecast(ftsm(Japan_gender_gap_national_CDF_fisherz_fts), h = 20, method = "arima")$mean$y

## take the inverse Fisher Z transformation

Japan_gender_gap_national_CDF_fisherz_fts_forecast_transform <- FisherZInv(Japan_gender_gap_national_CDF_fisherz_fts_forecast)
Japan_gender_gap_national_CDF_fisherz_fts_forecast_transform_ARIMA <- FisherZInv(Japan_gender_gap_national_CDF_fisherz_fts_forecast_ARIMA)

# plot

savefig("Fig_6a", width = 12, height = 10, toplines = 1, type = "png")
plot(Japan_gender_gap_national_CDF_fisherz_fts, col = gray(.7), xlab = "Age", 
     ylab = "Gender gap", main = "20-steps-ahead forecasts", ylim = c(0, 0.3))
lines(fts(ages, Japan_gender_gap_national_CDF_fisherz_fts_forecast_transform_ARIMA))
dev.off()

## forecast national female via FTS (take logit transformation)

Japan_female_pop_CDF_fts <- fts(ages[1:110], logit(Japan_female_pop_CDF[1:110,]))
Japan_female_pop_CDF_fts_forecast <- rbind(invlogit(forecast(ftsm(Japan_female_pop_CDF_fts), h = 20, method = "ets")$mean$y), rep(1, 20))
Japan_female_pop_CDF_fts_forecast_ARIMA <- rbind(invlogit(forecast(ftsm(Japan_female_pop_CDF_fts), h = 20, method = "arima")$mean$y), rep(1, 20))

## male_forecast = gender_gap + female forecasts

Japan_male_pop_CDF_fts_forecast <- Japan_female_pop_CDF_fts_forecast + 
  Japan_gender_gap_national_CDF_fisherz_fts_forecast_transform

Japan_male_pop_CDF_fts_forecast_ARIMA <- Japan_female_pop_CDF_fts_forecast_ARIMA + 
  Japan_gender_gap_national_CDF_fisherz_fts_forecast_transform_ARIMA

savefig("Fig_6b", width = 12, height = 10, toplines = 1, type = "png")
plot(fts(ages, Japan_male_pop_CDF_fts_forecast), xlab = "Age", ylab = "CDF", main = "20-steps-ahead forecasts")
lines(fts(ages, Japan_female_pop_CDF_fts_forecast))
arrows(97, 0.45, 93, 0.55)
text(97, 0.43, "Female")
arrows(75, 0.46, 83, 0.54)
text(75, 0.44, "Male")
dev.off()

savefig("Fig_6b_ARIMA", width = 12, height = 10, toplines = 1, type = "png")
plot(fts(ages, Japan_male_pop_CDF_fts_forecast_ARIMA), xlab = "Age", ylab = "CDF", main = "20-steps-ahead forecasts")
lines(fts(ages, Japan_female_pop_CDF_fts_forecast_ARIMA))
arrows(97, 0.45, 93, 0.55)
text(97, 0.43, "Female")
arrows(75, 0.46, 83, 0.54)
text(75, 0.44, "Male")
dev.off()

###########################################################
# from CDF to PDF for national life-table death count data
###########################################################

# ETS

Japan_female_pop_PDF_fts_forecast = Japan_male_pop_PDF_fts_forecast = matrix(NA, 111, 20)
for(ik in 1:20)
{
    Japan_female_pop_PDF_fts_forecast[,ik] = c(Japan_female_pop_CDF_fts_forecast[1,ik], diff(Japan_female_pop_CDF_fts_forecast[,ik])) * 10^5
    Japan_male_pop_PDF_fts_forecast[,ik]   = c(Japan_male_pop_CDF_fts_forecast[1,ik],   diff(Japan_male_pop_CDF_fts_forecast[,ik])) * 10^5
    rm(ik)    
}
rownames(Japan_female_pop_PDF_fts_forecast) = rownames(Japan_male_pop_PDF_fts_forecast) = ages
colnames(Japan_female_pop_PDF_fts_forecast) = colnames(Japan_male_pop_PDF_fts_forecast) = 1:20

# ARIMA

Japan_female_pop_PDF_fts_forecast_ARIMA = Japan_male_pop_PDF_fts_forecast_ARIMA = matrix(NA, 111, 20)
for(ik in 1:20)
{
    Japan_female_pop_PDF_fts_forecast_ARIMA[,ik] = c(Japan_female_pop_CDF_fts_forecast_ARIMA[1,ik], diff(Japan_female_pop_CDF_fts_forecast_ARIMA[,ik])) * 10^5
    Japan_male_pop_PDF_fts_forecast_ARIMA[,ik]   = c(Japan_male_pop_CDF_fts_forecast_ARIMA[1,ik],   diff(Japan_male_pop_CDF_fts_forecast_ARIMA[,ik])) * 10^5
    rm(ik)    
}
rownames(Japan_female_pop_PDF_fts_forecast_ARIMA) = rownames(Japan_male_pop_PDF_fts_forecast_ARIMA) = ages
colnames(Japan_female_pop_PDF_fts_forecast_ARIMA) = colnames(Japan_male_pop_PDF_fts_forecast_ARIMA) = 1:20

#######
# plot
#######

# ETS

savefig("Fig_6c", width = 12, height = 10, toplines = 1, type = "png")
plot(fts(ages, t(Japan_female_pop * 10^5)), xlab = "Age", ylab = "Life-table death count", 
     main = "Japanese female national data", col = gray(.7), ylim = c(0, 9000))
lines(fts(ages, Japan_female_pop_PDF_fts_forecast))
dev.off()

savefig("Fig_6d", width = 12, height = 10, toplines = 1, type = "png")
plot(fts(ages, t(Japan_male_pop * 10^5)), ylim = c(0, 9000),
     xlab = "Age", ylab = "Life-table death count", main = "Japanese male national data", col = gray(.7))
lines(fts(ages, Japan_male_pop_PDF_fts_forecast))
dev.off()

# ARIMA

savefig("Fig_6c_ARIMA", width = 12, height = 10, toplines = 1, type = "png")
plot(fts(ages, t(Japan_female_pop * 10^5)), ylim = range(Japan_female_pop_PDF_fts_forecast, Japan_male_pop_PDF_fts_forecast),
     xlab = "Age", ylab = "Life-table death count", main = "Japanese female national data", col = gray(.7))
lines(fts(ages, Japan_female_pop_PDF_fts_forecast_ARIMA))
dev.off()

savefig("Fig_6d_ARIMA", width = 12, height = 10, toplines = 1, type = "png")
plot(fts(ages, t(Japan_male_pop * 10^5)), ylim = range(Japan_female_pop_PDF_fts_forecast, 
                                                       Japan_male_pop_PDF_fts_forecast), 
     xlab = "Age", ylab = "", main = "Japanese male national data", col = gray(.7))
lines(fts(ages, Japan_male_pop_PDF_fts_forecast_ARIMA))
dev.off()

