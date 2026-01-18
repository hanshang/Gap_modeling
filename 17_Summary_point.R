#####################
# graphical displays
#####################

## ETS

# KLD

subnational_various_gap_M_KLD_ETS = cbind(subnational_gender_gap_KLD_JSD_M_ETS_mean[,1],
                                          subnational_gap_fore_male_ETS_mean[,1],
                                          double_gap_err_M_ets_mean[,1])

subnational_various_gap_F_KLD_ETS = cbind(subnational_gender_gap_KLD_JSD_F_ETS_mean[,1],
                                          subnational_gap_fore_female_ETS_mean[,1],
                                          double_gap_err_F_ets_mean[,1])

# JSD

subnational_various_gap_M_JSD_ETS = cbind(subnational_gender_gap_KLD_JSD_M_ETS_mean[,2],
                                          subnational_gap_fore_male_ETS_mean[,2],
                                          double_gap_err_M_ets_mean[,2])

subnational_various_gap_F_JSD_ETS = cbind(subnational_gender_gap_KLD_JSD_F_ETS_mean[,2],
                                          subnational_gap_fore_female_ETS_mean[,2],
                                          double_gap_err_F_ets_mean[,2])

## ARIMA

# KLD

subnational_various_gap_M_KLD_ARIMA = cbind(subnational_gender_gap_KLD_JSD_M_ARIMA_mean[,1],
                                            subnational_gap_fore_male_mean[,1],
                                            double_gap_err_M_mean[,1])

subnational_various_gap_F_KLD_ARIMA = cbind(subnational_gender_gap_KLD_JSD_F_ARIMA_mean[,1],
                                            subnational_gap_fore_female_mean[,1],
                                            double_gap_err_F_mean[,1])

# JSD

subnational_various_gap_M_JSD_ARIMA = cbind(subnational_gender_gap_KLD_JSD_M_ARIMA_mean[,2],
                                          subnational_gap_fore_male_mean[,2],
                                          double_gap_err_M_mean[,2])

subnational_various_gap_F_JSD_ARIMA = cbind(subnational_gender_gap_KLD_JSD_F_ARIMA_mean[,2],
                                            subnational_gap_fore_female_mean[,2],
                                            double_gap_err_F_mean[,2])

require(xtable)

# ARIMA

xtable(cbind(subnational_various_gap_F_KLD_ARIMA, subnational_various_gap_M_KLD_ARIMA), digits = 4)
xtable(cbind(subnational_various_gap_F_JSD_ARIMA, subnational_various_gap_M_JSD_ARIMA), digits = 4)

# ETS

xtable(cbind(subnational_various_gap_F_KLD_ETS, subnational_various_gap_M_KLD_ETS), digits = 4)
xtable(cbind(subnational_various_gap_F_JSD_ETS, subnational_various_gap_M_JSD_ETS), digits = 4)

####################
# graphical display
####################

# KLD + F 

savefig("Fig_9a", width = 12, height = 10, toplines = 0.5, type = "png")
matplot(1:15, subnational_various_gap_F_KLD_ETS, type = "l", ylim = c(0, 0.15), 
        lty = 1, xlab = "", ylab = "KLD", main = "Female")
matlines(1:15, subnational_various_gap_F_KLD_ARIMA, type = "l", lty = 2)
legend("topleft", c("Gender gap (ETS)", "Region gap or Double gap (ETS)", "Gender gap (ARIMA)", "Region gap or Double gap (ARIMA)"), 
       col = c(1, 3, 1, 3), lty = c(1, 1, 2, 2), cex = 0.6)
dev.off()

# KLD + M

savefig("Fig_9b", width = 12, height = 10, toplines = 0.5, type = "png")
matplot(1:15, subnational_various_gap_M_KLD_ETS, type = "l", ylim = c(0, 0.25), lty = 1, 
        xlab = "", ylab = "KLD", main = "Male")
matlines(1:15, subnational_various_gap_M_KLD_ARIMA, type = "l", lty = 2)
legend("topleft", c("Gender gap (ETS)", "Region gap (ETS)", "Double gap (ETS)", "Gender gap (ARIMA)", "Region gap (ARIMA)", "Double gap (ARIMA)"), 
       col = c(1:3, 1:3), lty = c(1, 1, 1, 2, 2, 2), cex = 0.6)
dev.off()

# JSD + F

savefig("Fig_9c", width = 12, height = 10, toplines = 0.5, type = "png")
matplot(1:15, subnational_various_gap_F_JSD_ETS, type = "l", ylim = c(0, 0.045), lty = 1, xlab = "Forecast horizon", ylab = "JSD", main = "")
matlines(1:15, subnational_various_gap_F_JSD_ARIMA, type = "l", lty = 2)
dev.off()

# JSD + M

savefig("Fig_9d", width = 12, height = 10, toplines = 0.5, type = "png")
matplot(1:15, subnational_various_gap_M_JSD_ETS, type = "l", ylim = c(0, 0.087), lty = 1, xlab = "Forecast horizon", ylab = "JSD", main = "")
matlines(1:15, subnational_various_gap_M_JSD_ARIMA, type = "l", lty = 2)
dev.off()

