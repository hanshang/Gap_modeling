#####################
# graphical displays
#####################

### alpha = 0.2

## ECP

# ARIMA

savefig("Fig_11a", width = 12, height = 10, toplines = 0.8, type = "png")
plot(1:15, subnational_gender_gap_score_F_ARIMA_PI_80_mean[,1], type = "l", lty = 1, col = 1, ylim = c(0.73, 0.9),
     xlab = "", ylab = "Empirical coverage probability", main = expression(paste("ARIMA, ", alpha == 0.2)))
lines(1:15, subnational_region_gap_score_F_ARIMA_PI_80_mean[,1], lty = 1, col = 2)
lines(1:15, subnational_double_gap_score_F_ARIMA_PI_80_mean[,1], lty = 1, col = 4)

lines(1:15, subnational_gender_gap_score_M_ARIMA_PI_80_mean[,1], lty = 2, col = 1) 
lines(1:15, subnational_region_gap_score_M_ARIMA_PI_80_mean[,1], lty = 2, col = 2)
lines(1:15, subnational_double_gap_score_M_ARIMA_PI_80_mean[,1], lty = 2, col = 4)
abline(h = 0.8, lty = 3)
dev.off()

# ETS

savefig("Fig_10a", width = 12, height = 10, toplines = 0.8, type = "png")
plot(1:15, subnational_gender_gap_score_F_ETS_PI_80_mean[,1], type = "l", lty = 1, col = 1, ylim = c(0.8, 1),
     xlab = "", ylab = "Empirical coverage probability", main = expression(paste("ETS, ", alpha == 0.2)))
lines(1:15, subnational_region_gap_score_F_ETS_PI_80_mean[,1], lty = 1, col = 2)
lines(1:15, subnational_double_gap_score_F_ETS_PI_80_mean[,1], lty = 1, col = 4)

lines(1:15, subnational_gender_gap_score_M_ETS_PI_80_mean[,1], lty = 2, col = 1) 
lines(1:15, subnational_region_gap_score_M_ETS_PI_80_mean[,1], lty = 2, col = 2)
lines(1:15, subnational_double_gap_score_M_ETS_PI_80_mean[,1], lty = 2, col = 4)
abline(h = 0.8, lty = 3)
dev.off()

## Mean interval score

# ARIMA

savefig("Fig_11b", width = 12, height = 10, toplines = 0.8, type = "png")
plot(1:15, subnational_gender_gap_score_F_ARIMA_PI_80_mean[,3], type = "l", lty = 1, col = 1, 
     ylim = c(0, 0.028), xlab = "", ylab = "Mean interval score", 
     main = expression(paste("ARIMA, ", alpha == 0.2)))
lines(1:15, subnational_region_gap_score_F_ARIMA_PI_80_mean[,3], lty = 1, col = 2)
lines(1:15, subnational_double_gap_score_F_ARIMA_PI_80_mean[,3], lty = 1, col = 4)

lines(1:15, subnational_gender_gap_score_M_ARIMA_PI_80_mean[,3], lty = 2, col = 1)
lines(1:15, subnational_region_gap_score_M_ARIMA_PI_80_mean[,3], lty = 2, col = 2)
lines(1:15, subnational_double_gap_score_M_ARIMA_PI_80_mean[,3], lty = 2, col = 4)
legend("topleft", c("Gender gap (F)", "Region gap (F)", "Double gap (F)", 
                     "Gender gap (M)", "Region gap (M)", "Double gap (M)"), col = c(1, 2, 4, 1, 2, 4), 
       lty = c(1, 1, 1, 2, 2, 2), ncol = 2, cex = 0.7)
dev.off()

# ETS

savefig("Fig_10b", width = 12, height = 10, toplines = 0.8, type = "png")
plot(1:15, subnational_gender_gap_score_F_ETS_PI_80_mean[,3], type = "l", lty = 1, col = 1, 
     ylim = c(0, 0.018), xlab = "", ylab = "Mean interval score", 
     main = expression(paste("ETS, ", alpha == 0.2)))
lines(1:15, subnational_region_gap_score_F_ETS_PI_80_mean[,3], lty = 1, col = 2)
lines(1:15, subnational_double_gap_score_F_ETS_PI_80_mean[,3], lty = 1, col = 4)

lines(1:15, subnational_gender_gap_score_M_ETS_PI_80_mean[,3], lty = 2, col = 1)
lines(1:15, subnational_region_gap_score_M_ETS_PI_80_mean[,3], lty = 2, col = 2)
lines(1:15, subnational_double_gap_score_M_ETS_PI_80_mean[,3], lty = 2, col = 4)
legend("topleft", c("Gender gap (F)", "Region gap (F)", "Double gap (F)", 
                    "Gender gap (M)", "Region gap (M)", "Double gap (M)"), col = c(1, 2, 4, 1, 2, 4), 
       lty = c(1, 1, 1, 2, 2, 2), ncol = 2, cex = 0.7)
dev.off()

### alpha = 0.05

## ECP

# ARIMA

savefig("Fig_11c", width = 12, height = 10, toplines = 0.8, type = "png")
plot(1:15, subnational_gender_gap_score_F_ARIMA_PI_95_mean[,1], type = "l", lty = 1, col = 1, 
     ylim = c(0.8, 1), xlab = "Forecast horizon", ylab = "Empirical coverage probability", 
     main = expression(paste("ARIMA, ", alpha == 0.05)))
lines(1:15, subnational_region_gap_score_F_ARIMA_PI_95_mean[,1], lty = 1, col = 2)
lines(1:15, subnational_double_gap_score_F_ARIMA_PI_95_mean[,1], lty = 1, col = 4)

lines(1:15, subnational_gender_gap_score_M_ARIMA_PI_95_mean[,1], lty = 2, col = 1) 
lines(1:15, subnational_region_gap_score_M_ARIMA_PI_95_mean[,1], lty = 2, col = 2)
lines(1:15, subnational_double_gap_score_M_ARIMA_PI_95_mean[,1], lty = 2, col = 4)
abline(h = 0.95, lty = 3)
dev.off()

# ETS

savefig("Fig_10c", width = 12, height = 10, toplines = 0.8, type = "png")
plot(1:15, subnational_gender_gap_score_F_ETS_PI_95_mean[,1], type = "l", lty = 1, col = 1, ylim = c(0.85, 1),
     xlab = "Forecast horizon", ylab = "Empirical coverage probability",
     main = expression(paste("ETS, ", alpha == 0.05)))
lines(1:15, subnational_region_gap_score_F_ETS_PI_95_mean[,1], lty = 1, col = 2)
lines(1:15, subnational_double_gap_score_F_ETS_PI_95_mean[,1], lty = 1, col = 4)

lines(1:15, subnational_gender_gap_score_M_ETS_PI_95_mean[,1], lty = 2, col = 1) 
lines(1:15, subnational_region_gap_score_M_ETS_PI_95_mean[,1], lty = 2, col = 2)
lines(1:15, subnational_double_gap_score_M_ETS_PI_95_mean[,1], lty = 2, col = 4)
abline(h = 0.95, lty = 3)
dev.off()

## Mean interval score

# ARIMA

savefig("Fig_11d", width = 12, height = 10, toplines = 0.8, type = "png")
plot(1:15, subnational_gender_gap_score_F_ARIMA_PI_95_mean[,3], type = "l", lty = 1, col = 1,
     ylim = c(0, 0.08), xlab = "Forecast horizon", ylab = "Mean interval score", 
     main = expression(paste("ARIMA, ", alpha == 0.05)))
lines(1:15, subnational_region_gap_score_F_ARIMA_PI_95_mean[,3], lty = 1, col = 2)
lines(1:15, subnational_double_gap_score_F_ARIMA_PI_95_mean[,3], lty = 1, col = 4)

lines(1:15, subnational_gender_gap_score_M_ARIMA_PI_95_mean[,3], lty = 2, col = 1)
lines(1:15, subnational_region_gap_score_M_ARIMA_PI_95_mean[,3], lty = 2, col = 2)
lines(1:15, subnational_double_gap_score_M_ARIMA_PI_95_mean[,3], lty = 2, col = 4)
dev.off()

# ETS

savefig("Fig_10d", width = 12, height = 10, toplines = 0.8, type = "png")
plot(1:15, subnational_gender_gap_score_F_ETS_PI_95_mean[,3], type = "l", lty = 1, col = 1,
     ylim = c(0, 0.04), xlab = "Forecast horizon", ylab = "Mean interval score", 
     main = expression(paste("ETS, ", alpha == 0.05)))
lines(1:15, subnational_region_gap_score_F_ETS_PI_95_mean[,3], lty = 1, col = 2)
lines(1:15, subnational_double_gap_score_F_ETS_PI_95_mean[,3], lty = 1, col = 4)

lines(1:15, subnational_gender_gap_score_M_ETS_PI_95_mean[,3], lty = 2, col = 1)
lines(1:15, subnational_region_gap_score_M_ETS_PI_95_mean[,3], lty = 2, col = 2)
lines(1:15, subnational_double_gap_score_M_ETS_PI_95_mean[,3], lty = 2, col = 4)
dev.off()

# summary

output <- rbind(cbind(rbind(colMeans(subnational_gender_gap_score_F_ARIMA_PI_80_mean),
                            colMeans(subnational_region_gap_score_F_ARIMA_PI_80_mean),
                            colMeans(subnational_double_gap_score_F_ARIMA_PI_80_mean),
                            
                            colMeans(subnational_gender_gap_score_F_ARIMA_PI_95_mean),
                            colMeans(subnational_region_gap_score_F_ARIMA_PI_95_mean),
                            colMeans(subnational_double_gap_score_F_ARIMA_PI_95_mean)),

                     rbind(colMeans(subnational_gender_gap_score_M_ARIMA_PI_80_mean),
                            colMeans(subnational_region_gap_score_M_ARIMA_PI_80_mean),
                            colMeans(subnational_double_gap_score_M_ARIMA_PI_80_mean),
                            
                            colMeans(subnational_gender_gap_score_M_ARIMA_PI_95_mean),
                            colMeans(subnational_region_gap_score_M_ARIMA_PI_95_mean),
                            colMeans(subnational_double_gap_score_M_ARIMA_PI_95_mean))),

              cbind(rbind(colMeans(subnational_gender_gap_score_F_ETS_PI_80_mean),
                          colMeans(subnational_region_gap_score_F_ETS_PI_80_mean),
                          colMeans(subnational_double_gap_score_F_ETS_PI_80_mean),
                          
                          colMeans(subnational_gender_gap_score_F_ETS_PI_95_mean),
                          colMeans(subnational_region_gap_score_F_ETS_PI_95_mean),
                          colMeans(subnational_double_gap_score_F_ETS_PI_95_mean)),
              
                    rbind(colMeans(subnational_gender_gap_score_M_ETS_PI_80_mean),
                          colMeans(subnational_region_gap_score_M_ETS_PI_80_mean),
                          colMeans(subnational_double_gap_score_M_ETS_PI_80_mean),
                          
                          colMeans(subnational_gender_gap_score_M_ETS_PI_95_mean),
                          colMeans(subnational_region_gap_score_M_ETS_PI_95_mean),
                          colMeans(subnational_double_gap_score_M_ETS_PI_95_mean))))

rownames(output) = rep(c("Gender gap", "Region gap", "Double gap"), 4)
xtable(output, digits = 3)

