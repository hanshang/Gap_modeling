###########################################################################
# visualizing integral measure of the gender gap among 47 subnational data
###########################################################################

savefig("Fig_4a", width = 16, height = 10, toplines = 1.5, pointsize = 4, type = "png")
filled.contour(years, 1:47, subnational_gender_diff_CDF_integral_measure_mat, ylim = c(47, 1), 
               main = "Integral measure of the gender gap \n Japanese subnational data (1947-2023)", 
               xlab = "Year", ylab = "Prefecture")
dev.off()


savefig("Fig_4b", width = 16, height = 10, toplines = 1.5, pointsize = 4, type = "png")
filled.contour(years, 1:47, subnational_gender_diff_CDF_W1_measure_mat, ylim = c(47, 1), 
               main = "Wasserstein distance of the gender gap \n Japanese subnational data (1947-2023)", 
               xlab = "Year", ylab = "Prefecture")
dev.off()

#####################################
# forecasting subnational gender gap
#####################################

## forecast subnational gender gap for 20-years-ahead

subnational_gender_diff_CDF_forecast = array(NA, dim = c(111, 20, 47))
for(ik in 1:length(subnational_gender_diff_CDF))
{
    fts_obj <- fts(ages, FisherZ(subnational_gender_diff_CDF[[ik]]))
    subnational_gender_diff_CDF_forecast[,,ik] = FisherZInv(forecast(ftsm(fts_obj), h = 20)$mean$y)
    print(ik); rm(ik); rm(fts_obj)
}

## forecast female populations for 20-years-ahead (take logit transformation)

subnational_female_CDF_forecast = array(NA, dim = c(111, 20, 47))
for(ik in 1:length(subnational_female_prefecture_dx_CDF))
{
    # replace 0 by a small epsilon value
  
    if(any(subnational_female_prefecture_dx_CDF[[ik]] == 0))
    {
        fts_obj <- replace(subnational_female_prefecture_dx_CDF[[ik]] , which(subnational_female_prefecture_dx_CDF[[ik]] == 0), 10^-6)
    }
    else
    {
        fts_obj <- subnational_female_prefecture_dx_CDF[[ik]]
    }
    colnames(fts_obj) = 1:ncol(subnational_female_prefecture_dx_CDF[[ik]])
    logit_fts_obj <- logit(fts_obj[1:110,])
    subnational_female_CDF_forecast[,,ik] = rbind(invlogit(forecast(ftsm(fts(ages[1:110], logit_fts_obj)), h = 20)$mean$y), rep(1, 20))
    print(ik); rm(ik); rm(fts_obj)
}

## forecast male populations for 20-years-ahead

subnational_male_CDF_forecast = subnational_female_CDF_forecast + subnational_gender_diff_CDF_forecast

##########################################
# From CDF to PDF life-table death counts
##########################################

subnational_female_PDF_forecast = subnational_male_PDF_forecast = array(NA, dim = c(111, 20, 47))
for(ik in 1:47)
{
    for(ij in 1:20)
    {
        subnational_female_PDF_forecast[,ij,ik] = c(subnational_female_CDF_forecast[1,ij,ik], diff(subnational_female_CDF_forecast[,ij,ik])) * 10^5
        subnational_male_PDF_forecast[,ij,ik]   = c(subnational_male_CDF_forecast[1,ij,ik], diff(subnational_male_CDF_forecast[,ij,ik])) * 10^5
        rm(ij)
    }
    rm(ik)
}

