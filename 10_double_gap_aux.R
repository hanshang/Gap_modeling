# N_F_CDF: national female CDF
# subnational_region_F_diff_CDF: regional gap = subnational - national female CDF
# subnational_gender_diff_CDF: gender gap = subnational female - subnational male CDF
# fh: forecast horizon
# fmethod: forecasting method

double_gap_fore_fun_interval <- function(index, N_F_CDF, fh, fmethod)
{
    # N_nrow is number of ages
    # N_ncol is number of years
  
    N_nrow = nrow(N_F_CDF)
    N_ncol = ncol(N_F_CDF)
    N_prefecture_ncol = ncol(subnational_region_F_diff_CDF[[index]])
    
    # ensure female national CDF >= 0
    
    if(any(N_F_CDF <= 0))
    {
        N_F_CDF = replace(N_F_CDF, which(N_F_CDF == 0), 10^-8)
    }
    
	  Japan_female_pop_CDF_forecast = c(invlogit(forecast(ftsm(fts(ages[1:(N_nrow - 1)], logit(N_F_CDF[1:(N_nrow - 1),tail(1:N_ncol, N_prefecture_ncol)]))), h = fh, method = fmethod)$mean$y[,fh]), 1)
    
    # female region gap between subnational and national data
    
	  subnational_region_F_diff_CDF_forecast = FisherZInv(forecast(ftsm(fts(ages, FisherZ(subnational_region_F_diff_CDF[[index]][,tail(1:N_ncol, N_prefecture_ncol)]))), h = fh, method = fmethod)$mean$y[,fh])
	  
	  # female region gap + national female forecasts
    
	  subnational_female_pop_CDF_forecast = Japan_female_pop_CDF_forecast + subnational_region_F_diff_CDF_forecast
	  
	  # gender gap between subnational males and females
    
	  gender_gap_fisherz_fts_forecast_transform = FisherZInv(forecast(ftsm(fts(ages, FisherZ(subnational_gender_diff_CDF[[index]][,tail(1:N_ncol, N_prefecture_ncol)]))), h = fh, method = fmethod)$mean$y[,fh])
	  
    # subnational male forecasts
	  
	  subnational_male_pop_CDF_forecast = subnational_female_pop_CDF_forecast + gender_gap_fisherz_fts_forecast_transform
    
    # turn CDF to PDF
    
	  if(any(subnational_female_pop_CDF_forecast < 0))
	  {
  	    subnational_female_pop_CDF_forecast = replace(subnational_female_pop_CDF_forecast, which(subnational_female_pop_CDF_forecast < 0), 10^-8)    
	  }
	  else if(any(subnational_female_pop_CDF_forecast > 1))
	  {
	      subnational_female_pop_CDF_forecast = replace(subnational_female_pop_CDF_forecast, which(subnational_female_pop_CDF_forecast > 1), 1)
	  }
	  
	  if(any(subnational_male_pop_CDF_forecast < 0))
	  {
	      subnational_male_pop_CDF_forecast = replace(subnational_male_pop_CDF_forecast, which(subnational_male_pop_CDF_forecast < 0), 10^-8)    
	  }
	  else if(any(subnational_male_pop_CDF_forecast > 1))
	  {
  	    subnational_male_pop_CDF_forecast = replace(subnational_male_pop_CDF_forecast, which(subnational_male_pop_CDF_forecast > 1), 1)
	  }
	  
	  # from CDF to PDF
	  
	  subnational_female_pop_PDF_forecast = c(subnational_female_pop_CDF_forecast[1], diff(subnational_female_pop_CDF_forecast))
	  subnational_male_pop_PDF_forecast   = c(subnational_male_pop_CDF_forecast[1],   diff(subnational_male_pop_CDF_forecast))
	  
	  return(list(male_fore = subnational_male_pop_PDF_forecast, female_fore = subnational_female_pop_PDF_forecast))
}

# prefecture_index: index of prefecture
# national_M_CDF: national male CDFs
# national_F_CDF: national female CDFs
# PDF_M_holdout: holdout densities
# alpha: level of significance

double_gap_fore_fun_conformal <- function(prefecture_index, national_F_CDF, 
                                          PDF_M_holdout, PDF_F_holdout, horizon,
                                          fore_method, length_test_data, alpha)
{
    n_row = nrow(national_F_CDF)
    n_col = ncol(subnational_region_F_diff_CDF[[prefecture_index]])
    
    q_alpha_mat_M = q_alpha_mat_F = matrix(NA, n_age, (length_test_data + 1 - horizon))
    int_score_M = int_score_F = matrix(NA, (length_test_data + 1 - horizon), 3)
    for(iwk in 1:(length_test_data + 1 - horizon))
    {
        # in-sample calibration
        
        n_validation = n_col - 16
        validation_set = 6:(n_validation + iwk - horizon)
        fore_validation_M = fore_validation_F = matrix(NA, n_row, length(validation_set))
        for(ij in validation_set)
        {
            dum = double_gap_fore_fun_interval(index = prefecture_index, 
                                               N_F_CDF = national_F_CDF[,tail(1:ncol(national_F_CDF), n_col)[1:ij]], 
                                               fh = horizon, fmethod = fore_method)
            fore_validation_M[,(ij - 5)] = dum$male_fore
            fore_validation_F[,(ij - 5)] = dum$female_fore
            rm(ij); rm(dum)
        }
        
        # holdout data
        
        PDF_M = matrix(t(PDF_M_holdout[(6 + horizon):((n_validation + iwk)),]), n_row, )
        PDF_F = matrix(t(PDF_F_holdout[(6 + horizon):((n_validation + iwk)),]), n_row, )
        
        # compute absolute residuals between holdout data and forecasts
        
        resi_M = abs(PDF_M - fore_validation_M)
        resi_F = abs(PDF_F - fore_validation_F)
        
        q_alpha_M = q_alpha_F = vector("numeric", n_row)
        for(ij in 1:n_row)
        {
            ## male
            
            # (1 - alpha) quantile of the absolute residuals
            
            rq_fit <- rq(resi_M[ij,] ~ 1, tau = (1 - alpha))
            
            # use information criterion to select AR order
            
            AR_p <- max(ar(resi_M[ij,])$order, 1)
            lastlags <- tail(x = resi_M[ij,], AR_p)
            newdf <- as.data.frame(t(lastlags))
            names(newdf) <- paste0("lag", 1:AR_p)
            
            q_alpha_M[ij] = predict(rq_fit, newdf)
            rm(rq_fit)
            
            ## female
            
            rq_fit <- rq(resi_F[ij,] ~ 1, tau = (1 - alpha))
            
            # use information criterion to select AR order
            
            AR_p <- max(ar(resi_F[ij,])$order, 1)
            lastlags <- tail(x = resi_F[ij,], AR_p)
            newdf <- as.data.frame(t(lastlags))
            names(newdf) <- paste0("lag", 1:AR_p)
            
            q_alpha_F[ij] = predict(rq_fit, newdf)
            rm(ij); rm(rq_fit)
        }
        q_alpha_mat_M[,iwk] = q_alpha_M
        q_alpha_mat_F[,iwk] = q_alpha_F
      
        # out-of-sample forecasts
        
        dum = double_gap_fore_fun_interval(index = prefecture_index, 
                                           N_F_CDF = national_F_CDF[,tail(1:77, n_col)[1:(n_col - (length_test_data + 1) + iwk)]],
                                           fh = horizon, fmethod = fore_method)
        forecast_M = dum$male_fore
        forecast_F = dum$female_fore
        rm(dum)
        
        # holdout data
        
        int_score_M[iwk,] = interval_score(holdout = PDF_M_holdout[(n_col - length_test_data - 1 + iwk + horizon),], 
                                           lb = forecast_M - q_alpha_M,
                                           ub = forecast_M + q_alpha_M, 
                                           alpha = alpha)
        
        int_score_F[iwk,] = interval_score(holdout = PDF_F_holdout[(n_col - length_test_data - 1 + iwk + horizon),], 
                                           lb = forecast_F - q_alpha_F,
                                           ub = forecast_F + q_alpha_F, 
                                           alpha = alpha)
        rm(iwk); rm(q_alpha_M); rm(q_alpha_F); rm(resi_M); rm(resi_F)
    }
    colnames(int_score_M) = colnames(int_score_F) = c("ECP", "CPD", "score")
    rownames(int_score_M) = rownames(int_score_F) = 
    colnames(q_alpha_mat_M) = colnames(q_alpha_mat_F) = 1:(length_test_data + 1 - horizon)
    rownames(q_alpha_mat_M) = rownames(q_alpha_mat_F) = 1:n_row
    
    rm(n_row); rm(n_col)
    return(list(int_score_M = colMeans(int_score_M), int_score_F = colMeans(int_score_F), 
                q_alpha_mat_M = rowMeans(q_alpha_mat_M), q_alpha_mat_F = rowMeans(q_alpha_mat_F)))
}

