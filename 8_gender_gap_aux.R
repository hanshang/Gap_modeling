#########################################
# training set: 1947-2003 (index: 1:57)
# testing set:  2004-2023 (index: 58:77)
#########################################

# female_data: CDF values
# male_data:   CDF values
# fh: forecast horizon from 1 to 20
# fmethod: forecasting method, ARIMA or ETS
# accuracy: point forecast accuracy or interval forecast accuracy
# nboot: number of bootstrap replications
# alpha: nominal coverage probability, such as alpha = 0.8 or 0.95

gender_gap_fore <- function(male_CDF, female_CDF, fh, fmethod)
{
    N_nrow = nrow(male_CDF)
    
    # gender gap
    
    gender_gap = male_CDF - female_CDF
    
    # take Fisher Z transformation
    
    gender_gap_fisherz = FisherZ(gender_gap)
    
    # fitting a FDM via the Fisher Z transformation
    
    gender_gap_forecast_val <- forecast(ftsm(fts(ages, gender_gap_fisherz)), h = fh, method = fmethod)
    
    # foreacsting female data via logit transformation
    
    if(any(female_CDF <= 0))
    {
      female_CDF = replace(female_CDF, which(female_CDF == 0), 10^-8)
    }
    
    female_forecast_val <- forecast(ftsm(fts(ages[1:(N_nrow - 1)], logit(female_CDF[1:(N_nrow - 1),])), 
                                         ngrid = 501), h = fh, method = fmethod)
    
    # obtaining male data
    
    gender_gap_fisherz_fts_forecast_transform <- FisherZInv(gender_gap_forecast_val$mean$y[,fh])
    female_pop_CDF_fts_forecast <- c(invlogit(female_forecast_val$mean$y[,fh]), 1)
    male_pop_CDF_fts_forecast <- female_pop_CDF_fts_forecast + gender_gap_fisherz_fts_forecast_transform
    
    # male data between 0 and 1
    
    if(any(male_pop_CDF_fts_forecast < 0))
    {
        male_pop_CDF_fts_forecast = replace(male_pop_CDF_fts_forecast, which(male_pop_CDF_fts_forecast < 0), 0)    
    }
    else if(any(male_pop_CDF_fts_forecast > 1))
    {
        male_pop_CDF_fts_forecast = replace(male_pop_CDF_fts_forecast, which(male_pop_CDF_fts_forecast > 1), 1)
    }
    
    # turn CDF to PDF
    
    female_pop_PDF_fts_forecast <- c(female_pop_CDF_fts_forecast[1], diff(female_pop_CDF_fts_forecast))
    male_pop_PDF_fts_forecast   <- c(male_pop_CDF_fts_forecast[1],   diff(male_pop_CDF_fts_forecast))
    return(list(male_fore = male_pop_PDF_fts_forecast, female_fore = female_pop_PDF_fts_forecast))
}

# CDF_M: male CDF (111 by n) data matrix
# CDF_F: female CDF
# PDF_M_holdout: holdout male PDF
# PDF_F_holdout: holdout female PDF
# horizon: forecast horizon
# fore_method: forecasting method, such as ARIMA or ETS
# length_test_data: length of test data
# accuracy_metric: point or interval forecast accuracy
# level_of_significance: level of significance

gender_gap_fore_fun <- function(CDF_M, CDF_F, PDF_M_holdout, PDF_F_holdout, horizon, fore_method, 
                                length_test_data)
{  
    n_col = ncol(CDF_M)
    n_row = nrow(CDF_M)
    
    forecast_M = forecast_F = matrix(NA, n_row, (length_test_data + 1 - horizon))
    for(ik in 1:(length_test_data + 1 - horizon))
    {
        dum = gender_gap_fore(male_CDF = CDF_M[,1:(n_col - (length_test_data + 1) + ik)], 
                              female_CDF = CDF_F[,1:(n_col - (length_test_data + 1) + ik)], 
                              fh = horizon, fmethod = fore_method)
        forecast_M[,ik] = dum$male_fore
        forecast_F[,ik] = dum$female_fore
        rm(ik); rm(dum)
    }
    
    # holdout data
    
    PDF_M = matrix(t(PDF_M_holdout[(n_col - length_test_data + horizon):n_col,]), n_row, (length_test_data + 1 - horizon))
    PDF_F = matrix(t(PDF_F_holdout[(n_col - length_test_data + horizon):n_col,]), n_row, (length_test_data + 1 - horizon))
    
    # KLD and JSD
    
    KL_div_val_M = JS_div_val_M = KL_div_val_F = JS_div_val_F = vector("numeric", (length_test_data + 1 - horizon))
    for(ij in 1:(length_test_data + 1 - horizon))
    {
        # symmetric KL dist
        
        KL_div_val_M[ij] = mean(KLdiv(cbind(forecast_M[,ij], PDF_M[,ij]))[2:3])
        KL_div_val_F[ij] = mean(KLdiv(cbind(forecast_F[,ij], PDF_F[,ij]))[2:3])
        
        # Jensen-Shannon dist
        
        JS_div_val_M[ij] = mean(KLdiv(cbind(forecast_M[,ij], 
                                            apply(cbind(forecast_M[,ij], PDF_M[,ij]), 1, geometric.mean)))[2:3])
        JS_div_val_F[ij] = mean(KLdiv(cbind(forecast_F[,ij], 
                                            apply(cbind(forecast_F[,ij], PDF_F[,ij]), 1, geometric.mean)))[2:3])
    }
    err_M = c(mean(KL_div_val_M), mean(JS_div_val_M))
    err_F = c(mean(KL_div_val_F), mean(JS_div_val_F))
    return(list(err_M = err_M, err_F = err_F, forecast_M = forecast_M, forecast_F = forecast_F, 
                PDF_M = PDF_M, PDF_F = PDF_F))
}

# CDF_M: CDF of male data
# CDF_F: CDF of female data
# PDF_M_holdout: holdout male data
# PDF_F_holdout: holdout female data
# horizon: forecast horizon
# fore_method: forecasting method
# length_test_data: length of test data
# alpha: level of significance

gender_gap_fore_fun_conformal <- function(CDF_M, CDF_F, PDF_M_holdout, PDF_F_holdout, 
                                          horizon, fore_method, length_test_data, alpha)
{
    n_col = ncol(CDF_M)
    n_row = nrow(CDF_M)
  
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
            dum = gender_gap_fore(male_CDF = CDF_M[,1:ij], female_CDF = CDF_F[,1:ij], 
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
        
        dum = gender_gap_fore(male_CDF = CDF_M[,1:(n_col - (length_test_data + 1) + iwk)], 
                              female_CDF = CDF_F[,1:(n_col - (length_test_data + 1) + iwk)], 
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

