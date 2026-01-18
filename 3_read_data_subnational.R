##################################
# subnational gender gap in Japan
##################################

subnational_female_prefecture_dx_CDF = subnational_male_prefecture_dx_CDF = 
subnational_gender_diff_CDF = subnational_gender_diff_CDF_integral_measure = 
subnational_gender_diff_CDF_W1 = list()
for(ik in 1:47)
{
    # compute cumulative relative life-table death counts
    
    subnational_female_pop_CDF = subnational_male_pop_CDF = matrix(NA, n_age, prefecture_n_year[ik])
    for(ij in 1:prefecture_n_year[ik])
    {
        subnational_female_pop_CDF[,ij] = cumsum((female_prefecture_dx[[ik]])[ij,])
        subnational_male_pop_CDF[,ij]   = cumsum((male_prefecture_dx[[ik]])[ij,])
        rm(ij)
    }
    subnational_female_prefecture_dx_CDF[[ik]] = subnational_female_pop_CDF
    subnational_male_prefecture_dx_CDF[[ik]]   = subnational_male_pop_CDF
    
    # compute subnational gender gap
    
    subnational_gender_diff_CDF[[ik]] = subnational_male_prefecture_dx_CDF[[ik]] - subnational_female_prefecture_dx_CDF[[ik]]
    
    # integral measure
    
    subnational_gender_diff_CDF_integral_measure[[ik]] = apply(subnational_gender_diff_CDF[[ik]], 2, sum)
    
    # Wasserstein of order 1
    
    wasserstein1d_value = vector("numeric", prefecture_n_year[ik])
    for(ij in 1:prefecture_n_year[ik])
    {
        wasserstein1d_value[ij] = wasserstein1d(a = (female_prefecture_dx[[ik]])[ij,], b = (male_prefecture_dx[[ik]])[ij,])
        rm(ij)
    }
    subnational_gender_diff_CDF_W1[[ik]] = wasserstein1d_value
    
    rm(subnational_female_pop_CDF); rm(subnational_male_pop_CDF)
    rm(ik); rm(wasserstein1d_value)
} 

###########################
# from a list to an matrix
###########################

subnational_gender_diff_CDF_integral_measure_mat = 
subnational_gender_diff_CDF_W1_measure_mat = matrix(NA, 77, 47)
for(ik in 1:46)
{
    subnational_gender_diff_CDF_integral_measure_mat[,ik] = subnational_gender_diff_CDF_integral_measure[[ik]]  
    subnational_gender_diff_CDF_W1_measure_mat[,ik] = subnational_gender_diff_CDF_W1[[ik]]
    rm(ik)
}

# Okinawa has 51 years from 1973 to 2023

subnational_gender_diff_CDF_integral_measure_mat[,47] = c(rep(NA, 26), subnational_gender_diff_CDF_integral_measure[[47]])
subnational_gender_diff_CDF_W1_measure_mat[,47] = c(rep(NA, 26), subnational_gender_diff_CDF_W1[[47]])

rownames(subnational_gender_diff_CDF_integral_measure_mat) = rownames(subnational_gender_diff_CDF_W1_measure_mat) = years
colnames(subnational_gender_diff_CDF_integral_measure_mat) = colnames(subnational_gender_diff_CDF_W1_measure_mat) = state


###########################
# subnational hetergeneity
###########################

subnational_region_M_diff_CDF = subnational_region_F_diff_CDF = 
subnational_region_M_diff_CDF_integral_measure = subnational_region_F_diff_CDF_integral_measure = list()
for(ik in 1:46)
{
    # region gap of (subnational - national)
    
    subnational_region_M_diff_CDF[[ik]] = subnational_male_prefecture_dx_CDF[[ik]] - Japan_male_pop_CDF
    subnational_region_F_diff_CDF[[ik]] = subnational_female_prefecture_dx_CDF[[ik]] - Japan_female_pop_CDF
    
    # integral measure
    
    subnational_region_M_diff_CDF_integral_measure[[ik]] = apply(subnational_region_M_diff_CDF[[ik]], 2, sum)
    subnational_region_F_diff_CDF_integral_measure[[ik]] = apply(subnational_region_F_diff_CDF[[ik]], 2, sum)
    print(ik); rm(ik)    
}

# including Okinawa from 1973 to 2023

subnational_region_M_diff_CDF[[47]] = subnational_male_prefecture_dx_CDF[[47]] - Japan_male_pop_CDF[,tail(1:77,51)]
subnational_region_F_diff_CDF[[47]] = subnational_female_prefecture_dx_CDF[[47]] - Japan_female_pop_CDF[,tail(1:77,51)]

subnational_region_M_diff_CDF_integral_measure[[47]] = apply(subnational_region_M_diff_CDF[[47]], 2, sum)
subnational_region_F_diff_CDF_integral_measure[[47]] = apply(subnational_region_F_diff_CDF[[47]], 2, sum)

## from a list to a matrix

subnational_region_M_diff_CDF_integral_measure_mat = subnational_region_F_diff_CDF_integral_measure_mat = matrix(NA, 77, 47)   
for(ik in 1:46)
{
    subnational_region_M_diff_CDF_integral_measure_mat[,ik] = subnational_region_M_diff_CDF_integral_measure[[ik]] 
    subnational_region_F_diff_CDF_integral_measure_mat[,ik] = subnational_region_F_diff_CDF_integral_measure[[ik]] 
    rm(ik)    
}

subnational_region_M_diff_CDF_integral_measure_mat[,47] = c(rep(NA, 26), subnational_region_M_diff_CDF_integral_measure[[47]])
subnational_region_F_diff_CDF_integral_measure_mat[,47] = c(rep(NA, 26), subnational_region_F_diff_CDF_integral_measure[[47]])

