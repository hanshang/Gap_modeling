# Gap_modeling
Subnational life-table death counts based on gender, region and double gaps

1. load_packages.R: load R packages
2. read_data_national.R: read Japanese national life-table death counts
3. read_data_subnational.R: read Japanese subnational life-table death counts
4. gender_gap_aux.R: internal functions for evaluating forecast accuracy based on gender gap
5. region_gap_aux.R: internal functions for evaluating forecast accuracy based on regional gap
6. double_gap_aux.R: internal functions for evaluating forecast accuracy based on double gap
7. gender_gap_national.R: 20-years-ahead out-of-sample national forecasts based on gender gap
8. gender_gap_subnational.R: 20-years-ahead out-of-sample subnational forecasts based on gender gap
9. regional_gap_subnational.R: 20-years-ahead out-of-sample subnational forecasts based on regional gap
10. double_gap_subnational.R: 20-years-ahead out-of-sample subnational forecasts based on double gap
11. gender_gap_national_compar_horizon_15.R: point forecast evaluation based on gender gap
12. region_gap_national_compar_horizon_15.R: point forecast evaluation based on regional gap
13. double_gap_national_compar_horizon_15.R: point forecast evaluation based on double gap
14. gender_gap_compar_compar_horizon_15_interval.R: interval forecast evaluation based on gender gap
15. region_gap_national_compar_horizon_15_interval.R: interval forecast evaluation based on regional gap
16. double_gap_national_compar_horizon_15_interval.R: interval forecast evaluation based on double gap
17. summary_point.R: result summary of point forecast accuracy comparison
18. summary_interval.R: result summary of interval forecast accuracy comparison
19. save_function.R: internal function for saving figures
