# stacking the results on top of each other.

library(readr)
library(dplyr)

files <- list.files("C:/Users/shokouhinid/sstp_2023_Wages/Dr. wages_code_final results/final_results_01232025/target_0.3/DIP/N20", 
                    pattern = "\\.csv$", 
                    full.names = TRUE)

combined_df <- files %>%
  lapply(read_csv) %>%
  bind_rows()
write.csv(combined_df, file = "C:/Users/shokouhinid/sstp_2023_Wages/Dr. wages_code_final results/final_results_01232025/target_0.3/DIP/N20/combined_cdp.csv")
