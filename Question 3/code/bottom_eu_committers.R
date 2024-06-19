bottom_eu_committers <- function(summary_data, bottom_n = 5) {
    bottom_summary <- summary_data %>%
        arrange(Total_Commitment) %>%
        head(bottom_n) %>%
        select(Country, Total_Commitment, Military_Commitment, Total_Bilateral_Commitment)
    return(bottom_summary)
}
