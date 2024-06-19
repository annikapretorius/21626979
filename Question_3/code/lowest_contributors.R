lowest_contributors <- function(summary_data, bottom_n = 5) {
    bottom_summary <- summary_data %>%
        arrange(Total_Allocation) %>%
        head(bottom_n) %>%
        select(Country, Total_Allocation, Military_Allocation, Total_Bilateral_Allocation)
    return(bottom_summary)
}
