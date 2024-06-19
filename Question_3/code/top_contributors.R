top_contributors <- function(summary_data, top_n = 5) {
    top_summary <- summary_data %>%
        arrange(desc(Total_Allocation)) %>%
        head(top_n) %>% select(Country, Total_Allocation, Military_Allocation,Total_Bilateral_Allocation)
    return(top_summary)
}
