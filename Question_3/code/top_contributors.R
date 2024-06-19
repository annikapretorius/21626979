#The top_contributors function processes the summary_data data frame to identify and return the top n EU countries based on their total financial allocations.
#By sorting the data and selecting specific columns, it provides a clear and focused view of the top contributors, highlighting the extent of their financial aid.

top_contributors <- function(summary_data, top_n = 5) {
    top_summary <- summary_data %>%
        arrange(desc(Total_Allocation)) %>%
        head(top_n) %>% select(Country, Total_Allocation, Military_Allocation,Total_Bilateral_Allocation)
    return(top_summary)
}
