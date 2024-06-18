top_eu_committers <- function(summary_data, top_n = 5) {
    top_summary <- summary_data %>%
        arrange(desc(Total_Commitment)) %>%
        head(top_n)%>% select(Country, Total_Commitment, Military_Commitment, Total_Bilateral_Commitment)
    return(top_summary)
}
