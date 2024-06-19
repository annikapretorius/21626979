# Define the summarize_commitments function
summarise_commitments <- function(commit_data) {

    eu_countries <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic",
                      "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary",
                      "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta",
                      "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia",
                      "Spain", "Sweden")

    eu_commit <- commit_data %>% filter(Country %in% eu_countries)

    commit_summary <- eu_commit %>%
        group_by(Country) %>%
        summarise(
            Total_Commitment = sum(`Financial commitments($ billion)`, na.rm = TRUE),
            Humanitarian_Commitment = sum(`Humanitarian commitments($ billion)`, na.rm = TRUE),
            Military_Commitment = sum(`Military commitments($ billion)`, na.rm = TRUE),
            Total_Bilateral_Commitment = sum(`Total bilateral commitments($ billion)`, na.rm = TRUE)
        ) %>%
        arrange(desc(Total_Commitment))

    eu_commission_summary <- commit_summary %>%
        summarise(
            Country = "EU(Commission and Council)",
            Total_Commitment = sum(Total_Commitment),
            Humanitarian_Commitment = sum(Humanitarian_Commitment),
            Military_Commitment = sum(Military_Commitment),
            Total_Bilateral_Commitment = sum(Total_Bilateral_Commitment)
        )

    final_summary <- bind_rows(commit_summary, eu_commission_summary)
    return(final_summary)
}
