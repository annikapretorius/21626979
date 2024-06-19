summarise_allocations <- function(alloc_data) {
    eu_countries <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic",
                      "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary",
                      "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta",
                      "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia",
                      "Spain", "Sweden")

    # Filter EU countries
    eu_alloc <- alloc_data %>% filter(Country %in% eu_countries)

    # Summarize allocations for individual countries
    alloc_summary <- eu_alloc %>%
        group_by(Country) %>%
        summarise(
            Total_Allocation = sum(`Financial allocations($ billion)`, na.rm = TRUE),
            Humanitarian_Allocation = sum(`Humanitarian allocations($ billion)`, na.rm = TRUE),
            Military_Allocation = sum(`Military allocations($ billion)`, na.rm = TRUE),
            Total_Bilateral_Allocation = sum(`Total bilateral allocations($ billion)`, na.rm = TRUE)
        ) %>%
        arrange(desc(Total_Allocation))

    # Summarize allocations for EU(Commission and Council)
    eu_commission_summary <- alloc_summary %>%
        summarise(
            Country = "EU(Commission and Council)",
            Total_Allocation = sum(Total_Allocation),
            Humanitarian_Allocation = sum(Humanitarian_Allocation),
            Military_Allocation = sum(Military_Allocation),
            Total_Bilateral_Allocation = sum(Total_Bilateral_Allocation)
        )

    # Combine individual and EU(Commission and Council) summaries
    final_summary <- bind_rows(alloc_summary, eu_commission_summary)

    return(final_summary)
}
