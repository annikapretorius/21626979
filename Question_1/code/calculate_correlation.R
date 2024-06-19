# Function to calculate the correlation between HBO top 25 names and top 25 baby names
calculate_correlation <- function(combined_df) {
    # Summarize the number of years each name stayed in the top 25 for baby names
    baby_names_persistence <- combined_df %>%
        group_by(Name_BabyNames) %>%
        summarize(BabyNamesYearsInTop25 = n_distinct(Year), .groups = 'drop')

    # Summarize the number of years each name stayed in the top 25 for HBO names
    hbo_names_persistence <- combined_df %>%
        group_by(Name_HBO) %>%
        summarize(HBONamesYearsInTop25 = n_distinct(Year), .groups = 'drop')

    # Combine the summaries by creating a common identifier
    combined_persistence <- full_join(baby_names_persistence, hbo_names_persistence, by = c("Name_BabyNames" = "Name_HBO"))

    # Calculate correlation
    correlation_value <- cor(combined_persistence$BabyNamesYearsInTop25, combined_persistence$HBONamesYearsInTop25, method = "spearman", use = "pairwise.complete.obs")

    return(list(combined_persistence = combined_persistence, correlation_value = correlation_value))
}
