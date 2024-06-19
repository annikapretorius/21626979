# Function to calculate and combine top 25 names for baby names and HBO names
combine_top_25_names <- function(baby_names_df, hbo_names_df, baby_names_count_col, hbo_names_popularity_col) {
    # Helper function to rank and filter top 25 names
    rank_and_filter_top_25 <- function(df, count_col) {
        df %>%
            group_by(Year) %>%
            arrange(desc(!!sym(count_col))) %>%
            mutate(Rank = row_number()) %>%
            filter(Rank <= 25) %>%
            ungroup() %>%
            select(Year, Name, !!sym(count_col), Rank)
    }

    # Calculate rank correlation for each year
    top_25_baby_names <- rank_and_filter_top_25(baby_names_df, baby_names_count_col) %>%
        arrange(Year)
    top_25_hbo_names <- rank_and_filter_top_25(hbo_names_df, hbo_names_popularity_col) %>%
        arrange(Year)

    # Add a unique row identifier for proper join
    top_25_baby_names <- top_25_baby_names %>%
        group_by(Year) %>%
        mutate(RowID = row_number()) %>%
        ungroup()

    top_25_hbo_names <- top_25_hbo_names %>%
        group_by(Year) %>%
        mutate(RowID = row_number()) %>%
        ungroup()

    # Combine the data by Year and RowID
    combined_result <- full_join(top_25_baby_names, top_25_hbo_names, by = c("Year", "RowID"), suffix = c("_BabyNames", "_HBO"))

    # Select the relevant columns
    combined_result_final_top_25_names <- combined_result %>%
        select(Year, Name_BabyNames, Rank_BabyNames, Name_HBO, Rank_HBO)

    return(combined_result_final_top_25_names)
}
