
# Function to rank and filter top 25 names
rank_and_filter_top_25 <- function(df, count_col) {
    df %>%
        group_by(Year) %>%
        arrange(desc(!!sym(count_col))) %>%
        mutate(Rank = row_number()) %>%
        filter(Rank <= 25) %>%
        ungroup() %>%
        select(Year, Name, Proportion, Rank)
}
