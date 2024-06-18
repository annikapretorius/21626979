# Function to rank and filter top 25 names
rank_and_filter_top_25 <- function(df, count_col) {
    df %>%
        group_by(Year, Name) %>%
        summarize(TotalCount = sum(!!sym(count_col)), .groups = 'drop') %>%
        group_by(Year) %>%
        arrange(desc(TotalCount)) %>%
        mutate(Rank = row_number()) %>%
        filter(Rank <= 25) %>%
        ungroup() %>%
        select(Year, Name, TotalCount, Rank) %>%
        arrange(Year)

}
