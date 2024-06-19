
#This function is used to identify the top 25 names for each year, separately for girls and boys.

rank_and_filter_top_25 <- function(df, count_col) {
    df %>%
        group_by(Year) %>%
        arrange(desc(!!sym(count_col))) %>%
        mutate(Rank = row_number()) %>%
        filter(Rank <= 25) %>%
        ungroup() %>%
        select(Year, Name, Proportion, Rank)
}
