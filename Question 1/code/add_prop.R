# Function to calculate proportion
add_prop <- function(df) {
    df %>%
        group_by(Year, Gender) %>%
        mutate(Proportion = Count / sum(Count)) %>%
        ungroup()
}
