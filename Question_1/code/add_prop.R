#This function is applied to both the girl and boy datasets to get the proportions of each name
#relative to the total number of births in that year and gender

add_prop <- function(df) {
    df %>%
        group_by(Year, Gender) %>%
        mutate(Proportion = Count / sum(Count)) %>%
        ungroup()
}
