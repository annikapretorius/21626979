# Function to count popular names per year
count_popular_names <- function(df) {
    df %>%
        group_by(Year) %>%
        summarise(PopularNamesCount = n())
}
