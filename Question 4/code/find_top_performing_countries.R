# Function to find the top 5 performing countries in the Olympics
find_top_performing_countries <- function(data, top_n = 5) {
    # Aggregate medal counts by country
    total_medals_by_country <- data %>%
        group_by(Country) %>%
        summarise(Total_Medals = sum(Total_Medals, na.rm = TRUE), .groups = 'drop') %>%
        arrange(desc(Total_Medals))

    # Get the top performing countries
    top_performing_countries <- total_medals_by_country %>%
        slice_head(n = top_n)

    return(top_performing_countries)
}
