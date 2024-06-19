# Function to summarize medal counts for a given country
summarise_country_medals <- function(data, country_name) {
    country_data <- data %>%
        filter(Country == country_name) %>%
        group_by(Year, Sport, Event, Medal) %>%
        summarise(Medal_Count = n(), .groups = 'drop') %>%
        arrange(Year, Sport, Event)

    return(country_data)
}
