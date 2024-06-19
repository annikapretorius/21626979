# Function to create a time series plot of medals over the years
create_time_series_plot <- function(data) {
    # Adjust medal counting to count one medal per team event
    adjusted_data <- data %>%
        distinct(Year, City, Sport, Discipline, Event, Medal, Country, Sport_Type) %>%
        mutate(Medal_Count = ifelse(Sport_Type == "Team", 1, 1))

    # Summarize the total medals per year for each country
    summary_data <- adjusted_data %>%
        group_by(Year, Country) %>%
        summarise(Total_Medals = sum(Medal_Count), .groups = 'drop')

    # Create the time series plot
    ggplot(summary_data, aes(x = Year, y = Total_Medals, color = Country)) +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "The Countries that Dominate in the Summer and Winter Olympics",
             x = "",
             y = "Total Medals",
             color = "Country") +
        theme_minimal() +
        theme(
            plot.title = element_text(hjust = 0.5, size = 16),
            axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 14),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 12)
        )
}
