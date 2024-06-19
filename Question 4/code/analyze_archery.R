# Function to analyze Archery individual events
analyze_archery <- function(data) {

    data <- add_sport_type(data)

    # Adjust medal counting to count one medal per team event
    adjusted_data <- data %>%
        distinct(Year, City, Sport, Athlete, Discipline, Event, Medal, Country, Sport_Type) %>%
        mutate(Medal_Count = ifelse(Sport_Type == "Team", 1, 1))

    # Summarize the total medals per year for each country
    summary_data <- adjusted_data %>%
        group_by(Year, Country, Sport, Event, Athlete, Medal) %>%
        summarise(Total_Medals = sum(Medal_Count), .groups = 'drop')

    # Filter the dataset for Archery and individual events
    archery_individual <- summary_data %>%
        filter(Sport == "Archery" & grepl("Individual", Event))

    # Summarize medal counts by country
    archery_medals_by_country <- archery_individual %>%
        group_by(Country) %>%
        summarise(Total_Medals = sum(Total_Medals)) %>%
        arrange(desc(Total_Medals))


    # Visualize medal counts by country
    plot1 <- ggplot(archery_medals_by_country, aes(x = reorder(Country, -Total_Medals), y = Total_Medals, fill = Country)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(title = "Total Medals in Archery Individual Events by Country",
             x = "Country",
             y = "Total Medals") +
        theme_minimal()

     print(plot1)
}
