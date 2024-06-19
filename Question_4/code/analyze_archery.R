# The analyze_archery function processes the Summer Olympics data to filter Archery individual events,
# counts the medals won by each country, and visualizes the total medals in a bar plot. It categorizes sports as team or individual,
#adjusts for duplicate rows, and summarizes the data to provide insights into the performance of countries in Archery individual events.

analyze_archery <- function(summer_data) {

    # Define team sports and events
    team_sports <- c("Basketball", "Volleyball", "Hockey", "Football", "Handball",
                     "Rugby", "Waterpolo", "Rowing", "Softball", "Baseball", "Curling", "Ice Hockey",
                     "Bobsleigh", "Luge", "Biathlon")

    team_events <- c("Military Patrol", "Four-Man", "Curling", "Ice Hockey", "Pairs", "Two-Man", "Teams",
                     "Team", "4X5 KM Relay", "4X10KM Relay", "Team Sprint", "Team Pursuit", "4X10KM Relay",
                     "Combined (4 Events)", "Five-Man", "Relay", "Team", "Double", "Pairs")

    # Add Sport_Type to summer_data
    summer_data <- summer_data %>%
        mutate(Sport_Type = ifelse(Sport %in% team_sports, "Team", "Individual")) %>%
        mutate(Sport_Type = ifelse(grepl(paste(team_events, collapse = "|"), Event), "Team", Sport_Type))

    # Filter the dataset for Archery and individual events
    archery_individual <- summer_data %>%
        filter(Sport == "Archery" & grepl("Individual", Event))

    # Adjust medal counting to count one medal per team event
    adjusted_data <- archery_individual %>%
        distinct(Year, City, Sport, Athlete, Discipline, Event, Medal, Country, Sport_Type) %>%
        mutate(Medal_Count = ifelse(Sport_Type == "Team", 1, 1))

    # Summarize medal counts by country
    archery_medals_by_country <- adjusted_data %>%
        group_by(Country) %>%
        summarise(Total_Medals = sum(Medal_Count), .groups = 'drop') %>%
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
