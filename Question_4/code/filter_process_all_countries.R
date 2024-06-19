filter_process_all_countries <- function(summer_data) {
    # Define team sports and team events
    team_sports <- c("Basketball", "Volleyball", "Hockey", "Football", "Handball",
                     "Rugby", "Waterpolo", "Rowing", "Softball", "Baseball", "Curling", "Ice Hockey",
                     "Bobsleigh", "Luge", "Biathlon")

    team_events <- c("Military Patrol", "Four-Man", "Curling", "Ice Hockey", "Pairs", "Two-Man", "Teams",
                     "Team", "4X5 KM Relay", "4X10KM Relay", "Team Sprint", "Team Pursuit", "4X10KM Relay",
                     "Combined (4 Events)", "Five-Man", "Relay", "Team", "Double", "Pairs")

    # Add the 'Sport_Type' column
    summer_data <- summer_data %>%
        mutate(Sport_Type = ifelse(Sport %in% team_sports, "Team", "Individual")) %>%
        mutate(Sport_Type = ifelse(grepl(paste(team_events, collapse = "|"), Event), "Team", Sport_Type))

    # Adjust medal counting to count one medal per team event
    adjusted_data <- summer_data %>%
        distinct(Year, City, Sport, Discipline, Event, Medal, Country, Sport_Type) %>%
        mutate(Medal_Count = ifelse(Sport_Type == "Team", 1, 1))

    # Aggregate medal counts by country
    medal_counts <- adjusted_data %>%
        group_by(Country, Medal) %>%
        summarise(Total_Medals = sum(Medal_Count), .groups = 'drop')

    # Pivot to get total medals per country
    total_medals <- medal_counts %>%
        pivot_wider(names_from = Medal, values_from = Total_Medals, values_fill = list(Total_Medals = 0)) %>%
        mutate(Total_Medals = Gold + Silver + Bronze)

    return(total_medals)
}
