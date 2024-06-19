# Function to analyze top countries' performance in Archery individual events by gender
analyze_top_countries_gender_performance <- function(summer_data) {

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
        distinct(Year, City, Sport, Athlete, Discipline, Event, Medal, Country, Gender, Sport_Type) %>%
        mutate(Medal_Count = ifelse(Sport_Type == "Team", 1, 1))

    # Summarize the total medals per country by year and gender
    summary_data <- adjusted_data %>%
        group_by(Year, Country, Gender) %>%
        summarise(Total_Medals = sum(Medal_Count), .groups = 'drop') %>%
        arrange(desc(Total_Medals))

    # Find top 5 countries
    top_countries <- summary_data %>%
        group_by(Country) %>%
        summarise(Total_Medals = sum(Total_Medals)) %>%
        arrange(desc(Total_Medals)) %>%
        slice_head(n = 6) %>%
        pull(Country)

    # Filter data for top 5 countries
    top_countries_data <- summary_data %>%
        filter(Country %in% top_countries)

    # Visualize the performance by gender within the top 5 countries over the years
    plot6 <- ggplot(top_countries_data, aes(x = Year, y = Total_Medals, color = Gender, group = interaction(Country, Gender))) +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        facet_wrap(~ Country, scales = "free_y") +
        labs(title = "Performance in Archery Individual Events by Gender",
             subtitle = "Comparing male and female performance within the top 5 countries",
             x = "Year",
             y = "Total Medals",
             color = "Gender") +
        theme_minimal() +
        theme(
            plot.title = element_text(hjust = 0.5, size = 16),
            plot.subtitle = element_text(hjust = 0.5, size = 12),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 12)
        )

    print(plot6)
}
