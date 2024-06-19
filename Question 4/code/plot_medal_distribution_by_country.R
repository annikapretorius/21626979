# Function to plot medal distribution by top 5 countries in a pie chart
plot_medal_distribution_by_country <- function(summer_data) {

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

    # Summarize the total medals per country
    archery_medals_by_country <- adjusted_data %>%
        group_by(Country) %>%
        summarise(Total_Medals = sum(Medal_Count), .groups = 'drop') %>%
        arrange(desc(Total_Medals)) %>%
        head(5)  # Get top 5 countries

    # Calculate percentages for labels
    archery_medals_by_country <- archery_medals_by_country %>%
        mutate(Percentage = Total_Medals / sum(Total_Medals) * 100,
               Label = paste0(Country, " (", sprintf("%.1f", Percentage), "%)"))

    # Visualize medal distribution by country in a pie chart
    plot3 <- ggplot(archery_medals_by_country, aes(x = "", y = Total_Medals, fill = Country)) +
        geom_bar(width = 1, stat = "identity") +
        coord_polar("y") +
        labs(title = "Medal Distribution by Top 5 Countries in Archery Individual Events",
             subtitle = "Between the time period 1900 to 2012",
             x = "",
             y = "") +
        theme_minimal() +
        theme(axis.text.x = element_blank(),
              axis.ticks = element_blank(),
              panel.grid = element_blank(),
              plot.title = element_text(hjust = 0.5, size = 15),
              plot.subtitle = element_text(hjust = 0.5, size = 12),
              legend.title = element_blank()) +
        geom_text(aes(label = Label), position = position_stack(vjust = 0.5))

    print(plot3)
}


