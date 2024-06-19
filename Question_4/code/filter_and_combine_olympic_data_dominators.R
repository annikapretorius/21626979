# Function to filter and combine datasets for countries doing well in both Olympics
filter_and_combine_olympic_data_dominators<- function(summer_data, winter_data) {
    # List of countries doing well in both Olympics
    countries_doing_well_in_both <- c("USA", "URS", "GER", "SWE")

    # Filter Summer dataset
    filtered_summer <- summer_data %>%
        filter(Country %in% countries_doing_well_in_both)

    # Filter Winter dataset
    filtered_winter <- winter_data %>%
        filter(Country %in% countries_doing_well_in_both)

    # Combine filtered datasets
    combined_data <- bind_rows(filtered_summer, filtered_winter)

    # Add the 'Sport Type' column to the combined data frame
    team_sports <- c("Basketball", "Volleyball", "Hockey", "Football", "Handball",
                     "Rugby", "Water Polo", "Rowing", "Softball", "Baseball", "Curling", "Ice Hockey",
                     "Bobsleigh", "Luge", "Biathlon")

    team_events <- c("Military Patrol", "Four-Man", "Curling", "Ice Hockey", "Pairs", "Two-Man", "Teams",
                     "Team", "4X5 KM Relay", "4X10KM Relay", "Team Sprint", "Team Pursuit", "4X10KM Relay",
                     "Combined (4 Events)", "Five-Man", "Relay", "Team", "Double", "Pairs")

    combined_data <- combined_data %>%
        mutate(Sport_Type = ifelse(Sport %in% team_sports, "Team", "Individual")) %>%
        mutate(Sport_Type = ifelse(grepl(paste(team_events, collapse = "|"), Event), "Team", Sport_Type))

    return(combined_data)
}
