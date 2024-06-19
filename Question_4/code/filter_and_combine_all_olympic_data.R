# Function to filter and combine datasets for countries doing well in both Olympics
filter_and_combine_all_olympic_data <- function(summer_data, winter_data) {
    # Combine Summer and Winter datasets without filtering
    combined_data <- bind_rows(summer_data, winter_data)

    # Define team sports and team events
    team_sports <- c("Basketball", "Volleyball", "Hockey", "Football", "Handball",
                     "Rugby", "Waterpolo", "Rowing", "Softball", "Baseball", "Curling", "Ice Hockey",
                     "Bobsleigh", "Luge", "Biathlon", "Ice Hockey", "Skeleton")

    team_events <- c("Military Patrol", "Four-Man", "Curling", "Ice Hockey", "Pairs", "Two-Man", "Teams",
                     "Team", "4X5 KM Relay", "4X10KM Relay", "Team Sprint", "Team Pursuit", "4X10KM Relay",
                     "Combined (4 Events)", "Five-Man", "Relay", "Team", "Double", "Pairs")

    # Classify Sport_Type for the combined dataset
    combined_data <- combined_data %>%
        mutate(Sport_Type = ifelse(Sport %in% team_sports, "Team", "Individual")) %>%
        mutate(Sport_Type = ifelse(grepl(paste(team_events, collapse = "|"), Event), "Team", Sport_Type))

    return(combined_data)
}
