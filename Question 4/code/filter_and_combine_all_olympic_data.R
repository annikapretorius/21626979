# Function to filter and combine datasets for countries doing well in both Olympics
filter_and_combine_all_olympic_data <- function(summer_data, winter_data) {

        # Combine Summer and Winter datasets without filtering
        combined_data <- bind_rows(summer_data, winter_data)

        team_sports <- c("Basketball", "Volleyball", "Hockey", "Football", "Handball",
                         "Rugby", "Waterpolo", "Rowing", "Softball", "Baseball", "Curling", "Ice Hockey",
                         "Bobsleigh", "Luge", "Biathlon")

        team_events <- c("Military Patrol", "Four-Man", "Curling", "Ice Hockey", "Pairs", "Two-Man", "Teams",
                         "Team", "4X5 KM Relay", "4X10KM Relay", "Team Sprint", "Team Pursuit", "4X10KM Relay",
                         "Combined (4 Events)", "Five-Man", "Relay", "Team", "Double", "Pairs")

        summer_data <- summer_data %>%
            mutate(Sport_Type = ifelse(Sport %in% team_sports, "Team", "Individual")) %>%
            mutate(Sport_Type = ifelse(grepl(paste(team_events, collapse = "|"), Event), "Team", Sport_Type))


    # Add the 'Sport Type' column for winter to the data frame
    team_sports <- c("Ice Hockey", "Bobsleigh", "Curling", "Luge", "Skeleton")

    team_events <- c("Military Patrol", "Four-Man", "Curling", "Ice Hockey", "Pairs", "Two-Man",
                     "Teams", "Team", "4X5 KM Relay", "4X10KM Relay", "Team Sprint",
                     "Team Pursuit", "Combined (4 Events)", "Five-Man")

    # Classify Sport_Type
    winter_data <- winter_data %>%
        mutate(Sport_Type = ifelse(Sport %in% team_sports, "Team", "Individual")) %>%
        mutate(Sport_Type = ifelse(grepl(paste(team_events, collapse = "|"), Event), "Team", Sport_Type)) %>%
        mutate(Sport_Type = ifelse(grepl("Relay|Team|Double|Pairs", Event), "Team", Sport_Type))

    return(combined_data)
}
