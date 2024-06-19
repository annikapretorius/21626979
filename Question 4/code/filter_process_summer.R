# Main function
filter_process_summer <- function(gdp_data, summer_data) {

    # Helper functions
    get_india_gdp <- function(gdp_data) {
        gdp_data %>%
            filter(Country == "India") %>%
            pull(`GDP per Capita`)
    }

    # Get India's GDP per Capita
    India_GDP <- get_india_gdp(gdp_data)

    find_similar_sized_economies <- function(gdp_data, India_GDP, top_n = 10) {
        gdp_data %>%
            mutate(GDP_Difference = abs(`GDP per Capita` - India_GDP)) %>%
            arrange(GDP_Difference) %>%
            filter(Country != "India") %>%
            head(top_n)
    }

    filter_emerging_markets <- function(gdp_data) {
        emerging_markets <- c("Brazil", "Russia", "India", "China", "South Africa",
                              "Mexico", "Indonesia", "Nigeria", "Turkey")
        gdp_data %>%
            filter(Country %in% emerging_markets)
    }

    filter_south_american_countries <- function(gdp_data) {
        south_american_countries <- c("Argentina", "Brazil", "Chile", "Colombia", "Peru", "Uruguay")
        gdp_data %>%
            filter(Country %in% south_american_countries)
    }

    # Create a mapping of country codes to names
    country_code_to_name <- c("RSA" = "South Africa", "USA" = "United States", "GBR" = "United Kingdom",
                              "RUS" = "Russia", "CHN" = "China", "BRA" = "Brazil", "IND" = "India",
                              "MEX" = "Mexico", "INA" = "Indonesia", "NGR" = "Nigeria", "TUR" = "Turkey",
                              "ARG" = "Argentina", "CHI" = "Chile", "COL" = "Colombia", "PER" = "Peru",
                              "URU" = "Uruguay")

    # Standardize country names
    gdp_data$Country <- recode(gdp_data$Country, !!!country_code_to_name)
    summer_data$Country <- recode(summer_data$Country, !!!country_code_to_name)

    # Find relevant countries
    similar_sized_economies <- find_similar_sized_economies(gdp_data, India_GDP)
    emerging_gdp_data <- filter_emerging_markets(gdp_data)
    south_american_gdp_data <- filter_south_american_countries(gdp_data)

    # Combine and filter
    combined_countries <- bind_rows(similar_sized_economies, emerging_gdp_data, south_american_gdp_data) %>%
        distinct(Country, .keep_all = TRUE)

    filtered_summer_olympics <- summer_data %>%
        filter(Country %in% combined_countries$Country)

    team_sports <- c("Basketball", "Volleyball", "Hockey", "Football", "Handball",
                     "Rugby", "Waterpolo", "Rowing", "Softball", "Baseball", "Curling", "Ice Hockey",
                     "Bobsleigh", "Luge", "Biathlon")

    team_events <- c("Military Patrol", "Four-Man", "Curling", "Ice Hockey", "Pairs", "Two-Man", "Teams",
                     "Team", "4X5 KM Relay", "4X10KM Relay", "Team Sprint", "Team Pursuit", "4X10KM Relay",
                     "Combined (4 Events)", "Five-Man", "Relay", "Team", "Double", "Pairs")

    summer_data <- summer_data %>%
        mutate(Sport_Type = ifelse(Sport %in% team_sports, "Team", "Individual")) %>%
        mutate(Sport_Type = ifelse(grepl(paste(team_events, collapse = "|"), Event), "Team", Sport_Type))


    # Adjust medal counting to count one medal per team event
    adjusted_data <- filtered_summer_olympics %>%
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
