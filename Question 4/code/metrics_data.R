
# Function to calculate metrics
metrics_data <- function(olympic_data, GDP) {

    # Rename columns in GDP dataset and select relevant columns
    GDP <- GDP %>%
        rename(country = Country, Country = Code) %>%
        select(Country, Population, `GDP per Capita`)

    # Adjust medal counting for team sports and summarize total medals per year for each country
    adjusted_data <- olympic_data %>%
        distinct(Year, City, Sport, Discipline, Event, Medal, Country, Sport_Type) %>%
        mutate(Medal_Count = ifelse(Sport_Type == "Team", 1, 1))

    summary_data <- adjusted_data %>%
        group_by(Country) %>%
        summarise(Total_Medals = sum(Medal_Count), .groups = 'drop')

    # Calculate metrics and handle NAs
    result <- summary_data %>%
        left_join(GDP, by = "Country") %>%
        replace_na(list(Population = 1, `GDP per Capita` = 1)) %>%
        mutate(
            Medals_Per_Capita = Total_Medals / Population,
            Medals_Per_GDP = Total_Medals / `GDP per Capita`
        )

    return(result)
}



