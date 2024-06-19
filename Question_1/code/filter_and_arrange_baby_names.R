# Function to filter and arrange Baby_Names
filter_and_arrange_baby_names <- function(data, start_year = 1950, end_year = 2014) {
    data <- data %>%
        mutate(Year = as.numeric(Year)) %>%
        filter(Year >= start_year & Year <= end_year) %>%
        arrange(Year)
    return(data)
}
