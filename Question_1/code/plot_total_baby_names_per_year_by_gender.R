# Function to plot total baby names per year by gender
plot_total_baby_names_per_year_by_gender <- function(girl_names_df, boy_names_df) {

    # Calculate total count per year for females
    total_females_per_year <- girl_names_df %>%
        group_by(Year) %>%
        summarize(TotalCount = sum(Count), .groups = 'drop')

    # Calculate total count per year for males
    total_males_per_year <- boy_names_df %>%
        group_by(Year) %>%
        summarize(TotalCount = sum(Count), .groups = 'drop')

    # Combine the data for plotting
    total_counts_per_year <- total_females_per_year %>%
        rename(Females = TotalCount) %>%
        left_join(total_males_per_year %>% rename(Males = TotalCount), by = "Year") %>%
        gather(key = "Gender", value = "TotalCount", Females, Males)

    # Plot the data
    g <- total_counts_per_year %>%
        ggplot(aes(x = Year, y = TotalCount, color = Gender)) +
        geom_line(size = 1) +
        labs(title = "Total Count of Baby Names per Year by Gender",
             x = "Year",
             y = "Total Count",
             color = "Gender") +
        theme_minimal()

    return(g)
}

