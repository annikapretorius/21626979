# Function to plot the persistence heatmap of the most persistent HBO names
plot_HBO_name_persistence <- function(df) {
    #The function selects the relevant columns (Year, Name_HBO, and Rank_HBO), renames them for consistency, filters out rows with missing names, and ensures there are no duplicate entries.
    #It also adds a Presence column to indicate the presence of a name in the top 25.
    top_25_HBO_names <- df %>%
        select(Year, Name_HBO, Rank_HBO) %>%
        rename(Name = Name_HBO, Rank = Rank_HBO) %>%
        filter(!is.na(Name)) %>%
        distinct()

    # The function ensures each name has an entry for every year within the range, even if it was not in the top 25 for that year.
    #It then replaces missing values in the Presence column with 0.
    top_25_HBO_names <- top_25_HBO_names %>%
        mutate(Presence = 1) %>%
        select(Year, Name, Presence)

    #  The function groups the data by Name and sums the Presence column to get the total number of years each name was in the top 25.
    # It then arranges the names in descending order of their total presence and selects the top 10 most persistent names.
    name_persistence_HBO <- top_25_HBO_names %>%
        group_by(Name) %>%
        summarize(TotalPresence = sum(Presence), .groups = 'drop') %>%
        arrange(desc(TotalPresence)) %>%
        top_n(10, TotalPresence)

    # The function creates a sequence of years from the minimum to the maximum year in the dataset and ensures each name has an entry for every year.
    #It then joins this expanded data with the top 25 names data and replaces missing values in the Presence column with 0.
    years <- seq(min(top_25_HBO_names$Year), max(top_25_HBO_names$Year))
    expanded_name_persistence <- name_persistence_HBO %>%
        crossing(Year = years) %>%
        left_join(top_25_HBO_names %>% select(Name, Year, Presence),
                  by = c("Name", "Year")) %>%
        replace_na(list(Presence = 0))

    # he function uses ggplot to create a heatmap, with the years on the x-axis and the names on the y-axis. The color gradient represents the presence of the names, with darker shades indicating more persistent names.
    #The plot is formatted with a minimal theme for clarity.
    g4 <- expanded_name_persistence %>%
        ggplot(aes(x = Year, y = Name, fill = Presence)) +
        geom_tile(color = "white") +
        scale_fill_gradient(low = "white", high = "blue") +
        labs(title = "Persistence of Top HBO Names Over Time",
             x = "Year",
             y = "Name",
             fill = "Presence") +
        theme_minimal() +
        theme(axis.text.y = element_text(size = 8))

    # Return the plot
    return(g4)
}
