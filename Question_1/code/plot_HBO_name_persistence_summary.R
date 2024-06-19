# Function to analyze and plot the persistence of HBO names
plot_HBO_name_persistence_summary <- function(combined_df) {

    # The function selects the relevant columns (Year, Name_HBO, and Rank_HBO), renames them for consistency, filters out rows with missing names, and ensures there are no duplicate entries.
    #It also adds a Presence column to indicate the presence of a name in the top 25.
    top_25_HBO_names <- combined_df %>%
        select(Year, Name_HBO, Rank_HBO) %>%
        rename(Name = Name_HBO, Rank = Rank_HBO) %>%
        filter(!is.na(Name)) %>%
        distinct() %>%
        mutate(Presence = 1)

    # Create a data frame indicating presence in top 25
    name_persistence_HBO <- top_25_HBO_names %>%
        select(Year, Name, Presence) %>%
        group_by(Name) %>%
        complete(Year = seq(min(Year), max(Year), by = 1)) %>%
        replace_na(list(Presence = 0)) %>%
        ungroup()

    # Summarize the number of years each name stayed in the top 25
    HBO_name_persistence_summary <- name_persistence_HBO %>%
        group_by(Name) %>%
        summarize(HBONamesYearsInTop25 = sum(Presence), .groups = 'drop') %>%
        arrange(desc(HBONamesYearsInTop25))

    # Filter the summary dataframe to include only the most persistent names
    filtered_HBO_summary <- HBO_name_persistence_summary %>%
        top_n(10, HBONamesYearsInTop25)

    # The function uses ggplot to create a bar plot, with the names on the y-axis and the number of years they stayed in the top 25 on the x-axis.
    #The bars are filled with steelblue color, and the plot is formatted with a minimal theme for clarity.
    g4 <- filtered_HBO_summary %>% ggplot(aes(x = reorder(Name, HBONamesYearsInTop25), y = HBONamesYearsInTop25)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        coord_flip() +
        labs(title = "Number of Years HBO Names Stayed in Top Rankings",
             x = "HBO Name",
             y = "Years in Top Rankings") +
        theme_minimal() +
        theme(axis.text.y = element_text(size = 8))

    return(g4)
}
