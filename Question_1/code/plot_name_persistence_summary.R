# Function to summarize and plot the persistence of baby names
plot_name_persistence_summary <- function(girl_df, boy_df) {

    # Function to calculate proportion
    add_prop <- function(df) {
        df %>%
            group_by(Year, Gender) %>%
            mutate(Proportion = Count / sum(Count)) %>%
            ungroup()
    }

    # Function to rank and filter top 25 names
    rank_and_filter_top_25 <- function(df, count_col) {
        df %>%
            group_by(Year) %>%
            arrange(desc(!!sym(count_col))) %>%
            mutate(Rank = row_number()) %>%
            filter(Rank <= 25) %>%
            ungroup() %>%
            select(Year, Name, Proportion, Rank)
    }

    # Calculate proportion of babies given a certain name for females and males
    girl_df_prop <- add_prop(girl_df)
    boy_df_prop <- add_prop(boy_df)

    # Rank and filter top 25 names for females and males
    top_25_girl_names <- rank_and_filter_top_25(girl_df_prop, "Count") %>%
        mutate(Gender = "F")
    top_25_boy_names <- rank_and_filter_top_25(boy_df_prop, "Count") %>%
        mutate(Gender = "M")

    # Combine the data frames
    top_25_names <- bind_rows(top_25_girl_names, top_25_boy_names)

    # Ensure correct data types and handle missing values explicitly
    top_25_names <- top_25_names %>%
        mutate(Year = as.integer(Year),
               Presence = 1) %>%
        replace_na(list(Presence = 0))

    # Remove duplicates if any
    top_25_names <- top_25_names %>% distinct()

    # Summarize the number of years each name stayed in the top 25
    name_persistence_summary <- top_25_names %>%
        group_by(Name, Gender) %>%
        summarize(BabyNamesYearsInTop25 = sum(Presence), .groups = 'drop') %>%
        arrange(desc(BabyNamesYearsInTop25))

    # Filter the name_persistence_summary dataframe to include only the most persistent names
    filtered_summary <- name_persistence_summary %>%
        group_by(Gender) %>%
        top_n(10, BabyNamesYearsInTop25)

    # Bar plot to show the number of years each name stayed in the top 25
    g3 <- filtered_summary %>% ggplot(aes(x = reorder(Name, BabyNamesYearsInTop25), y = BabyNamesYearsInTop25, fill = Gender)) +
        geom_bar(stat = "identity", position = "dodge") +
        coord_flip() +
        labs(title = "Number of Years in Top 25 Baby Names",
             x = "Name",
             y = "Years in Top 25",
             fill = "Gender") +
        theme_minimal() +
        theme(axis.text.y = element_text(size = 8))

    return(g3)
}
