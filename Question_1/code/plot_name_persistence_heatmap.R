
# Function to plot persistence heatmap of the most persistent baby names
plot_name_persistence_heatmap <- function(girl_df, boy_df) {

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

    #To verify the combined data before performing further operations.
    # The print statements are used to inspect the first few rows of the combined dataset.
    print("Data before pivoting:")
    print(head(top_25_names))

    # The replace_na function handles missing values by setting Presence to 0, and distinct removes any duplicate rows.
    top_25_names <- top_25_names %>%
        mutate(Year = as.integer(Year),
               Presence = 1) %>%
        replace_na(list(Presence = 0))

    # Remove duplicates if any
    top_25_names <- top_25_names %>% distinct()

    # Create a data frame indicating presence in top 25
    name_persistence <- top_25_names %>%
        group_by(Name, Gender) %>%
        summarize(TotalPresence = sum(Presence), .groups = 'drop') %>%
        arrange(desc(TotalPresence)) %>%
        group_by(Gender) %>%
        top_n(10, TotalPresence)

    # Expand the data frame for each year
    years <- seq(min(top_25_names$Year), max(top_25_names$Year))
    expanded_name_persistence <- name_persistence %>%
        crossing(Year = years) %>%
        left_join(top_25_names %>% select(Name, Gender, Year, Presence),
                  by = c("Name", "Gender", "Year")) %>%
        replace_na(list(Presence = 0))

    # The ggplot function is used to create the heatmap, with geom_tile to create the tiles for each year and name, and scale_fill_gradient to show the intensity of the presence.
    g2 <- expanded_name_persistence %>%
        ggplot(aes(x = Year, y = Name, fill = Presence)) +
        geom_tile(color = "white") +
        scale_fill_gradient(low = "white", high = "blue") +
        facet_wrap(~ Gender, scales = "free_y") +
        labs(title = "Persistence of Top Baby Names Over Time",
             x = "Year",
             y = "Name",
             fill = "Presence") +
        theme_minimal() +
        theme(axis.text.y = element_text(size = 8))  # Adjusted y-axis text size for readability

    return(g2)
}
