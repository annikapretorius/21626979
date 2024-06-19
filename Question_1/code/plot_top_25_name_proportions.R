# Function to plot the proportion of top 25 baby names over time
plot_top_25_name_proportions <- function(girl_names_df, boy_names_df) {


    # Calculate proportion of babies given a certain name for females and males
    girl_names_df_prop <- add_prop(girl_names_df)
    boy_names_df_prop <- add_prop(boy_names_df)

    # Rank and filter top 25 names for females and males
    top_25_girl_names <- rank_and_filter_top_25(girl_names_df_prop, "Count")
    top_25_boy_names <- rank_and_filter_top_25(boy_names_df_prop, "Count")

    #The bind_rows function combines the datasets, and the mutate function adds a Gender column to differentiate between female and male names.

    top_25_names <- bind_rows(top_25_girl_names, top_25_boy_names) %>%
        mutate(Gender = ifelse(Name %in% top_25_girl_names$Name, "Female", "Male"))

    # The ggplot function is used to create the plot, with geom_line to draw lines for each name's proportion over the years. The facet_wrap function creates separate plots for females and males.
    g <- ggplot(top_25_names, aes(x = Year, y = Proportion, color = Name, group = Name)) +
        geom_line(size = 1) +
        facet_wrap(~ Gender, scales = "free_y") +
        labs(title = "Proportion of Top 25 Baby Names Over Time",
             x = "Year",
             y = "Proportion",
             color = "Name") +
        theme_minimal() +
        theme(legend.position = "none")

    return(g)
}
