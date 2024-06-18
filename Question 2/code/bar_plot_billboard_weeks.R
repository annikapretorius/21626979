# Define the function to filter, summarize, and create the bar plot
bar_plot_billboard_weeks <- function(data) {

    # Summarize the total weeks on board for each band
    total_weeks <- billboard_filtered %>%
        group_by(band) %>%
        summarise(total_weeks = sum(weeks_on_board, na.rm = TRUE))

    # Create the bar plot
    ggplot(total_weeks, aes(x = band, y = total_weeks, fill = band)) +
        geom_bar(stat = "identity", width = 0.6, show.legend = FALSE) +  # Adjust bar width
        geom_text(aes(label = total_weeks), vjust = -0.5, color = "white", size = 5) +  # Add data labels
        labs(title = "Total Weeks in Billboard Top 100", y = "Total Weeks", x = "Band") +
        theme_minimal(base_family = "sans") +
        theme(
            axis.text.x = element_text(size = 12, face = "bold", color = "black"),
            axis.text.y = element_text(size = 12, face = "bold", color = "black"),
            plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
            plot.background = element_rect(fill = "#f5f5f5", color = NA),
            panel.background = element_rect(fill = "#f5f5f5", color = NA),
            panel.grid.major = element_line(color = "grey80"),
            panel.grid.minor = element_line(color = "grey90")
        ) +
        scale_fill_manual(values = c("Coldplay" = "#1f77b4", "Metallica" = "#ff7f0e"))  # Custom colors
}
