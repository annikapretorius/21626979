# Create a function to generate a heatmap of song attributes
combined_heatmap <- function(data, metrics, title) {
    # Summarize the data by calculating the mean for each attribute by band
    summary_data <- data %>%
        group_by(band) %>%
        summarise(across(all_of(metrics), mean, na.rm = TRUE)) %>%
        pivot_longer(cols = -band, names_to = "attribute", values_to = "value")

    ggplot(summary_data, aes(x = attribute, y = band, fill = value)) +
        geom_tile(color = "white") +
        labs(title = title, x = "Attributes", y = "Band") +
        theme_minimal(base_family = "sans") +
        theme(
            axis.text.x = element_text(size = 12, color = "black", angle = 45, hjust = 1),
            axis.text.y = element_text(size = 12, color = "black"),
            plot.title = element_text(size = 16, face = "bold", color = "black"),
            panel.grid.major = element_line(color = "grey80"),
            panel.grid.minor = element_line(color = "grey90"),
            legend.title = element_blank(),
            legend.position = "right"
        ) +
        scale_fill_gradient(low = "white", high = "darkred")
}
