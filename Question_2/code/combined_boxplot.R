# Create a function to generate box plots for comparison
combined_boxplot <- function(data, metric, title) {
    ggplot(data, aes(x = band, y = get(metric), fill = band)) +
        geom_boxplot(outlier.shape = NA, color = "white") +  # Remove outlier points and add border color
        labs(title = title, y = metric, x = "Band") +
        theme_minimal(base_family = "sans") +
        theme(
            axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12, color = "white"),
            axis.text.y = element_text(size = 12, color = "white"),
            plot.title = element_text(size = 16, face = "bold", color = "white"),
            plot.background = element_rect(fill = "black"),
            panel.background = element_rect(fill = "black"),
            panel.grid.major = element_line(color = "gray30"),
            panel.grid.minor = element_line(color = "gray30"),
            legend.position = "none"
        ) +
        scale_fill_manual(values = c("Coldplay" = "skyblue", "Metallica" = "darkred"))
}

