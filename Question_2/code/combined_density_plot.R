#The function uses ggplot2 to plot the density distribution of the specified metric for each band. The aes function maps the metric to the x-axis and uses the band variable to fill the density plot with different colors.
#The geom_density function creates the density plot with a specified transparency (alpha = 0.6).

combined_density_plot <- function(data, metric, title) {
    ggplot(data, aes(x = get(metric), fill = band)) +
        geom_density(alpha = 0.6) +
        labs(title = title, x = metric, y = "Density") +
        theme_minimal(base_family = "sans") +
        theme(
            axis.text.x = element_text(size = 12, color = "black"),
            axis.text.y = element_text(size = 12, color = "black"),
            plot.title = element_text(size = 16, face = "bold", color = "black"),
            panel.grid.major = element_line(color = "grey80"),
            panel.grid.minor = element_line(color = "grey90"),
            legend.title = element_blank(),
            legend.position = "top"
        ) +
        scale_fill_manual(values = c("Coldplay" = "skyblue", "Metallica" = "darkred"))
}

