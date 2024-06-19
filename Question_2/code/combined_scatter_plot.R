#he function uses ggplot2 to plot the relationship between the specified x and y metrics for each band.
#The aes function maps the x_metric to the x-axis and the y_metric to the y-axis, with the band variable used to color the points.

combined_scatter_plot <- function(data, x_metric, y_metric, title) {
    ggplot(data, aes(x = get(x_metric), y = get(y_metric), color = band)) +
        geom_point(alpha = 0.6) +
        labs(title = title, x = x_metric, y = y_metric) +
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
        scale_color_manual(values = c("Coldplay" = "darkblue", "Metallica" = "darkred"))
}
