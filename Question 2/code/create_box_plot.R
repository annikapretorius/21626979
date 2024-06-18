# Create a function to generate box plots for comparison
create_box_plot <- function(data, metric, title) {
    ggplot(data, aes(x = band, y = get(metric), fill = band)) +
        geom_boxplot() +
        labs(title = title, y = metric) +
        theme_minimal()
}
