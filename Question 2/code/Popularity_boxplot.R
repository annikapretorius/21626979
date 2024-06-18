# Define the function to create the popularity box plot for the top 20 albums
popularity_boxplot <- function(data, xaxis_size = 10, xaxis_rows = 3) {

    # Summarize the median popularity and create a summary data frame
    dfplot <- data %>%
        group_by(album) %>%
        summarise(popularity = median(popularity, na.rm = TRUE)) %>%
        arrange(desc(popularity)) %>%
        slice_head(n = 20)  # Select the top 20 most popular albums

    # Filter the dataset to include only the top 20 most popular albums
    data <- data %>%
        filter(album %in% dfplot$album)

    # Adjust the order of the albums based on median popularity
    order <- dfplot %>% arrange(popularity) %>% pull(album)
    data <- data %>%
        mutate(album = factor(album, levels = order))

    # Create a color palette with sufficient colors
    palette_size <- length(unique(data$album))
    color_palette <- colorRampPalette(brewer.pal(8, "Dark2"))(palette_size)

    # Create the box plot for popularity by album with a dark theme
    g <- ggplot(data, aes(x = album, y = popularity, fill = album)) +
        geom_boxplot(outlier.color = "white", outlier.size = 1.5) +  # Adjust outlier points
        labs(title = "Popularity by Album", y = "Popularity", x = "Album", caption = "Data source: Spotify") +
        theme_minimal(base_family = "sans") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, color = "white", size = xaxis_size),
              axis.text.y = element_text(color = "white", size = 10),
              plot.title = element_text(color = "white", size = 14),
              plot.background = element_rect(fill = "black"),
              panel.background = element_rect(fill = "black"),
              panel.grid.major = element_line(color = "gray30"),
              panel.grid.minor = element_line(color = "gray30"),
              legend.position = "none") +
        scale_fill_manual(values = color_palette) +
        coord_flip()  # Flip coordinates for better readability

    return(g)
}

