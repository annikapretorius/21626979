#The popularity_boxplot function is designed to create a visually appealing box plot that compares the popularity of the top 20 albums based on their median popularity scores.

popularity_boxplot <- function(data, xaxis_size = 10, xaxis_rows = 3) {

    # The function groups the data by album, calculates the median popularity for each album, arranges the albums in descending order of median popularity, and selects the top 20 albums.
    # This helps in focusing on the most popular albums for the visualization.
    dfplot <- data %>%
        group_by(album) %>%
        summarise(popularity = median(popularity, na.rm = TRUE)) %>%
        arrange(desc(popularity)) %>%
        slice_head(n = 20)  # Select the top 20 most popular albums

    #The function filters the original dataset to keep only the rows corresponding to the top 20 albums identified in the previous step.
    data <- data %>%
        filter(album %in% dfplot$album)

    # The function arranges the albums in the order of their median popularity to ensure a logical and visually appealing presentation in the plot.
    order <- dfplot %>% arrange(popularity) %>% pull(album)
    data <- data %>%
        mutate(album = factor(album, levels = order))

    # Creates a color palette with lots of colours
    palette_size <- length(unique(data$album))
    color_palette <- colorRampPalette(brewer.pal(8, "Dark2"))(palette_size)

    # The function uses ggplot2 to create a box plot that displays the distribution of popularity scores for each album.
    #It applies a dark theme for visual aesthetics, just like in the question, and includes labels and captions, and adjusts the appearance of various plot elements to enhance readability.
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

