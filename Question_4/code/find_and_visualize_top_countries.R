
# Function to find top performing countries based on a given metric and visualize the results
find_and_visualize_top_countries <- function(data, metric, top_n = 5) {
    # Find top performing countries
    top_countries <- data %>%
        arrange(desc(.data[[metric]])) %>%
        slice_head(n = top_n)

    # Plot top countries by the given metric
    p <- ggplot(top_countries, aes(x = reorder(Country, .data[[metric]]), y = .data[[metric]])) +
        geom_bar(stat = "identity", fill = ifelse(metric == "Medals_Per_Capita", "skyblue", "orange")) +
        labs(title = paste("Top", top_n, "Countries by", gsub("_", " ", metric)),
             x = "Country",
             y = gsub("_", " ", metric)) +
        theme_minimal()

    print(p)
}
