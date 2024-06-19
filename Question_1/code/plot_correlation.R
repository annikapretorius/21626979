# Function to create a plot of the correlation
plot_correlation <- function(combined_persistence, correlation_value) {
    ggplot(combined_persistence, aes(x = BabyNamesYearsInTop25, y = HBONamesYearsInTop25)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE, color = "blue") +
        labs(title = paste("Correlation between Persistence of Baby Names and HBO Names: ", round(correlation_value, 2)),
             x = "Years in Top 25 Baby Names",
             y = "Years in Top HBO Rankings") +
        theme_minimal()
}
