#The plot_top_humanitarian_allocators function processes the alloc_summary data frame to identify and visualize the top n EU countries based on their humanitarian aid allocations.
#By sorting the data and creating a bar plot, it provides a clear and focused view of the top humanitarian aid contributors, highlighting their contributions.

plot_top_humanitarian_allocators <- function(alloc_summary, top_n = 10) {
    top_allocators <- alloc_summary %>%
        arrange(desc(Humanitarian_Allocation)) %>%
        head(top_n)

    ggplot(top_allocators, aes(x = reorder(Country, Humanitarian_Allocation), y = Humanitarian_Allocation, fill = Country)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(title = "Top Humanitarian Aid Allocators",
             x = "Country",
             y = "Humanitarian Allocation ($ Billion)") +
        theme_minimal() +
        theme(legend.position = "none") # Remove legend if not needed
}
