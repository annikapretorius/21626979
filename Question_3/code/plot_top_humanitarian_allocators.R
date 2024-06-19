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
