#The plot_top_humanitarian_committers function processes the commit_summary data frame to identify and visualize the top n EU countries based on their humanitarian aid commitments.
#By sorting the data and creating a bar plot, it provides a clear and focused view of the top humanitarian aid committers, highlighting their contributions.

plot_top_humanitarian_committers <- function(commit_summary, top_n = 10) {
    top_committers <- commit_summary %>%
        arrange(desc(Humanitarian_Commitment)) %>%
        head(top_n)

    ggplot(top_committers, aes(x = reorder(Country, Humanitarian_Commitment), y = Humanitarian_Commitment, fill = Country)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(title = "Top Humanitarian Aid Committers",
             x = "Country",
             y = "Humanitarian Commitment ($ Billion)") +
        theme_minimal() +
        theme(legend.position = "none") # Remove legend if not needed
}
