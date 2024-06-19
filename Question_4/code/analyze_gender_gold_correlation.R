# Function to analyze correlation between gender and winning a gold medal in Archery
analyze_gender_gold_correlation <- function(data) {

    # Filter the dataset for Archery individual events
    archery_individual <- data %>%
        filter(Sport == "Archery" & grepl("Individual", Event))

    # Identify the top 10 countries in Archery individual events
    top_countries <- archery_individual %>%
        group_by(Country) %>%
        summarise(Total_Medals = sum(ifelse(Medal == "Gold", 1, 0))) %>%
        arrange(desc(Total_Medals)) %>%
        slice_head(n = 10) %>%
        pull(Country)

    # Filter the dataset to include only the top 10 countries
    archery_individual <- archery_individual %>%
        filter(Country %in% top_countries)

    # Create a binary outcome variable indicating whether an athlete won a gold medal
    archery_individual <- archery_individual %>%
        mutate(Won_Gold = ifelse(Medal == "Gold", 1, 0))

    # Fits a generalized linear model with Won_Gold as the response variable and Gender as the predictor, using the binomial family for logistic regression.
    model <- glm(Won_Gold ~ Gender, data = archery_individual, family = binomial)

    # Summarize the model results
    model_summary <- tidy(model)

    # Print the model summary
    print(model_summary)

    # Visualize the predicted probabilities
    archery_individual <- archery_individual %>%
        mutate(Predicted_Probability = predict(model, type = "response"))

    plot <- ggplot(archery_individual, aes(x = Gender, y = Predicted_Probability, color = Gender)) +
        geom_jitter(width = 0.2, height = 0) +
        labs(title = "Predicted Probability of Winning a Gold Medal by Gender",
             subtitle = "Archery Individual Events (Top 10 Countries)",
             x = "Gender",
             y = "Predicted Probability") +
        theme_minimal() +
        theme(
            plot.title = element_text(hjust = 0.5, size = 14),
            plot.subtitle = element_text(hjust = 0.5, size = 14),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 12)
        )

    print(plot)
}

