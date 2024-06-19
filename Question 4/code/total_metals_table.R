# Function to create a table for PDF knitting
total_metals_table <- function(data) {
    data %>%
        kable("latex", booktabs = TRUE, caption = "Total Medals by Country", position = "center") %>%
        kable_styling(latex_options = c("striped", "hold_position", "scale_down"))
}
