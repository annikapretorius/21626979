
# Function to create a table from the baby names dataframe
Baby_Names_table <- function(df, Latex = TRUE) {

    library(tidyverse)
    library(xtable)

     # Remove xtable's comments
    options(xtable.comment = FALSE)

    # Rank names by count within each year and keep only the top 25
    Result <- df %>%
        group_by(Year) %>%
        arrange(desc(Count)) %>%
        mutate(Rank = row_number()) %>%
        filter(Rank <= 25) %>%
        ungroup() %>%
        select(Year, Name, Count, Rank)

    # Calculate the percentage for each name
    Result <- Result %>%
        group_by(Year) %>%
        mutate(Percent = Count / sum(Count) * 100) %>%
        ungroup() %>%
        mutate(Percent = paste0(round(Percent, 3), "%"))
    if(Latex) {
        Tab <- xtable(Result, caption = "Baby Names \\label{tab1}")

        Tab <- print.xtable(Tab,
                            tabular.environment = "longtable",
                            floating = FALSE,
                            table.placement = 'H',
                            booktabs = TRUE,
                            include.rownames = FALSE,
                            comment = FALSE,
                            caption.placement = 'top',
                            size="\\fontsize{12pt}{13pt}\\selectfont"
        )
    } else {
        Tab <- knitr::kable(Result)
    }

    Tab
}

