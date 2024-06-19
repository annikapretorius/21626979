#The total_metals_table function is designed to create a formatted table summarizing the total medals won by countries.
#It offers the option to generate the table in LaTeX format for inclusion in LaTeX documents or in a more general format using the knitr package.

# Function to create and print a table for PDF knitting
total_metals_table <- function(data, Latex = TRUE) {
    library(xtable)
    library(dplyr)
    library(knitr)
    library(kableExtra)

    options(xtable.comment = FALSE)

    # Mutate the data frame to include formatted totals
    Result <- data %>%
        mutate(
            Gold = as.integer(Gold),
            Silver = as.integer(Silver),
            Bronze = as.integer(Bronze),
            Total_Medals = as.integer(Total_Medals)
        )

    if (Latex) {
        Tab <- xtable(Result, caption = "Total Medals by Country \\label{tab:total_medals}")
        print.xtable(Tab,
                     tabular.environment = "longtable",
                     floating = FALSE,
                     table.placement = 'H',
                     booktabs = TRUE,
                     include.rownames = FALSE,
                     comment = FALSE,
                     caption.placement = 'top',
                     size = "\\fontsize{11pt}{12pt}\\selectfont")
    } else {
        Tab <- kable(Result, format = "html", caption = "Total Medals by Country") %>%
            kable_styling(bootstrap_options = c("striped", "hover"))
        return(Tab)
    }
}
