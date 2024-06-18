# General function to build and display a table
build_allocations_table<- function(data, Latex = TRUE) {
    library(xtable)
    options(xtable.comment = FALSE)

    # Mutate the data frame to include formatted percentages if necessary
    Result <- data %>%
        mutate(
            Total_Allocation = round(Total_Allocation, 2),
            Military_Allocation = round(Military_Allocation, 2),
            Total_Bilateral_Allocation = round(Total_Bilateral_Allocation, 2)
        )

    if (Latex) {
        Tab <- xtable(Result, caption = "Summary of EU Aid to Ukraine \\label{tab1}")
        Tab <- print.xtable(Tab,
                            tabular.environment = "longtable",
                            floating = FALSE,
                            table.placement = 'H',
                            booktabs = T,
                            include.rownames = FALSE,
                            comment = FALSE,
                            caption.placement = 'top',
                            size = "\\fontsize{12pt}{13pt}\\selectfont")
    } else {
        Tab <- knitr::kable(Result, caption = "Summary of EU Aid to Ukraine") %>%
            kable_styling(bootstrap_options = c("striped", "hover"))
    }

    return(Tab)
}
