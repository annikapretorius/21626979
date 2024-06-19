build_commit_table <- function(data, Latex = TRUE) {
    library(xtable)
    options(xtable.comment = FALSE)

    # Mutate the data frame to include formatted percentages if necessary
    Result <- data %>%
        mutate(
            Total_Commitment = round(Total_Commitment, 2),
            Military_Commitment = round(Military_Commitment, 2),
            Total_Bilateral_Commitment = round(Total_Bilateral_Commitment, 2)
        )

    if (Latex) {
        Tab <- xtable(Result, caption = "Summary of EU Commitments to Ukraine \\label{tab2}")
        Tab <- print.xtable(Tab,
                            tabular.environment = "longtable",
                            floating = FALSE,
                            table.placement = 'H',
                            booktabs = T,
                            include.rownames = FALSE,
                            comment = FALSE,
                            caption.placement = 'top',
                            size = "\\fontsize{11pt}{12pt}\\selectfont")
        Tab <- paste0("\\begin{table}[H]\\centering", Tab, "\\end{table}")
    } else {
        Tab <- knitr::kable(Result, caption = "Summary of EU Commitments to Ukraine") %>%
            kable_styling(bootstrap_options = c("striped", "hover"))
    }

    return(Tab)
}
