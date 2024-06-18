# Function to left join HBO_Titles and HBO_Credits by id and get the year per id
join_HBO_titles_credits <- function(titles, credits) {

   titles <- HBO_Titles %>% select(id, title, release_year)

    joined_data <- HBO_Credits %>%
        left_join(HBO_Titles, by = "id") %>%
        distinct()
    return(joined_data)
}
