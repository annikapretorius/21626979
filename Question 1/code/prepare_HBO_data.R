# Function to join HBO titles and credits, and prepare the HBO data
prepare_HBO_data <- function(HBO_Titles, HBO_Credits, start_year = 1950, end_year = 2014) {

    join_HBO_titles_credits <- function(titles, credits) {
        joined_data <- credits %>%
            left_join(titles, by = "id") %>%
            select(id, name, release_year, tmdb_popularity) %>%
            distinct()
        return(joined_data)
    }
    # Join HBO titles and credits
    HBO_joined <- join_HBO_titles_credits(HBO_Titles, HBO_Credits) %>%
        rename(Name = name) %>%
        mutate(Name = str_extract(Name, "^[^\\s]+"))

    # Select and rename columns, filter by year
    HBO_joined_df <- HBO_joined %>%
        select(Name, id, release_year, tmdb_popularity) %>%
        rename(Year = release_year) %>%
        filter(Year >= start_year & Year <= end_year)

    return(HBO_joined_df)
}

