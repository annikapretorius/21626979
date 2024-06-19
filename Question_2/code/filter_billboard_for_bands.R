# Function to filter the Billboard dataset for Coldplay and Metallica
filter_billboard_for_bands <- function(billboard_df) {
    # Filter the Billboard dataset for Coldplay and Metallica
    billboard_filtered_data <- billboard_df %>%
        filter(grepl("Coldplay", artist, ignore.case = TRUE) | grepl("Metallica", artist, ignore.case = TRUE)) %>%
        mutate(band = ifelse(grepl("Coldplay", artist, ignore.case = TRUE), "Coldplay", "Metallica")) %>%
        rename(weeks_on_board = `weeks-on-board`)

    return(billboard_filtered_data)
}

