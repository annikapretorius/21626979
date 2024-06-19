# Define the function to filter and transform Coldplay data
filter_coldplay_data <- function(coldplay_data) {
    coldplay_filtered <- coldplay_data %>%
        filter(!grepl("live", name, ignore.case = TRUE) &
                   !grepl("demo", name, ignore.case = TRUE) &
                   !grepl("tapes", name, ignore.case = TRUE)) %>%
        mutate(band = "Coldplay") %>%
        rename(album = album_name)

    return(coldplay_filtered)
}
