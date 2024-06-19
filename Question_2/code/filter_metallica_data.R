# Define the function to filter and transform Metallica data
filter_metallica_data <- function(metallica_data) {
    metallica_filtered <- metallica_data %>%
        filter(!grepl("live", name, ignore.case = TRUE) &
                   !grepl("demo", name, ignore.case = TRUE) &
                   !grepl("tapes", name, ignore.case = TRUE)) %>%
        mutate(band = "Metallica")

    return(metallica_filtered)
}
