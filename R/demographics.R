ggplot(gender_perc, aes(x = gender, y = perc)) +
    geom_col() +
    geom_text(aes(label = paste0(perc, '%')), vjust = -0.45, size = 4.5) +
    labs(subtitle = 'Percent of respondents by self-identified gender') +
    ylab(NULL) +
    xlab(NULL) +
    ylim(c(0,95)) +
    theme_minimal() +
    theme(text = element_text(size = 14),
          axis.text = element_text(size = 12))

loc_perc <- loc_perc %>%
    mutate(location2 = factor(location,
                             levels = c("Europe", "North America", "Asia",
                                        "South America", "Oceania", "Africa",
                                        "Central America"),
                             labels = c("Europe", "N America", "Asia",
                                        "S America", "Oceania", "Africa",
                                        "C America")))
ggplot(loc_perc, aes(x = reorder(location2, -perc), y = perc)) +
    geom_col() +
    geom_text(aes(label = paste0(perc, '%')), vjust = -0.45, size = 4.5) +
    labs(title = 'Percent of respondents by self-identified region') +
    ylab(NULL) +
    xlab(NULL) +
    ylim(c(0,55)) +
    theme_minimal() +
    theme(text = element_text(size = 14),
          axis.text = element_text(size = 12))
