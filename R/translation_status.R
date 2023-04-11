tally_status <- message_status |>
    filter(!language %in% c("Persian", "English_GB", "English")) |>
    mutate(group = ifelse(fuzzy, "fuzzy",
                          ifelse(translated, "translated",
                                 "untranslated")),
           group = factor(group,
                          levels = c("untranslated", "translated", "fuzzy")),
           language = factor(language,
                             c("Chinese_CN", "Chinese_TW", "Danish", "French", "German", "Italian",
                               "Japanese",   "Korean", "Lithuanian",
                               "Norwegian Nynorsk", "Polish",
                               "Portuguese_BR", "Russian", "Spanish", "Turkish"),
                             c("Chin_CN", "Chin_TW", "Danish", "French", "German", "Italian",
                               "Japanese",   "Korean", "Lithuanian",
                               "Norw_NN", "Polish",
                               "Port_BR", "Russian", "Spanish", "Turkish"))) |>
    group_by(language, group) |>
    tally() |>
    arrange(group, n)
m <- xtabs(~language, message_status)[[1]]
ggplot(tally_status,
       aes(fill = group, x = fct_inorder(language), y = n/m * 100)) +
    geom_bar(stat = "identity", position = "stack") +
    labs(x = NULL, y = "%",
         subtitle = "Translation status in R") +
    scale_fill_manual(values = c("grey", "steelblue", "orange")) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          legend.title=element_blank(),
          plot.margin = margin(0.1, 0.4, 0.1, 0.4, "cm"))


c("zh_CN", "zh_TW", "da", "fr", "de", "it",
  "ja",   "ko", "lt",
  "nn", "po",
  "pt_BR", "ru", "es", "tr")
