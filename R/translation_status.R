# run message_status.R first to get message_status
# copied from https://github.com/r-devel/rcontribution/blob/main/collaboration_campfires/translations/translation_status.R

library(dplyr)
library(forcats)
library(ggplot2)
library(readr)

## new - from local update
message_status <- read_csv("data/message_status.csv")

tally_status <- message_status |>
    filter(!language %in% c("Persian", "English_GB", "English")) |>
    mutate(group = ifelse(fuzzy, "fuzzy",
                          ifelse(translated, "translated",
                                 "untranslated")),
           group = factor(group,
                          levels = c("untranslated", "translated", "fuzzy")),
           #language = factor(language,
        #                     c("Chinese_CN", "Chinese_TW", "Danish", "French", "German", "Italian",
          #                     "Japanese",   "Korean", "Lithuanian",
           #                    "Norwegian Nynorsk", "Polish",
            #                   "Portuguese_BR", "Russian", "Spanish", "Turkish"),
             #                c("Chin_CN", "Chin_TW", "Danish", "French", "German", "Italian",
              #                 "Japanese",   "Korean", "Lithuanian",
               #                "Norw_NN", "Polish",
                #               "Port_BR", "Russian", "Spanish", "Turkish"))
        ) |>
    group_by(language, group) |>
    tally() |>
    mutate(y = n / sum(n)) |>
    ungroup() |>
    complete(language, group, fill = list(n = 0, y = 0)) |>
    arrange(group, n)


ggplot(tally_status,
       aes(fill = group, x = fct_inorder(language), y = y*100)) +
    geom_bar(stat = "identity", position = "stack") +
    labs(x = NULL, y = "%",
         subtitle = "Translation status in R") +
    scale_fill_manual(values = c("grey", "steelblue", "orange")) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          legend.title=element_blank(),
          plot.margin = margin(0.1, 0.4, 0.1, 0.4, "cm"))

# Ella's redo
#head(tally_status)
# TODO: change colours, tweak axis text settings, ggtext instead of legend
ggplot(tally_status,
       aes(fill = group, y = fct_inorder(language), x = y)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = NULL, y = NULL,
       title = "Translation status in R") +
  scale_fill_manual(values = c("grey", "steelblue", "orange")) +
  scale_x_continuous(expand = c(0, 0), labels = scales::label_percent()) +
  warwickplots:::theme_warwick() +
  theme(legend.title=element_blank(),
        legend.position = "right")

# language codes, ca case want to use short names
c("zh_CN", "zh_TW", "da", "fr", "de", "it",
  "ja",   "ko", "lt",
  "nn", "po",
  "pt_BR", "ru", "es", "tr")

# ## old - from collab campfires
# message_status0 <- read_csv("https://raw.githubusercontent.com/r-devel/rcontribution/main/collaboration_campfires/translations/message_status.csv")
# 
# tally_status0 <- message_status0 |>
#     filter(!language %in% c("Persian", "English_GB", "English")) |>
#     mutate(group = ifelse(fuzzy, "fuzzy",
#                           ifelse(translated, "translated",
#                                  "untranslated")),
#            group = factor(group,
#                           levels = c("untranslated", "translated", "fuzzy")),
#            #language = factor(language,
#            #                     c("Chinese_CN", "Chinese_TW", "Danish", "French", "German", "Italian",
#            #                     "Japanese",   "Korean", "Lithuanian",
#            #                    "Norwegian Nynorsk", "Polish",
#            #                   "Portuguese_BR", "Russian", "Spanish", "Turkish"),
#            #                c("Chin_CN", "Chin_TW", "Danish", "French", "German", "Italian",
#            #                 "Japanese",   "Korean", "Lithuanian",
#            #                "Norw_NN", "Polish",
#            #               "Port_BR", "Russian", "Spanish", "Turkish"))
#     ) |>
#     group_by(language, group) |>
#     tally() |>
#     mutate(y = n / sum(n)) |>
#     ungroup() |>
#     complete(language, group, fill = list(n = 0, y = 0)) |>
#     arrange(group, n)
# 
# 
# ggplot(tally_status0,
#        aes(fill = group, x = fct_inorder(language), y = y*100)) +
#     geom_bar(stat = "identity", position = "stack") +
#     labs(x = NULL, y = "%",
#          subtitle = "Translation status in R") +
#     scale_fill_manual(values = c("grey", "steelblue", "orange")) +
#     scale_y_continuous(expand = c(0, 0)) +
#     theme_minimal() +
#     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
#           legend.title=element_blank(),
#           plot.margin = margin(0.1, 0.4, 0.1, 0.4, "cm"))
# 
# View(left_join(tally_status0, tally_status, by = c("language", "group")))
# 
# # -> more translated and less fuzzy for Portuguese BR
# # -> similar to lesser extent for Spanish
# # -> few small changes elsewhere
# # -> does not include Q1 update
