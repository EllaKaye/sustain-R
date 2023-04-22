# run message_status.R first to get message_status
# copied from https://github.com/r-devel/rcontribution/blob/main/collaboration_campfires/translations/translation_status.R

library(dplyr)
library(forcats)
library(ggplot2)
library(readr)
library(ggtext)
library(tidyr)

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

tally_status <- tally_status %>%
  mutate(group = fct_relevel(group, c("untranslated", "fuzzy", "translated")))

readr::write_csv(tally_status, here::here("data", "tally_status.csv"))

aubergine_pal <- warwickplots::warwick_palettes$aubergine


#head(tally_status)
# TODO: tweak axis text settings, ggtext instead of legend, reorder factor

dark_text <- "#2e2e2f"
mid_text <-  "#4d4e4f"
light_text <- "#747576"
pale_text <- "#ebebeb"


  
  
ggplot(tally_status, aes(fill = group, y = fct_inorder(language), x = y)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = NULL, 
       y = NULL,
       #title = "Translation status in R",
       title = "Percent of <span style = 'color:#552D62;'>**translated**</span>,
       <span style = 'color:#886C91;'>**fuzzy**</span> and <span style = 'color:#c4c4c4;'>**untranslated**</span> messages") +
  scale_fill_manual(values = c("#c4c4c4", aubergine_pal[5], aubergine_pal[1])) +
  scale_x_continuous(expand = c(0, 0), labels = scales::label_percent()) +
  warwickplots:::theme_warwick(base_size = 24) +
  theme(legend.position = 'none',
        axis.text.x = element_text(size = rel(1), colour = mid_text),
        axis.text.y = element_text(size = rel(1), colour = mid_text),
        plot.margin = margin(0.25, 1.2, 0.25, 0.25,"cm"),
        plot.title = element_textbox_simple(size = rel(1.4),
                                            margin = margin(12, 0, 12, 0)),
        panel.grid = element_blank())

ggsave("translation_status.png", path = here::here("figures"), device = "png", dpi = 320)
ggsave("translation_status.svg", path = here::here("figures"), device = grDevices::svg())


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
