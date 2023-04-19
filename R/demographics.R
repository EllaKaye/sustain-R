library(readr)
library(ggplot2)
library(dplyr)
library(warwickplots)

## load Pinto et al. 2018 survey data - do this once
#surv_path <- 'https://raw.githubusercontent.com/fronchetti/SANER-2018/master/Survey/actual.csv'
#surv_data <- readr::read_csv(surv_path)
#write_csv(surv_data, here::here("data", "surv_data.csv"))

surv_data <- read_csv("data/surv_data.csv")

## select relevant columns (gender, age, location, and education)
df <- surv_data %>% 
  select(1:5)
rm(surv_data)

## rename columns with short names
names(df) <- c('timestamp', 'age', 'gender', 'location', 'highest_education')

## a bit of variable cleaning
df$age <- gsub('--', 'to', df$age)
df$gender[df$gender == 'non-binary'] <- 'Non-binary'
df$age <- as.factor(df$age)
df$gender <- as.factor(df$gender)
df$location <- as.factor(df$location) 
df$highest_education <- factor(df$highest_education, 
                               levels = c("Bachelor's", "Master's", "Doctorate's", NA))

## repeated objects to be used in plots below
sample_size <- 'Total sample size is 1,553 respondents'
data_src <- 'Data source: Pinto et al., 2018, DOI: 10.1109/SANER.2018.8330263'

## Gender plot -----
gender_perc <- df %>%
  group_by(gender) %>%
  summarise(n = n()) %>%
  mutate(perc = (round(n / sum(n), 3)*100)) %>% 
  select(-c(n)) %>% 
  ungroup()

write_csv(gender_perc, here::here("data", "gender_perc.csv"))

aubergine <- warwick_palettes$aubergine[1]
dark_text <- "#2e2e2f"
mid_text <-  "#4d4e4f"
light_text <- "#747576"
pale_text <- "#ebebeb"


# ggplot(gender_perc, aes(x = reorder(gender, -perc), y = perc)) +
#     geom_col(fill = aubergine) +
#     geom_text(aes(label = paste0(perc, '%')), vjust = -0.45, size = 4.5) +
#     labs(title = 'Percent of respondents by self-identified gender') +
#     ylim(c(0,95)) +
#     warwickplots:::theme_warwick() +
#     theme(axis.title.x = element_blank(),
#           axis.title.y = element_blank(),
#           axis.text.y = element_blank(),
#           panel.grid.minor = element_blank(),
#           panel.grid.major.x = element_blank())
#| echo: false
ggplot(gender_perc, aes(y = fct_reorder(gender, perc), x = perc)) +
  geom_col(fill = aubergine) +
  geom_text(aes(label = paste0(perc, '%')), hjust = -0.3, size = 12, colour = mid_text) +
  labs(title = 'Percent of respondents by self-identified gender') +
  xlim(c(0,110)) +
  warwickplots:::theme_warwick(base_size = 36) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(colour = mid_text, size = rel(1)),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())

ggsave("gender.png", path = here::here("figures"), device = "png", dpi = 320)

## location plot

loc_perc

loc_perc <- df %>%
  group_by(location) %>%
  summarise(n = n()) %>%
  mutate(perc = (round(n / sum(n), 3)*100)) %>% 
  arrange(desc(perc)) %>% 
  ungroup()

loc_perc <- loc_perc %>%
    mutate(location2 = factor(location,
                             levels = c("Europe", "North America", "Asia",
                                        "South America", "Oceania", "Africa",
                                        "Central America"),
                             labels = c("Europe", "North America", "Asia",
                                        "South America", "Oceania", "Africa",
                                        "Central America")))

write_csv(loc_perc, here::here("data", "loc_perc.csv"))

# ggplot(loc_perc, aes(x = reorder(location2, -perc), y = perc)) +
#   geom_col(fill = aubergine) +
#   geom_text(aes(label = paste0(perc, '%')), vjust = -0.45, size = 4.5) +
#   labs(title = 'Percent of respondents by self-identified gender') +
#   ylim(c(0,55)) +
#   warwickplots:::theme_warwick() +
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         axis.text.y = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.grid.major.x = element_blank())
# 
# ggplot(loc_perc, aes(x = reorder(location2, -perc), y = perc)) +
#     geom_col() +
#     geom_text(aes(label = paste0(perc, '%')), vjust = -0.45, size = 4.5) +
#     labs(title = 'Percent of respondents by self-identified region') +
#     ylab(NULL) +
#     xlab(NULL) +
#     ylim(c(0,55)) +
#     warwickplots:::theme_warwick()
    #theme(text = element_text(size = 14),
    #      axis.text = element_text(size = 12))

ggplot(loc_perc, aes(y = fct_reorder(location2, perc), x = perc)) +
  geom_col(fill = aubergine) +
  geom_text(aes(label = paste0(perc, '%')), hjust = -0.3, size = 12, colour = mid_text) +
  labs(title = 'Percent of respondents by self-identified location') +
  xlim(c(0,70)) +
  warwickplots:::theme_warwick(base_size = 36) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = rel(1), colour = mid_text),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())

ggsave("location.png", path = here::here("figures"), device = "png", dpi = 320)

