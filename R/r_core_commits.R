# https://www.r-bloggers.com/2018/03/guide-to-tidy-git-analysis/
# Parts 1 and 2
library(tidyverse)
library(glue)
library(stringr)
library(forcats)
# Part 3
library(tidygraph)
library(ggraph)
library(tidytext)

system(glue('git log -3'))

log_format_options <- c(datetime = "cd", commit = "h", parents = "p",
                        author = "an", subject = "s")
option_delim <- "\t"
log_format   <- paste(glue("%{log_format_options}"), collapse = "\t")
log_options  <- glue('--pretty=format:"{log_format}" --date=format:"%Y-%m-%d %H:%M:%S"')
log_cmd      <- glue('git log {log_options}')
log_cmd

system(glue('{log_cmd} -3'))

history_logs <- system(log_cmd, intern = TRUE) %>%
    str_split_fixed(option_delim, length(log_format_options)) %>%
    as_tibble() %>%
    setNames(names(log_format_options))

history_logs <- history_logs %>%
    mutate(author = case_when(
        str_detect(tolower(author), "thomas") ~ "tlumley",
        str_detect(tolower(author), "paul") ~ "murrell",
        str_detect(tolower(author), "martyn") ~ "plummer",
        str_detect(tolower(author), "ltierney") ~ "luke",
        TRUE ~ author
    ))

r_core <- tribble(
    ~name, ~username,
    "Douglas Bates",      "bates",
    "John Chambers",      "jmc",
    "Peter Dalgaard",     "pd",
    "Robert Gentleman",   "rgentlem",
    "Kurt Hornik",        "hornik",
    "Ross Ihaka",         "ihaka",
    "Tomas Kalibera",     "kalibera",
    "Michael Lawrence",   "lawrence",
    "Friedrich Leisch",   "leisch",
    "Uwe Ligges",         "ligges",
    "Thomas Lumley",      "tlumley", #thomas
    "Martin Maechler",    "maechler",
    "Sebastian Meyer",    "smeyer",
    "Paul Murrell",       "murrell", #paul
    "Martyn Plummer",     "plummer", #martyn
    "Brian Ripley",       "ripley",
    "Deepayan Sarkar",    "deepayan",
    "Duncan Temple Lang", "duncan",
    "Luke Tierney",       "luke", #ltierney
    "Simon Urbanek",      "urbaneks",
    "Heiner Schwarte",    "", # up to October 1999,
    "Guido Masarotto",    "guido", # up to June 2003,
    "Stefano Iacus",      "iacus", # up to July 2014,
    "Seth Falcon",        "falcon",# up to August 2015,
    "Duncan Murdoch",     "murdoch", # up to September 2017
    "Martin Morgan",      "morgan") # up to June 2021.

history_logs <- left_join(history_logs, r_core, by = c("author" = "username"))

# check
table(history_logs[is.na(history_logs$name),"author"])

history_logs$year <- substr(history_logs$datetime, 1, 4)

dat <- history_logs %>%
    count(name, year)

#ggplot(history_logs, aes(x = year, fill = name)) +
#    geom_bar(position = "stack")
    

dat2 <- history_logs %>%
    group_by(year) %>%
    summarize(n = n_distinct(name))


ggplot(dat2, aes(x = year, y = n)) +
    geom_bar(stat = "identity")

xtabs(~ name + year, data = history_logs)

dat3 <- history_logs %>%
    filter(!is.na(name)) %>%
    mutate(name = fct_infreq(name)) %>%
    group_by(name, year) %>%
    summarise(log_commits = log10(n()),
              commits = 10^log10(n()))

ggplot(dat3, aes(year, fct_rev(name))) +
    geom_tile(aes(fill = commits)) +
    labs(x = NULL, y = NULL, subtitle = "Core Developer Commits 1997 - 2022") +
    scale_fill_binned(type = "viridis", trans = "log10", limits=c(0.1,5000),
                      breaks = c(0.1, 1, 10, 100, 1000)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
