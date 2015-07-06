library(dplyr)
library(magrittr)
library(tm)
library(proxy)
library(ggplot2)
library(RColorBrewer)



# Plot EDA
load(file = './data/sotu_df.RData')

ggplot(data = sotu, aes(x = year, y = char_content)) +
    geom_line()


sotu %>%
    arrange(desc(char_content)) %>%
    select(file_name, char_content)











# TODO: filter out addresses to joint sessions that are not SOTU
sotu[1]
str_to_lower(string = sotu[1])
str_detect(string = str_to_lower(string = sotu[1]),
           patter = 'state')
str_detect(string = str_to_lower(string = sotu[1]),
           patter = 'union')  # joint, session























# TODO: pull dates from text, str_extract



# TODO: pull president name from text, file name
# TODO: Extra Data - party of president
# TODO: Extra Data - dates of war, indicator of war time.
# TODO: [EDA] Word cloud of freqeunt terms, infrequent, # speeches included
