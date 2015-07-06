library(tm)










# TODO: filter out addresses to joint sessions that are not SOTU
sotu[1]
str_to_lower(string = sotu[1])
str_detect(string = str_to_lower(string = sotu[1]),
           patter = 'state')
str_detect(string = str_to_lower(string = sotu[1]),
           patter = 'union')  # joint, session


# TODO: pull dates from text
str_detect(string = sotu[2], pattern = '[0-9]{4}')




# TODO: pull president name from text, file name
# TODO: Extra Data - party of president
# TODO: Extra Data - dates of war, indicator of war time.
# TODO: [EDA] Word cloud of freqeunt terms, infrequent, # speeches included
