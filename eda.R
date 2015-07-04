library(stringr)
library(dplyr)



files <- list.files(path = './data/sotu/')

parse_sotu_text <- function(file_name) {
  full_path <- paste('./data/sotu/', file_name, sep = '')
  raw <- scan(file = full_path, what = character(), sep = '\n')
  raw <- raw[nchar(raw) > 1]  # delete empty lines, single whitespace char
  sotu_content <- raw[3:length(raw)]  # first 2 lines are headers
  
  output <- data_frame(file_name = file_name,
                       head1 = raw[1],
                       head2 = raw[2],
                       char_content = sum(nchar(sotu_content)),
                       content = paste0(sotu_content, sep = ' ', collapse = ' '))
  return(output)
}
parse_sotu_text(file_name = '1966-Johnson.txt')










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

