library(stringr)
library(plyr)
library(dplyr)


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


# List files and read all into DataFrame
files <- list.files(path = './data/sotu/')

sotu <- rbind_all(alply(.data = files, 
                        .margins = 1, 
                        .fun = parse_sotu_text, 
                        .progress = 'text'))



# TODO: Extract year from header1





# Write R Object to data storage
save(sotu, file = './data/sotu_df.RData')

#load(file = './data/sotu_df.RData')







