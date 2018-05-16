library(tidyverse)
library(tm)

#extract text from pdf
read <- readPDF(control = list(text = "-layout"))
document <- Corpus(URISource("./Flavor-Bible-epub.pdf"), 
                   readerControl = list(reader = read))

#include only pages with flavor matching charts
doc <- content(document[[1]]) %>% strsplit("\r\n")
line_of_text <- doc[42:811] %>% unlist()
page_num <- sapply(42:811, function(x) rep(x, length(doc[[x]]))) %>% unlist()

#make df but get rid of first 15 lines on page 42 b/c no flavors or headings
df <- data_frame(page = page_num, text = line_of_text)[-c(1:15),]      

#pronouns for detecting sentences
pronouns <- c(" I | YOU | WE | THEY | THEIR | MY | OUR ")

df <- df %>% 
        mutate(#detect uppercase, indenting, leading dashes, pronouns
                caps = str_detect(text, pattern = "^[[:upper:]]{3,}"),
                indent = str_detect(text, pattern = "^[[:space:]]{1,}"),
                dashes = str_trim(text,"left") %>%
                        str_detect(., pattern = "^[-–—]"),
                sentence = str_detect(text %>% toupper(), pronouns)) %>% 
        group_by(page) %>% 
        #label pages with fewer than two indents total
        mutate(few_indents = sum(indent) < 2) %>% 
        ungroup %>% 
        mutate(#then write the new rules for each class
                heading = caps & !few_indents & !dashes & !indent,
                flavor = ifelse(few_indents, 
                                !dashes & !sentence, 
                                indent & !sentence & !dashes),
                ignore = !heading & !flavor)

df2 <- df[!df$ignore,]

headings_vec <- df2$text[df2$heading] #collect headings for ref

headings <- headings_vec[cumsum(df2$heading)] #refer with cumsum -- tricky ;)

df2$text <- str_trim(df2$text, "both") #trim indents

#long form data frame of headings and flavors
df3 <- data.frame(main = headings, 
                  pairing = df2$text,
                  stringsAsFactors = FALSE) %>% 
           filter(main != pairing) #remove rows with main in both columns

# write.csv(df3, "flavor_bible_full.csv",
#           row.names = FALSE,
#           fileEncoding = "UTF-8") #for display in shinyapp