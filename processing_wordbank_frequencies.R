## Bodo Winter
## August 25, 2015
## Downloading the wordbank production data
## with the help of: http://langcog.github.io/wordbankr/

library(wordbankr)
library(dplyr)

## Load in iconicity data:

setwd('/Users/teeniematlock/Desktop/research/iconicity/evolang_AOA_analysis/')
icon <- read.csv('iconicity_ratings_both.csv')

## Making the connection and gtting the admin and item data:

src <- connect_to_wordbank()
common_tables <- get_common_tables(src)
admins <- get_administration_data()
items <- get_item_data()

## Reducing items to English items:

engl_items <- items[items$language == 'English',]

## Retrieve words for which we have iconicity ratings and reduce wordbank items to that set:

all_words <- as.character(icon$Word)
engl_items <- engl_items[engl_items$item %in% all_words,]

## Get all word productions:

engl_ws_data <- get_instrument_data(instrument_language = 'English',
	instrument_form = 'WS',
	items = engl_items$item_id,
	administrations = admins)

## Add the actual word names to those productions:

engl_ws_data$Word <- engl_items[match(engl_ws_data$num_item_id,engl_items$num_item_id),]$item

## Save this:

write.table(engl_ws_data,
	'/Users/teeniematlock/Desktop/research/iconicity/evolang_AOA_analysis/engl_ws_data.csv',
	sep = ',',
	row.names = F)

## Reduce data frame to only those that are value == 'produces':

xdata <- filter(engl_ws_data, value == 'produces')

## Aggregate by Word and Age:

itemfreq_by_age <- table(xdata$Word, xdata$age)
itemfreq_long <- as.data.frame(itemfreq_by_age)
colnames(itemfreq_long) = c('Word', 'Age', 'Freq')

## Take logarithm (log10):

itemfreq_long$LogFreq <- log10(itemfreq_long$Freq + 1)		# smooth since there are 0's

## Write this to table:

write.table(itemfreq_long, 'wordbank_item_frequencies.csv', sep = ',', row.names = F)

