## Bodo Winter
## August 25, 2015
## Downloading the CHILDES production frequencies
## all_childfreq_tables.html was downloaded from http://childfreq.sumsar.net/ on August 25, 2015

library(XML)

## Load in the HTML file that displays the query results:

setwd('/Users/teeniematlock/Desktop/research/iconicity/evolang_AOA_analysis/')
xdata <- paste(readLines('all_childfreq_tables.html'), collapse = '\n')

## Parse the HTML tables out of that:

xlist <- readHTMLTable(xdata)

## Get the words out of this (this needs to be done since the search interface might have omitted some words):

html <- htmlTreeParse(xdata, useInternal = T)

## Parse the h4 tags out of this:

xwords <- xpathSApply(html, '//h4', xmlValue)

## Get the data frames in the lists into the correct format (and append the names of the words):

for (i in 1:length(xlist)) {
	xtemp <- xlist[[i]]
	colnames(xtemp) <- as.character(unlist(xtemp[1,]))
	xtemp <- xtemp[-1,]
	xtemp$Word <- xwords[i]
	xlist[[i]] <- xtemp
	}

## Make this list into a data frame:

xdf <- c()
for (i in 1:length(xlist)) {
	xdf <- rbind(xdf, xlist[[i]])
	if ( i %% 100 == 0) {
		cat(paste(i, '\n'))
		}
	}

## Cleaning the table:

colnames(xdf) <- c('Age', 'Freq_100000w', 'Freq', 'No_of_transcripts', 'TotalFreq', 'Word', 'LogFreq')

## Take the logarithm of frequency:

xdf$LogFreq <- log10(as.numeric(as.character(xdf$Freq_100000w)) + 1)

## Write to table:

write.table(xdf,
	'childes_child_freq.csv',
	sep = ',', row.names = F)


