## Bodo Winter
## August 24, 2015
## Analysis of iconicity data for Evolang

## Analysis is split into three parts:
## (1) - Analysis of Brysbaert et al. (2014) AOA ratings
## (2) - Analysis of MCDI %used, retrieved from Wordbank (%
## (3) - Analysis of CHILDES productions
## (4) - Analysis of MCDI raw child production data
## (5) - Analysis of Parental Input Frequency from CHILDES


library(ggplot2)
library(car)
library(dplyr)
library(reshape2)
library(lme4)


##------------------------------------------------------------------
## Preprocessing:
##------------------------------------------------------------------

## Load in datasets:

setwd('/Users/teeniematlock/Desktop/research/iconicity/evolang_AOA_analysis/')
icon <- read.csv('iconicity_ratings_both.csv')
mon <- read.csv('monaghan2014_systematicity.csv')
mcdi <- read.csv('MCDI_item_data.csv')				# from wordbank
parent <- readLines('CHILDES_parentfreq.txt')		# what a messy format!
childes <- read.csv('childes_child_freq.csv')
wbank <- read.csv('wordbank_item_frequencies.csv')

## Clean up the parental input frequencies:

# parent <- parent[-c(length(parent)-1, length(parent))]		# last two entries are broken
parent <- strsplit(parent, ' ')
parent <- lapply(parent, FUN = function(x) x[x!= ''])
parent <- parent[!sapply(parent, FUN = function(x) any(is.na(x)))]
parent.df <- t(as.data.frame(parent))
parent.df <- as.data.frame(parent.df)
colnames(parent.df) = c('ParentFrequency', 'Word')
rownames(parent.df) = 1:nrow(parent.df)

## Merge parental input frequencies with main data frame;

icon$ParentFrequency <- parent.df[match(icon$Word, parent.df$Word), ]$ParentFrequency
icon$ParentFrequency <- as.numeric(as.character(icon$ParentFrequency))
icon$LogParentFreq <- log10(icon$ParentFrequency)

## Merge Monaghan et al. (2014) dataset with main iconicity dataset:

icon$Systematicity <- mon[match(icon$Word, mon$word),]$relativeIconicity
length(na.omit(icon$Systematicity))		# 799 words for which there is a match to Monaghan et al. (2014)

## Rename:

colnames(mcdi) <- gsub('X', 'mcdi_',colnames(mcdi))

## Add MCDI AOA values to main dataset:

icon <- cbind(icon,
	mcdi[match(icon$Word, mcdi$definition), grep('mcdi_', colnames(mcdi))])



##------------------------------------------------------------------
## Part #1: Brysbaert AOA ratings:
##------------------------------------------------------------------

## Plot iconicity against AOA:

quartz('', 11, 5)
ggplot(icon,
	aes(x = AOA, y = Written)) +
	geom_point(shape = 16) + 
	geom_smooth(method = 'lm') +
	facet_grid(~ContentPOS)

## Make an analysis of this, first a marginal analysis ignoring all other factors:

xmdl <- lm(AOA ~ Written, icon)
summary(xmdl)

## For the fuller analysis, center all variables:

icon <- mutate(icon,
	Written_c = Written - mean(Written, na.rm = T),
	Systematicity_c = Systematicity - mean(Systematicity, na.rm = T),
	WordFreq_c = WordFreq - mean(WordFreq, na.rm = T))

## A fuller analysis, controlling for the other variables:

xmdl <- lm(AOA ~ Written_c + Systematicity_c +
	WordFreq_c + ContentPOS, icon)
summary(xmdl)
vif(xmdl)			# actually o.k. (no collinearity issue)

## Check how much _unique_ variance iconicity contributes above and beyond systematicity:

xmdl.noicon <- lm(AOA ~ 1 + Systematicity_c +
	WordFreq_c + ContentPOS, icon)
summary(xmdl.noicon)$r.squared
summary(xmdl)$r.squared				# about 1%

## Check how much _unique_ variance iconicity contributes above and beyond only POS & frequency:

xmdl.1 <- lm(AOA ~ WordFreq_c + ContentPOS, icon)
xmdl.2 <- lm(AOA ~ Written_c + WordFreq_c + ContentPOS, icon)
summary(xmdl.1)$r.squared
summary(xmdl.2)$r.squared			# ~5%

## What is the relationship between systematicity and the iconicity ratings?:

quartz('', 8, 6)
ggplot(icon,
	aes(x = Systematicity, y = Written)) +
	geom_point(shape = 16) +
	geom_smooth(method = 'lm')
cor.test(icon$Systematicity, icon$Written)			# not related!

## Compare AIC values of iconicity against systematicity on subset for which there are both:

all_completes <- complete.cases(icon$Written_c) & complete.cases(icon$Systematicity_c)
icon.subset <- icon[all_completes, ]
xmdl.icon <- lm(AOA ~ Written_c + WordFreq_c, icon.subset)
xmdl.sys <- lm(AOA ~ Systematicity_c + WordFreq_c, icon.subset)
AIC(xmdl.icon)
AIC(xmdl.sys)
diff(c(AIC(xmdl.sys), AIC(xmdl.icon)))



##------------------------------------------------------------------
## Part #2: MCDI analysis of "proportion of children used this word"
##------------------------------------------------------------------

## Make into long format so that every word has a %acquired value for each month:

icon.relevant <- dplyr:::select(icon,
	Word, Written, WordFreq, Systematicity,
	(mcdi_16:mcdi_30))
licon <- melt(icon.relevant,
	id.vars = c('Word', 'Written', 'WordFreq', 'Systematicity'))
licon <- rename(licon,
	Month = variable, PercentAcquired = value)
licon$Month <- as.numeric(gsub('mcdi_', '', as.character(licon$Month)))

## Make a plot of this relationship:

quartz('', 11, 6)
ggplot(licon,
	aes(x = Written, y = PercentAcquired, col = Month)) + 
	geom_point(shape = 16) +
	geom_smooth(method = 'lm') + 
	facet_wrap(~Month)

## For every month, compute the fit of the Iconicity-only model:

AIC_vals <- c()
for(i in unique(licon$Month)) {
	temporary_subset <- filter(licon, Month == i)
	
	temporary_model <- lm(PercentAcquired ~ Written, temporary_subset)
		
	AIC_vals <- c(AIC_vals, AIC(temporary_model))
	}
quartz('', 9, 6)
plot(unique(licon$Month), AIC_vals, type = 'l', lwd = 2)

## For modeling, center again:

licon <- mutate(licon,
	Written_c = Written - mean(Written, na.rm = T),
	Systematicity_c = Systematicity - mean(Systematicity, na.rm = T),
	WordFreq_c = WordFreq - mean(WordFreq, na.rm = T),
	Month_c = Month - mean(Month, na.rm = T))

## Model this relationship, first with the full data:

xmdl <- lmer(PercentAcquired ~ Written_c + Month_c + WordFreq_c +
	Written_c:Month_c + WordFreq_c:Month_c +
	(1|Word), licon)
summary(xmdl)




##------------------------------------------------------------------
## Part #3: Childes production frequency
##------------------------------------------------------------------

## How many words do we have in CHILDES?

length(unique(childes$Word))		# 1,951

## Add iconicity, frequency and systematicity data to childes:

childes$Written <- icon[match(childes$Word, icon$Word), ]$Written
childes$Systematicity <- icon[match(childes$Word, icon$Word), ]$Systematicity
childes$FreqSUBTLEX <- icon[match(childes$Word, icon$Word), ]$WordFreq

## Make a plot of this:

quartz('', 11, 6.5)
ggplot(childes[childes$LogFreq != 0,],
	aes(x = Written, y = LogFreq, col = Age)) +
	geom_point(shape = 16) +
	geom_smooth(method = 'lm') + 
	facet_wrap(~Age)			# what is happening here?

## Calculate adult inflated frequency:

childes$InflatedFreq <- log10(childes$Freq + 1)
childes$InflatedFreq <- childes$InflatedFreq / childes$FreqSUBTLEX

## Check whether this makes sense:

head(arrange(childes, desc(InflatedFreq)), 20)
head(arrange(childes, InflatedFreq), 20)

## Plot this inflated frequency measure:

quartz('', 11, 6.5)
ggplot(childes[childes$InflatedFreq != 0,],
	aes(x = Written, y = InflatedFreq, col = Age)) +
	geom_point(shape = 16) +
	geom_smooth(method = 'lm') + 
	facet_wrap(~Age)			# what is happening here?

## Center for analysis:

childes <- mutate(childes,
	Written_c = Written - mean(Written, na.rm = T),
	Systematicity_c = Systematicity - mean(Systematicity, na.rm = T),
	Age_c = Age - mean(Age, na.rm = T),
	FreqSUBTLEX_c = FreqSUBTLEX - mean(FreqSUBTLEX, na.rm = T))

## Make an analysis of this:

summary(lm(InflatedFreq ~ Written_c + Written_c:Age_c, childes))
summary(lm(LogFreq ~ Written_c  + FreqSUBTLEX_c +
	FreqSUBTLEX_c:Age_c + Written_c:Age_c, childes))
	# these two produce conceptually similar results

## Compute slopes:

childes_slopes <- data.frame(Word = unique(childes$Word))
childes_slopes$Slope <- numeric(nrow(childes_slopes))
for (i in childes_slopes$Word) {
	xtemp <- childes[childes$Word == i,]
	childes_slopes[childes_slopes$Word == i,]$Slope <- coef(lm(LogFreq ~ Age, xtemp))[2]
	}

## Check whether the result makes intuitive sense:

head(arrange(childes_slopes, desc(Slope)), 20)
head(arrange(childes_slopes, Slope), 20)

## Add iconicity to this:

childes_slopes$Written <- icon[match(childes_slopes$Word, icon$Word),]$Written
childes_slopes$Systematicity <- icon[match(childes_slopes$Word, icon$Word),]$Systematicity

## Plot both:

quartz('', 9, 6)
ggplot(childes_slopes,
	aes(x = Slope, y = Written)) +
	geom_point(shape = 16) +
	geom_smooth(method = 'lm')
# interpretation: those that increase in frequency are less likely to be iconic;
# those that decrease in frequency are more iconic




##------------------------------------------------------------------
## Part #4: Wordbank production frequency; IGNORE THIS FOR NOW!!!! (it's BULLSHIT)
##------------------------------------------------------------------

# ## Add iconicity data, systematicity scores and SUBTLEX frequency data to wordbank:

# wbank$Written <- icon[match(wbank$Word, icon$Word), ]$Written
# wbank$Systematicity <- icon[match(wbank$Word, icon$Word), ]$Systematicity
# wbank$WordFreq <- icon[match(wbank$Word, icon$Word), ]$WordFreq

# ## Make a plot of this child production frequency against iconicity:

# quartz('', 11, 6.5)
# ggplot(wbank,
	# aes(x = Written, y = LogFreq, col = Age)) +
	# geom_point(shape = 16) +
	# geom_smooth(method = 'lm') + 
	# facet_wrap(~Age)

# ## For every month, compute the fit of the Iconicity-only model:

# AIC_vals <- c()
# slopes <- c()
# for(i in unique(wbank$Age)) {
	# temporary_subset <- filter(wbank, Age == i)
	
	# temporary_model <- lm(LogFreq ~ Written, temporary_subset)
		
	# AIC_vals <- c(AIC_vals, AIC(temporary_model))
	# slopes <- c(slopes, coef(temporary_model)[2])
	# }

# ## Plot AIC values:

# quartz('', 9, 6)
# plot(unique(wbank$Age), AIC_vals, type = 'l', lwd = 2)

# ## Plot slopes:

# quartz('', 9, 6)
# plot(unique(wbank$Age), slopes, type = 'l', lwd = 2)

# ## Create a measure of "inflated in child language frequency" by using SUBTLEX:

# wbank$InflatedFreq <- wbank$LogFreq/wbank$WordFreq

# ## Plot this:

# quartz('', 11, 6.5)
# ggplot(wbank,
	# aes(x = Written, y = InflatedFreq, col = Age)) +
	# geom_point(shape = 16) +
	# geom_smooth(method = 'lm') + 
	# facet_wrap(~Age)

# ## Take an average inflated measure for each word (across all ages):

# wbank_sums <- aggregate(Freq ~ Word, wbank, sum)
# wbank_sums$LogFreq <- wbank_sums$Freq
# wbank_sums$LogFreqSUBTLEX <- wbank[match(wbank_sums$Word,wbank$Word),]$WordFreq
# wbank_sums$Written <- wbank[match(wbank_sums$Word,wbank$Word),]$Written
# wbank_sums$Systematicity <- icon[match(wbank_sums$Word,icon$Word),]$Systematicity
# wbank_sums$InflatedFreq <- wbank_sums$LogFreq / wbank_sums$LogFreqSUBTLEX

# ## Sort by inflated freq to see whether it make sense (are the highest words more 'child-y'?):

# head(arrange(wbank_sums, desc(InflatedFreq)))

# ## Plot inflated freq against written iconicity:

# quartz('', 9, 6)
# ggplot(wbank_sums,
	# aes(x = Written, y = InflatedFreq)) +
	# geom_point(shape = 16) +
	# geom_smooth(method = 'lm')

# ## Plot inflated freq against systematicity:

# quartz('', 9, 6)
# ggplot(wbank_sums,
	# aes(x = Systematicity, y = InflatedFreq)) +
	# geom_point(shape = 16) +
	# geom_smooth(method = 'lm')

# ## Make models of this:

# summary(lm(InflatedFreq ~ Written, wbank_sums))
# summary(lm(InflatedFreq ~ Systematicity, wbank_sums))			# basically no r-squared

# ## For modeling main data, center:

# wbank <- mutate(wbank,
	# Written_c = Written - mean(Written, na.rm = T),
	# Systematicity_c = Systematicity - mean(Systematicity, na.rm = T),
	# InflatedFreq_c = InflatedFreq - mean(InflatedFreq, na.rm = T),
	# WordFreq_c = WordFreq - mean(WordFreq, na.rm = T),
	# Age_c = Age - mean(Age, na.rm = T))

# ## Model this relationship, first with the full data:

# xmdl <- lmer(InflatedFreq ~ Written_c + Age_c + WordFreq_c + 
	# Written_c:Age_c + WordFreq_c:Age_c + 
	# (1|Word), wbank)
# summary(xmdl)

# ## Calculate frequency change slopes, i.e., words that
# ## decreased or increased in frequency over the 14 month period:

# wbank_slopes <- data.frame(Word = unique(wbank$Word))
# wbank_slopes$Slope <- numeric(nrow(wbank_slopes))
# for (i in wbank_slopes$Word) {
	# xtemp <- wbank[wbank$Word == i,]
	# wbank_slopes[wbank_slopes$Word == i,]$Slope <- coef(lm(LogFreq ~ Age, xtemp))[2]
	# }

# ## Check whether this makes sense:

# head(arrange(wbank_slopes, desc(Slope)), 20)

# ## Add iconicity:

# wbank_slopes$Written <- icon[match(wbank_slopes$Word, icon$Word), ]$Written

# ## Make a plot of frequency increase/decrease (slopes) against iconicity:

# quartz('', 9, 6)
# ggplot(wbank_slopes,
	# aes(x = Written, y = Slope)) +
	# geom_point(shape = 16) +
	# geom_smooth(method = 'lm')

# summary(lm(Slope ~ Written, wbank_slopes))




##------------------------------------------------------------------
## Part #5: Parental input frequency
##------------------------------------------------------------------

## Create a relative frequency measure (which words are relatively more inflated in input frequency):

icon$RelativeFreq <- icon$LogParentFreq / icon$WordFreq
icon <- mutate(icon,
	RelativeFreq_c = RelativeFreq - mean(RelativeFreq, na.rm = T))

## Check which words are inflated:

dplyr:::select(icon, RelativeFreq, Word) %>% arrange(RelativeFreq)

## How is this relative frequency related to the overall population frequency?

quartz('', 9, 6)
ggplot(icon,
	aes(x = WordFreq, y = RelativeFreq)) + 
	geom_point(shape = 16)

## See whether this relative frequency measure predicts iconicity:

quartz('', 9, 6)
ggplot(icon,
	aes(x = RelativeFreq, y = Written)) + 
	geom_point(shape = 16) + geom_smooth(method = 'lm')
	
## Model this, first with a simple model:

xmdl.parent <- lm(RelativeFreq ~ Written, icon)
summary(xmdl.parent)

## Then incorporating the other factors:

xmdl.parent2 <- lm(RelativeFreq ~ Written_c + WordFreq_c, icon)
summary(xmdl.parent2)

## How much does iconicity contribute above and beyond frequency?

summary(xmdl.parent2)$r.squared
summary(xmdl.parent)$r.squared



