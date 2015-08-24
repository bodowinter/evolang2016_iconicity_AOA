## Bodo Winter
## August 24, 2015
## Analysis of iconicity data for Evolang

## Analysis is split into three parts:
## (1) - Analysis of Brysbaert et al. (2014) AOA ratings
## (2) - Analysis of MCDI usage statistics
## (3) - Analysis of Parental Input Frequency

library(ggplot2)
library(car)
library(dplyr)
library(qpcR)
library(reshape2)
library(lme4)


##------------------------------------------------------------------
## Preprocessing:
##------------------------------------------------------------------

## Load in datasets:

setwd('/Users/teeniematlock/Desktop/research/iconicity/evolang_AOA_analysis/')
icon <- read.csv('iconicity_ratings_both.csv')
mon <- read.csv('monaghan2014_systematicity.csv')
mcdi <- read.csv('MCDI_item_data.csv')
parent <- readLines('CHILDES_parentfreq.txt')		# what a messy format!

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

## Make an analysis of this, first a partial analysis ignoring all other factors:

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
vif(xmdl)			# actually o.k.

## Check how much _unique_ variance iconicity contributes:

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
evidence(xmdl.icon, xmdl.sys)			# evidence ratios; 30 times more support for iconicity model



##------------------------------------------------------------------
## Part #2: MCDI analysis
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
## Part #3: Parental input frequency
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



