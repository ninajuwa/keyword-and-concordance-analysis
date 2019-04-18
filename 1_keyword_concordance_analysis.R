#### KEYWORD ANALYSIS ####

#based on a script in Gries (2016: 197f.)
#list of motion verbs and BNC lemma frequencies based on Cifuentes Ferez (2008) and Feist & Duffy (in preparation)

#### packages & functions needed ####

#cf. Gries (2016: 197f.)
log.0 <- function (some.number, base=exp(1)) { # define a function to compute a log of some number with some base (default: natural log)
  ifelse(some.number==0,              # if the number of which the log is computed is 0,
         0,                           # then return 0,
         log(some.number, base=base)) # otherwise return the log of the non-zero number to the user-defined or default base
}

library(gdata)
library(vcd)
library(ggplot2)

#### load data ####
motion_verbs <- read.delim(file = file.choose(), sep = "\t") # file: "2_KW_motion_verb_list.csv"

#variables:
# $English.Motion.verb = motion verb lemma
# $Lemma.Freq.BNC = lemma frequency of the verb in the BNC
# $Lemma.Freq.Music.Corp = lemma frequency of the verb in the music criticism corpus
# $Semantic-Components = kind of verb (e.g. manner or path), adopted from Cifuentes Ferez (2008)

#### calculate keyness (with BNC as reference corpus) ####

#for the keyword analysis, we need the total number of words for both coropora
total.music <- 6679261 #target corpus
total.BNC <- 98313429 # reference corpus

#add normalised frequencies  (here per 1,000 words)
motion_verbs$BNC.permille <-  round(motion_verbs$Lemma.Freq.BNC/total.BNC*1000,4)
motion_verbs$musicorp.permille <- round(motion_verbs$Lemma.Freq.Music.Corp/total.music*1000,4) 

#This provides a first impression of the over/underrepresentation of the motion verbs in the music criticism corpus
#Furthermore, we will use these normalised frequencies below to determine positive (significantly overrepresented in music criticism corpus) and negative keywords (significantly underrepresented in music criticism corpus)

#calculate observed and expected frequencies (based on Gries 2016)
obs.as <- motion_verbs$Lemma.Freq.Music.Corp #observed lemma frequencies target corpus
obs.bs <- motion_verbs$Lemma.Freq.BNC #observed lemma frequencies reference corpus
obs.cs <- total.music - motion_verbs$Lemma.Freq.Music.Corp #all other words target corpus
obs.ds <- total.BNC - motion_verbs$Lemma.Freq.BNC #all other words reference corpus
totals <- total.music + total.BNC #all words in both corpora

exp.as <- (obs.as+obs.bs)*(obs.as+obs.cs)/totals # the frequency expected for obs.as if the null hypothesis was true
exp.bs <- (obs.as+obs.bs)*(obs.bs+obs.ds)/totals # the frequency expected for obs.bs if the null hypothesis was true
exp.cs <- (obs.as+obs.cs)*(obs.cs+obs.ds)/totals # the frequency expected for obs.cs if the null hypothesis was true
exp.ds <- (obs.bs+obs.ds)*(obs.cs+obs.ds)/totals # the frequency expected for obs.ds if the null hypothesis was true

# compute log-likelihood ratios for all words, save results into a separate column
motion_verbs$llrs <- 2*(
  obs.as*log.0(obs.as/exp.as)+
    obs.bs*log.0(obs.bs/exp.bs)+
    obs.cs*log.0(obs.cs/exp.cs)+
    obs.ds*log.0(obs.ds/exp.ds)
)

#### KW analysis RESULTS ####

##positive keywords####
key.motion.verbs <- motion_verbs[motion_verbs$llrs > qchisq(0.05,1,lower.tail = F) & motion_verbs$musicorp.permille > motion_verbs$BNC.permille,]
#this creates a new data frame with keywords only, i.e. those whose llrs is sufficiently high, i.e. significant in relation to a threshold level of p = .05 ( LL > 3.84, corresponding to p < 0.05), furthermore it singles out all positive keywords (as opposed to negative ones)

#sort by keyness
key.motion.verbs.sorted <- key.motion.verbs[order(key.motion.verbs$llrs, decreasing = T),]
head(key.motion.verbs.sorted)

## with respect to kind of verb
key.motion.verbs.sorted <- drop.levels(key.motion.verbs.sorted)
table(key.motion.verbs.sorted$Semantic.components)

## collapse categories
#"tail" will be considered a path verb
key.motion.verbs.sorted[key.motion.verbs.sorted$English.Motion.verb=="tail", 4] <- "Path"
#which are Path + Manner + Ground
key.motion.verbs.sorted[key.motion.verbs.sorted$Semantic.components=="Path + Manner + Ground",1] # only swoop
#"swoop" will be considered a path + manner verb
key.motion.verbs.sorted[key.motion.verbs.sorted$English.Motion.verb=="swoop", 4] <- "Path + Manner"
#which are Ground (air) + Manner 
key.motion.verbs.sorted[key.motion.verbs.sorted$Semantic.components=="Ground (air) + Manner ", 1] # only waft
#"waft" will be considered a manner verb
key.motion.verbs.sorted[key.motion.verbs.sorted$English.Motion.verb=="waft", 4] <- "Manner"
#which are Ground (on air) 
key.motion.verbs.sorted[key.motion.verbs.sorted$Semantic.components=="Ground (on air)", 1] # only hover
#"hover" will be considered a manner verb (in line with Levin 1993: 251)
key.motion.verbs.sorted[key.motion.verbs.sorted$English.Motion.verb=="hover", 4] <- "Manner"

key.motion.verbs.sorted <- drop.levels(key.motion.verbs.sorted)
table(key.motion.verbs.sorted$Semantic.components)

## which key motion verbs?
#path
key.motion.verbs.sorted[key.motion.verbs.sorted$Semantic.components=="Path", c(1,3,7)]

#manner
key.motion.verbs.sorted[key.motion.verbs.sorted$Semantic.components=="Manner", c(1,3,7)]

#co-motion
key.motion.verbs.sorted[key.motion.verbs.sorted$Semantic.components=="Figure + CO-MOTION", c(1,3,7)]

#path + manner
key.motion.verbs.sorted[key.motion.verbs.sorted$Semantic.components=="Path + Manner", c(1,3,7)]

#### comparision of manner and path frequencies to BNC ####
## type frequency ####
# N of types for motion verb type in music criticism corpus:
table(key.motion.verbs.sorted$Semantic.components)

# N of types for motion verb type in BNC:

#first, collapse categories
#tail in BNC as path
motion_verbs[motion_verbs$English.Motion.verb=="tail", 4] <-  "Path"
#waft in BNC as manner
motion_verbs[motion_verbs$English.Motion.verb=="waft", 4] <- "Manner"
#hover in BNC as manner
motion_verbs[motion_verbs$English.Motion.verb=="hover", 4] <- "Manner"

key.verbs.type.freq.BNC <- sort(table(motion_verbs$Semantic.components), decreasing = T); key.verbs.type.freq.BNC

#Does type frequency of verb types significantly differ in music corpus (keywords only) compared to BNC (all)?

verb.types <- data.frame(music=c(36,10), BNC=c(82,31))
rownames(verb.types) <- c("manner", "path"); verb.types
chisq.test(verb.types) # not significant

#the distribution of number of verb types for manner verbs compared to path verbs of the key motion verbs in music criticism does not significantly differ from the number of manner and path verb types in the BNC, the proportions are the same, i.e. there are not surprisingly more manner verb types among the keywords in the music criticism corpus

#### token frequency ####

## music criticism corpus:
key.verbs.token.freq <- sort(tapply(key.motion.verbs.sorted$Lemma.Freq.Music.Corp, key.motion.verbs.sorted$Semantic.components, FUN = sum), decreasing = T); key.verbs.token.freq

#BNC:
key.verbs.token.freq.BNC <- sort(tapply(motion_verbs$Lemma.Freq.BNC, motion_verbs$Semantic.components, FUN = sum), decreasing = T); key.verbs.token.freq.BNC

verb.tokens <- data.frame(music=c(3258,6402), BNC=c(177817,592846))
rownames(verb.tokens) <- c("manner", "path"); verb.tokens
chisq.test(verb.tokens) # significant

chisq.test(verb.tokens)$residuals # indicates that it is the number of manner verbs in music criticism that contributes to the significant chi-2 value 

## negative keywords ####
negative.key.motion.verbs <- motion_verbs[motion_verbs$llrs > qchisq(0.05,1,lower.tail = F) & motion_verbs$musicorp.permille < motion_verbs$BNC.permille,]

#sort by keyness
negative.key.motion.verbs.sorted <- negative.key.motion.verbs[order(negative.key.motion.verbs$llrs, decreasing = T),]
head(negative.key.motion.verbs.sorted)

#### CONCORDANCE ANALYSIS ####

rm(list=ls(all=TRUE)) # clear memory

#load data:
all.verbs.new <- read.delim(file = file.choose(), quote = "") # file: "3_concordances.csv"

##clean data
#correct POS
summary(all.verbs.new$POS)
all.verbs.new$POS <- gsub("noun", "N", all.verbs.new$POS)
all.verbs.new$POS <- as.factor(all.verbs.new$POS)
all.verbs.new <- drop.levels(all.verbs.new)

#subsume "tumble" under manner verbs
summary(all.verbs.new$VERB_TYPE)
all.verbs.new$VERB_TYPE[all.verbs.new$LEMMA=="tumble"] <- "manner"
all.verbs.new <- drop.levels(all.verbs.new)

#exclude unclear cases
summary(all.verbs.new$METAPHOR.MUSIC)

all.verbs.new <- all.verbs.new[all.verbs.new$METAPHOR.MUSIC!="exclude",]
all.verbs.new <- all.verbs.new[all.verbs.new$METAPHOR.MUSIC!="unclear",]
all.verbs.new <- drop.levels(all.verbs.new)
summary(all.verbs.new$METAPHOR.MUSIC)

summary(all.verbs.new$METAPHOR.MUSIC)/length(all.verbs.new$METAPHOR.MUSIC) * 100

#Literal cases: subsume them under "no" in extra colum
all.verbs.new$MUSICAL_MOTION <- gsub("L", "no", all.verbs.new$METAPHOR.MUSIC)
all.verbs.new$MUSICAL_MOTION <- as.factor(all.verbs.new$MUSICAL_MOTION)

#### descriptive stats ####

### musical motion instances ####
summary(all.verbs.new$MUSICAL_MOTION)

#percentage musical motion yes / no
summary(all.verbs.new$MUSICAL_MOTION)/length(all.verbs.new$MUSICAL_MOTION)

#musical motion instances in relation to POS
table(all.verbs.new$MUSICAL_MOTION, all.verbs.new$POS)
table(all.verbs.new$POS[all.verbs.new$MUSICAL_MOTION=="yes"])
table(all.verbs.new$POS[all.verbs.new$MUSICAL_MOTION=="yes"])/length(all.verbs.new$POS[all.verbs.new$MUSICAL_MOTION=="yes"])

#### musical motion in relation to verb type ####
### frequency of musical motion use in relation to manner / path verb
table(all.verbs.new$VERB_TYPE, all.verbs.new$MUSICAL_MOTION)
# %
prop.table(table(all.verbs.new$VERB_TYPE, all.verbs.new$MUSICAL_MOTION), 1)*100

# Is the difference significant?
chisq.test(table(all.verbs.new$VERB_TYPE, all.verbs.new$MUSICAL_MOTION))# significant

#plot
ggplot(all.verbs.new, aes(x=VERB_TYPE)) + geom_bar(aes(fill=MUSICAL_MOTION), position = "dodge") + ylab("Frequency of motion verb instances")

### musical motion instances in relation to REGISTER and verb type
addmargins(table(all.verbs.new$VERB_TYPE[all.verbs.new$MUSICAL_MOTION=="yes"], all.verbs.new$REGISTER[all.verbs.new$MUSICAL_MOTION=="yes"]))

prop.table(table(all.verbs.new$VERB_TYPE[all.verbs.new$MUSICAL_MOTION=="yes"], all.verbs.new$REGISTER[all.verbs.new$MUSICAL_MOTION=="yes"]),1)

chisq.test(table(all.verbs.new$VERB_TYPE[all.verbs.new$MUSICAL_MOTION=="yes"], all.verbs.new$REGISTER[all.verbs.new$MUSICAL_MOTION=="yes"]))

#plot
#create subset, only musical motion = yes
musical.motion <- all.verbs.new[all.verbs.new$MUSICAL_MOTION=="yes",]

ggplot(musical.motion, aes(x=VERB_TYPE)) + geom_bar(aes(fill=REGISTER), position = "dodge") + ylab("Frequency of musical motion instances")


#### deictic uses of musical motion instances ####
musical.motion <- drop.levels(musical.motion)
addmargins(table(musical.motion$Deictic))
#%
table(musical.motion$Deictic)/length(musical.motion$Deictic)

is.na(musical.motion$Deictic) # row 318 is NA, delete:
musical.motion <- musical.motion[-(318),]

qplot(musical.motion$Deictic) + ylab("Deictic uses of Musical Motion") + ylim(c(0,850)) + xlab("")

# deicticness in relation to verb type 
table(musical.motion$Deictic, musical.motion$VERB_TYPE)
prop.table(table(musical.motion$Deictic, musical.motion$VERB_TYPE),2)*100

#significant?
chisq.test(table(musical.motion$Deictic, musical.motion$VERB_TYPE))
#X-squared = 67.158, df = 1, p-value = 2.506e-16
chisq.test(table(musical.motion$Deictic, musical.motion$VERB_TYPE))$residuals

ggplot(musical.motion, aes(x=VERB_TYPE)) + geom_bar(aes(fill=Deictic), position = "dodge") + ylab("Frequency of musical motion instances")

# deicticness in relation to verb lemma
table(musical.motion$Deictic, musical.motion$LEMMA)

prop.table(table(musical.motion$Deictic, musical.motion$LEMMA),2)*100

musical.motion$VERB_TYPE <- relevel(musical.motion$VERB_TYPE,"path")
ggplot(musical.motion, aes(x=LEMMA)) + geom_bar(aes(fill=Deictic)) + facet_wrap(~VERB_TYPE, scales = "free") + ylim(c(0,180)) + ylab("Frequency of musical motion instances")

##### type of motion ####
#translational or self-contained
#in relation to verb type
table(musical.motion$MOTION_TYPE, musical.motion$VERB_TYPE)
addmargins(table(musical.motion$MOTION_TYPE, musical.motion$VERB_TYPE))
prop.table(table(musical.motion$MOTION_TYPE, musical.motion$VERB_TYPE),2)
