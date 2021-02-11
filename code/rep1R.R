# set working directory
getwd()
path <- '/home/jovyan'
setwd(path)
getwd()

# set subdirectories
# dir.create checks whether subdirectories exist; if missing, creates them
dir.create("./data",showWarnings = TRUE)
dir.create("./data/original",showWarnings = TRUE)
dir.create("./data/working",showWarnings = TRUE)
dir.create("./figures",showWarnings = TRUE)
dir.create("./tables",showWarnings = TRUE)

#set options
# Change plot size to 4 x 4; set digits
options(repr.plot.width=4, repr.plot.height=4, digits=13)

# authors packages
mypackages <- c("Hmisc", "tidyverse", "GGally", "car", "interplot", "interflex", "gridExtra", "coefplot", "psych")
install.packages(mypackages, lib="/network/rit/home/mi122167/Rv360lib", verbose=TRUE)


library(Hmisc, tidyverse)

#install necessary packages
install.packages("pacman") # pacman = package manager
library(pacman) 

# this sequence works to load some dependencies first
p_load(#stringi, 
    Hmisc, 
       #xml2, rvest, tidyselect, dplyr, tidyr, lubridate, readr, fs, 
    tidyverse, 
       GGally, 
       #openxlsx, 
    car, 
       #statmod, minqa, AR, sobolseq, timeR, stop, redblack, qsort, rescale, nloptr, 
    interplot, 
       #reshape2, ModelMetrics, 
    interflex, 
       gridExtra, 
       coefplot, 
       #mnormt, 
    psych)
# additional packages
#p_load(data.table, xtable, stargazer, repr)

# document session info
sessionInfo()

# read data using relative file path
dat <- read.csv("./data/original/metoo_data.csv", stringsAsFactors = FALSE)

# save working data file
datetoday <- gsub("-", "", as.character(Sys.Date()))
save.image(paste("./data/working/working", datetoday, ".RData", sep=""))

### DATA CLEANING ####### ######

# recode: experimental condition ####
dat$condition2[dat$condition==1] <- "Jokes"
dat$condition2[dat$condition==2] <- "Assault"
dat$condition2[dat$condition==3] <- "Control"
dat$condition2 <- as.factor(dat$condition2)

## relevel so Control is baseline category
dat$condition2 <- relevel(dat$condition2, "Control")

# new variable: pid3 ####
table(dat$pid7)
dat$pid3 <- NA
dat$pid3[dat$pid7=="Lean Democrat" | dat$pid7=="Strong Democrat" | dat$pid7=="Not very strong Democrat"] <- "Democrat"
dat$pid3[dat$pid7=="Lean Republican" | dat$pid7=="Strong Republican" | dat$pid7=="Not very strong Republican"] <- "Republican"
dat$pid3[dat$pid7=="Independent" | dat$pid7=="Not sure"] <- "Independent"

dat$pid3 <- as.factor(dat$pid3)

# recode: punishment ####

# punishment 1
dat$needmoreevidence[dat$punishment_1=="Agree strongly"] <- 5
dat$needmoreevidence[dat$punishment_1=="Agree somewhat"] <- 4
dat$needmoreevidence[dat$punishment_1=="Neither disagree nor agree"] <- 3
dat$needmoreevidence[dat$punishment_1=="Disagree somewhat"] <- 2
dat$needmoreevidence[dat$punishment_1=="Disagree strongly"] <- 1
table(dat$needmoreevidence)

# punishment 2
dat$apology[dat$punishment_2=="Agree strongly"] <- 5
dat$apology[dat$punishment_2=="Agree somewhat"] <- 4
dat$apology[dat$punishment_2=="Neither disagree nor agree"] <- 3
dat$apology[dat$punishment_2=="Disagree somewhat"] <- 2
dat$apology[dat$punishment_2=="Disagree strongly"] <- 1

# punishment 3
dat$longtimeago[dat$punishment_3=="Agree strongly"] <- 5
dat$longtimeago[dat$punishment_3=="Agree somewhat"] <- 4
dat$longtimeago[dat$punishment_3=="Neither disagree nor agree"] <- 3
dat$longtimeago[dat$punishment_3=="Disagree somewhat"] <- 2
dat$longtimeago[dat$punishment_3=="Disagree strongly"] <- 1
table(dat$longtimeago)


# punishment 4
dat$resign[dat$punishment_4=="Agree strongly"] <- 5
dat$resign[dat$punishment_4=="Agree somewhat"] <- 4
dat$resign[dat$punishment_4=="Neither disagree nor agree"] <- 3
dat$resign[dat$punishment_4=="Disagree somewhat"] <- 2
dat$resign[dat$punishment_4=="Disagree strongly"] <- 1


# punishment 5
dat$elitecues[dat$punishment_5=="Agree strongly"] <- 5
dat$elitecues[dat$punishment_5=="Agree somewhat"] <- 4
dat$elitecues[dat$punishment_5=="Neither disagree nor agree"] <- 3
dat$elitecues[dat$punishment_5=="Disagree somewhat"] <- 2
dat$elitecues[dat$punishment_5=="Disagree strongly"] <- 1

# recode punishment: reverse codes
# need more evidence
dat$needmoreevidence_reverse[dat$punishment_1=="Agree strongly"] <- 1
dat$needmoreevidence_reverse[dat$punishment_1=="Agree somewhat"] <- 2
dat$needmoreevidence_reverse[dat$punishment_1=="Neither disagree nor agree"] <- 3
dat$needmoreevidence_reverse[dat$punishment_1=="Disagree somewhat"] <- 4
dat$needmoreevidence_reverse[dat$punishment_1=="Disagree strongly"] <- 5
table(dat$needmoreevidence_reverse, dat$needmoreevidence)

# long time ago
dat$longtimeago_reverse[dat$longtimeago==5] <- 1
dat$longtimeago_reverse[dat$longtimeago==4] <- 2
dat$longtimeago_reverse[dat$longtimeago==3] <- 3
dat$longtimeago_reverse[dat$longtimeago==2] <- 4
dat$longtimeago_reverse[dat$longtimeago==1] <- 5 
table(dat$longtimeago_reverse, dat$longtimeago)

# new variable: mean punitiveness score ####
dat$meanpunishment <- ((dat$apology+dat$resign+dat$needmoreevidence_reverse+dat$longtimeago_reverse)/4)

## new variable: same party as legislator####
table(dat$senator_party)

dat$sameparty[dat$pid3=="Democrat" & dat$senator_party=="Democrat" | dat$pid3=="Republican" & dat$senator_party=="Republican"] <- "Same party"
dat$sameparty[dat$pid3=="Democrat" & dat$senator_party=="Republican" | dat$pid3=="Republican" & dat$senator_party=="Democrat"] <- "Opposite party"
dat$sameparty[dat$pid3=="Independent"] <- "Independents/Not sures"

dat$sameparty <- as.factor(dat$sameparty)

# recode: pre sexism ####
# sexism_1,2,4 reverse coded

dat$pre_sexism_1 <- recode(dat$pre_sexism_1,"'Agree strongly'= 5; 'Agree somewhat'= 4; 'Neither disagree nor agree'= 3; 'Disagree somewhat'= 2; 'Disagree strongly'=1")
dat$pre_sexism_1 <- as.numeric(dat$pre_sexism_1)

dat$pre_sexism_2 <- recode(dat$pre_sexism_2,"'Agree strongly'= 5; 'Agree somewhat'= 4; 'Neither disagree nor agree'= 3; 'Disagree somewhat'= 2; 'Disagree strongly'=1")
dat$pre_sexism_2 <- as.numeric(dat$pre_sexism_2)

dat$pre_sexism_4 <- recode(dat$pre_sexism_4,"'Agree strongly'= 5; 'Agree somewhat'= 4; 'Neither disagree nor agree'= 3; 'Disagree somewhat'= 2; 'Disagree strongly'=1")
dat$pre_sexism_4 <- as.numeric(dat$pre_sexism_4)

dat$pre_sexism_3 <- recode(dat$pre_sexism_3,"'Agree strongly'= 1; 'Agree somewhat'= 2; 'Neither disagree nor agree'= 3; 'Disagree somewhat'= 4; 'Disagree strongly'=5")
dat$pre_sexism_3 <- as.numeric(dat$pre_sexism_3)

# recode: post sexism ####
# sexism_1,2,4 reverse coded

dat$post_sexism_1 <- recode(dat$post_sexism_1,"'Agree strongly'= 5; 'Agree somewhat'= 4; 'Neither disagree nor agree'= 3; 'Disagree somewhat'= 2; 'Disagree strongly'=1")
dat$post_sexism_1 <- as.numeric(dat$post_sexism_1)

dat$post_sexism_2 <- recode(dat$post_sexism_2,"'Agree strongly'= 5; 'Agree somewhat'= 4; 'Neither disagree nor agree'= 3; 'Disagree somewhat'= 2; 'Disagree strongly'=1")
dat$post_sexism_2 <- as.numeric(dat$post_sexism_2)

dat$post_sexism_4 <- recode(dat$post_sexism_4,"'Agree strongly'= 5; 'Agree somewhat'= 4; 'Neither disagree nor agree'= 3; 'Disagree somewhat'= 2; 'Disagree strongly'=1")
dat$post_sexism_4 <- as.numeric(dat$post_sexism_4)

dat$post_sexism_3 <- recode(dat$post_sexism_3,"'Agree strongly'= 1; 'Agree somewhat'= 2; 'Neither disagree nor agree'= 3; 'Disagree somewhat'= 4; 'Disagree strongly'=5")
dat$post_sexism_3 <- as.numeric(dat$post_sexism_3)

# new variable: pre_sexism ####

dat$pre_sexism <- ((dat$pre_sexism_1 + dat$pre_sexism_2 + dat$pre_sexism_3 + dat$pre_sexism_4)/4)


# new variable: post_sexism ####

dat$post_sexism <- ((dat$post_sexism_1 + dat$post_sexism_2 + dat$post_sexism_3 + dat$post_sexism_4)/4)

### new variable: raw change from pretest to posttest ####
# favorability
dat$change_favorability <- (dat$post_favorability - dat$pre_favorability)
summary(dat$change_favorability)

# vote
dat$change_vote <- (dat$post_vote - dat$pre_vote)
summary(dat$change_vote)

# sexism
dat$change_sexism <- (dat$post_sexism - dat$pre_sexism)
summary(dat$change_sexism)

### new variable: percent change from pretest to posttest ##### favorability
dat$perchange_favorability <- ((((dat$post_favorability+1) - (dat$pre_favorability+1))/(dat$pre_favorability+1))*100)
summary(dat$perchange_favorability)

# vote
dat$perchange_vote <- ((((dat$post_vote+1) - (dat$pre_vote+1))/(dat$pre_vote+1))*100)
summary(dat$perchange_vote)

# sexism
dat$perchange_sexism <- ((((dat$post_sexism+1) - (dat$pre_sexism+1))/(dat$pre_sexism+1))*100)
summary(dat$perchange_sexism)

# subset: without independents/notsures ####
partydat <- subset(dat, dat$sameparty!="Independents/Not sures")

# subset: people that share party with senator, people that do not share party with senator
samepartydat <- subset(dat, dat$sameparty=="Same party")
opppartydat <- subset(dat, dat$sameparty=="Opposite party")

#

# save working data file
datetoday <- gsub("-", "", as.character(Sys.Date()))
save.image(paste("./data/working/working", datetoday, ".RData", sep=""))
# if prefer csv, can save each data object also as csv
write.csv(dat,'./data/working/dat.csv', row.names=FALSE)
write.csv(partydat,'./data/working/partydat.csv', row.names=FALSE)
write.csv(samepartydat,'./data/working/samepartydat.csv', row.names=FALSE)
write.csv(opppartydat,'./data/working/opppartydat.csv', row.names=FALSE)

## ttests: just post_values ####

t.test(post_favorability ~ condition2,
                       data = dat,
                       subset = condition2 %in% c("Assault", "Control"))

t.test(post_favorability ~ condition2,
       data = dat,
       subset = condition2 %in% c("Jokes", "Control"))

t.test(post_favorability ~ condition2,
       data = dat,
       subset = condition2 %in% c("Jokes", "Assault"))

t.test(post_vote ~ condition2,
       data = dat,
       subset = condition2 %in% c("Assault", "Control"))

t.test(post_vote ~ condition2,
       data = dat,
       subset = condition2 %in% c("Jokes", "Control"))

t.test(post_vote ~ condition2,
       data = dat,
       subset = condition2 %in% c("Jokes", "Assault"))

#

## of sample (Table A1 in Online Appendix) ####

prop.table(table(dat$gender))
prop.table(table(dat$race))
prop.table(table(dat$ideo5))
prop.table(table(dat$pid3))
prop.table(table(dat$age))

## of pre_sexism ####

mean(dat$pre_sexism)
sd(dat$pre_sexism)

pre_sexism_dat <- c("pre_sexism_1", "pre_sexism_2", "pre_sexism_3", "pre_sexism_4")
pre_sexism_dat <- dat[pre_sexism_dat]

psych::alpha(pre_sexism_dat)

# of post_sexism

mean(dat$post_sexism)
sd(dat$post_sexism)

post_sexism_dat <- c("post_sexism_1", "post_sexism_2", "post_sexism_3", "post_sexism_4")
post_sexism_dat <- dat[post_sexism_dat]

psych::alpha(post_sexism_dat)


## of pre_favorability and pre_vote ####

mean(dat$pre_favorability)
sd(dat$pre_favorability)

mean(dat$pre_vote)
sd(dat$pre_vote)

## of mean punitiveness ####

mean(dat$meanpunishment, na.rm=TRUE)
sd(dat$meanpunishment, na.rm=TRUE)

punish_index <- c("apology", "resign", "needmoreevidence_reverse", "longtimeago_reverse")
punish_index <- dat[punish_index]

psych::alpha(post_sexism_dat)

# of agreement with elite cues statement ####
mean(dat$elitecues, na.rm=TRUE)
sd(dat$elitecues, na.rm=TRUE)

#

### EFFECTS ON FAVORABILITY, VOTE, SEXISM, PUNISHMENT ####

# percent change favorability ####

perfav_sameparty <- lm(perchange_favorability ~ condition2, data=samepartydat)
summary(perfav_sameparty)

perfav_oppparty <- lm(perchange_favorability ~ condition2, data=opppartydat)
summary(perfav_oppparty)

#### FIGURE 1 ####
plot1 <- multiplot(perfav_sameparty, perfav_oppparty, intercept=F, title="", ylab=" ", xlab="Average Treatment Effect", newNames=c("condition2Assault"="Assault", "condition2Jokes"="Jokes"), innerCI=1, outerCI=2, lwdOuter=.5, lwdInner=.5, pointSize=4, dodgeHeight=.5, decreasing=TRUE) + theme_bw() +
  theme(legend.position="bottom") + scale_colour_manual(labels = c("Opposite party", "Same party"), values = c("black", "black")) + scale_shape_manual(labels = c("Opposite party", "Same party"), values=c(17, 19)) + 
  theme(axis.text=element_text(size=18),axis.title=element_text(size=18), plot.subtitle = element_text(size = 14), plot.title = element_text(size = 18), legend.text = element_text(size=16), legend.title=element_blank()) +
  labs(title="Treatment Effects on Change in Favorability", subtitle="Baseline category = control condition") +
  coord_cartesian(xlim=c(0, -45))

plot1

# tiff: coefplot_fav ####
tiff(file="coefplot_favorability.tiff", width=7, height=6, units="in", res=800)
plot1
dev.off()


# percent change vote ####

pervote_sameparty <- lm(perchange_vote ~ condition2, data=samepartydat)
summary(pervote_sameparty)

pervote_oppparty <- lm(perchange_vote ~ condition2, data=opppartydat)
summary(pervote_oppparty)

linearHypothesis(pervote_oppparty, "condition2Assault=condition2Jokes")

#### FIGURE 2 ####
plot2 <- multiplot(pervote_sameparty, pervote_oppparty, intercept=F, title="", ylab=" ", xlab="Average Treatment Effect", newNames=c("condition2Assault"="Assault", "condition2Jokes"="Jokes"), innerCI=1, outerCI=2, lwdOuter=.5, lwdInner=.5, pointSize=4, dodgeHeight=.5, decreasing=TRUE) + theme_bw() +
  theme(legend.position="bottom") + scale_colour_manual(labels = c("Opposite party", "Same party"), values = c("black", "black")) + scale_shape_manual(labels = c("Opposite party", "Same party"), values=c(17, 19)) + 
  theme(axis.text=element_text(size=18),axis.title=element_text(size=18), plot.subtitle = element_text(size = 14), plot.title = element_text(size = 18), legend.text = element_text(size=16), legend.title=element_blank()) +
  labs(title="Treatment Effects on Change in Electoral Support", subtitle="Baseline category = control condition")  +
  coord_cartesian(xlim=c(0, -45))

plot2

# tiff: coefplot_vote ####
tiff(file="coefplot_vote.tiff", width=7, height=6, units="in", res=800)
plot2
dev.off()

# percent change sexism ####

persexism_sameparty <- lm(perchange_sexism ~ condition2, data=samepartydat)
summary(persexism_sameparty)

persexism_oppparty <- lm(perchange_sexism ~ condition2, data=opppartydat)
summary(persexism_oppparty)

#### FIGURE 3 ####

plot3 <- multiplot(persexism_sameparty, persexism_oppparty, intercept=F, title="", ylab=" ", xlab="Average Treatment Effect", newNames=c("condition2Assault"="Assault", "condition2Jokes"="Jokes"), innerCI=1, outerCI=2, lwdOuter=.5, lwdInner=.5, pointSize=4, dodgeHeight=.5, decreasing=TRUE) + theme_bw() +
  theme(legend.position="bottom") + scale_colour_manual(labels = c("Opposite party", "Same party"), values = c("black", "black")) + scale_shape_manual(labels = c("Opposite party", "Same party"), values=c(17, 19)) + 
  theme(axis.text=element_text(size=18),axis.title=element_text(size=18), plot.subtitle = element_text(size = 14), plot.title = element_text(size = 18), legend.text = element_text(size=16), legend.title=element_blank()) +
  labs(title="Treatment Effects on Change in Sexism", subtitle="Baseline category = control condition")  +
  coord_cartesian(xlim=c(5, -5))

plot3

# tiff: coefplot_sexism ####
tiff(file="coefplot_sexism.tiff", width=7, height=6, units="in", res=800)
plot3
dev.off()


### punishment #####

#### FIGURE 4 ####
treatment_partydat <- subset(partydat, partydat$condition2!="Control")

punish_meansplot <- ggplot(treatment_partydat, aes(x=condition2, y=meanpunishment)) + stat_summary(fun.data= mean_cl_normal, fun.args = list(conf.int = 0.84)) +
  geom_hline(yintercept = 2.5, linetype="dashed", color="gray") +
  facet_wrap(~sameparty) +
  theme_bw() +
  theme(axis.text=element_text(size=16),axis.title=element_text(size=18), plot.subtitle = element_text(size = 14), plot.title = element_text(size = 16), strip.text = element_text(size = 16)) +
  labs(x="", y="Mean Punitiveness (1-5)", title="") +
  coord_cartesian(ylim=c(2.3,3.3))

punish_meansplot

tiff(file="punish_meansplot.tiff", width=7, height=5, units="in", res=800)
punish_meansplot
dev.off()

### difference in means for agreement with "apology" ####
t.test(apology ~ sameparty,
       data = partydat,
       subset = condition2 %in% c("Assault"))

t.test(apology ~ sameparty,
       data = partydat,
       subset = condition2 %in% c("Jokes"))

#

## CONDITIONAL ON SEXISM ####

# favorability
model.sexism <- lm(perchange_favorability ~ condition2 + pre_sexism + condition2*pre_sexism, data=dat)
summary(model.sexism)

#### FIGURE 5 ####
sexism_fav_interplot <- interplot(m=model.sexism, var1="condition2", var2="pre_sexism", rfill="lightgrey", ralpha=.35, facet_labs = c("Assault", "Jokes")) + theme_bw() + 
  geom_line(size=.7) + 
  geom_hline(yintercept=0, linetype="dashed", colour="darkgrey") + 
  ylim(-60, 3) +
  labs(x="Sexism (1-5)", y="Treatment Effect", title="Conditional Effects of Sexism on Change in Favorability") +
  theme(axis.text=element_text(size=16),axis.title=element_text(size=18), plot.subtitle = element_text(size = 16), plot.title = element_text(size = 20), strip.text = element_text(size = 16))

sexism_fav_interplot


tiff(file="sexism_fav_plot.tiff", width=9, height=5.5, units="in", res=800)
sexism_fav_interplot
dev.off()

# vote
model.sexism <- lm(perchange_vote ~ condition2 + pre_sexism + condition2*pre_sexism, data=dat)
summary(model.sexism)

#### FIGURE 6 ####
sexism_vote_interplot <- interplot(m=model.sexism, var1="condition2", var2="pre_sexism", rfill="lightgrey", ralpha=.35, facet_labs = c("Assault", "Jokes")) + theme_bw() + 
  geom_line(size=.7) + 
  geom_hline(yintercept=0, linetype="dashed", colour="darkgrey") + 
  ylim(-60, 7) +
  labs(x="Sexism (1-5)", y="Treatment Effect", title="Conditional Effects of Sexism on Change in Electoral Support") +
  theme(axis.text=element_text(size=16),axis.title=element_text(size=18), plot.subtitle = element_text(size = 16), plot.title = element_text(size = 20), strip.text = element_text(size = 16))

sexism_vote_interplot

tiff(file="sexism_vote_plot.tiff", width=9, height=5.5, units="in", res=800)
sexism_vote_interplot
dev.off()


#### FIGURE 7 ####
punishplot<-ggplot(dat, aes(pre_sexism, meanpunishment)) + 
  geom_smooth(method="lm", color="black") +
  theme_bw() +
  theme(axis.text=element_text(size=18),axis.title=element_text(size=20), plot.title = element_text(size = 20)) +
  xlab("Sexism (1-5)") +
  ylab("Mean Punitiveness (1-5)") +
  ggtitle("Effect of Sexism on Punitiveness") + coord_cartesian(ylim=c(2, 3.5))

punishplot

tiff(file="punish_sexism.tiff", width=10, height=8, units="in", res=800)
punishplot
dev.off()

#

### ONLINE APPENDIX ####### ####

# raw change favorability ####

fav_sameparty <- lm(change_favorability ~ condition2, data=samepartydat)
summary(fav_sameparty)

fav_oppparty <- lm(change_favorability ~ condition2, data=opppartydat)
summary(fav_oppparty)

#### FIGURE A1 ####
plot1raw <- multiplot(fav_sameparty, fav_oppparty, intercept=F, title="", ylab=" ", xlab="ATE on (Raw) Change", newNames=c("condition2Assault"="Assault", "condition2Jokes"="Jokes"), innerCI=1, outerCI=2, lwdOuter=.5, lwdInner=.5, pointSize=4, dodgeHeight=.5, decreasing=TRUE) + theme_bw() +
  theme(legend.position="bottom") + scale_colour_manual(labels = c("Opposite party", "Same party"), values = c("black", "black")) + scale_shape_manual(labels = c("Opposite party", "Same party"), values=c(17, 19)) + 
  theme(axis.text=element_text(size=18),axis.title=element_text(size=18), plot.subtitle = element_text(size = 14), plot.title = element_text(size = 18), legend.text = element_text(size=16), legend.title=element_blank()) +
  labs(title="Treatment Effects on Change in Favorability", subtitle="Baseline category = control condition") +
  coord_cartesian(xlim=c(0, -3.5))

plot1raw

# tiff: coefplot_fav ####
tiff(file="raw_coefplot_favorability.tiff", width=7, height=6, units="in", res=800)
plot1raw
dev.off()


# raw change vote ####

vote_sameparty <- lm(change_vote ~ condition2, data=samepartydat)
summary(vote_sameparty)

vote_oppparty <- lm(change_vote ~ condition2, data=opppartydat)
summary(pervote_oppparty)

#### FIGURE A2 ####
plot2raw <- multiplot(vote_sameparty, vote_oppparty, intercept=F, title="", ylab=" ", xlab="ATE on (Raw) Change", newNames=c("condition2Assault"="Assault", "condition2Jokes"="Jokes"), innerCI=1, outerCI=2, lwdOuter=.5, lwdInner=.5, pointSize=4, dodgeHeight=.5, decreasing=TRUE) + theme_bw() +
  theme(legend.position="bottom") + scale_colour_manual(labels = c("Opposite party", "Same party"), values = c("black", "black")) + scale_shape_manual(labels = c("Opposite party", "Same party"), values=c(17, 19)) + 
  theme(axis.text=element_text(size=18),axis.title=element_text(size=18), plot.subtitle = element_text(size = 14), plot.title = element_text(size = 18), legend.text = element_text(size=16), legend.title=element_blank()) +
  labs(title="Treatment Effects on Change in Electoral Support", subtitle="Baseline category = control condition")+
  coord_cartesian(xlim=c(0, -3.5))

plot2raw

# tiff: coefplot_vote ####
tiff(file="raw_coefplot_vote.tiff", width=7, height=6, units="in", res=800)
plot2raw
dev.off()

# raw change sexism ####

sexism_sameparty <- lm(change_sexism ~ condition2, data=samepartydat)
summary(sexism_sameparty)

sexism_oppparty <- lm(change_sexism ~ condition2, data=opppartydat)
summary(sexism_oppparty)

#### FIGURE A3 ####
plot3raw <- multiplot(sexism_sameparty, sexism_oppparty, intercept=F, title="", ylab=" ", xlab="ATE on (Raw) Change", newNames=c("condition2Assault"="Assault", "condition2Jokes"="Jokes"), innerCI=1, outerCI=2, lwdOuter=.5, lwdInner=.5, pointSize=4, dodgeHeight=.5, decreasing=TRUE) + theme_bw() +
  theme(legend.position="bottom") + scale_colour_manual(labels = c("Opposite party", "Same party"), values = c("black", "black")) + scale_shape_manual(labels = c("Opposite party", "Same party"), values=c(17, 19)) + 
  theme(axis.text=element_text(size=18),axis.title=element_text(size=18), plot.subtitle = element_text(size = 14), plot.title = element_text(size = 18), legend.text = element_text(size=16), legend.title=element_blank()) +
  labs(title="Treatment Effects on Change in Sexism", subtitle="Baseline category = control condition") +   coord_cartesian(xlim=c(0.15, -0.1))

plot3raw

# tiff: coefplot_sexism ####
tiff(file="raw_coefplot_sexism.tiff", width=7, height=6, units="in", res=800)
plot3raw
dev.off()

## GENDER #####

# subset: men & women
men <- subset(dat, dat$gender=="Man")
women <- subset(dat, dat$gender=="Woman")

# gender ### percent change favorability ###

perfav_men <- lm(perchange_favorability ~ condition2, data=men)
summary(perfav_men)

perfav_women <- lm(perchange_favorability ~ condition2, data=women)
summary(perfav_women)

#### FIGURE A4 ####
plot1gender <- multiplot(perfav_men, perfav_women, intercept=F, title="", ylab=" ", xlab="Average Treatment Effect", newNames=c("condition2Assault"="Assault", "condition2Jokes"="Jokes"), innerCI=1, outerCI=2, lwdOuter=.5, lwdInner=.5, pointSize=4, dodgeHeight=.5, decreasing=TRUE) + theme_bw() +
  theme(legend.position="bottom") + scale_colour_manual(labels = c("Men", "Women"), values = c("black", "black")) + scale_shape_manual(labels = c("Men", "Women"), values=c(17, 19)) + 
  theme(axis.text=element_text(size=18),axis.title=element_text(size=18), plot.subtitle = element_text(size = 14), plot.title = element_text(size = 18), legend.text = element_text(size=16), legend.title=element_blank()) +
  labs(title="Treatment Effects on Change in Favorability", subtitle="Baseline category = control condition") +
  coord_cartesian(xlim=c(0, -45))

plot1gender

# tiff: coefplot_fav GENDER ###
tiff(file="coefplot_favorability_GENDER.tiff", width=7, height=6, units="in", res=800)
plot1gender
dev.off()


# gender ### percent change vote ###

pervote_men <- lm(perchange_vote ~ condition2, data=men)
summary(pervote_men)

pervote_women <- lm(perchange_vote ~ condition2, data=women)
summary(pervote_women)

#### FIGURE A5 ####
plot2gender <- multiplot(pervote_men, pervote_women, intercept=F, title="", ylab=" ", xlab="Average Treatment Effect", newNames=c("condition2Assault"="Assault", "condition2Jokes"="Jokes"), innerCI=1, outerCI=2, lwdOuter=.5, lwdInner=.5, pointSize=4, dodgeHeight=.5, decreasing=TRUE) + theme_bw() +
  theme(legend.position="bottom") + scale_colour_manual(labels = c("Men", "Women"), values = c("black", "black")) + scale_shape_manual(labels = c("Men", "Women"), values=c(17, 19)) + 
  theme(axis.text=element_text(size=18),axis.title=element_text(size=18), plot.subtitle = element_text(size = 14), plot.title = element_text(size = 18), legend.text = element_text(size=16), legend.title=element_blank()) +
  labs(title="Treatment Effects on Change in Electoral Support", subtitle="Baseline category = control condition") +
  coord_cartesian(xlim=c(0, -45))

plot2gender

# tiff: coefplot_vote GENDER ###
tiff(file="coefplot_vote_GENDER.tiff", width=7, height=6, units="in", res=800)
plot2gender
dev.off()


### PARTY AFFILIATION ####

# subset: dems, repubs, independents/notsures
dems <- subset(dat, dat$pid3=="Democrat")
repubs <- subset(dat, dat$pid3=="Republican")
trueinds <- subset(dat, dat$pid7=="Independent")


# party ### percent change favorability ###

perfav_dems <- lm(perchange_favorability ~ condition2, data=dems)
summary(perfav_dems)

perfav_repubs <- lm(perchange_favorability ~ condition2, data=repubs)
summary(perfav_repubs)

perfav_inds <- lm(perchange_favorability ~ condition2, data=trueinds)
summary(perfav_inds)

#### FIGURE A6 ####
plot1party <- multiplot(perfav_dems, perfav_repubs,perfav_inds, intercept=F, title="", ylab=" ", xlab="Average Treatment Effect", newNames=c("condition2Assault"="Assault", "condition2Jokes"="Jokes"), innerCI=1, outerCI=2, lwdOuter=.5, lwdInner=.5, pointSize=4, dodgeHeight=.5, decreasing=TRUE) + theme_bw() +
  theme(legend.position="bottom") + scale_colour_manual(labels = c("Democrats", "Independents",  "Republicans"), values = c("blue2", "grey50", "firebrick3")) + scale_shape_manual(labels = c("Democrats", "Independents", "Republicans"), values=c(17, 19, 15)) + 
  theme(axis.text=element_text(size=18),axis.title=element_text(size=18), plot.subtitle = element_text(size = 14), plot.title = element_text(size = 18), legend.text = element_text(size=16), legend.title=element_blank()) +
  labs(title="Treatment Effects on Change in Favorability", subtitle="Baseline category = control condition") +
  coord_cartesian(xlim=c(0, -50))

plot1party

# tiff: coefplot_fav PARTY ###
tiff(file="coefplot_favorability_PARTY.tiff", width=7, height=6, units="in", res=800)
plot1party
dev.off()


# party ### percent change vote ###

pervote_dems <- lm(perchange_vote ~ condition2, data=dems)
summary(pervote_dems)

pervote_repubs <- lm(perchange_vote ~ condition2, data=repubs)
summary(pervote_repubs)

pervote_inds <- lm(perchange_vote ~ condition2, data=trueinds)
summary(pervote_inds)

#### FIGURE A7 ####
plot2party <- multiplot(pervote_dems, pervote_repubs,pervote_inds, intercept=F, title="", ylab=" ", xlab="Average Treatment Effect", newNames=c("condition2Assault"="Assault", "condition2Jokes"="Jokes"), innerCI=1, outerCI=2, lwdOuter=.5, lwdInner=.5, pointSize=4, dodgeHeight=.5, decreasing=TRUE) + theme_bw() +
  theme(legend.position="bottom") + scale_colour_manual(labels = c("Democrats", "Independents",  "Republicans"), values = c("blue2", "grey50", "firebrick3")) + scale_shape_manual(labels = c("Democrats", "Independents", "Republicans"), values=c(17, 19, 15)) + 
  theme(axis.text=element_text(size=18),axis.title=element_text(size=18), plot.subtitle = element_text(size = 14), plot.title = element_text(size = 18), legend.text = element_text(size=16), legend.title=element_blank()) +
  labs(title="Treatment Effects on Change in Electoral Support", subtitle="Baseline category = control condition") +
  coord_cartesian(xlim=c(0, -45))

plot2party

# tiff: coefplot_vote PARTY ###
tiff(file="coefplot_vote_PARTY.tiff", width=7, height=6, units="in", res=800)
plot2party
dev.off()

## AGE ####

# subset: age groups
table(dat$age)
subsets<-split(dat, dat$age, drop=TRUE)

# age ### percent change favorability ###

perfav_age1 <- lm(perchange_favorability ~ condition2, data=subsets[[1]])
summary(perfav_age1)

perfav_age2 <- lm(perchange_favorability ~ condition2, data=subsets[[2]])

perfav_age3 <- lm(perchange_favorability ~ condition2, data=subsets[[3]])

perfav_age4 <- lm(perchange_favorability ~ condition2, data=subsets[[4]])

perfav_age5 <- lm(perchange_favorability ~ condition2, data=subsets[[5]])

perfav_age6 <- lm(perchange_favorability ~ condition2, data=subsets[[6]])

#### FIGURE A8 ####
plot1age <- multiplot(perfav_age1, perfav_age2, perfav_age3, perfav_age4, perfav_age5, perfav_age6, intercept=F, title="", ylab=" ", xlab="Average Treatment Effect", newNames=c("condition2Assault"="Assault", "condition2Jokes"="Jokes"), innerCI=1, outerCI=2, lwdOuter=.5, lwdInner=.5, pointSize=4, dodgeHeight=.5, decreasing=TRUE) + theme_bw() +
  theme(legend.position="bottom") +
  scale_colour_manual(labels = c("18-24", "25-34",  "35-44", "45-54", "55-64", "65 or over"), values = c("indianred1", "lightsalmon", "steelblue3", "mediumaquamarine", "mediumorchid2", "darkgoldenrod3")) + scale_shape_manual(labels = c("18-24", "25-34",  "35-44", "45-54", "55-64", "65 or over"), values=c(20, 0, 1, 2, 10, 7, 9)) +
  theme(axis.text=element_text(size=18),axis.title=element_text(size=18), plot.subtitle = element_text(size = 14), plot.title = element_text(size = 18), legend.text = element_text(size=16), legend.title=element_blank()) +
  labs(title="Treatment Effects on Change in Favorability", subtitle="Baseline category = control condition") +
  coord_cartesian(xlim=c(0, -65))

plot1age

# tiff: coefplot_fav AGE ###
tiff(file="coefplot_favorability_AGE.tiff", width=7, height=6, units="in", res=800)
plot1age
dev.off()

# age ### percent change vote ###

pervote_age1 <- lm(perchange_vote ~ condition2, data=subsets[[1]])
summary(pervote_age1)

pervote_age2 <- lm(perchange_vote ~ condition2, data=subsets[[2]])

pervote_age3 <- lm(perchange_vote ~ condition2, data=subsets[[3]])

pervote_age4 <- lm(perchange_vote ~ condition2, data=subsets[[4]])

pervote_age5 <- lm(perchange_vote ~ condition2, data=subsets[[5]])

pervote_age6 <- lm(perchange_vote ~ condition2, data=subsets[[6]])

#### FIGURE A9 ####
plot2age <- multiplot(pervote_age1, pervote_age2, pervote_age3, pervote_age4, pervote_age5, pervote_age6, intercept=F, title="", ylab=" ", xlab="Average Treatment Effect", newNames=c("condition2Assault"="Assault", "condition2Jokes"="Jokes"), innerCI=1, outerCI=2, lwdOuter=.5, lwdInner=.5, pointSize=4, dodgeHeight=.5, decreasing=TRUE) + theme_bw() +
  theme(legend.position="bottom") +
  scale_colour_manual(labels = c("18-24", "25-34",  "35-44", "45-54", "55-64", "65 or over"), values = c("indianred1", "lightsalmon", "steelblue3", "mediumaquamarine", "mediumorchid2", "darkgoldenrod3")) + scale_shape_manual(labels = c("18-24", "25-34",  "35-44", "45-54", "55-64", "65 or over"), values=c(20, 0, 1, 2, 10, 7, 9)) +
  theme(axis.text=element_text(size=18),axis.title=element_text(size=18), plot.subtitle = element_text(size = 14), plot.title = element_text(size = 18), legend.text = element_text(size=16), legend.title=element_blank()) +
  labs(title="Treatment Effects on Change in Electoral Support", subtitle="Baseline category = control condition") +
  coord_cartesian(xlim=c(0, -60))

plot2age


# tiff: coefplot_vote AGE ###
tiff(file="coefplot_vote_AGE.tiff", width=7, height=6, units="in", res=800)
plot2age
dev.off()

## NEWS INTEREST #####

highnews <- subset(dat, dat$newsinterest=="Most of the time")
nothigh <- subset(dat, dat$newsinterest!="Most of the time")


# news interest ### percent change favorability ###

perfav_highnews <- lm(perchange_favorability ~ condition2, data=highnews)
summary(perfav_highnews)

perfav_nothigh <- lm(perchange_favorability ~ condition2, data=nothigh)
summary(perfav_nothigh)

#### FIGURE A10 ####
plot1news <- multiplot(perfav_highnews, perfav_nothigh, intercept=F, title="", ylab=" ", xlab="Average Treatment Effect", newNames=c("condition2Assault"="Assault", "condition2Jokes"="Jokes"), innerCI=1, outerCI=2, lwdOuter=.5, lwdInner=.5, pointSize=4, dodgeHeight=.5, decreasing=TRUE) + theme_bw() + scale_colour_manual(labels = c("High news interest", "All other respondents"), values = c("black", "black")) + scale_shape_manual(labels = c("High news interest", "All other respondents"), values=c(17, 19)) + 
  theme(legend.position="bottom") +
  theme(axis.text=element_text(size=18),axis.title=element_text(size=18), plot.subtitle = element_text(size = 14), plot.title = element_text(size = 18), legend.text = element_text(size=16), legend.title=element_blank()) +
  labs(title="Treatment Effects on Change in Favorability", subtitle="Baseline category = control condition") +
  coord_cartesian(xlim=c(0, -45))

plot1news

# tiff: coefplot_fav NEWS ###
tiff(file="coefplot_favorability_NEWS.tiff", width=7, height=6, units="in", res=800)
plot1news
dev.off()


# news interest ### percent change vote ###

pervote_highnews <- lm(perchange_vote ~ condition2, data=highnews)
summary(pervote_highnews)

pervote_nothigh <- lm(perchange_vote ~ condition2, data=nothigh)
summary(pervote_nothigh)

#### FIGURE A11 ####
plot2news <- multiplot(pervote_highnews, pervote_nothigh, intercept=F, title="", ylab=" ", xlab="Average Treatment Effect", newNames=c("condition2Assault"="Assault", "condition2Jokes"="Jokes"), innerCI=1, outerCI=2, lwdOuter=.5, lwdInner=.5, pointSize=4, dodgeHeight=.5, decreasing=TRUE) + theme_bw() + scale_colour_manual(labels = c("High news interest", "All other respondents"), values = c("black", "black")) + scale_shape_manual(labels = c("High news interest", "All other respondents"), values=c(17, 19)) + 
  theme(legend.position="bottom") +
  theme(axis.text=element_text(size=18),axis.title=element_text(size=18), plot.subtitle = element_text(size = 14), plot.title = element_text(size = 18), legend.text = element_text(size=16), legend.title=element_blank()) +
  labs(title="Treatment Effects on Change in Electoral Support", subtitle="Baseline category = control condition") +
  coord_cartesian(xlim=c(0, -45))

plot2news


# tiff: coefplot_vote NEWS ###
tiff(file="coefplot_vote_NEWS.tiff", width=7, height=6, units="in", res=800)
plot2news
dev.off()


