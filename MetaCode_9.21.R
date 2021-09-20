# 0. Load packages ----

library(ggplot2)
library(rjags)
library(dplyr)
library(tidyr)
library(cowplot)

setwd("/Users/evesophia/Dropbox/research/Meta")
d_full<-read.csv("Inv_GC_data_1August2021.csv", stringsAsFactors = FALSE) # note there are some issues with some symbols/characters


# 1. Recoding to simplify a few variables (invasion mechanism, response scale and class) ----

# ... make a new category of broad invasion mechanism type ----
d_full$mechanism_broad[d_full$mechanism=="competition"]<-"competition"
d_full$mechanism_broad[d_full$mechanism%in%c("herbivory", "parasitism", "predation")]<-"predation"
d_full$mechanism_broad[d_full$mechanism%in%c("chemical", "physical", "poisoning/toxicity", "structural")]<-"chemical/physical"

# ... response scale ----
# combine individual + population scales -> species
d_full$response_scale[d_full$response_scale%in%c("individual", "population")]<-"species"

# ... response class ----
d_full$response_class[d_full$response_class%in%c("nutrient cycling", "resources")]<-"nutrient"
d_full$response_class[d_full$response_class%in%c("immunity", "metabolism")]<-"physiology"
d_full$response_class[d_full$response_class=="growth"]<-"size"


# 2. Data exploration (can skip this) ----

# how many unique studies?
length(unique(d_full$studyID))  # 95 total 

# global environmental change (gc) factor ----
gc<-data.frame(n.cases = table(d_full$gc_factor), n.studies = tapply(d_full$studyID, d_full$gc_factor, function(x) length(unique(x))))
names(gc)<-c("gc_factor", "n.cases", "n.studies")

# how many studies have multiple gc factors?
table(tapply(d_full$gc_factor, d_full$studyID, function(x) length(unique(x))))
# 7 have 2 

# gc vs. ecosystem setting
## focusing in on temperature, nitrogen, and drought (most common)
gc_broad_setting<-group_by(d_full, setting, gc_factor) %>%
  summarise(n = length(unique(studyID)))%>% filter(gc_factor%in%c('temperature', 'drought', 'nitrogen'))
gc_broad_setting$gc_factor[gc_broad_setting$gc_factor=='temperature']<-'warming'
# plot
ggplot(data = gc_broad_setting, aes(x = gc_factor, y = n, fill = setting)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("skyblue", "blue","forestgreen"))+
  labs(y = "Number of studies", x = "GEC")+
  theme_bw()


# invasion ----
# invader taxon
tapply(d_full$studyID, d_full$species_taxon, function(x) length(unique(x))) # #studies
table(d_full$species_taxon) # #observations

# trophic level (producer vs. consumer) 
tapply(d_full$studyID, d_full$trophic, function(x) length(unique(x))) # #studies
table(d_full$trophic) # #observations
# trophic level vs. ecosystem setting
sett_troph<-group_by(d_full, trophic, setting) %>%
  summarise(n = length(unique(studyID)))
d_full$trsett<-paste(d_full$trophic, d_full$setting)  
tapply(d_full$studyID, d_full$trsett, function(x) length(unique(x)))/length(unique(d_full$studyID)) #  proportion of studies
table(d_full$trsett)/nrow(d_full)  # proportion of observations
# trophic level vs. ecosystem setting vs. invasion mechanism
d_full$trsettm<-paste(d_full$trophic, d_full$setting, d_full$mechanism_broad)
tapply(d_full$studyID, d_full$trsettm, function(x) length(unique(x)))/length(unique(d_full$studyID))  #  proportion of studies
table(d_full$trsettm)/nrow(d_full)  # proportion of observations
# trophic level vs. gc
gc_troph<-group_by(d_full, trophic, gc_factor) %>%
  summarise(n = length(unique(studyID)))


# invasion mechanism
tapply(d_full$studyID, d_full$mechanism, function(x) length(unique(x))) # #studies
table(d_full$mechanism) # #observations
mech<-data.frame(n.cases = table(d_full$mechanism_broad), n.studies = tapply(d_full$studyID, d_full$mechanism_broad, function(x) length(unique(x))))
names(mech)<-c("mechanism_broad", "n.cases", "n.studies")
# inv. mechanism versus taxon (# studies)
mech_broad_taxon<-group_by(d_full, species_taxon, mechanism_broad) %>%
  summarise(n = length(unique(studyID))) #plants mostly dominating competition, animals have more predation
ggplot(data = mech_broad_taxon, aes(x = mechanism_broad, y = n, fill =  species_taxon)) +
  geom_bar(stat = "identity", position = "stack") +
  ylab("Number of studies")
# inv. mechanism versus trophic level (# studies)
mech_broad_trophic<-group_by(d_full, trophic, mechanism_broad) %>%
  summarise(n = length(unique(studyID)))
ggplot(data = mech_broad_trophic, aes(x = mechanism_broad, y = n, fill = trophic)) +
  geom_bar(stat = "identity", position = "stack") +
  ylab("Number of studies")
# inv. mechanism versus ecosystem setting (studies)
mech_broad_setting<-group_by(d_full, setting, mechanism_broad) %>%
  summarise(n = length(unique(studyID)))
ggplot(data = mech_broad_setting, aes(x = mechanism_broad, y = n, fill = setting)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(y = "Number of studies", x = "Invasion mechanism")+
  theme_bw()
# inv. mechanism versus GC (studies)
mech_broad_gc_factor<-group_by(d_full, gc_factor, mechanism_broad) %>%
  summarise(n = length(unique(studyID)))%>% filter(gc_factor%in%c('temperature', 'drought', 'nitrogen'))
mech_broad_gc_factor$gc_factor[mech_broad_gc_factor$gc_factor=='temperature']<-'warming'
ggplot(data = mech_broad_gc_factor, aes(x = gc_factor, y = n, fill = mechanism_broad)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("darkturquoise", "orange", "orchid"), "Invasion\nmechanism")+
  labs(y = "Number of studies", x = "GEC")+
  theme_bw()


# response ----
# response scale
rs<-data.frame(n.cases = table(d_full$response_scale), n.studies = tapply(d_full$studyID, d_full$response_scale, function(x) length(unique(x))))
names(rs)<-c("response_scale", "n.cases", "n.studies")

# response class (type of response measure)
rc<-data.frame(n.cases = table(d_full$response_class), n.studies = tapply(d_full$studyID, d_full$response_class, function(x) length(unique(x))))
names(rc)<-c("response_class", "n.cases", "n.studies")
# response class versus trophic level(studies)
resp_broad_trophic<-group_by(d_full, trophic, response_class) %>%
  summarise(n = length(unique(studyID)))
ggplot(data = resp_broad_trophic, aes(x = response_class, y = n, fill = trophic)) +
  geom_bar(stat = "identity", position = "stack") +
  ylab("Number of studies") ### reproduction is all measured on plants
# response class versus gc 
resp_broad_gc<-group_by(d_full, gc_factor, response_class) %>%
  summarise(n = n())
ggplot(data = resp_broad_gc, aes(x = response_class, y = n, fill = gc_factor)) +
  geom_bar(stat = "identity", position = "stack")+
  labs(x = "Number of cases", y = "Response class")+
  theme_bw()
# response class versus response scale (studies)
resp_broad_scale<-group_by(d_full, response_scale, response_class) %>%
  summarise(n = length(unique(studyID)))
ggplot(data = resp_broad_scale, aes(x = response_class, y = n, fill = response_scale)) +
  geom_bar(stat = "identity", position = "stack") +
  ylab("Number of studies")
# to organize these by scale:
## nutrient,  # all 3
## diversity, # community
## abundance, allocation, biomass, reproduction, size, survival # community + species
## behavior, physiology  # just species


# ecosystem ----
# continent
cont<-data.frame(n.cases = table(d_full$continent), n.studies = tapply(d_full$studyID, d_full$continent, function(x) length(unique(x))))
names(cont)<-c("continent", "n.cases", "n.studies")

# broad setting (terrestrial, freshwater, marine)
sett<-data.frame(n.cases = table(d_full$setting), n.studies = tapply(d_full$studyID, d_full$setting, function(x) length(unique(x))))
names(sett)<-c("setting", "n.cases", "n.studies")


# experiment type ----
exp<-data.frame(n.cases = table(d_full$study_type), n.studies = tapply(d_full$studyID, d_full$study_type, function(x) length(unique(x))))
names(exp)<-c("study_type", "n.cases", "n.studies")
# experiment type versus gc
exp_exp_gc_factor<-group_by(d_full, gc_factor, study_type) %>%
  summarise(n = length(unique(studyID)))%>% filter(gc_factor%in%c('temperature', 'drought', 'nitrogen'))
exp_exp_gc_factor$gc_factor[exp_exp_gc_factor$gc_factor=='temperature']<-'warming'
ggplot(data = exp_exp_gc_factor, aes(x = gc_factor, y = n, fill = study_type)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("darkturquoise", "orange", "orchid"), "Invasion\nexpanism")+
  labs(y = "Number of studies", x = "GEC")+
  theme_bw()


# 3. Calculate effect sizes ----

# ... Change signs (directional effects) ----
# need to make direction (+/-) of effect size to indicate whether treatment effects are normatively negative (detrimental) or positive (beneficial)
# (multiply by -1 the responses where > does not equal better)
length(d_full$studyID[d_full$benefit=="no"]);length(unique(d_full$studyID[d_full$benefit=="no"])) # there are 55 of these from 28 studies
# how many obs are we unsure about the direction of benefit/detriment?
length(d_full$studyID[d_full$benefit_certainty=="no"]); length(unique(d_full$studyID[d_full$benefit_certainty=="no"])) # 97 from 36 studies
## to look more at these:
#ben_check<-d_full[d_full$benefit_certainty=="no", c("studyID", "response_metric", "response_class", "response_class", "response_scale")]
#table(ben_check$response_class)
## a lot of nutrient and allocation, biomass, but also includes (body) size, physiology (immune function), behavior, abundance, and one measure of diversity (community composition really)

# retain original values
d_full$mean_control_orig<-d_full$mean_control
d_full$mean_inv_orig<-d_full$mean_inv
d_full$mean_gc_orig<-d_full$mean_gc
d_full$mean_inv_gc_orig<-d_full$mean_inv_gc

# replace means with -1*mean if the response is not considered beneficial
d_full$mean_control[d_full$benefit == "no"] = (-1)*d_full$mean_control[d_full$benefit == "no"] 
d_full$mean_inv[d_full$benefit == "no"] = (-1)*d_full$mean_inv[d_full$benefit == "no"] 
d_full$mean_gc[d_full$benefit == "no"] = (-1)*d_full$mean_gc[d_full$benefit == "no"] 
d_full$mean_inv_gc[d_full$benefit == "no"] = (-1)*d_full$mean_inv_gc[d_full$benefit == "no"] 


# ... Standardize variance statistics to standard deviation ----
table(d_full$var_statistic)

# create new column and carry over SDs
d_full$SD_control[d_full$var_statistic == "SD"] <- d_full$var_control[d_full$var_statistic == "SD"]
d_full$SD_inv[d_full$var_statistic == "SD"] <- d_full$var_inv[d_full$var_statistic == "SD"]
d_full$SD_gc[d_full$var_statistic == "SD"] <- d_full$var_gc[d_full$var_statistic == "SD"]
d_full$SD_inv_gc[d_full$var_statistic == "SD"] <- d_full$var_inv_gc[d_full$var_statistic == "SD"]

# for standard error, multiply by the sqrt of n
d_full$SD_control[d_full$var_statistic == "SE"] <- d_full$var_control[d_full$var_statistic == "SE"]*sqrt(d_full$n_control[d_full$var_statistic == "SE"])
d_full$SD_inv[d_full$var_statistic == "SE"] <- d_full$var_inv[d_full$var_statistic == "SE"]*sqrt(d_full$n_inv[d_full$var_statistic == "SE"])
d_full$SD_gc[d_full$var_statistic == "SE"]  <- d_full$var_gc[d_full$var_statistic == "SE"]*sqrt(d_full$n_gc[d_full$var_statistic == "SE"])
d_full$SD_inv_gc[d_full$var_statistic == "SE"]  <- d_full$var_inv_gc[d_full$var_statistic == "SE"]*sqrt(d_full$n_inv_gc[d_full$var_statistic == "SE"])

# for confidence intervals,
# sqrt(n)*CI/3.92 (since CI here is upper limit-lower limit) https://handbook-5-1.cochrane.org/chapter_7/7_7_3_2_obtaining_standard_deviations_from_standard_errors_and.htm
d_full$SD_control[d_full$var_statistic == "CI"] <- sqrt(d_full$n_control[d_full$var_statistic == "CI"])*d_full$var_control[d_full$var_statistic == "CI"]/3.92
d_full$SD_inv[d_full$var_statistic == "CI"] <- sqrt(d_full$n_inv[d_full$var_statistic == "CI"])*d_full$var_inv[d_full$var_statistic == "CI"]/3.92
d_full$SD_gc[d_full$var_statistic == "CI"] <- sqrt(d_full$n_gc[d_full$var_statistic == "CI"])*d_full$var_gc[d_full$var_statistic == "CI"]/3.92
d_full$SD_inv_gc[d_full$var_statistic == "CI"] <- sqrt(d_full$n_inv_gc[d_full$var_statistic == "CI"])*d_full$var_inv_gc[d_full$var_statistic == "CI"]/3.92

# replace zero SDs with a very small number
d_full$SD_control[d_full$SD_control==0]<-0.000001
d_full$SD_inv[d_full$SD_inv==0]<-0.000001
d_full$SD_gc[d_full$SD_gc==0]<-0.000001
d_full$SD_inv_gc[d_full$SD_inv_gc==0]<-0.000001

# ... Calc. effect sizes for treatments ----

### (from Koricheva et al. 2013 Handbook of meta-analysis in ecology and evolution)
# J = 1 - (3/(4*(n.base + n.tr - 2) - 1))   
# S = (sqrt ((n.tr - 1)*sd.tr^2 + (n.base - 1)*sd.base^2)/(n.base + n.tr - 2))
# d = (mu.tr - mu.base)/S * J
# var = (1 - (3/(4*(n.tr + n.base - 2) - 1)))^2 * ((n.tr + n.base - 2)/((n.tr*n.base/(n.tr + n.base)) * (n.tr + n.base - 4)))
### (this last from Hamman et al. 2018)

# J's for treatments    ## correction for small sample sizes
d_full$J_inv <- 1 - (3/(4*(d_full$n_control + d_full$n_inv - 2) - 1))
d_full$J_gc <- 1 - (3/(4*(d_full$n_control + d_full$n_gc - 2) - 1))
d_full$J_inv_gc <- 1 - (3/(4*(d_full$n_control + d_full$n_inv_gc - 2) - 1))

# S's for treatments   ## pooled SD
d_full$S_inv <- (sqrt ((d_full$n_inv - 1)*d_full$SD_inv^2 + (d_full$n_control - 1)*d_full$SD_control^2)/(d_full$n_control + d_full$n_inv - 2))
d_full$S_gc <- (sqrt ((d_full$n_gc - 1)*d_full$SD_gc^2 + (d_full$n_control - 1)*d_full$SD_control^2)/(d_full$n_control + d_full$n_gc - 2))
d_full$S_inv_gc <- (sqrt ((d_full$n_inv_gc - 1)*d_full$SD_inv_gc^2 + (d_full$n_control - 1)*d_full$SD_control^2)/(d_full$n_control + d_full$n_inv_gc - 2))

# ES's (d's) for treatments
d_full$d_inv <- (d_full$mean_inv - d_full$mean_control) / d_full$S_inv *d_full$J_inv
d_full$d_gc <- (d_full$mean_gc - d_full$mean_control) / d_full$S_gc *d_full$J_gc
d_full$d_inv_gc <- (d_full$mean_inv_gc - d_full$mean_control) / d_full$S_inv_gc *d_full$J_inv_gc

# variance of ES's (sigma^2)
d_full$d_var_inv <- (1 - (3/(4*(d_full$n_inv + d_full$n_control - 2) - 1)))^2 * ((d_full$n_inv + d_full$n_control - 2)/((d_full$n_inv*d_full$n_control/(d_full$n_inv + d_full$n_control)) * (d_full$n_inv + d_full$n_control - 4)))
d_full$d_var_gc <- (1 - (3/(4*(d_full$n_gc + d_full$n_control - 2) - 1)))^2 * ((d_full$n_gc + d_full$n_control - 2)/((d_full$n_gc*d_full$n_control/(d_full$n_gc + d_full$n_control)) * (d_full$n_gc + d_full$n_control - 4)))
d_full$d_var_inv_gc <- (1 - (3/(4*(d_full$n_inv_gc + d_full$n_control - 2) - 1)))^2 * ((d_full$n_inv_gc + d_full$n_control - 2)/((d_full$n_inv_gc*d_full$n_control/(d_full$n_inv_gc + d_full$n_control)) * (d_full$n_inv_gc + d_full$n_control - 4)))


# 5. Calc. Predicted Additive effect ----

### (from Jackson et al. 2016)
# predicted mean of inv + gc (Additive)
# mu.pred = (mu.tr1 - mu.control) + (mu.tr2 - mu.control) + mu.control 
d_full$mean_pred_add <- (d_full$mean_inv - d_full$mean_control) + (d_full$mean_gc - d_full$mean_control) + d_full$mean_control

# Replace impossible values with zero:
# calculate "original" predicted additive effects (without any sign changes)
d_full$mean_pred_add_orig <- (d_full$mean_inv_orig - d_full$mean_control_orig) + (d_full$mean_gc_orig - d_full$mean_control_orig) + d_full$mean_control_orig
# which ones are predicted to be negative?
test<-d_full[d_full$mean_pred_add_orig<0,c("studyID","response_class","response_metric","response_units","mean_control_orig","mean_inv_orig","mean_gc_orig","mean_pred_add_orig")]
dim(test) # 49 obs.
unique(test$response_metric) # many of these are things that can't be negative (e.g. biomass, species richness)
## all of those that can be negative also have negative values for at least one of the means 
# what about percents?
#test<-d_full[d_full$mean_pred_add_orig>100,c("studyID","response_class","response_metric","response_units","mean_control_orig","mean_inv_orig","mean_gc_orig","mean_pred_add_orig")]
## looks like only percents over 100 were already over 100 (e.g. cover)
# Replace impossible negatives with zeros
## define by those where the "orginal" (without sign change) predicted additive is <0 but none of the treatment means are
d_full$mean_pred_add[d_full$mean_pred_add_orig<0&d_full$mean_control_orig>0&d_full$mean_gc_orig>0&
                       d_full$mean_inv_orig>0&d_full$mean_inv_gc_orig>0]<-0

# pooled SD and n
d_full$SD_pred_add <- (sqrt ((d_full$n_inv - 1)*d_full$SD_inv^2 + (d_full$n_gc - 1)*d_full$SD_gc^2)/(d_full$n_gc + d_full$n_inv - 2))
d_full$n_pred_add <- d_full$n_inv + d_full$n_gc

# calculate d comparing inv_gc (observed interaction/combined treatment effect) to Predicted Additive
# J = 1 - (3/(4*(n.base + n.tr - 2) - 1))   
# S = (sqrt ((n.tr - 1)*sd.tr^2 + (n.base - 1)*sd.base^2)/(n.base + n.tr - 2))
# d = (mu.tr - mu.base)/S * J
d_full$J_pred_add <- 1 - (3/(4*(d_full$n_inv_gc + d_full$n_pred_add - 2) - 1))
d_full$S_pred_add <- (sqrt ((d_full$n_inv_gc - 1)*d_full$SD_inv_gc^2 + (d_full$n_pred_add - 1)*d_full$SD_pred_add^2)/(d_full$n_pred_add + d_full$n_inv_gc - 2))
d_full$d_pred_add <- (d_full$mean_inv_gc - d_full$mean_pred_add) / d_full$S_pred_add *d_full$J_pred_add
d_full$d_var_pred_add <-(1 - (3/(4*(d_full$n_pred_add + d_full$n_inv_gc - 2) - 1)))^2 * ((d_full$n_pred_add + d_full$n_inv_gc - 2)/((d_full$n_pred_add*d_full$n_inv_gc/(d_full$n_pred_add + d_full$n_inv_gc)) * (d_full$n_pred_add + d_full$n_inv_gc - 4)))



# 6. Classify interactions ----

# based on framework from Piggott et al 2015

# define confidence intervals for d_pred_add
d_full$d_CI_pred_add <- 1.96 * (sqrt(d_full$d_var_pred_add) / sqrt(d_full$n_pred_add))
d_full$d_lower_pred_add <- d_full$d_pred_add - d_full$d_CI_pred_add
d_full$d_higher_pred_add <- d_full$d_pred_add + d_full$d_CI_pred_add

# creating two columns here:
## one, int_cat, has Additive, Synergistic, Antagonistic
## the second, int_cat_dir, has + or - for Synergistic and Antagonistic 
### based on whether higher or lower than expected (different from Piggott et al)

# 1. is d different from 0 (based on CI)? if not -> Additive
d_full$int_cat[d_full$d_lower_pred_add<=0 & d_full$d_higher_pred_add>=0] <- "Additive"
d_full$int_cat_dir[d_full$d_lower_pred_add<=0 & d_full$d_higher_pred_add>=0] <- "Additive"

# 2. does d fall outside bounds of control and treatments? if yes -> Synergistic
# for this, define confidence intervals for d_inv_gc 
## to see whether they overlap the d's of any of the treatments or control (Piggott et al 2015, Crain et al 2008)
d_full$d_CI_inv_gc <- 1.96 * (sqrt(d_full$d_var_inv_gc) / sqrt(d_full$n_inv_gc))
d_full$d_lower_inv_gc <- d_full$d_inv_gc - d_full$d_CI_inv_gc
d_full$d_higher_inv_gc <- d_full$d_inv_gc + d_full$d_CI_inv_gc
# then define synergies
d_full$int_cat[d_full$d_lower_inv_gc>0 & d_full$d_lower_pred_add>0 & d_full$d_lower_inv_gc>d_full$d_inv & d_full$d_lower_inv_gc>d_full$d_gc]<- "Synergistic"
d_full$int_cat[d_full$d_higher_inv_gc<0 & d_full$d_higher_pred_add<0 & d_full$d_higher_inv_gc<d_full$d_inv & d_full$d_higher_inv_gc<d_full$d_gc]<- "Synergistic"
d_full$int_cat_dir[d_full$d_lower_inv_gc>0 & d_full$d_lower_pred_add>0 & d_full$d_lower_inv_gc>d_full$d_inv & d_full$d_lower_inv_gc>d_full$d_gc]<- "Synergistic +"
d_full$int_cat_dir[d_full$d_higher_inv_gc<0 & d_full$d_higher_pred_add<0 & d_full$d_higher_inv_gc<d_full$d_inv & d_full$d_higher_inv_gc<d_full$d_gc]<- "Synergistic -"

# 3. otherwise, Antagonistic
d_full$int_cat_dir[d_full$d_lower_pred_add>0 & is.na(d_full$int_cat)] <- "Antagonistic +"
d_full$int_cat_dir[d_full$d_higher_pred_add<0 & is.na(d_full$int_cat)] <- "Antagonistic -"
d_full$int_cat[is.na(d_full$int_cat)]<-"Antagonistic"


table(d_full$int_cat)
# Additive Antagonistic  Synergistic 
#        20          317          130 
table(d_full$int_cat)/nrow(d_full) # proportion

table(d_full$int_cat_dir)
# Additive Antagonistic - Antagonistic +  Synergistic -  Synergistic + 
#     20            145            171             77             53 
table(d_full$int_cat_dir)/nrow(d_full) # proportion



# 7. Clip datasets for analysis ----

# ... Remove outliers for d analysis (d_cl) ----
hist(d_full$d_inv); hist(d_full$d_inv[abs(d_full$d_inv)<1000])
hist(d_full$d_gc); hist(d_full$d_gc[abs(d_full$d_gc)<1000])
hist(d_full$d_inv_gc); hist(d_full$d_inv_gc[abs(d_full$d_inv_gc)<1000])

# z-scores for d's with abs. val. <200
d_200<-d_full[d_full$d_inv>(-200) & d_full$d_inv<200 & d_full$d_gc>(-200) & d_full$d_gc<200 & d_full$d_inv_gc>(-200) & d_full$d_inv_gc<200 &
                !is.na(d_full$d_inv) & !is.na(d_full$d_gc) & !is.na(d_full$d_inv_gc),c('d_inv', 'd_gc','d_inv_gc')]
nrow(d_full)-nrow(d_200) #5 cases removed
test_bounds<-data.frame(mean = apply(d_200, 2, mean), sd = apply(d_200, 2, sd))
test_bounds$upper<-test_bounds$mean+3*test_bounds$sd
test_bounds$lower<-test_bounds$mean-3*test_bounds$sd
test_bounds
# 1 point is over 200, others are >1000

# visual assessment of outliers (d's with abs. val. <200)
test_plot<-data.frame(d = c(d_200$d_inv, d_200$d_gc, d_200$d_inv_gc),
                      treatment = factor(rep(c("INV", "GEC", "INV&GEC"), each = nrow(d_200)), levels = c("INV", "GEC", "INV&GEC")))
test_plot$over<-factor(ifelse(test_plot$d>(-30)&test_plot$d<30, "in", "out"))
ggplot(data = test_plot, aes(x=treatment, y=d)) +
  geom_point(position = position_jitter(width = 0.03, height = 0),
             size = 1, alpha = 0.4, aes(color = over)) +
  scale_color_manual("Inclusion", values = c("black", "red"))+
  theme_bw()+
  labs(x = "Treatment", y = "Hedges' d")

# are these normal-ish? (more or less)
hist(d_full$d_inv[abs(d_full$d_inv)<30])
hist(d_full$d_gc[abs(d_full$d_gc)<30])
hist(d_full$d_inv_gc[abs(d_full$d_inv_gc)<30])

# clip dataset for analysis to d's between -30 and 30
d_cl<-d_full[d_full$d_inv>(-30) & d_full$d_inv<30 & d_full$d_gc>(-30) & d_full$d_gc<30 & d_full$d_inv_gc>(-30) & d_full$d_inv_gc<30 &
               !is.na(d_full$d_inv) & !is.na(d_full$d_gc) & !is.na(d_full$d_inv_gc),] 
dim(d_cl) # 451 obs
nrow(d_full)-nrow(d_cl) # #clipped = 16
length(unique(d_cl$studyID)) # all 95 studies

# gc's in the clipped dataset
data.frame(n.cases = table(d_full$gc_factor), n.studies = tapply(d_full$studyID, d_full$gc_factor, function(x) length(unique(x))))

table(d_cl$int_cat)
# Additive Antagonistic  Synergistic 
#      18          305          128 

table(d_cl$int_cat_dir)
table(d_cl$int_cat_dir)/nrow(d_cl)
# Additive Antagonistic - Antagonistic +  Synergistic -  Synergistic + 
# 0.03991131     0.31042129     0.36585366     0.16851441     0.11529933 

# how many are terrestrial plant competitors?
length(d_cl$studyID[d_cl$species_taxon=="plant"&d_cl$mechanism_broad=="competition"&d_cl$setting=="terrestrial"])/nrow(d_cl) # 47% of cases
length(unique(d_cl$studyID[d_cl$species_taxon=="plant"&d_cl$mechanism_broad=="competition"&d_cl$setting=="terrestrial"]))/length(unique(d_cl$studyID)) # 42% of studies

# represented countries
tapply(d_cl$studyID, d_cl$country, function(x) length(unique(x)))
# and continents
tapply(d_cl$studyID, d_cl$continent, function(x) length(unique(x)))

# response classes
tapply(d_cl$studyID, d_cl$response_class, function(x) length(unique(x)))

# ... to test sensitivity to defining direction of benefit for response classes  ----
# remove cases where benefit certainty = no
d_ben<-d_cl[d_cl$benefit_certainty=="yes"&!d_cl$response_class%in%c('nutrient','allocation','behavior'),] 
nrow(d_cl)-nrow(d_ben) #144 cases removed, including 95 obs with unknown ben and 49 additional with nutrient, behavior, or allocation response classes
dim(d_ben) # 307
length(unique(d_ben$studyID)) # only 78 studies
table(d_ben$int_cat_dir)/nrow(d_ben) # more syn-
apply(d_ben[,c("d_inv", "d_gc", "d_inv_gc")], 2, mean)
table(d_ben$response_class)

# ... and to look at the subset of just plant invaders ----
d_pl<-d_cl[d_cl$species_taxon=="plant",]
dim(d_pl) # 331
nrow(d_cl)-nrow(d_pl) # 120 are not plants
length(unique(d_pl$studyID)) # only 66 studies

# setting intercept group for models

length(d_cl$studyID[d_cl$gc_factor=="temperature"&d_cl$mechanism_broad=="competition"&d_cl$study_type=="lab/greenhouse"&d_cl$setting=="terrestrial"&d_cl$response_class=="biomass"]) # 5 obs
length(unique(d_cl$studyID[d_cl$gc_factor=="temperature"&d_cl$mechanism_broad=="competition"&d_cl$study_type=="lab/greenhouse"&d_cl$setting=="terrestrial"&d_cl$response_class=="biomass"])) # 2 studies
length(d_cl$studyID[d_cl$gc_factor=="nitrogen"&d_cl$mechanism_broad=="competition"&d_cl$study_type=="lab/greenhouse"&d_cl$setting=="terrestrial"&d_cl$response_class=="biomass"]) # 29 obs
length(unique(d_cl$studyID[d_cl$gc_factor=="nitrogen"&d_cl$mechanism_broad=="competition"&d_cl$study_type=="lab/greenhouse"&d_cl$setting=="terrestrial"&d_cl$response_class=="biomass"])) # 11 studies
# with ben
length(d_ben$studyID[d_ben$gc_factor=="temperature"&d_ben$mechanism_broad=="competition"&d_ben$study_type=="lab/greenhouse"&d_ben$setting=="terrestrial"&d_ben$response_class=="biomass"]) # 5 obs
length(unique(d_ben$studyID[d_ben$gc_factor=="temperature"&d_ben$mechanism_broad=="competition"&d_ben$study_type=="lab/greenhouse"&d_ben$setting=="terrestrial"&d_ben$response_class=="biomass"])) # 2 studies
length(d_ben$studyID[d_ben$gc_factor=="nitrogen"&d_ben$mechanism_broad=="competition"&d_ben$study_type=="lab/greenhouse"&d_ben$setting=="terrestrial"&d_ben$response_class=="biomass"]) # 29 obs
length(unique(d_ben$studyID[d_ben$gc_factor=="nitrogen"&d_ben$mechanism_broad=="competition"&d_ben$study_type=="lab/greenhouse"&d_ben$setting=="terrestrial"&d_ben$response_class=="biomass"])) # 11 studies
# with plants
length(d_pl$studyID[d_pl$gc_factor=="temperature"&d_pl$mechanism_broad=="competition"&d_pl$study_type=="lab/greenhouse"&d_pl$setting=="terrestrial"&d_pl$response_class=="biomass"]) # 5 obs
length(unique(d_pl$studyID[d_pl$gc_factor=="temperature"&d_pl$mechanism_broad=="competition"&d_pl$study_type=="lab/greenhouse"&d_pl$setting=="terrestrial"&d_pl$response_class=="biomass"])) # 2 studies
length(d_pl$studyID[d_pl$gc_factor=="nitrogen"&d_pl$mechanism_broad=="competition"&d_pl$study_type=="lab/greenhouse"&d_pl$setting=="terrestrial"&d_pl$response_class=="biomass"]) # 29 obs
length(unique(d_pl$studyID[d_pl$gc_factor=="nitrogen"&d_pl$mechanism_broad=="competition"&d_pl$study_type=="lab/greenhouse"&d_pl$setting=="terrestrial"&d_pl$response_class=="biomass"])) # 11 studies

# ... Clip dataset to one observation per study to address psuedoreplication on Fisher's test (d_cl_ints)  ----

# sort to choose ecosystem-level (large scale) first, and less numerous response classes first (to be able to compare across all response classes)
d_cl_ints<-d_cl
d_cl_ints$response_scale<-factor(d_cl_ints$response_scale, levels = c("ecosystem", "community", "species"))

d_cl_ints$response_class<-factor(d_cl_ints$response_class, levels = c("behavior", "diversity","reproduction","allocation", "physiology","survival","abundance","nutrient","size","biomass" ))
d_cl_ints<-d_cl_ints[order(d_cl_ints$response_class),] # this way, end up with at least 3 of each response class
d_cl_ints<-d_cl_ints[order(d_cl_ints$response_scale),]

# choosing randomly:
d_cl_ints<-d_cl_ints[!duplicated(d_cl_ints$studyID),] 
dim(d_cl_ints)# so, 95 data points

table(d_cl_ints$response_scale)
table(d_cl_ints$response_class)


# what is the distribution for this dataset?
table(d_cl_ints$int_cat_dir)/nrow(d_cl_ints)
# more syn+


# 8a. Models with single predictors (comparing d's of treatments) ----

# add observation ID for a random effect
d_cl$obs_ID<-1:nrow(d_cl)

# ... Means ----
# intercept = gc treatment

# data for model input
data_int <- list(
  d = c(d_cl$d_inv, d_cl$d_gc, d_cl$d_inv_gc),
  d.var = c(d_cl$d_var_inv, d_cl$d_var_gc, d_cl$d_var_inv_gc), # variance around d's
  inv = rep(c(1,0,0), each = nrow(d_cl)), # invasion treatment dummy variable
  invgc = rep(c(0,0,1), each = nrow(d_cl)), # inv*gc treatment dummy variable
  n = 3*nrow(d_cl),
  study.id = rep(as.numeric(as.factor(d_cl$studyID)),3),
  n.study = length(unique(d_cl$studyID)),
  obs.id = rep(d_cl$obs_ID, 3),
  n.obs = nrow(d_cl)
)

sink("intercept_hier_model.R") # This is the file name for the jags code
cat(" 
model{
# priors
sd ~ dunif(0, 100) 
tau <- 1/sd^2
b0 ~ dnorm(0, 1/10000) # use taus, not sigmas
I ~ dnorm(0, 1/10000)
IG ~ dnorm(0, 1/10000)
sd.study ~ dunif(0, 100)
tau.study <-1/sd.study^2
sd.obs ~dunif(0,100)
tau.obs<-1/sd.obs^2

# random effect variance
for (k in 1:n.study){
study.eff[k] ~ dnorm(0, tau.study)
}

for(j in 1:n.obs){
obs[j] ~ dnorm(0, tau.obs)
}

# likelihood
for (i in 1:n){
d[i] ~ dnorm(y[i], d.tau[i]) # observed d and variance
y[i] ~ dnorm(mu[i], tau)
mu[i] <- b0 + I*inv[i] + IG*invgc[i] + study.eff[study.id[i]] + obs[obs.id[i]]
d.tau[i] <- 1/(d.var[i]*d.var[i])

# simulated data (for checks)
y.sim[i] ~ dnorm(mu[i], tau)
# residuals
residual[i]<-(y[i]-mu[i])
#sq.resid[i]<-(residual[i])^2
# sums of squares differences
sqdif.data[i]<-(y[i]-mu[i])^2 
sqdif.sim[i]<-(y.sim[i]-mu[i])^2

}

# derived values
sumsq.data<-sum(sqdif.data[ ])
sumsq.sim<-sum(sqdif.sim[ ])
p.sumsq<-step(sumsq.sim - sumsq.data) # discrepancy
## mean
mean.data <- mean(y[ ]) # these are empty cause it's the whole vector
mean.sim <- mean(y.sim[ ])
p.mean <- step(mean.sim - mean.data)

}

",fill = TRUE)
sink()


# inits and run times
inits = list(list(b0 = 0, sd = 4, sd.study = 3, I = -1, IG = -1),
             list(b0 = 3, sd = 5, sd.study = 4, I = 2, IG = -3),
             list(b0 = -3, sd = 3, sd.study = 2, I = -3, IG = 1))

n.adapt = 30000 # for choosing the sampler and mixing 
n.update = 1000 # burn-in
n.iter = 50000 # keep  

set.seed(1)
jm_int = jags.model("intercept_hier_model.R", data = data_int, inits = inits,
                    n.chains = length(inits), n.adapt = n.adapt)
update(jm_int , n.iter = n.update)

cj_int = coda.samples(jm_int, variable.names=c("b0", "sd", "I",  "IG", "sd.study", "sd.obs"),
                      n.iter = n.iter, thin = 1)
#plot(cj_int) 
gelman.diag(cj_int)

# checking the fit
#zj_int = jags.samples(jm_int, variable.names=c("p.sumsq", "p.mean", "residual", "mu"), # "sumsq.data", "sumsq.sim", # "psuedoR2.num"
#                      n.iter = n.iter, thin = 1)
#zj_int$p.sumsq # 0.506 
#zj_int$p.mean # 0.502 
##sumsq.data<-zj_int$sumsq.data[,,1] 
##sumsq.sim<-zj_int$sumsq.sim[,,1]
##plot(sumsq.sim~sumsq.data) 

# Bayesian R2
# https://avehtari.github.io/ROS-Examples/Rsquared/rsquared.html
# http://www.stat.columbia.edu/~gelman/research/unpublished/bayes_R2_v3.pdf
#var(zj_int$mu)/(var(zj_int$mu)+var(zj_int$residual)) # median. could also look at a whole posterior of R2
# 0.51
#rm(zj_int)

# model output
out_int<-data.frame(rbind(cj_int[[1]], cj_int[[2]], cj_int[[3]])) 
names(out_int)

# plot means
out_mean<-data.frame(Gmean=out_int$b0, Imean=out_int$b0+out_int$I, IGmean=out_int$b0+out_int$IG)
out_int_plot_mean<-data.frame(mean = apply(out_mean, 2, mean), 
                              lower = apply(out_mean, 2, function(x) quantile(x, probs = c(.025))),
                              upper = apply(out_mean, 2, function(y) quantile(y, probs = c(.975))),
                              treatment = c("GC","Inv","Inv+GC")) %>%
  gather(end, value, -4)
out_int_plot_mean$treatment<- factor(out_int_plot_mean$treatment, levels = c("Inv+GC","Inv","GC"))
ggplot(data = out_int_plot_mean[!out_int_plot_mean$end=='mean',], aes(x=value, y=treatment)) +
  geom_line(stat = "identity", position = "identity", size = 4, colour = "grey40") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(data =out_int_plot_mean[out_int_plot_mean$end=='mean',], aes(x=value, y=treatment), 
             size = 3, colour = "white") +
  theme_bw() +
  labs(x = "Mean effect size (Hedges' d)  +/- 95% credible interval") +
  theme(axis.text.x = element_text(size = 13, colour = "black"), axis.title.x = element_text(size=15, vjust = 0.5), 
        axis.text.y = element_text(size = 13, colour = "black"), axis.title.y=element_blank(),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))  

# plot difs btw treatments
out_int_difs<-data.frame(inv.gc = out_int$I, invgc.gc = out_int$IG, 
                         inv.invgc = out_int$I-out_int$IG) 
out_int_plot_difs<-data.frame(mean = apply(out_int_difs, 2, mean), 
                              lower = apply(out_int_difs, 2, function(x) quantile(x, probs = c(.025))),
                              upper = apply(out_int_difs, 2, function(y) quantile(y, probs = c(.975))),
                              treatment = c("INV vs GEC","INV&GEC vs GEC", "INV vs INV&GEC")) %>%
  gather(end, value, -4)
ggplot(data = out_int_plot_difs[!out_int_plot_difs$end=='mean',], aes(x=value, y=treatment)) +
  geom_line(stat = "identity", position = "identity", size = 4, colour = "grey40") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(data =out_int_plot_difs[out_int_plot_difs$end=='mean',], aes(x=value, y=treatment), 
             size = 3, colour = "white") +
  theme_bw() +
  labs(x = "Posterior distributions(mean +/- 95% credible interval)") +
  theme(axis.text.x = element_text(size = 13, colour = "black"), axis.title.x = element_text(size=15, vjust = 0.5), 
        axis.text.y = element_text(size = 13, colour = "black"), axis.title.y=element_blank(),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))  


# ... gc factor ----
# intercept = temperature, gc treatment

data_gc<- list(
  d = c(d_cl$d_inv, d_cl$d_gc, d_cl$d_inv_gc),
  d.var = c(d_cl$d_var_inv, d_cl$d_var_gc, d_cl$d_var_inv_gc),
  inv = rep(c(1,0,0), each = nrow(d_cl)),
  invgc = rep(c(0,0,1), each = nrow(d_cl)),
  gc.drought = rep(ifelse(d_cl$gc_factor=="drought", 1, 0),3),
  gc.nitrogen = rep(ifelse(d_cl$gc_factor=="nitrogen", 1, 0),3),
  gc.CO2 = rep(ifelse(d_cl$gc_factor=="CO2", 1, 0),3),
  gc.O2 = rep(ifelse(d_cl$gc_factor=="O2", 1, 0),3),
  gc.pH = rep(ifelse(d_cl$gc_factor=="pH", 1, 0),3),
  n = 3*nrow(d_cl),
  study.id = rep(as.numeric(as.factor(d_cl$studyID)),3),
  n.study = length(unique(d_cl$studyID)),
  obs.id = rep(d_cl$obs_ID, 3),
  n.obs = nrow(d_cl)
)

sink("gc_hier_model.R") # This is the file name for the jags code
cat(" 
model{
# priors
sd ~ dunif(0, 100) 
tau <- 1/sd^2
b0 ~ dnorm(0, 1/10000) # use taus, not sigmas
I ~ dnorm(0, 1/10000)
IG ~ dnorm(0, 1/10000)
DROU ~ dnorm(0, 1/10000)
NITR ~ dnorm(0, 1/10000)
CO2 ~ dnorm(0, 1/10000)
O2 ~ dnorm(0, 1/10000)
PH ~ dnorm(0, 1/10000)
I.DROU ~ dnorm(0, 1/10000)
I.NITR ~ dnorm(0, 1/10000)
I.CO2 ~ dnorm(0, 1/10000)
I.O2 ~ dnorm(0, 1/10000)
I.PH ~ dnorm(0, 1/10000)
IG.DROU ~ dnorm(0, 1/10000)
IG.NITR ~ dnorm(0, 1/10000)
IG.CO2 ~ dnorm(0, 1/10000)
IG.O2 ~ dnorm(0, 1/10000)
IG.PH ~ dnorm(0, 1/10000)

sd.study ~ dunif(0, 100)
tau.study <-1/sd.study^2
sd.obs ~dunif(0,100)
tau.obs<-1/sd.obs^2


# random effect variance
## study
for (k in 1:n.study){
study.eff[k] ~ dnorm(0, tau.study)
}

for(j in 1:n.obs){
  obs[j] ~ dnorm(0, tau.obs)
}

# likelihood
for (i in 1:n){
d.tau[i] <- 1/(d.var[i]*d.var[i])
d[i] ~ dnorm(y[i], d.tau[i]) # observed d and variance
y[i] ~ dnorm(mu[i], tau)
mu[i] <- b0 + I*inv[i] + IG*invgc[i] + DROU*gc.drought[i] + NITR*gc.nitrogen[i] + CO2*gc.CO2[i] + O2*gc.O2[i] + PH*gc.pH[i]  +  
I.DROU*inv[i]*gc.drought[i] + I.NITR*inv[i]*gc.nitrogen[i] + I.CO2*inv[i]*gc.CO2[i] + I.O2*inv[i]*gc.O2[i] + I.PH*inv[i]*gc.pH[i]  +  
IG.DROU*invgc[i]*gc.drought[i] + IG.NITR*invgc[i]*gc.nitrogen[i] + IG.CO2*invgc[i]*gc.CO2[i] + IG.O2*invgc[i]*gc.O2[i] + IG.PH*invgc[i]*gc.pH[i]  +  
study.eff[study.id[i]]  + obs[obs.id[i]]

# simulated data (for checks)
y.sim[i] ~ dnorm(mu[i], tau)
# residuals
residual[i]<-(y[i]-mu[i])
# sums of squares differences
sqdif.data[i]<-(y[i]-mu[i])^2 
sqdif.sim[i]<-(y.sim[i]-mu[i])^2
}

# derived values
sumsq.data<-sum(sqdif.data[ ])
sumsq.sim<-sum(sqdif.sim[ ])
p.sumsq<-step(sumsq.sim - sumsq.data) # discrepancy
## mean
mean.data <- mean(y[ ]) # these are empty cause it's the whole vector
mean.sim <- mean(y.sim[ ])
p.mean <- step(mean.sim - mean.data)

}

",fill = TRUE)
sink()


# inits and run times
# inits determined from posteriors
inits = list(list(b0 = 0, sd = 4, sd.study = 3, I = -1, IG = -1, PH = -5, O2 = 15, NITR = 0, CO2 = 5, DROU = -6,
                  IG.O2 = -10, IG.PH = 0, IG.CO2 = -5, IG.DROU = 2, IG.NITR = -2,
                  I.DROU = 2, I.NITR = 1, I.O2 = -20, I.PH =5, I.CO2 = -10),
             list(b0 = 3, sd = 5, sd.study = 4, I = 2, IG = -3, PH = 11, O2 = 5, NITR = -5, CO2 = 0, DROU = -2,
                  IG.O2 = 10, IG.PH = 5, IG.CO2 = 7, IG.DROU = -4, IG.NITR = 2,
                  I.DROU = -2, I.NITR = -1, I.O2 = 5, I.PH =-5, I.CO2 = -5),
             list(b0 = -3, sd = 3, sd.study = 1, I = -3, IG = 1, PH = 2, O2 = -5, NITR = 20, CO2 = -5, DROU = 2,
                  IG.O2 = 5, IG.PH = -10, IG.CO2 = 10, IG.DROU = -2, IG.NITR = 4,
                  I.DROU = 4, I.NITR = 3, I.O2 = -10, I.PH =-10, I.CO2 = 5))

n.adapt = 30000 # for choosing the sampler and mixing 
n.update = 1000 # burn-in
n.iter = 50000 # keep  


set.seed(1)
jm_gc = jags.model("gc_hier_model.R", data = data_gc, inits = inits,
                   n.chains = length(inits), n.adapt = n.adapt)
update(jm_gc , n.iter = n.update)

cj_gc = coda.samples(jm_gc, variable.names=c("b0", "sd", "I",  "IG", "sd.study", "sd.obs",
                                             "DROU","NITR","CO2","O2","PH",
                                             "I.DROU","I.NITR","I.CO2","I.O2","I.PH",
                                             "IG.DROU","IG.NITR","IG.CO2","IG.O2","IG.PH"),
                     n.iter = n.iter, thin = 1)
#plot(cj_gc) # b0 not amazing
gelman.diag(cj_gc) 

# checking the fit
#zj_gc = jags.samples(jm_gc, variable.names=c("p.sumsq", "p.mean", "residual", "mu"), # "sumsq.data", "sumsq.sim", # "psuedoR2.num"
#                     n.iter = n.iter, thin = 1)
#zj_gc$p.sumsq # 0.504
#zj_gc$p.mean # 0.498 
##sumsq.data<-zj_gc$sumsq.data[,,1] 
##sumsq.sim<-zj_gc$sumsq.sim[,,1]
##plot(sumsq.sim~sumsq.data) 
# Bayesian R2
#var(zj_gc$mu)/(var(zj_gc$mu)+var(zj_gc$residual)) 
# 0.52
#rm(zj_gc)

# model output
out_gc<-data.frame(rbind(cj_gc[[1]], cj_gc[[2]], cj_gc[[3]])) %>%
  select(-sd.study, sd)

# plot all means (for supplemental)
out_gc_means<-data.frame(g.temp = out_gc$b0, g.drought = out_gc$b0+out_gc$DROU, g.nitrogen = out_gc$b0+out_gc$NITR, g.co2 = out_gc$b0+out_gc$CO2, g.o2 = out_gc$b0+out_gc$O2, g.pH = out_gc$b0+out_gc$PH, 
                         i.temp = out_gc$b0+out_gc$I, i.drought = out_gc$b0+out_gc$I+out_gc$DROU+out_gc$I.DROU, i.nitrogen = out_gc$b0+out_gc$I+out_gc$NITR+out_gc$I.NITR, 
                         i.co2 = out_gc$b0+out_gc$I+out_gc$CO2+out_gc$I.CO2 , i.o2 = out_gc$b0+out_gc$I+out_gc$O2+out_gc$I.O2, i.pH = out_gc$b0+out_gc$I+out_gc$PH+out_gc$I.PH, 
                         ig.temp = out_gc$b0+out_gc$IG, ig.drought = out_gc$b0+out_gc$IG+out_gc$DROU+out_gc$IG.DROU, ig.nitrogen = out_gc$b0+out_gc$IG+out_gc$NITR+out_gc$IG.NITR, 
                         ig.co2 = out_gc$b0+out_gc$IG+out_gc$CO2+out_gc$IG.CO2 , ig.o2 = out_gc$b0+out_gc$IG+out_gc$O2+out_gc$IG.O2, ig.pH = out_gc$b0+out_gc$IG+out_gc$PH+out_gc$IG.PH)
out_gc_plot<-data.frame(mean = apply(out_gc_means, 2, mean), 
                        lower = apply(out_gc_means, 2, function(x) quantile(x, probs = c(.025))),
                        upper = apply(out_gc_means, 2, function(y) quantile(y, probs = c(.975))),
                        name = names(out_gc_means),
                        beta = factor(rep(c("temperature", "drought","nitrogen","CO2","O2","pH"), 3), # 
                                      levels = c("temperature", "drought","nitrogen","CO2","O2","pH")), # 
                        treatment = rep(c("GC","Inv","Inv+GC"), each = 6)) %>%
  gather(end, value, -c(4:6))
out_gc_plot$treatment<- factor(out_gc_plot$treatment, levels = c("Inv+GC","Inv","GC"))

ggplot(data = out_gc_plot[!out_gc_plot$end=='mean',], aes(x=value, y=treatment)) +
  geom_line(stat = "identity", position = "identity", size = 4, colour = "grey40") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(data =out_gc_plot[out_gc_plot$end=='mean',], aes(x=value, y=treatment), 
             size = 3, colour = "white") +
  facet_grid(beta~.)+
  theme_bw() +
  labs(x = "Mean effect size (Hedges'd) +/- 95% credible interval") +
  theme(axis.text.x = element_text(size = 13, colour = "black"), axis.title.x = element_text(size=15, vjust = 0.5), 
        axis.text.y = element_text(size = 13, colour = "black"), axis.title.y=element_blank(),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))  

# plot means for just temp, drought, N
out_gc2<-data.frame(g.temp = out_gc$b0, g.drought = out_gc$b0+out_gc$DROU, g.nitrogen = out_gc$b0+out_gc$NITR,
                    i.temp = out_gc$b0+out_gc$I, i.drought = out_gc$b0+out_gc$I+out_gc$DROU+out_gc$I.DROU, i.nitrogen = out_gc$b0+out_gc$I+out_gc$NITR+out_gc$I.NITR, 
                    ig.temp = out_gc$b0+out_gc$IG, ig.drought = out_gc$b0+out_gc$IG+out_gc$DROU+out_gc$IG.DROU, ig.nitrogen = out_gc$b0+out_gc$IG+out_gc$NITR+out_gc$IG.NITR)
out_gc_plot<-data.frame(mean = apply(out_gc2, 2, mean), 
                        lower = apply(out_gc2, 2, function(x) quantile(x, probs = c(.025))),
                        upper = apply(out_gc2, 2, function(y) quantile(y, probs = c(.975))),
                        name = names(out_gc2),
                        beta = factor(rep(c("temperature", "drought","nitrogen"), 3), # 
                                      levels = c("temperature", "drought","nitrogen")), # 
                        treatment = rep(c("GC","Inv","Inv+GC"), each = 3)) %>%
  gather(end, value, -c(4:6))
out_gc_plot$treatment<- factor(out_gc_plot$treatment, levels = c("Inv+GC","Inv","GC"))

ggplot(data = out_gc_plot[!out_gc_plot$end=='mean',], aes(x=value, y=treatment)) +
  geom_line(stat = "identity", position = "identity", size = 4, colour = "grey40") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(data =out_gc_plot[out_gc_plot$end=='mean',], aes(x=value, y=treatment), 
             size = 3, colour = "white") +
  facet_grid(beta~.)+
  theme_bw() +
  labs(x = "Mean effect size (Hedges'd) +/- 95% credible interval") +
  theme(axis.text.x = element_text(size = 13, colour = "black"), axis.title.x = element_text(size=15, vjust = 0.5), 
        axis.text.y = element_text(size = 13, colour = "black"), axis.title.y=element_blank(),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))  


# ... invasion mechanism ----
# intercept = competition

data_mech<- list(
  d = c(d_cl$d_inv, d_cl$d_gc, d_cl$d_inv_gc),
  d.var = c(d_cl$d_var_inv, d_cl$d_var_gc, d_cl$d_var_inv_gc),
  inv = rep(c(1,0,0), each = nrow(d_cl)),
  invgc = rep(c(0,0,1), each = nrow(d_cl)),
  mech.pred = rep(ifelse(d_cl$mechanism_broad =="predation", 1, 0),3),
  mech.chem = rep(ifelse(d_cl$mechanism_broad =="chemical/physical", 1, 0),3),
  n = 3*nrow(d_cl),
  study.id = rep(as.numeric(as.factor(d_cl$studyID)),3),
  n.study = length(unique(d_cl$studyID)),
  obs.id = rep(d_cl$obs_ID, 3),
  n.obs = nrow(d_cl)
)

sink("mech_hier_model.R") # This is the file name for the jags code
cat(" 
model{
# priors
sd ~ dunif(0, 100) 
tau <- 1/sd^2
b0 ~ dnorm(0, 1/10000) # use taus, not sigmas
I ~ dnorm(0, 1/10000)
IG ~ dnorm(0, 1/10000)

PRED ~ dnorm(0, 1/10000)
CHEM ~ dnorm(0, 1/10000)
I.PRED ~ dnorm(0, 1/10000)
I.CHEM ~ dnorm(0, 1/10000)
IG.PRED ~ dnorm(0, 1/10000)
IG.CHEM ~ dnorm(0, 1/10000)

sd.study ~ dunif(0, 100)
tau.study <-1/sd.study^2
sd.obs ~dunif(0,100)
tau.obs<-1/sd.obs^2


# random effect variance
## study
for (k in 1:n.study){
study.eff[k] ~ dnorm(0, tau.study)
}

for(j in 1:n.obs){
  obs[j] ~ dnorm(0, tau.obs)
}

# likelihood
for (i in 1:n){
d.tau[i] <- 1/(d.var[i]*d.var[i])
d[i] ~ dnorm(y[i], d.tau[i]) # observed d and variance
y[i] ~ dnorm(mu[i], tau)
mu[i] <- b0 + I*inv[i] + IG*invgc[i] + PRED*mech.pred[i] + CHEM*mech.chem[i] + 
I.PRED*inv[i]*mech.pred[i] + I.CHEM*inv[i]*mech.chem[i] + 
IG.PRED*invgc[i]*mech.pred[i] + IG.CHEM*invgc[i]*mech.chem[i] + 
study.eff[study.id[i]] + obs[obs.id[i]]

# simulated data (for checks)
y.sim[i] ~ dnorm(mu[i], tau)
# residuals
residual[i]<-(y[i]-mu[i])
# sums of squares differences
sqdif.data[i]<-(y[i]-mu[i])^2 
sqdif.sim[i]<-(y.sim[i]-mu[i])^2
}

# derived values
sumsq.data<-sum(sqdif.data[ ])
sumsq.sim<-sum(sqdif.sim[ ])
p.sumsq<-step(sumsq.sim - sumsq.data) # discrepancy
## mean
mean.data <- mean(y[ ]) # these are empty cause it's the whole vector
mean.sim <- mean(y.sim[ ])
p.mean <- step(mean.sim - mean.data)

}


",fill = TRUE)
sink()


# inits and run times
inits = list(list(b0 = 0, sd = 4, sd.study = 3, I = -1, IG = -1, 
                  CHEM = 0, STR = 6, PRED = 6,
                  IG.STR = -5, IG.PRED = 2, IG.CHEM = -1,
                  I.PRED = 2, I.CHEM = 1, I.STR = -10),
             list(b0 = 3, sd = 5, sd.study = 4, I = 2, IG = -3, 
                  CHEM = -4, STR = 0, PRED = -2,
                  IG.STR = 6, IG.PRED = 4, IG.CHEM = 2,
                  I.PRED = -4, I.CHEM = -1, I.STR = -5),
             list(b0 = -3, sd = 3, sd.study = 1, I = -3, IG = 1, 
                  CHEM = 2, STR = -4, PRED = 2,
                  IG.STR = -4, IG.PRED = -2, IG.CHEM = 6,
                  I.PRED = -2, I.CHEM = 6, I.STR = 5))

n.adapt = 30000 # for choosing the sampler and mixing 
n.update = 1000 # burn-in
n.iter = 50000 # keep  

set.seed(1)
jm_mech = jags.model("mech_hier_model.R", data = data_mech, 
                     n.chains = length(inits), n.adapt = n.adapt)
update(jm_mech , n.iter = n.update)

cj_mech = coda.samples(jm_mech, variable.names=c("b0", "sd", "I",  "IG", "sd.study","sd.obs",
                                                 "PRED","CHEM",
                                                 "I.PRED","I.CHEM",
                                                 "IG.PRED","IG.CHEM"),
                       n.iter = n.iter, thin = 1)
#plot(cj_mech) 
gelman.diag(cj_mech)

# checking the fit
#zj_mech = jags.samples(jm_mech, variable.names=c("p.sumsq", "p.mean", "residual", "mu"), # "sumsq.data", "sumsq.sim", # "psuedoR2.num"
#                       n.iter = n.iter, thin = 1)
#zj_mech$p.sumsq # 0.506 
#zj_mech$p.mean # 0.501 
##sumsq.data<-zj_mech$sumsq.data[,,1] 
##sumsq.sim<-zj_mech$sumsq.sim[,,1]
##plot(sumsq.sim~sumsq.data) 
# Bayesian R2
#var(zj_mech$mu)/(var(zj_mech$mu)+var(zj_mech$residual)) 
# 0.514
#rm(zj_mech)

# model output
out_mech<-data.frame(rbind(cj_mech[[1]], cj_mech[[2]], cj_mech[[3]]))%>%
  select(-sd.study, sd)
# means
out_mech2<-data.frame(g.COMP = out_mech$b0, g.PRED = out_mech$b0+out_mech$PRED, g.CHEM = out_mech$b0+out_mech$CHEM, #g.STR = out_mech$b0+out_mech$STR, 
                      i.COMP = out_mech$b0+out_mech$I, i.PRED = out_mech$b0+out_mech$I+out_mech$PRED+out_mech$I.PRED, i.CHEM = out_mech$b0+out_mech$I+out_mech$CHEM+out_mech$I.CHEM, #i.STR = out_mech$b0+out_mech$I+out_mech$STR+out_mech$I.STR, 
                      ig.COMP = out_mech$b0+out_mech$IG, ig.PRED = out_mech$b0+out_mech$IG+out_mech$PRED+out_mech$IG.PRED, ig.CHEM = out_mech$b0+out_mech$IG+out_mech$CHEM+out_mech$IG.CHEM)#, ig.STR = out_mech$b0+out_mech$IG+out_mech$STR+out_mech$IG.STR)
out_mech_plot<-data.frame(mean = apply(out_mech2, 2, mean), 
                          lower = apply(out_mech2, 2, function(x) quantile(x, probs = c(.025))),
                          upper = apply(out_mech2, 2, function(y) quantile(y, probs = c(.975))),
                          beta = factor(rep(c("competition", "predation", "chemical/physical"), 3), # ,  "structural"
                                        levels = c("competition", "predation", "chemical/physical")),
                          treatment = rep(c("GC","Inv","Inv+GC"), each = 3)) %>%
  gather(end, value, -c(4:5))
out_mech_plot$treatment<- factor(out_mech_plot$treatment, levels = c("Inv+GC","Inv","GC"))

ggplot(data = out_mech_plot[!out_mech_plot$end=='mean',], aes(x=value, y=treatment)) +
  geom_line(stat = "identity", position = "identity", size = 4, colour = "grey40") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(data =out_mech_plot[out_mech_plot$end=='mean',], aes(x=value, y=treatment), 
             size = 3, colour = "white") +
  facet_grid(beta~.)+
  theme_bw() +
  labs(x = "Mean effect size (Hedges'd) +/- 95% credible interval") +
  theme(axis.text.x = element_text(size = 13, colour = "black"), axis.title.x = element_text(size=15, vjust = 0.5), 
        axis.text.y = element_text(size = 13, colour = "black"), axis.title.y=element_blank(),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))  


# ... response class ----
# intercept = biomass
rc<-data.frame(n.cases = table(d_full$response_class), n.studies = tapply(d_full$studyID, d_full$response_class, function(x) length(unique(x))))
names(rc)<-c("response_class", "n.cases", "n.studies")

data_rclass<- list(
  d = c(d_cl$d_inv, d_cl$d_gc, d_cl$d_inv_gc),
  d.var = c(d_cl$d_var_inv, d_cl$d_var_gc, d_cl$d_var_inv_gc),
  inv = rep(c(1,0,0), each = nrow(d_cl)),
  invgc = rep(c(0,0,1), each = nrow(d_cl)),
  n = 3*nrow(d_cl),
  study.id = rep(as.numeric(as.factor(d_cl$studyID)),3),
  n.study = length(unique(d_cl$studyID)),
  obs.id = rep(d_cl$obs_ID, 3),
  n.obs = nrow(d_cl),
  abun = rep(ifelse(d_cl$response_class=="abundance", 1, 0),3),
  allo = rep(ifelse(d_cl$response_class =="allocation", 1, 0),3),
  beh = rep(ifelse(d_cl$response_class =="behavior", 1, 0),3),
  div = rep(ifelse(d_cl$response_class =="diversity", 1, 0),3),
  nutr = rep(ifelse(d_cl$response_class =="nutrient", 1, 0),3),
  physi = rep(ifelse(d_cl$response_class=="physiology", 1, 0),3),
  repr = rep(ifelse(d_cl$response_class=="reproduction", 1, 0),3),
  size = rep(ifelse(d_cl$response_class=="size", 1, 0),3),
  surv = rep(ifelse(d_cl$response_class=="survival", 1, 0),3)
)

sink("rclass_hier_model.R") # This is the file name for the jags code
cat(" 
model{
# priors
sd ~ dunif(0, 100) 
tau <- 1/sd^2
b0 ~ dnorm(0, 1/10000) # use taus, not sigmas
I ~ dnorm(0, 1/10000)
IG ~ dnorm(0, 1/10000)
ABUN ~ dnorm(0, 1/10000)
ALLO ~ dnorm(0, 1/10000)
BEH ~ dnorm(0, 1/10000)
DIV ~ dnorm(0, 1/10000)
NUTR ~ dnorm(0, 1/10000)
PHYSI ~ dnorm(0, 1/10000)
REPR ~ dnorm(0, 1/10000)
SIZE ~ dnorm(0, 1/10000)
SURV ~ dnorm(0, 1/10000)
I.ABUN ~ dnorm(0, 1/10000)
I.ALLO ~ dnorm(0, 1/10000)
I.BEH ~ dnorm(0, 1/10000)
I.DIV ~ dnorm(0, 1/10000)
I.NUTR ~ dnorm(0, 1/10000)
I.PHYSI ~ dnorm(0, 1/10000)
I.REPR ~ dnorm(0, 1/10000)
I.SIZE ~ dnorm(0, 1/10000)
I.SURV ~ dnorm(0, 1/10000)
IG.ABUN ~ dnorm(0, 1/10000)
IG.ALLO ~ dnorm(0, 1/10000)
IG.BEH ~ dnorm(0, 1/10000)
IG.DIV ~ dnorm(0, 1/10000)
IG.NUTR ~ dnorm(0, 1/10000)
IG.PHYSI ~ dnorm(0, 1/10000)
IG.REPR ~ dnorm(0, 1/10000)
IG.SIZE ~ dnorm(0, 1/10000)
IG.SURV ~ dnorm(0, 1/10000)

sd.study ~ dunif(0, 100)
tau.study <-1/sd.study^2
sd.obs ~dunif(0,100)
tau.obs<-1/sd.obs^2


# random effect variance
## study
for (k in 1:n.study){
study.eff[k] ~ dnorm(0, tau.study)
}
for(j in 1:n.obs){
  obs[j] ~ dnorm(0, tau.obs)
}

# likelihood
for (i in 1:n){
d.tau[i] <- 1/(d.var[i]*d.var[i])
d[i] ~ dnorm(y[i], d.tau[i]) # observed d and variance
y[i] ~ dnorm(mu[i], tau)
mu[i] <- b0 + I*inv[i] + IG*invgc[i] + 
ABUN* abun[i] + ALLO* allo[i] + BEH* beh[i] + DIV* div[i]  +  
NUTR* nutr[i] + PHYSI* physi[i] + REPR* repr[i] + SIZE* size[i] + SURV* surv[i] +
I.ABUN*inv[i]* abun[i] + I.ALLO*inv[i]* allo[i] + I.BEH*inv[i]* beh[i] + I.DIV*inv[i]* div[i]  + 
I.NUTR*inv[i]* nutr[i] + I.PHYSI*inv[i]* physi[i] + I.REPR*inv[i]* repr[i] + I.SIZE*inv[i]* size[i] + I.SURV*inv[i]* surv[i] +
IG.ABUN*invgc[i]* abun[i] + IG.ALLO*invgc[i]* allo[i] + IG.BEH*invgc[i]* beh[i] + IG.DIV*invgc[i]* div[i]  + 
IG.NUTR*invgc[i]* nutr[i] + IG.PHYSI*invgc[i]* physi[i] + IG.REPR*invgc[i]* repr[i] + IG.SIZE*invgc[i]* size[i] +IG.SURV*invgc[i]* surv[i] +
study.eff[study.id[i]] + obs[obs.id[i]]

# simulated data (for checks)
y.sim[i] ~ dnorm(mu[i], tau)
# residuals
residual[i]<-(y[i]-mu[i])
# sums of squares differences
sqdif.data[i]<-(y[i]-mu[i])^2 
sqdif.sim[i]<-(y.sim[i]-mu[i])^2
}

# derived values
sumsq.data<-sum(sqdif.data[ ])
sumsq.sim<-sum(sqdif.sim[ ])
p.sumsq<-step(sumsq.sim - sumsq.data) # discrepancy
## mean
mean.data <- mean(y[ ]) # these are empty cause it's the whole vector
mean.sim <- mean(y.sim[ ])
p.mean <- step(mean.sim - mean.data)

}

",fill = TRUE)
sink()


# inits and run times
inits = list(list(b0 = 0, sd = 4, sd.study = 2, I = -1, IG = -3, 
                  ABUN = 4, ALLO = -4, BEH = 10, DIV = -7, NUTR = -5, PHYSI = 5, REPR = 6, SIZE = -3, SURV = -4,
                  I.ABUN = 6, I.ALLO = -4, I.BEH = -5, I.DIV =5, I.NUTR = -3, I.PHYSI = 6, I.REPR = 5, I.SIZE = -4, I.SURV = 4,
                  IG.ABUN = -4, IG.ALLO = -3, IG.BEH = -10, IG.DIV = 6, IG.NUTR = -2, IG.PHYSI = -4, IG.REPR = 5, IG.SIZE = -3, IG.SURV = 4),
             list(b0 = -3, sd = 5, sd.study = 5, I = 2, IG = -1, 
                  ABUN = -2, ALLO = 5, BEH = 5, DIV = 6, NUTR = 5, PHYSI = -4, REPR = 0, SIZE = 3, SURV = 4,
                  I.ABUN = 6, I.ALLO = 6, I.BEH = -10, I.DIV =-5, I.NUTR = -4, I.PHYSI = 4, I.REPR = -5, I.SIZE = 5, I.SURV = 2,
                  IG.ABUN = -4, IG.ALLO = 8, IG.BEH = 10, IG.DIV = 5, IG.NUTR = 6, IG.PHYSI = 7, IG.REPR = -5, IG.SIZE = -4, IG.SURV = -7),
             list(b0 = 3, sd = 3, sd.study = 5, I = -3, IG = 1, 
                  ABUN = -4, ALLO = 5, BEH = -6, DIV = -6, NUTR = -4, PHYSI = -2, REPR = -6, SIZE = -4, SURV = 3,
                  I.ABUN = 4, I.ALLO = 5, I.BEH = 10, I.DIV =4, I.NUTR = 4, I.PHYSI = -2, I.REPR = -2, I.SIZE = 6, I.SURV = -6,
                  IG.ABUN = 3, IG.ALLO = 6, IG.BEH = -5, IG.DIV = -5, IG.NUTR = -4, IG.PHYSI = 4, IG.REPR = -6, IG.SIZE = 4, IG.SURV = -8))   



n.adapt = 30000 # for choosing the sampler and mixing 
n.update = 1000 # burn-in
n.iter = 50000 # keep  

set.seed(1)
jm_rclass = jags.model("rclass_hier_model.R", data = data_rclass, inits = inits,
                       n.chains = length(inits), n.adapt = n.adapt)
update(jm_rclass , n.iter = n.update)

cj_rclass = coda.samples(jm_rclass, variable.names=c("b0", "sd", "I",  "IG", "sd.study","sd.obs",
                                                     "ABUN","ALLO","BEH", "DIV", 
                                                     "NUTR","PHYSI","REPR",'SIZE',"SURV",
                                                     "I.ABUN","I.ALLO","I.BEH", "I.DIV", 
                                                     "I.NUTR","I.PHYSI","I.REPR",'I.SIZE',"I.SURV",
                                                     "IG.ABUN","IG.ALLO","IG.BEH", "IG.DIV", 
                                                     "IG.NUTR","IG.PHYSI","IG.REPR",'IG.SIZE',"IG.SURV"),
                         n.iter = n.iter, thin = 1)
#plot(cj_rclass)  #b0 not great
gelman.diag(cj_rclass)

# checking the fit
#zj_rclass = jags.samples(jm_rclass, variable.names=c("p.sumsq", "p.mean", "residual", "mu"), # "sumsq.data", "sumsq.sim", # "psuedoR2.num"
#                         n.iter = n.iter, thin = 1)
#zj_rclass$p.sumsq # 0.505 
#zj_rclass$p.mean # 0.500 
##sumsq.data<-zj_rclass$sumsq.data[,,1] 
##sumsq.sim<-zj_rclass$sumsq.sim[,,1]
##plot(sumsq.sim~sumsq.data) 
# Bayesian R2
#var(zj_rclass$mu)/(var(zj_rclass$mu)+var(zj_rclass$residual)) 
# 0.52
#rm(zj_rclass)

# model output
out_rclass<-data.frame(rbind(cj_rclass[[1]], cj_rclass[[2]], cj_rclass[[3]])) %>%
  select(-sd, -sd.study)
names(out_rclass)
# means
out_rclass_means<-data.frame(g.abun = out_rclass$b0+out_rclass$ABUN, g.allo = out_rclass$b0+out_rclass$ALLO,  
                             g.beh = out_rclass$b0+out_rclass$BEH, g.biom = out_rclass$b0,g.div = out_rclass$b0+out_rclass$DIV,#g.gro = out_rclass$b0+out_rclass$GRO,
                             g.nutr = out_rclass$b0+out_rclass$NUTR, g.PHYSI = out_rclass$b0+out_rclass$PHYSI, g.REPR = out_rclass$b0+out_rclass$REPR, 
                             g.size = out_rclass$b0+out_rclass$SIZE,g.surv = out_rclass$b0+out_rclass$SURV,
                             i.abun = out_rclass$b0+out_rclass$I+out_rclass$ABUN+out_rclass$I.ABUN, i.allo = out_rclass$b0+out_rclass$I+out_rclass$ALLO+out_rclass$I.ALLO, 
                             i.beh = out_rclass$b0+out_rclass$I+out_rclass$BEH+out_rclass$I.BEH,i.biom = out_rclass$b0+out_rclass$I,  i.div = out_rclass$b0+out_rclass$I+out_rclass$DIV+out_rclass$I.DIV, #i.gro = out_rclass$b0+out_rclass$I+out_rclass$GRO+out_rclass$I.GRO,
                             i.nutr = out_rclass$b0+out_rclass$I+out_rclass$NUTR+out_rclass$I.NUTR, i.PHYSI = out_rclass$b0+out_rclass$I+out_rclass$PHYSI+out_rclass$I.PHYSI, i.REPR = out_rclass$b0+out_rclass$I+out_rclass$REPR+out_rclass$I.REPR, 
                             i.size = out_rclass$b0+out_rclass$I+out_rclass$SIZE+out_rclass$I.SIZE, i.surv = out_rclass$b0+out_rclass$I+out_rclass$SURV+out_rclass$I.SURV,
                             ig.abun = out_rclass$b0+out_rclass$IG+out_rclass$ABUN+out_rclass$IG.ABUN, ig.allo = out_rclass$b0+out_rclass$IG+out_rclass$ALLO+out_rclass$IG.ALLO, 
                             ig.beh = out_rclass$b0+out_rclass$IG+out_rclass$BEH+out_rclass$IG.BEH, ig.biom = out_rclass$b0+out_rclass$IG, ig.div = out_rclass$b0+out_rclass$IG+out_rclass$DIV+out_rclass$IG.DIV, #ig.gro = out_rclass$b0+out_rclass$IG+out_rclass$GRO+out_rclass$IG.GRO,
                             ig.nutr = out_rclass$b0+out_rclass$IG+out_rclass$NUTR+out_rclass$IG.NUTR, ig.PHYSI = out_rclass$b0+out_rclass$IG+out_rclass$PHYSI+out_rclass$IG.PHYSI, ig.REPR = out_rclass$b0+out_rclass$IG+out_rclass$REPR+out_rclass$IG.REPR, 
                             ig.size = out_rclass$b0+out_rclass$IG+out_rclass$SIZE+out_rclass$IG.SIZE, ig.surv = out_rclass$b0+out_rclass$IG+out_rclass$SURV+out_rclass$IG.SURV)

out_rclass_plot<-data.frame(mean = apply(out_rclass_means, 2, mean), 
                            lower = apply(out_rclass_means, 2, function(x) quantile(x, probs = c(.025))),
                            upper = apply(out_rclass_means, 2, function(y) quantile(y, probs = c(.975))),
                            name = names(out_rclass_means),
                            beta = factor(rep(rc$response_class, 3)), 
                            treatment = rep(c("GC","Inv","Inv+GC"), each = nrow(rc))) %>%
  gather(end, value, -c(4:6))
out_rclass_plot$treatment<- factor(out_rclass_plot$treatment, levels = c("Inv+GC","Inv","GC"))

ggplot(data = out_rclass_plot[!out_rclass_plot$end=='mean',], aes(x=value, y=treatment)) +
  geom_line(stat = "identity", position = "identity", size = 4, colour = "grey40") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(data =out_rclass_plot[out_rclass_plot$end=='mean',], aes(x=value, y=treatment), 
             size = 3, colour = "white") +
  facet_grid(beta~.)+
  theme_bw() +
  labs(x = "Mean effect size (Hedges'd) +/- 95% credible interval") +
  theme(axis.text.x = element_text(size = 13, colour = "black"), axis.title.x = element_text(size=15, vjust = 0.5), 
        axis.text.y = element_text(size = 13, colour = "black"), axis.title.y=element_blank(),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))  


# ... setting ----
# intercept = terrestrial

data_sett<- list(
  d = c(d_cl$d_inv, d_cl$d_gc, d_cl$d_inv_gc),
  d.var = c(d_cl$d_var_inv, d_cl$d_var_gc, d_cl$d_var_inv_gc),
  inv = rep(c(1,0,0), each = nrow(d_cl)),
  invgc = rep(c(0,0,1), each = nrow(d_cl)),
  sett.fresh = rep(ifelse(d_cl$setting =="freshwater", 1, 0),3),
  sett.marine = rep(ifelse(d_cl$setting =="marine", 1, 0),3),
  n = 3*nrow(d_cl),
  study.id = rep(as.numeric(as.factor(d_cl$studyID)),3),
  n.study = length(unique(d_cl$studyID)),
  obs.id = rep(d_cl$obs_ID, 3),
  n.obs = nrow(d_cl)
)

sink("sett_hier_model.R") # This is the file name for the jags code
cat(" 
model{
# priors
sd ~ dunif(0, 100) 
tau <- 1/sd^2
b0 ~ dnorm(0, 1/10000) # use taus, not sigmas
I ~ dnorm(0, 1/10000)
IG ~ dnorm(0, 1/10000)

FRESH ~ dnorm(0, 1/10000)
MARINE ~ dnorm(0, 1/10000)
I.FRESH ~ dnorm(0, 1/10000)
I.MARINE ~ dnorm(0, 1/10000)
IG.FRESH ~ dnorm(0, 1/10000)
IG.MARINE ~ dnorm(0, 1/10000)

sd.study ~ dunif(0, 100)
tau.study <-1/sd.study^2
sd.obs ~dunif(0,100)
tau.obs<-1/sd.obs^2

# random effect variance
## study
for (k in 1:n.study){
study.eff[k] ~ dnorm(0, tau.study)
}
for(j in 1:n.obs){
  obs[j] ~ dnorm(0, tau.obs)
}

# likelihood
for (i in 1:n){
d.tau[i] <- 1/(d.var[i]*d.var[i])
d[i] ~ dnorm(y[i], d.tau[i]) # observed d and variance
y[i] ~ dnorm(mu[i], tau)
mu[i] <- b0 + I*inv[i] + IG*invgc[i] + FRESH*sett.fresh[i] + MARINE*sett.marine[i] + 
I.FRESH*inv[i]*sett.fresh[i] + I.MARINE*inv[i]*sett.marine[i] + 
IG.FRESH*invgc[i]*sett.fresh[i] + IG.MARINE*invgc[i]*sett.marine[i] + 
study.eff[study.id[i]] + obs[obs.id[i]]

# simulated data (for checks)
y.sim[i] ~ dnorm(mu[i], tau)
# residuals
residual[i]<-(y[i]-mu[i])
# sums of squares differences
sqdif.data[i]<-(y[i]-mu[i])^2 
sqdif.sim[i]<-(y.sim[i]-mu[i])^2
}

# derived values
sumsq.data<-sum(sqdif.data[ ])
sumsq.sim<-sum(sqdif.sim[ ])
p.sumsq<-step(sumsq.sim - sumsq.data) # discrepancy
## mean
mean.data <- mean(y[ ]) # these are empty cause it's the whole vector
mean.sim <- mean(y.sim[ ])
p.mean <- step(mean.sim - mean.data)

}


",fill = TRUE)
sink()


# inits and run times
inits = list(list(b0 = 0, sd = 4, sd.study = 3, I = -1, IG = -1, 
                  MARINE = 0, FRESH = 6,
                  IG.FRESH = 2, IG.MARINE = -2,
                  I.FRESH = 2, I.MARINE = 4),
             list(b0 = 3, sd = 5, sd.study = 5, I = 2, IG = -3, 
                  MARINE = -4, FRESH = -2,
                  IG.FRESH = 4, IG.MARINE = 4,
                  I.FRESH = -4, I.MARINE = -2),
             list(b0 = -3, sd = 2, sd.study = 1, I = -3, IG = 1, 
                  MARINE = -4, FRESH = 2,
                  IG.FRESH = -2, IG.MARINE = 6,
                  I.FRESH = -2, I.MARINE = 6))

n.adapt = 30000 # for choosing the sampler and mixing 
n.update = 1000 # burn-in
n.iter = 50000 # keep  

set.seed(1)
jm_sett = jags.model("sett_hier_model.R", data = data_sett, 
                     n.chains = length(inits), n.adapt = n.adapt)
update(jm_sett , n.iter = n.update)

cj_sett = coda.samples(jm_sett, variable.names=c("b0", "sd", "I",  "IG", "sd.study","sd.obs",
                                                 "FRESH","MARINE",
                                                 "I.FRESH","I.MARINE",
                                                 "IG.FRESH","IG.MARINE"),
                       n.iter = n.iter, thin = 1)
#plot(cj_sett) 
gelman.diag(cj_sett)

# checking the fit
#zj_sett = jags.samples(jm_sett, variable.names=c("p.sumsq", "p.mean", "residual", "mu"), # "sumsq.data", "sumsq.sim", # "psuedoR2.num"
#                       n.iter = n.iter, thin = 1)
#zj_sett$p.sumsq # 0.507
#zj_sett$p.mean # 0.501
##sumsq.data<-zj_sett$sumsq.data[,,1] 
##sumsq.sim<-zj_sett$sumsq.sim[,,1]
##plot(sumsq.sim~sumsq.data) 
# Bayesian R2
#var(zj_sett$mu)/(var(zj_sett$mu)+var(zj_sett$residual)) 
# 0.52
#rm(zj_sett)

# model output
out_sett<-data.frame(rbind(cj_sett[[1]], cj_sett[[2]], cj_sett[[3]]))%>%
  select(-sd.study, sd)
#means
out_sett2<-data.frame(g.TERR = out_sett$b0, g.FRESH = out_sett$b0+out_sett$FRESH, g.MARINE = out_sett$b0+out_sett$MARINE, 
                      i.TERR = out_sett$b0+out_sett$I, i.FRESH = out_sett$b0+out_sett$I+out_sett$FRESH+out_sett$I.FRESH, i.MARINE = out_sett$b0+out_sett$I+out_sett$MARINE+out_sett$I.MARINE, 
                      ig.TERR = out_sett$b0+out_sett$IG, ig.FRESH = out_sett$b0+out_sett$IG+out_sett$FRESH+out_sett$IG.FRESH, ig.MARINE = out_sett$b0+out_sett$IG+out_sett$MARINE+out_sett$IG.MARINE)
out_sett_plot<-data.frame(mean = apply(out_sett2, 2, mean), 
                          lower = apply(out_sett2, 2, function(x) quantile(x, probs = c(.025))),
                          upper = apply(out_sett2, 2, function(y) quantile(y, probs = c(.975))),
                          beta = factor(rep(c("terrestrial", "freshwater", "marine"), 3),
                                        levels = c("terrestrial", "freshwater", "marine")),
                          treatment = rep(c("GC","Inv","Inv+GC"), each = 3)) %>%
  gather(end, value, -c(4:5))
out_sett_plot$treatment<- factor(out_sett_plot$treatment, levels = c("Inv+GC","Inv","GC"))

ggplot(data = out_sett_plot[!out_sett_plot$end=='mean',], aes(x=value, y=treatment)) +
  geom_line(stat = "identity", position = "identity", size = 4, colour = "grey40") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(data =out_sett_plot[out_sett_plot$end=='mean',], aes(x=value, y=treatment), 
             size = 3, colour = "white") +
  facet_grid(beta~.)+
  theme_bw() +
  labs(x = "Mean effect size (Hedges'd) +/- 95% credible interval") +
  theme(axis.text.x = element_text(size = 13, colour = "black"), axis.title.x = element_text(size=15, vjust = 0.5), 
        axis.text.y = element_text(size = 13, colour = "black"), axis.title.y=element_blank(),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))  


# ... experiment type ----
# intercept = mesocosm

data_exp<- list(
  d = c(d_cl$d_inv, d_cl$d_gc, d_cl$d_inv_gc),
  d.var = c(d_cl$d_var_inv, d_cl$d_var_gc, d_cl$d_var_inv_gc),
  inv = rep(c(1,0,0), each = nrow(d_cl)),
  invgc = rep(c(0,0,1), each = nrow(d_cl)),
  exp.lab = rep(ifelse(d_cl$study_type =="lab/greenhouse", 1, 0),3),
  exp.field = rep(ifelse(d_cl$study_type =="field", 1, 0),3),
  n = 3*nrow(d_cl),
  study.id = rep(as.numeric(as.factor(d_cl$studyID)),3),
  n.study = length(unique(d_cl$studyID)),
  obs.id = rep(d_cl$obs_ID, 3),
  n.obs = nrow(d_cl)
)

sink("exp_hier_model.R") # This is the file name for the jags code
cat(" 
model{
# priors
sd ~ dunif(0, 100) 
tau <- 1/sd^2
b0 ~ dnorm(0, 1/10000) # use taus, not sigmas
I ~ dnorm(0, 1/10000)
IG ~ dnorm(0, 1/10000)

LAB ~ dnorm(0, 1/10000)
FIELD ~ dnorm(0, 1/10000)
I.LAB ~ dnorm(0, 1/10000)
I.FIELD ~ dnorm(0, 1/10000)
IG.LAB ~ dnorm(0, 1/10000)
IG.FIELD ~ dnorm(0, 1/10000)

sd.study ~ dunif(0, 100)
tau.study <-1/sd.study^2
sd.obs ~dunif(0,100)
tau.obs<-1/sd.obs^2


# random effect variance
## study
for (k in 1:n.study){
study.eff[k] ~ dnorm(0, tau.study)
}
for(j in 1:n.obs){
  obs[j] ~ dnorm(0, tau.obs)
}

# likelihood
for (i in 1:n){
d.tau[i] <- 1/(d.var[i]*d.var[i])
d[i] ~ dnorm(y[i], d.tau[i]) # observed d and variance
y[i] ~ dnorm(mu[i], tau)
mu[i] <- b0 + I*inv[i] + IG*invgc[i] + LAB*exp.lab[i] + FIELD*exp.field[i] + 
I.LAB*inv[i]*exp.lab[i] + I.FIELD*inv[i]*exp.field[i] + 
IG.LAB*invgc[i]*exp.lab[i] + IG.FIELD*invgc[i]*exp.field[i] + 
study.eff[study.id[i]] + obs[obs.id[i]]

# simulated data (for checks)
y.sim[i] ~ dnorm(mu[i], tau)
# residuals
residual[i]<-(y[i]-mu[i])
# sums of squares differences
sqdif.data[i]<-(y[i]-mu[i])^2 
sqdif.sim[i]<-(y.sim[i]-mu[i])^2
}

# derived values
sumsq.data<-sum(sqdif.data[ ])
sumsq.sim<-sum(sqdif.sim[ ])
p.sumsq<-step(sumsq.sim - sumsq.data) # discrepancy
## mean
mean.data <- mean(y[ ]) # these are empty cause it's the whole vector
mean.sim <- mean(y.sim[ ])
p.mean <- step(mean.sim - mean.data)

}


",fill = TRUE)
sink()


# inits and run times
inits = list(list(b0 = 0, sd = 4, sd.study = 3, I = -1, IG = -1, 
                  FIELD = 0, LAB = -6,
                  IG.LAB = 2, IG.FIELD = -1,
                  I.LAB = 2, I.FIELD = 1),
             list(b0 = 3, sd = 5, sd.study = 5, I = 2, IG = -3, 
                  FIELD = -4,LAB = -2,
                  IG.LAB = -4, IG.FIELD = 3,
                  I.LAB = -4, I.FIELD = -2),
             list(b0 = -3, sd = 3, sd.study = 1, I = -3, IG = 1, 
                  FIELD = 4, LAB = 4,
                  IG.LAB = -2, IG.FIELD = -4,
                  I.LAB = -2, I.FIELD = 6))

n.adapt = 30000 # for choosing the sampler and mixing 
n.update = 1000 # burn-in
n.iter = 50000 # keep  

set.seed(1)
jm_exp = jags.model("exp_hier_model.R", data = data_exp, 
                    n.chains = length(inits), n.adapt = n.adapt)
update(jm_exp , n.iter = n.update)

cj_exp = coda.samples(jm_exp, variable.names=c("b0", "sd", "I",  "IG", "sd.study","sd.obs",
                                               "LAB","FIELD",
                                               "I.LAB","I.FIELD",
                                               "IG.LAB","IG.FIELD"),
                      n.iter = n.iter, thin = 1)
#plot(cj_exp) 
gelman.diag(cj_exp)

# checking the fit
#zj_exp = jags.samples(jm_exp, variable.names=c("p.sumsq", "p.mean", "residual", "mu"), # "sumsq.data", "sumsq.sim", # "psuedoR2.num"
#                      n.iter = n.iter, thin = 1)
#zj_exp$p.sumsq # 0.507 
#zj_exp$p.mean # 0.501 
##sumsq.data<-zj_exp$sumsq.data[,,1] 
##sumsq.sim<-zj_exp$sumsq.sim[,,1]
##plot(sumsq.sim~sumsq.data) 
# Bayesian R2
#var(zj_exp$mu)/(var(zj_exp$mu)+var(zj_exp$residual)) 
# 0.52
#rm(zj_exp)

# model output
out_exp<-data.frame(rbind(cj_exp[[1]], cj_exp[[2]], cj_exp[[3]]))%>%
  select(-sd.study, sd)
# means
out_exp2<-data.frame(g.MESO = out_exp$b0, g.LAB = out_exp$b0+out_exp$LAB, g.FIELD = out_exp$b0+out_exp$FIELD, 
                     i.MESO = out_exp$b0+out_exp$I, i.LAB = out_exp$b0+out_exp$I+out_exp$LAB+out_exp$I.LAB, i.FIELD = out_exp$b0+out_exp$I+out_exp$FIELD+out_exp$I.FIELD,
                     ig.MESO = out_exp$b0+out_exp$IG, ig.LAB = out_exp$b0+out_exp$IG+out_exp$LAB+out_exp$IG.LAB, ig.FIELD = out_exp$b0+out_exp$IG+out_exp$FIELD+out_exp$IG.FIELD)
out_exp_plot<-data.frame(mean = apply(out_exp2, 2, mean), 
                         lower = apply(out_exp2, 2, function(x) quantile(x, probs = c(.025))),
                         upper = apply(out_exp2, 2, function(y) quantile(y, probs = c(.975))),
                         beta = factor(rep(c("mesocosm", "lab/greenhouse", "field"), 3),
                                       levels = c("mesocosm", "lab/greenhouse", "field")),
                         treatment = rep(c("GC","Inv","Inv+GC"), each = 3)) %>%
  gather(end, value, -c(4:5))
out_exp_plot$treatment<- factor(out_exp_plot$treatment, levels = c("Inv+GC","Inv","GC"))

ggplot(data = out_exp_plot[!out_exp_plot$end=='mean',], aes(x=value, y=treatment)) +
  geom_line(stat = "identity", position = "identity", size = 4, colour = "grey40") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(data =out_exp_plot[out_exp_plot$end=='mean',], aes(x=value, y=treatment), 
             size = 3, colour = "white") +
  facet_grid(beta~.)+
  theme_bw() +
  labs(x = "Mean effect size (Hedges'd) +/- 95% credible interval") +
  theme(axis.text.x = element_text(size = 13, colour = "black"), axis.title.x = element_text(size=15, vjust = 0.5), 
        axis.text.y = element_text(size = 13, colour = "black"), axis.title.y=element_blank(),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))  


# 8b. ... with known benefit dataset ----
# ... Means ----
d_ben$obs_ID<-1:nrow(d_ben) # for random effect
# intercept = gc
data_int_ben <- list(
  d = c(d_ben$d_inv, d_ben$d_gc, d_ben$d_inv_gc),
  d.var = c(d_ben$d_var_inv, d_ben$d_var_gc, d_ben$d_var_inv_gc),
  inv = rep(c(1,0,0), each = nrow(d_ben)),
  invgc = rep(c(0,0,1), each = nrow(d_ben)),
  n = 3*nrow(d_ben),
  study.id = rep(as.numeric(as.factor(d_ben$studyID)),3),
  n.study = length(unique(d_ben$studyID)),
  obs.id = rep(d_ben$obs_ID, 3),
  n.obs = nrow(d_ben)
  
)

# inits and run times
inits = list(list(b0 = 0, sd = 4, sd.study = 3, I = -1, IG = -1),
             list(b0 = 3, sd = 5, sd.study = 4, I = 2, IG = -3),
             list(b0 = -3, sd = 3, sd.study = 2, I = -3, IG = 1))

n.adapt = 30000 # for choosing the sampler and mixing 
n.update = 1000 # burn-in
n.iter = 50000 # keep  

set.seed(1)
jm_int_ben = jags.model("intercept_hier_model.R", data = data_int_ben, inits = inits,
                        n.chains = length(inits), n.adapt = n.adapt)
update(jm_int_ben , n.iter = n.update)

cj_int_ben = coda.samples(jm_int_ben, variable.names=c("b0", "sd", "I",  "IG", "sd.study", "sd.obs"),
                          n.iter = n.iter, thin = 1)
#plot(cj_int_ben) 
gelman.diag(cj_int_ben)


# 8c. ... with plant invaders only ----
d_pl$obs_ID<-1:nrow(d_pl) # for random effect
# intercept = gc
data_int_pl <- list(
  d = c(d_pl$d_inv, d_pl$d_gc, d_pl$d_inv_gc),
  d.var = c(d_pl$d_var_inv, d_pl$d_var_gc, d_pl$d_var_inv_gc),
  inv = rep(c(1,0,0), each = nrow(d_pl)),
  invgc = rep(c(0,0,1), each = nrow(d_pl)),
  n = 3*nrow(d_pl),
  study.id = rep(as.numeric(as.factor(d_pl$studyID)),3),
  n.study = length(unique(d_pl$studyID)),
  obs.id = rep(d_pl$obs_ID, 3),
  n.obs = nrow(d_pl)
)

set.seed(1)
jm_int_pl = jags.model("intercept_hier_model.R", data = data_int_pl, inits = inits,
                       n.chains = length(inits), n.adapt = n.adapt)
update(jm_int_pl , n.iter = n.update)

cj_int_pl = coda.samples(jm_int_pl, variable.names=c("b0", "sd", "I",  "IG", "sd.study", "sd.obs"),
                         n.iter = n.iter, thin = 1)
#plot(cj_int_pl) 
gelman.diag(cj_int_pl)


# 9. Full model for individual treatments ----

# this one is separated out for each treatment (too many parameters with interactions)
# intercept = temperature, competition, terrestrial, lab/greenhouse, biomass # this is 5 obs (chosen bc near means)

data<- list(
  i.d = d_cl$d_inv,
  i.d.var = d_cl$d_var_inv,
  g.d = d_cl$d_gc,
  g.d.var = d_cl$d_var_gc,
  ig.d = d_cl$d_inv_gc,
  ig.d.var = d_cl$d_var_inv_gc,
  n = nrow(d_cl),
  study.id = as.numeric(as.factor(d_cl$studyID)),
  n.study = length(unique(d_cl$studyID)),
  gc.drought = ifelse(d_cl$gc_factor=="drought", 1, 0),
  gc.nitrogen = ifelse(d_cl$gc_factor=="nitrogen", 1, 0),
  gc.CO2 = ifelse(d_cl$gc_factor=="CO2", 1, 0),
  gc.O2 = ifelse(d_cl$gc_factor=="O2", 1, 0),
  gc.pH = ifelse(d_cl$gc_factor=="pH", 1, 0),
  mech.pred = ifelse(d_cl$mechanism_broad =="predation", 1, 0),
  mech.chem = ifelse(d_cl$mechanism_broad =="chemical/physical", 1, 0),
  sett.fresh = ifelse(d_cl$setting =="freshwater", 1, 0),
  sett.marine = ifelse(d_cl$setting =="marine", 1, 0),
  exp.field = ifelse(d_cl$study_type =="field", 1, 0),
  exp.meso = ifelse(d_cl$study_type =="mesocosm", 1, 0),
  abun = ifelse(d_cl$response_class=="abundance", 1, 0),
  allo = ifelse(d_cl$response_class =="allocation", 1, 0),
  div = ifelse(d_cl$response_class =="diversity", 1, 0),
  beh = ifelse(d_cl$response_class =="behavior", 1, 0),
  nutr = ifelse(d_cl$response_class =="nutrient", 1, 0),
  physi = ifelse(d_cl$response_class=="physiology", 1, 0),
  repr = ifelse(d_cl$response_class=="reproduction", 1, 0),
  size = ifelse(d_cl$response_class=="size", 1, 0),
  surv = ifelse(d_cl$response_class=="survival", 1, 0)
)

sink("all_hier_model.R") # This is the file name for the jags code
cat(" 
model{
# priors
i.sd ~ dunif(0, 100) 
i.tau <- 1/i.sd^2
i.b0 ~ dnorm(0, 1/10000) # use taus, not sigmas
i.im.pred ~ dnorm(0, 1/10000)
i.im.chem ~ dnorm(0, 1/10000)
i.gc.drought ~ dnorm(0, 1/10000)
i.gc.nitrogen ~ dnorm(0, 1/10000)
i.gc.CO2 ~ dnorm(0, 1/10000)
i.gc.O2 ~ dnorm(0, 1/10000)
i.gc.pH ~ dnorm(0, 1/10000)
i.es.fresh ~ dnorm(0, 1/10000)
i.es.marine ~ dnorm(0, 1/10000)
i.ex.field  ~ dnorm(0, 1/10000)
i.ex.meso  ~ dnorm(0, 1/10000)
i.rc.abun ~ dnorm(0, 1/10000)
i.rc.allo ~ dnorm(0, 1/10000)
i.rc.div ~ dnorm(0, 1/10000)
i.rc.beh ~ dnorm(0, 1/10000)
i.rc.nutr ~ dnorm(0, 1/10000)
i.rc.physi ~ dnorm(0, 1/10000)
i.rc.repr ~ dnorm(0, 1/10000)
i.rc.size ~ dnorm(0, 1/10000)
i.rc.surv ~ dnorm(0, 1/10000)
i.sd.study ~ dunif(0, 100)
i.tau.study <-1/i.sd.study^2
g.sd ~ dunif(0, 100) 
g.tau <- 1/g.sd^2
g.b0 ~ dnorm(0, 1/10000) # use taus, not sigmas
g.im.pred ~ dnorm(0, 1/10000)
g.im.chem ~ dnorm(0, 1/10000)
g.gc.drought ~ dnorm(0, 1/10000)
g.gc.nitrogen ~ dnorm(0, 1/10000)
g.gc.CO2 ~ dnorm(0, 1/10000)
g.gc.O2 ~ dnorm(0, 1/10000)
g.gc.pH ~ dnorm(0, 1/10000)
g.es.fresh ~ dnorm(0, 1/10000)
g.es.marine ~ dnorm(0, 1/10000)
g.ex.field  ~ dnorm(0, 1/10000)
g.ex.meso  ~ dnorm(0, 1/10000)
g.rc.abun ~ dnorm(0, 1/10000)
g.rc.allo ~ dnorm(0, 1/10000)
g.rc.div ~ dnorm(0, 1/10000)
g.rc.beh ~ dnorm(0, 1/10000)
g.rc.nutr ~ dnorm(0, 1/10000)
g.rc.physi ~ dnorm(0, 1/10000)
g.rc.repr ~ dnorm(0, 1/10000)
g.rc.size ~ dnorm(0, 1/10000)
g.rc.surv ~ dnorm(0, 1/10000)
g.sd.study ~ dunif(0, 100)
g.tau.study <-1/g.sd.study^2
ig.sd ~ dunif(0, 100) 
ig.tau <- 1/ig.sd^2
ig.b0 ~ dnorm(0, 1/10000) # use taus, not sigmas
ig.im.pred ~ dnorm(0, 1/10000)
ig.im.chem ~ dnorm(0, 1/10000)
ig.gc.drought ~ dnorm(0, 1/10000)
ig.gc.nitrogen ~ dnorm(0, 1/10000)
ig.gc.CO2 ~ dnorm(0, 1/10000)
ig.gc.O2 ~ dnorm(0, 1/10000)
ig.gc.pH ~ dnorm(0, 1/10000)
ig.es.fresh ~ dnorm(0, 1/10000)
ig.es.marine ~ dnorm(0, 1/10000)
ig.ex.field  ~ dnorm(0, 1/10000)
ig.ex.meso  ~ dnorm(0, 1/10000)
ig.rc.abun ~ dnorm(0, 1/10000)
ig.rc.allo ~ dnorm(0, 1/10000)
ig.rc.div ~ dnorm(0, 1/10000)
ig.rc.beh ~ dnorm(0, 1/10000)
ig.rc.nutr ~ dnorm(0, 1/10000)
ig.rc.physi ~ dnorm(0, 1/10000)
ig.rc.repr ~ dnorm(0, 1/10000)
ig.rc.size ~ dnorm(0, 1/10000)
ig.rc.surv ~ dnorm(0, 1/10000)
ig.sd.study ~ dunif(0, 100)
ig.tau.study <-1/ig.sd.study^2


# random effect variance
## study
for (k in 1:n.study){
i.study.eff[k] ~ dnorm(0, i.tau.study)
g.study.eff[k] ~ dnorm(0, g.tau.study)
ig.study.eff[k] ~ dnorm(0, ig.tau.study)
}

# likelihood
for (i in 1:n){
i.d[i] ~ dnorm(i.y[i], i.d.tau[i]) # observed d and variance
i.y[i] ~ dnorm(i.mu[i], i.tau)
i.mu[i] <- i.b0 + i.im.pred*mech.pred[i] + i.im.chem*mech.chem[i]  + i.gc.drought*gc.drought[i] + i.gc.nitrogen*gc.nitrogen[i] + i.gc.CO2*gc.CO2[i] + i.gc.O2*gc.O2[i] + i.gc.pH*gc.pH[i]  + i.es.fresh*sett.fresh[i] + i.es.marine*sett.marine[i] + i.ex.field*exp.field[i] +i.ex.meso*exp.meso[i]+
  i.rc.abun*abun[i] + i.rc.allo*allo[i] + i.rc.div*div[i] + i.rc.beh*beh[i] + 
  i.rc.nutr*nutr[i] + i.rc.physi*physi[i] + i.rc.repr*repr[i] + i.rc.size*size[i] + i.rc.surv*surv[i] + 
  i.study.eff[study.id[i]] 
i.d.tau[i] <- 1/(i.d.var[i]*i.d.var[i])

g.d[i] ~ dnorm(g.y[i], g.d.tau[i]) # observed d and variance
g.y[i] ~ dnorm(g.mu[i], g.tau)
g.mu[i] <- g.b0 + g.im.pred*mech.pred[i] + g.im.chem*mech.chem[i]  + g.gc.drought*gc.drought[i] + g.gc.nitrogen*gc.nitrogen[i] + g.gc.CO2*gc.CO2[i] + g.gc.O2*gc.O2[i] + g.gc.pH*gc.pH[i]  + g.es.fresh*sett.fresh[i] + g.es.marine*sett.marine[i] + g.ex.field*exp.field[i] +g.ex.meso*exp.meso[i]+
  g.rc.abun*abun[i] + g.rc.allo*allo[i] + g.rc.div*div[i] + g.rc.beh*beh[i] + 
  g.rc.nutr*nutr[i] + g.rc.physi*physi[i] + g.rc.repr*repr[i] + g.rc.size*size[i] + g.rc.surv*surv[i] + 
  g.study.eff[study.id[i]] 
g.d.tau[i] <- 1/(g.d.var[i]*g.d.var[i])

ig.d[i] ~ dnorm(ig.y[i], ig.d.tau[i]) # observed d and variance
ig.y[i] ~ dnorm(ig.mu[i], ig.tau)
ig.mu[i] <- ig.b0 + ig.im.pred*mech.pred[i] + ig.im.chem*mech.chem[i]  + ig.gc.drought*gc.drought[i] + ig.gc.nitrogen*gc.nitrogen[i] + ig.gc.CO2*gc.CO2[i] + ig.gc.O2*gc.O2[i] + ig.gc.pH*gc.pH[i]  + ig.es.fresh*sett.fresh[i] + ig.es.marine*sett.marine[i] + ig.ex.field*exp.field[i] +ig.ex.meso*exp.meso[i]+
  ig.rc.abun*abun[i] + ig.rc.allo*allo[i] + ig.rc.div*div[i] + ig.rc.beh*beh[i] + 
  ig.rc.nutr*nutr[i] + ig.rc.physi*physi[i] + ig.rc.repr*repr[i] + ig.rc.size*size[i] + ig.rc.surv*surv[i] + 
  ig.study.eff[study.id[i]] 
ig.d.tau[i] <- 1/(ig.d.var[i]*ig.d.var[i])


# simulated data (for checks)
i.y.sim[i] ~ dnorm(i.mu[i], i.tau)
# residuals
i.residual[i]<-(i.y[i]-i.mu[i])
# sums of squares differences
i.sqdif.data[i]<-(i.y[i]-i.mu[i])^2 
i.sqdif.sim[i]<-(i.y.sim[i]-i.mu[i])^2

g.y.sim[i] ~ dnorm(g.mu[i], g.tau)
# residuals
g.residual[i]<-(g.y[i]-g.mu[i])
# sums of squares differences
g.sqdif.data[i]<-(g.y[i]-g.mu[i])^2 
g.sqdif.sim[i]<-(g.y.sim[i]-g.mu[i])^2

ig.y.sim[i] ~ dnorm(ig.mu[i], ig.tau)
# residuals
ig.residual[i]<-(ig.y[i]-ig.mu[i])
# sums of squares differences
ig.sqdif.data[i]<-(ig.y[i]-ig.mu[i])^2 
ig.sqdif.sim[i]<-(ig.y.sim[i]-ig.mu[i])^2
}

i.sumsq.data<-sum(i.sqdif.data[ ])
i.sumsq.sim<-sum(i.sqdif.sim[ ])
i.p.sumsq<-step(i.sumsq.sim - i.sumsq.data) # discrepancy
## mean
i.mean.data <- mean(i.y[ ]) # these are empty cause it's the whole vector
i.mean.sim <- mean(i.y.sim[ ])
i.p.mean <- step(i.mean.sim - i.mean.data)

g.sumsq.data<-sum(g.sqdif.data[ ])
g.sumsq.sim<-sum(g.sqdif.sim[ ])
g.p.sumsq<-step(g.sumsq.sim - g.sumsq.data) # discrepancy
## mean
g.mean.data <- mean(g.y[ ]) # these are empty cause it's the whole vector
g.mean.sim <- mean(g.y.sim[ ])
g.p.mean <- step(g.mean.sim - g.mean.data)

ig.sumsq.data<-sum(ig.sqdif.data[ ])
ig.sumsq.sim<-sum(ig.sqdif.sim[ ])
ig.p.sumsq<-step(ig.sumsq.sim - ig.sumsq.data) # discrepancy
## mean
ig.mean.data <- mean(ig.y[ ]) # these are empty cause it's the whole vector
ig.mean.sim <- mean(ig.y.sim[ ])
ig.p.mean <- step(ig.mean.sim - ig.mean.data)

}

",fill = TRUE)
sink()


# inits and run times
inits<-list(list(i.b0 = -1.2,i.sd.study = 2,i.sd = 1,i.im.pred = 3,i.im.chem = 2.4,i.gc.drought = -4.5,i.gc.nitrogen = -0.2,i.gc.CO2 = 4.2,i.gc.O2 = -4.6,i.gc.pH = -2.1,i.es.fresh = 0,i.es.marine = 1.1,i.ex.field = -2.4,i.ex.meso = -0.8,i.rc.abun = -1.3,i.rc.allo = 4.4,i.rc.div = -3.8,i.rc.beh = -4.3,i.rc.gro = 4.6,i.rc.nutr = -0.6,i.rc.physi = -1.3,i.rc.repr = -3.3,i.rc.size = -4.5,i.rc.surv = 1.6,g.b0 = 0.8,g.sd.study = 4,g.sd = 1,g.im.pred = -4.4,g.im.chem = -3.4,g.gc.drought = -0.2,g.gc.nitrogen = -5,g.gc.CO2 = -0.6,g.gc.O2 = -2.4,g.gc.pH = 4.4,g.es.fresh = 2.2,g.es.marine = -3.4,g.ex.field = -0.2,g.ex.meso = 1.9,g.rc.abun = -0.4,g.rc.allo = 4.6,g.rc.div = 2.1,g.rc.beh = -1,g.rc.gro = -3.8,g.rc.nutr = -2.6,g.rc.physi = 3.6,g.rc.repr = -0.6,g.rc.size = 0,g.rc.surv = 1.9,ig.b0 = 2.6,ig.sd.study = 3.4,ig.sd = 3.5,ig.im.pred = 4.5,ig.im.chem = 0.9,ig.gc.drought = 0,ig.gc.nitrogen = -3.1,ig.gc.CO2 = -5,ig.gc.O2 = 3.8,ig.gc.pH = -3.7,ig.es.fresh = -4.8,ig.es.marine = 4.4,ig.ex.field = -2.1,ig.ex.meso = -3.4,ig.rc.abun = -1,ig.rc.allo = -0.4,ig.rc.div = -0.7,ig.rc.beh = 0.2,ig.rc.gro = 3.5,ig.rc.nutr = -4.4,ig.rc.physi = 0.5,ig.rc.repr = 1.9,ig.rc.size = 1.6,ig.rc.surv = 1.6),
            list(i.b0 = -0.3,i.sd.study = 3,i.sd = 5,i.im.pred = 3.5,i.im.chem = 2.6,i.gc.drought = 0.3,i.gc.nitrogen = 3.7,i.gc.CO2 = -0.3,i.gc.O2 = -4.9,i.gc.pH = 2.3,i.es.fresh = 2.2,i.es.marine = -3.1,i.ex.field = 1.5,i.ex.meso = 0.4,i.rc.abun = -1.6,i.rc.allo = 1.4,i.rc.div = 3.3,i.rc.beh = 2.1,i.rc.gro = -1.5,i.rc.nutr = -3.7,i.rc.physi = -1.1,i.rc.repr = 4.3,i.rc.size = 3,i.rc.surv = 2.6,g.b0 = 4.6,g.sd.study = 4.9,g.sd = 1.1,g.im.pred = -4.7,g.im.chem = -1.6,g.gc.drought = -2.2,g.gc.nitrogen = -3.8,g.gc.CO2 = -4.6,g.gc.O2 = -1.3,g.gc.pH = -1.6,g.es.fresh = -3.3,g.es.marine = 1.2,g.ex.field = -1,g.ex.meso = 4.6,g.rc.abun = 1.5,g.rc.allo = -1.7,g.rc.div = -3,g.rc.beh = -3.8,g.rc.gro = 5,g.rc.nutr = -1.2,g.rc.physi = 0.6,g.rc.repr = 2.3,g.rc.size = 3.7,g.rc.surv = 0.7,ig.b0 = -4.9,ig.sd.study = 4.1,ig.sd = 2.7,ig.im.pred = -1.2,ig.im.chem = -4.1,ig.gc.drought = -4.5,ig.gc.nitrogen = 3.2,ig.gc.CO2 = 3.3,ig.gc.O2 = 1.5,ig.gc.pH = -3.7,ig.es.fresh = -1.6,ig.es.marine = 2.3,ig.ex.field = 4.1,ig.ex.meso = 2,ig.rc.abun = -2.6,ig.rc.allo = 1.4,ig.rc.div = -2.2,ig.rc.beh = 4.6,ig.rc.gro = -3.4,ig.rc.nutr = -0.8,ig.rc.physi = -2.5,ig.rc.repr = -4.1,ig.rc.size = 3.3,ig.rc.surv = 0.3),
            list(i.b0 = 1.7,i.sd.study = 4,i.sd = 3.4,i.im.pred = 2.4,i.im.chem = -1.5,i.gc.drought = 4.5,i.gc.nitrogen = 1.5,i.gc.CO2 = -4.6,i.gc.O2 = 1,i.gc.pH = -0.8,i.es.fresh = -4.2,i.es.marine = 0.3,i.ex.field = 4.6,i.ex.meso = 2.1,i.rc.abun = 0.5,i.rc.allo = -2.6,i.rc.div = 2.8,i.rc.beh = 1.5,i.rc.gro = 3.3,i.rc.nutr = 1.5,i.rc.physi = -0.2,i.rc.repr = 0,i.rc.size = -1.2,i.rc.surv = -0.5,g.b0 = 3.1,g.sd.study = 3,g.sd = 3.5,g.im.pred = 2.5,g.im.chem = 4.8,g.gc.drought = 4.7,g.gc.nitrogen = -1.5,g.gc.CO2 = -1.1,g.gc.O2 = 4.5,g.gc.pH = -3.9,g.es.fresh = 4.3,g.es.marine = -1.5,g.ex.field = 0.3,g.ex.meso = 0.4,g.rc.abun = 2.1,g.rc.allo = -0.9,g.rc.div = -3.5,g.rc.beh = -1.6,g.rc.gro = 1.3,g.rc.nutr = -4.4,g.rc.physi = 3.5,g.rc.repr = -2.9,g.rc.size = 0.4,g.rc.surv = -3.6,ig.b0 = -1.8,ig.sd.study = 1.2,ig.sd = 2.4,ig.im.pred = 1.3,ig.im.chem = -0.1,ig.gc.drought = 4.4,ig.gc.nitrogen = 3.6,ig.gc.CO2 = -1.3,ig.gc.O2 = -1.9,ig.gc.pH = 3.3,ig.es.fresh = -0.5,ig.es.marine = -1.8,ig.ex.field = -4,ig.ex.meso = -4.4,ig.rc.abun = 1.9,ig.rc.allo = 1.7,ig.rc.div = 4,ig.rc.beh = -2,ig.rc.gro = 4.3,ig.rc.nutr = -3,ig.rc.physi = 2.9,ig.rc.repr = -2.8,ig.rc.size = -4.7,ig.rc.surv = 3.6))


n.adapt = 30000 # for choosing the sampler and mixing 
n.update = 1000 # burn-in
n.iter = 50000 # keep  

set.seed(1)
jm_main = jags.model("all_hier_model.R", data = data, inits = inits,
                     n.chains = length(inits), n.adapt = n.adapt)
update(jm_main , n.iter = n.update)

cj_main = coda.samples(jm_main, variable.names=c("i.b0", "i.sd.study", "i.sd",
                                                 "i.im.pred", "i.im.chem", 
                                                 "i.gc.drought", "i.gc.nitrogen",  
                                                 "i.gc.CO2" , "i.gc.O2", "i.gc.pH",
                                                 "i.es.fresh", "i.es.marine", 
                                                 "i.ex.field", "i.ex.meso", 
                                                 "i.rc.abun","i.rc.allo","i.rc.div", "i.rc.beh",#"i.rc.gro",
                                                 "i.rc.nutr","i.rc.physi","i.rc.repr","i.rc.size","i.rc.surv",
                                                 "g.b0", "g.sd.study", "g.sd",
                                                 "g.im.pred", "g.im.chem", 
                                                 "g.gc.drought", "g.gc.nitrogen",  
                                                 "g.gc.CO2" , "g.gc.O2", "g.gc.pH",
                                                 "g.es.fresh", "g.es.marine", 
                                                 "g.ex.field", "g.ex.meso", 
                                                 "g.rc.abun","g.rc.allo","g.rc.div", "g.rc.beh",#"g.rc.gro",
                                                 "g.rc.nutr","g.rc.physi","g.rc.repr","g.rc.size","g.rc.surv",
                                                 "ig.b0", "ig.sd.study", "ig.sd",
                                                 "ig.im.pred", "ig.im.chem", 
                                                 "ig.gc.drought", "ig.gc.nitrogen",  
                                                 "ig.gc.CO2" , "ig.gc.O2", "ig.gc.pH",
                                                 "ig.es.fresh", "ig.es.marine", 
                                                 "ig.ex.field", "ig.ex.meso", 
                                                 "ig.rc.abun","ig.rc.allo","ig.rc.div", "ig.rc.beh",#"ig.rc.gro",
                                                 "ig.rc.nutr","ig.rc.physi","ig.rc.repr","ig.rc.size","ig.rc.surv"),
                       n.iter = n.iter, thin = 1)
#plot(cj_main) 
gelman.diag(cj_main) # some 1.01s here

# checking the fit
#zj_main = jags.samples(jm_main, variable.names=c("i.p.sumsq", "i.p.mean", "i.residual", "i.mu",
#                                                 "g.p.sumsq", "g.p.mean", "g.residual", "g.mu",
#                                                 "ig.p.sumsq", "ig.p.mean", "ig.residual", "ig.mu"), # "sumsq.data", "sumsq.sim", # "psuedoR2.num"
#                       n.iter = n.iter, thin = 1)
#zj_main$i.p.sumsq # 0.509 
#zj_main$i.p.mean # 0.500 
##i.sumsq.data<-zj_main$i.sumsq.data[,,1] 
##i.sumsq.sim<-zj_main$i.sumsq.sim[,,1]
##plot(i.sumsq.sim~i.sumsq.data) 
# Bayesian R2
#var(zj_main$i.mu)/(var(zj_main$i.mu)+var(zj_main$i.residual)) 
# 0.36
#zj_main$g.p.sumsq # 0.509 
#zj_main$g.p.mean # 0.503
##g.sumsq.data<-zj_main$g.sumsq.data[,,1] 
##g.sumsq.sim<-zj_main$g.sumsq.sim[,,1]
##plot(g.sumsq.sim~g.sumsq.data) 
# Bayesian R2
#var(zj_main$g.mu)/(var(zj_main$g.mu)+var(zj_main$g.residual)) 
# 0.27

#zj_main$ig.p.sumsq # 0.511
#zj_main$ig.p.mean # 0.500 
##ig.sumsq.data<-zj_main$ig.sumsq.data[,,1] 
##ig.sumsq.sim<-zj_main$ig.sumsq.sim[,,1]
##plot(ig.sumsq.sim~ig.sumsq.data) 
# Bayesian R2
#var(zj_main$ig.mu)/(var(zj_main$ig.mu)+var(zj_main$ig.residual)) 
# 0.33
#rm(zj_main)

# model output
out_main<-data.frame(rbind(cj_main[[1]], cj_main[[2]], cj_main[[3]])) %>%
  select(-g.sd, -i.sd, -ig.sd, -i.sd.study, -g.sd.study, -ig.sd.study)
names(out_main)
out_main_plot<-data.frame(mean = apply(out_main, 2, mean), 
                          lower = apply(out_main, 2, function(x) quantile(x, probs = c(.025))),
                          upper = apply(out_main, 2, function(y) quantile(y, probs = c(.975))),
                          beta = factor(rep(c("intercept", "freshwater", "marine",
                                              "field", "mesocosm",
                                              "CO2", "O2", "drought","nitrogen", "pH", 
                                              "chemical/physical", "predation",
                                              "abundance", "allocation","behavior", "diversity", 
                                              #"growth", 
                                              "nutrients", "physiology", "reproduction", 
                                              "size", "survival"),3),
                                        levels = c("intercept", "freshwater", "marine",
                                                   "field", "mesocosm",
                                                   "CO2", "O2", "drought","nitrogen", "pH", 
                                                   "chemical/physical", "predation",
                                                   "abundance", "allocation","behavior", "diversity", 
                                                   #"growth", 
                                                   "nutrients", "physiology", "reproduction", 
                                                   "size", "survival")),
                          treatment = rep(c("GEC","INV","INV&GEC"), each = 21))
out_main_plot$color<-ifelse(out_main_plot$lower<0&out_main_plot$upper>0, "notsig", "sig")
out_main_plot<-out_main_plot%>%gather(end, value, 1:3)
out_main_plot$treatment<- factor(out_main_plot$treatment, levels = c("GEC","INV","INV&GEC"))

ggplot(data = out_main_plot[!out_main_plot$end=='mean',], aes(x=value, y=beta)) +
  geom_line(stat = "identity", position = "identity", size = 4, aes(colour = color)) +
  scale_color_manual(values = c("grey60", "black"), guide = "none")+
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(data =out_main_plot[out_main_plot$end=='mean',], aes(x=value, y=beta), 
             size = 3, colour = "white") +
  facet_grid(treatment~.)+
  theme_bw() +
  labs(x = "Mean effect size (Hedges'd) +/- 95% credible interval") +
  theme(axis.text.x = element_text(size = 13, colour = "black"), axis.title.x = element_text(size=15, vjust = 0.5), 
        axis.text.y = element_text(size = 13, colour = "black"), axis.title.y=element_blank(),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))  


# 10a. Fisher's tests ----
# ... gc factor ----
print(type_gc <- table(d_cl$int_cat_dir, d_cl$gc_factor))
fisher.test(type_gc, simulate.p.value = TRUE) # p-value =  0.02549
# plot
type_gc_plot<-data.frame(table(d_cl$int_cat_dir, d_cl$gc_factor)) %>%
  group_by(Var2) %>% mutate(sum = sum(Freq), prop = Freq/sum) %>% data.frame()  %>% 
  select(int_cat_dir=Var1, gc_factor=Var2, prop=prop)
type_gc_plot$int_cat_dir<-factor(type_gc_plot$int_cat_dir, levels = c("Synergistic +","Antagonistic +", "Additive", "Antagonistic -","Synergistic -"))
ggplot(data = type_gc_plot, aes(x = gc_factor, y = prop, fill = int_cat_dir)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual("Interaction\nCategory", values = c("darkred", "lightcoral", "grey", "lightblue","darkblue")) +
  theme_bw() +
  xlab("Global environmental change factor")+
  ylab("Proportion")+
  #scale_x_discrete(labels = c("Drought", "Nitrogen", "Temperature"))+
  theme(axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10))

# with just the 3 most common
d_cl_gc<-d_cl[d_cl$gc_factor%in%c('temperature','drought','nitrogen'),]
print(type_gc2 <- table(d_cl_gc$int_cat_dir, d_cl_gc$gc_factor))
fisher.test(type_gc2, simulate.p.value = TRUE) # p-value =  0.004498
# plot
type_gc_plot2<-data.frame(table(d_cl_gc$int_cat_dir, d_cl_gc$gc_factor)) %>%
  group_by(Var2) %>% mutate(sum = sum(Freq), prop = Freq/sum) %>% data.frame() %>% select(int_cat_dir=Var1, gc_factor=Var2, prop=prop)
type_gc_plot2$int_cat_dir<-factor(type_gc_plot2$int_cat_dir, levels = c("Synergistic +","Antagonistic +", "Additive", "Antagonistic -","Synergistic -"))
ggplot(data = type_gc_plot2, aes(x = gc_factor, y = prop, fill = int_cat_dir)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual("Interaction\nCategory", values = c("darkred", "lightcoral", "grey", "lightblue","darkblue")) +
  theme_bw() +
  xlab("Global environmental change factor")+
  ylab("Proportion")+
  #scale_x_discrete(labels = c("Drought", "Nitrogen", "Temperature"))+
  theme(axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10))


# ...inv mechanism----
print(type_mech <- table(d_cl$int_cat_dir, d_cl$mechanism_broad))
fisher.test(type_mech, simulate.p.value = TRUE) # p-value = 0.6607
type_mech_plot<-data.frame(table(d_cl$int_cat_dir, d_cl$mechanism_broad)) %>%
  group_by(Var2) %>% mutate(sum = sum(Freq), prop = Freq/sum) %>% data.frame() %>% select(int_cat_dir=Var1, mechanism_broad=Var2, prop=prop)
type_mech_plot$int_cat_dir<-factor(type_mech_plot$int_cat_dir, levels = c("Synergistic +","Antagonistic +", "Additive", "Antagonistic -","Synergistic -"))
ggplot(data = type_mech_plot, aes(x = mechanism_broad, y = prop, fill = int_cat_dir)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual("Interaction\nCategory", values = c("darkred", "lightcoral", "grey", "lightblue","darkblue")) +
  theme_bw() +
  xlab("Invasion mechanism")+
  ylab("Proportion")+
  theme(axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10))


# ...response class----
print(type_rc <- table(d_cl$int_cat_dir, d_cl$response_class))
fisher.test(type_rc, simulate.p.value = TRUE) # p-value =  0.0004998
table(d_cl$response_class)
tapply(d_cl$studyID, d_cl$response_class, function(x) length(unique(x))) 
# plot
type_rc_plot<-data.frame(table(d_cl$int_cat_dir, d_cl$response_class)) %>%
  group_by(Var2) %>% mutate(sum = sum(Freq), prop = Freq/sum) %>% data.frame() %>% 
  select(int_cat_dir=Var1, response_class=Var2, prop=prop)%>%
  filter(!response_class%in%c('behavior','reproduction'))
type_rc_plot$int_cat_dir<-factor(type_rc_plot$int_cat_dir, levels = c("Synergistic +","Antagonistic +", "Additive", "Antagonistic -","Synergistic -"))
ggplot(data = type_rc_plot, aes(x = response_class, y = prop, fill = int_cat_dir)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual("Interaction\nCategory", values = c("darkred", "lightcoral", "grey", "lightblue","darkblue")) +
  theme_bw() +
  xlab("Response type")+
  ylab("Proportion")+
  theme(axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10))


# ...experiment type----
print(type_exp <- table(d_cl$int_cat_dir, d_cl$study_type))
fisher.test(type_exp, simulate.p.value = TRUE) # p-value =  0.1544
# plot
type_exp_plot<-data.frame(table(d_cl$int_cat_dir, d_cl$study_type)) %>%
  group_by(Var2) %>% mutate(sum = sum(Freq), prop = Freq/sum) %>% data.frame() %>% select(int_cat_dir=Var1, exp=Var2, prop=prop)
type_exp_plot$int_cat_dir<-factor(type_exp_plot$int_cat_dir, levels = c("Synergistic +","Antagonistic +", "Additive", "Antagonistic -","Synergistic -"))
ggplot(data = type_exp_plot, aes(x = exp, y = prop, fill = int_cat_dir)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual("Interaction\nCategory", values = c("darkred", "lightcoral", "grey", "lightblue","darkblue")) +
  theme_bw() +
  xlab("Experiment type")+
  ylab("Proportion")+
  theme(axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10))


# ...broad ecosystem/setting----
print(type_sett <- table(d_cl$int_cat_dir, d_cl$setting))
fisher.test(type_sett, simulate.p.value = TRUE) # p-value =  0.2769
# plot
type_sett_plot<-data.frame(table(d_cl$int_cat_dir, d_cl$setting)) %>%
  group_by(Var2) %>% mutate(sum = sum(Freq), prop = Freq/sum) %>% data.frame() %>% select(int_cat_dir=Var1, setting=Var2, prop=prop)
type_sett_plot$int_cat_dir<-factor(type_sett_plot$int_cat_dir, levels = c("Synergistic +","Antagonistic +", "Additive", "Antagonistic -","Synergistic -"))
ggplot(data = type_sett_plot, aes(x = setting, y = prop, fill = int_cat_dir)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual("Interaction\nCategory", values = c("darkred", "lightcoral", "grey", "lightblue","darkblue")) +
  theme_bw() +
  xlab("Ecosystem setting")+
  ylab("Proportion")+
  scale_x_discrete(labels = c("Freshwater", "Marine", "Terrestrial"))+
  theme(axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10))

# 10b. Fisher's tests on reduced dataset (one obs per study) ----

# ... gc factor ----
print(type_gc2 <- table(d_cl_ints$int_cat_dir, d_cl_ints$gc_factor))
fisher.test(type_gc2, simulate.p.value = TRUE) # p-value =  0.981
# plot
type_gc_plot<-data.frame(table(d_cl_ints$int_cat_dir, d_cl_ints$gc_factor)) %>%
  group_by(Var2) %>% mutate(sum = sum(Freq), prop = Freq/sum) %>% data.frame()  %>% 
  select(int_cat_dir=Var1, gc_factor=Var2, prop=prop, n =Freq)
type_gc_plot$int_cat_dir<-factor(type_gc_plot$int_cat_dir, levels = c("Synergistic +","Antagonistic +", "Additive", "Antagonistic -","Synergistic -"))
ggplot(data = type_gc_plot, aes(x = gc_factor, y = prop, fill = int_cat_dir)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual("Interaction\nCategory", values = c("darkred", "lightcoral", "grey", "lightblue","darkblue")) +
  theme_bw() +
  xlab("Global environmental change factor")+
  ylab("Proportion")+
  #scale_x_discrete(labels = c("Drought", "Nitrogen", "Temperature"))+
  theme(axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10))
#### with just 3??
d_cl_ints_gc<-d_cl_ints[d_cl_ints$gc_factor%in%c('temperature','drought','nitrogen'),]
print(type_gc2 <- table(d_cl_ints_gc$int_cat_dir, d_cl_ints_gc$gc_factor))
fisher.test(type_gc2, simulate.p.value = TRUE) # p-value =  0.6707
### plot
type_gc_plot2<-data.frame(table(d_cl_ints_gc$int_cat_dir, d_cl_ints_gc$gc_factor)) %>%
  group_by(Var2) %>% mutate(sum = sum(Freq), prop = Freq/sum) %>% data.frame() %>% select(int_cat_dir=Var1, gc_factor=Var2, prop=prop)
type_gc_plot2$int_cat_dir<-factor(type_gc_plot2$int_cat_dir, levels = c("Synergistic +","Antagonistic +", "Additive", "Antagonistic -","Synergistic -"))
ggplot(data = type_gc_plot2, aes(x = gc_factor, y = prop, fill = int_cat_dir)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual("Interaction\nCategory", values = c("darkred", "lightcoral", "grey", "lightblue","darkblue")) +
  theme_bw() +
  xlab("Global environmental change factor")+
  ylab("Proportion")+
  #scale_x_discrete(labels = c("Drought", "Nitrogen", "Temperature"))+
  theme(axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10))
# looks like N has more S+ than the other 2, temp has more s+


# ...inv mechanism----
print(type_mech2 <- table(d_cl_ints$int_cat, d_cl_ints$mechanism_broad))
fisher.test(type_mech2, simulate.p.value = TRUE) # p-value = 0.3788
type_mech_plot<-data.frame(table(d_cl_ints$int_cat_dir, d_cl_ints$mechanism_broad)) %>%
  group_by(Var2) %>% mutate(sum = sum(Freq), prop = Freq/sum) %>% data.frame() %>% select(int_cat_dir=Var1, mechanism_broad=Var2, prop=prop)
type_mech_plot$int_cat_dir<-factor(type_mech_plot$int_cat_dir, levels = c("Synergistic +","Antagonistic +", "Additive", "Antagonistic -","Synergistic -"))
ggplot(data = type_mech_plot, aes(x = mechanism_broad, y = prop, fill = int_cat_dir)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual("Interaction\nCategory", values = c("darkred", "lightcoral", "grey", "lightblue","darkblue")) +
  theme_bw() +
  xlab("Invasion mechanism")+
  ylab("Proportion")+
  theme(axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10))
# predation has a lot more syn+


# ...response class----
print(type_rc2 <- table(d_cl_ints$int_cat_dir, d_cl_ints$response_class))
fisher.test(type_rc2, simulate.p.value = TRUE) # p-value =  0.4588
### plot
type_rc_plot1<-data.frame(table(d_cl_ints$int_cat, d_cl_ints$response_class)) %>%
  group_by(Var2) %>% mutate(sum = sum(Freq), prop = Freq/sum) %>% data.frame() %>% select(int_cat=Var1, rc_factor=Var2, prop=prop) %>%
  filter(!rc_factor%in%c('feeding','reproduction'))
ggplot(data = type_rc_plot1, aes(x = rc_factor, y = prop, fill = int_cat)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual("Interaction\nCategory", values = c("grey80", "grey50", "black")) +
  theme_bw() +
  xlab("Response type")+
  ylab("Proportion")+
  theme(axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10))
# diversity and carbon have most S
type_rc_plot<-data.frame(table(d_cl_ints$int_cat_dir, d_cl_ints$response_class)) %>%
  group_by(Var2) %>% mutate(sum = sum(Freq), prop = Freq/sum) %>% data.frame() %>% select(int_cat_dir=Var1, rc_factor=Var2, prop=prop)%>%
  filter(!rc_factor%in%c('feeding','reproduction'))
type_rc_plot$int_cat_dir<-factor(type_rc_plot$int_cat_dir, levels = c("Synergistic +","Antagonistic +", "Additive", "Antagonistic -","Synergistic -"))
ggplot(data = type_rc_plot, aes(x = rc_factor, y = prop, fill = int_cat_dir)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual("Interaction\nCategory", values = c("darkred", "lightcoral", "grey", "lightblue","darkblue")) +
  theme_bw() +
  xlab("Response type")+
  ylab("Proportion")+
  theme(axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10))
# this is stupid


# ...experiment type----
print(type_exp2 <- table(d_cl_ints$int_cat_dir, d_cl_ints$study_type))
fisher.test(type_exp2, simulate.p.value = TRUE) # p-value = 0.7876
# plot
type_exp_plot<-data.frame(table(d_cl_ints$int_cat_dir, d_cl_ints$study_type)) %>%
  group_by(Var2) %>% mutate(sum = sum(Freq), prop = Freq/sum) %>% data.frame() %>% select(int_cat_dir=Var1, exp=Var2, prop=prop)
type_exp_plot$int_cat_dir<-factor(type_exp_plot$int_cat_dir, levels = c("Synergistic +","Antagonistic +", "Additive", "Antagonistic -","Synergistic -"))
ggplot(data = type_exp_plot, aes(x = exp, y = prop, fill = int_cat_dir)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual("Interaction\nCategory", values = c("darkred", "lightcoral", "grey", "lightblue","darkblue")) +
  theme_bw() +
  xlab("Experiment type")+
  ylab("Proportion")+
  theme(axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10))
# so actually these are different. more s- in field and meso, more s+ with lab/greenhouse (interesting)


# ...broad ecosystem/setting----
# looks like more Synergistic in marine
print(type_sett2 <- table(d_cl_ints$int_cat_dir, d_cl_ints$setting))
fisher.test(type_sett2, simulate.p.value = TRUE) # p-value = 0.8466
# plot
type_sett_plot<-data.frame(table(d_cl_ints$int_cat_dir, d_cl_ints$setting)) %>%
  group_by(Var2) %>% mutate(sum = sum(Freq), prop = Freq/sum) %>% data.frame() %>% select(int_cat_dir=Var1, setting=Var2, prop=prop)
type_sett_plot$int_cat_dir<-factor(type_sett_plot$int_cat_dir, levels = c("Synergistic +","Antagonistic +", "Additive", "Antagonistic -","Synergistic -"))
ggplot(data = type_sett_plot, aes(x = setting, y = prop, fill = int_cat_dir)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual("Interaction\nCategory", values = c("darkred", "lightcoral", "grey", "lightblue","darkblue")) +
  theme_bw() +
  xlab("Ecosystem setting")+
  ylab("Proportion")+
  scale_x_discrete(labels = c("Freshwater", "Marine", "Terrestrial"))+
  theme(axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10))
# more syn- in the field


# 11. Figures ----
# Figure 1: Conceptual model ----

fake_data<-data.frame(case = rep(c('--', "-+"), each = 4),
                       effect = factor(rep(c('INV', 'GEC', 'Predicted\nAdditive\nINV&GEC',  'Observed\nINV&GEC'), times = 2), 
                                       levels = c('INV', 'GEC','Predicted\nAdditive\nINV&GEC',  'Observed\nINV&GEC')),
                       type = factor(rep(c('INV', 'GEC', 'PredAdd', ''), 2),
                                     levels = c('INV', 'GEC', 'PredAdd',  '')),
                       val = c(-2, -4, -6, NA,
                               -1, 4, 3,  NA))


conc.a<-
  ggplot(data = fake_data[fake_data$case=="--",], 
         aes(x = effect, y = val)) + 
  geom_hline(yintercept = 0)+
  ylim(-7.5, 2)+
  geom_rect(aes(xmin = 'Predicted\nAdditive\nINV&GEC', xmax = Inf, ymin = 0, ymax = Inf), # syn+
            fill = "#016699") +
  geom_rect(aes(xmin = 'Predicted\nAdditive\nINV&GEC', xmax = Inf, ymin = 0, ymax = -5.5), # an+
            fill = "#67A3C2") +
  geom_rect(aes(xmin = 'Predicted\nAdditive\nINV&GEC', xmax = Inf, ymin = -5.5, ymax = -6.5), # add
            fill = "#DDDDDD") +
  geom_rect(aes(xmin = 'Predicted\nAdditive\nINV&GEC', xmax = Inf, ymin = -6.5, ymax = -Inf), # syn-
            fill = "#BE3526") +
  geom_segment(x = 3, xend = 8.6, y = -5.5, yend = -5.5, linetype = "dashed") +
  geom_segment(x = 3, xend = 8.6, y = -6.5, yend = -6.5, linetype = "dashed") +
  geom_bar(stat = "identity", position = "stack",
           aes(fill = type),
           color = "black") +
  scale_fill_manual(values = c("white", "white", "grey")) +
  #geom_vline(xintercept = 2.5) +
  #geom_vline(xintercept = 3.5)  +
  geom_label(label = "Synergistic -", x = 4, y = -7, label.size = 0, alpha = 0.001) +
  geom_label(label = "Additive", x = 4, y = -6, label.size = 0, alpha = 0.001) + 
  geom_label(label = "Antagonistic +", x = 4, y = -2.5, label.size = 0, alpha = 0.001) +
  geom_label(label = "Synergistic +", x = 4, y = 1, label.size = 0, alpha = 0.001)+
  # add error bars
  geom_segment(x = 3, xend = 3, y = -6.5, yend = -5.5, size = 0.7) +
  geom_segment(y = -5.5, yend = -5.5, x = 2.9, xend = 3.1, size = 0.7) +
  geom_segment(y = -6.5, yend = -6.5, x = 2.9, xend = 3.1, size = 0.7) +
  geom_segment(x = 1, xend = 1, y = -2.2, yend = -1.8, size = 0.7) +
  geom_segment(y = -2.2, yend = -2.2, x = 0.9, xend = 1.1, size = 0.7) +
  geom_segment(y = -1.8, yend = -1.8, x = 0.9, xend = 1.1, size = 0.7) +
  geom_segment(x = 2, xend = 2, y = -4.2, yend = -3.8, size = 0.7) +
  geom_segment(y = -4.2, yend = -4.2, x = 1.9, xend = 2.1, size = 0.7) +
  geom_segment(y = -3.8, yend = -3.8, x = 1.9, xend = 2.1, size = 0.7) +
  theme_bw() +
  ylab("Effect size (treatment - control)") +
  theme(legend.position = "none", 
        axis.title.y = element_text(size=13, vjust = 0.7),
        axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=10)) +
  xlab("") 



conc.b<-
  ggplot(data = fake_data[fake_data$case=="-+",], 
         aes(x = effect, y = val)) + 
  ylim(-3,6) +
  geom_rect(aes(xmin = 'Predicted\nAdditive\nINV&GEC', xmax = Inf, ymin = 4, ymax = Inf), # syn+
            fill = "#016699") +
  geom_rect(aes(xmin = 'Predicted\nAdditive\nINV&GEC', xmax = Inf, ymin = 3.5, ymax = 4), # an+
            fill = "#67A3C2") +
  geom_rect(aes(xmin = 'Predicted\nAdditive\nINV&GEC', xmax = Inf, ymin = 2.5, ymax = 3.5), # add
            fill = "#DDDDDD") +
  geom_rect(aes(xmin = 'Predicted\nAdditive\nINV&GEC', xmax = Inf, ymin = -1, ymax = 2.5), # an-
            fill = "#D8867D") +
  geom_rect(aes(xmin = 'Predicted\nAdditive\nINV&GEC', xmax = Inf, ymin = -1, ymax = -Inf), # syn-
            fill = "#BE3526") +
  geom_segment(x = 3, xend = 7.6, y = 3.5, yend = 3.5, linetype = "dashed") +
  geom_segment(x = 3, xend = 7.6, y = 2.5, yend = 2.5, linetype = "dashed") +
  geom_segment(x = 2.4, xend = 7.6, y = 4, yend = 4, linetype = "dashed") +
  geom_segment(x = 1.4, xend = 7.6, y = -1, yend = -1, linetype = "dashed", alpha = 1) +
  geom_bar(stat = "identity", position = "stack",
           aes(fill = type),
           color = "black") +
  scale_fill_manual(values = c("white", "white", "grey")) +
  #geom_vline(xintercept = 2.5) +
  #geom_vline(xintercept = 3.5)  +
  geom_label(label = "Synergistic -", x = 4, y = -2, label.size = 0, alpha = 0.001)+
  #annotate("text", label = "Synergistic -", x = 4, y = -2, size = 4)+  # this also works
  geom_label(label = "Additive", x = 4, y = 3, label.size = 0, alpha = 0.001) +  
  geom_label(label = "Antagonistic +", x = 4, y = 3.76, label.size = 0, alpha = 0.001) +
  geom_label(label = "Synergistic +", x = 4, y = 5, label.size = 0, alpha = 0.001)+
  geom_label(label = "Antagonistic -", x = 4, y = 1, label.size = 0, alpha = 0.001) +
  geom_label(label = "Antagonistic -", x = 4, y = -0.5, label.size = 0, alpha = 0.001) +
  geom_segment(x = 3, xend = 3, y = 2.5, yend = 3.5, size = 0.7) +
  geom_segment(y = 3.5, yend = 3.5, x = 2.9, xend = 3.1, size = 0.7) +
  geom_segment(y = 2.5, yend = 2.5, x = 2.9, xend = 3.1, size = 0.7) +
  geom_segment(x = 1, xend = 1, y = -1.2, yend = -0.8, size = 0.7) +
  geom_segment(y = -1.2, yend = -1.2, x = 0.9, xend = 1.1, size = 0.7) +
  geom_segment(y = -0.8, yend = -0.8, x = 0.9, xend = 1.1, size = 0.7) +
  geom_segment(x = 2, xend = 2, y = 4.2, yend = 3.8, size = 0.7) +
  geom_segment(y = 4.2, yend = 4.2, x = 1.9, xend = 2.1, size = 0.7) +
  geom_segment(y = 3.8, yend = 3.8, x = 1.9, xend = 2.1, size = 0.7) +geom_hline(yintercept = 0)+
  theme_bw() +
  ylab("Effect size (treatment - control)") +
  theme(legend.position = "none",
        axis.title.y = element_text(size=13, vjust = 2.5),
        axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=10),
        plot.margin = margin(0, 0, 0, 0.5, "cm")) + # top, right, bottom, left
  xlab("") 

plot_grid(conc.a, conc.b, nrow = 2, ncol = 1)


# Figure 2: Mean comparison ----
out_int<-data.frame(rbind(cj_int[[1]], cj_int[[2]], cj_int[[3]])) %>%
  select(-sd.study, -sd)
out_mean<-data.frame(Gmean=out_int$b0, Imean=out_int$b0+out_int$I, IGmean=out_int$b0+out_int$IG)
out_int_plot_mean<-data.frame(mean = apply(out_mean, 2, mean), 
                              lower = apply(out_mean, 2, function(x) quantile(x, probs = c(.025))),
                              upper = apply(out_mean, 2, function(y) quantile(y, probs = c(.975))),
                              treatment = c("GEC","INV","INV&GEC")) 
out_int_plot_mean$sig<-ifelse(out_int_plot_mean$lower<0&out_int_plot_mean$upper>0, "not", "sig")
out_int_plot_mean<-out_int_plot_mean %>% gather(end, value, c(1:3))
out_int_plot_mean$treatment<- factor(out_int_plot_mean$treatment, levels = c("GEC","INV","INV&GEC"))

fig2a<-ggplot(data = out_int_plot_mean[!out_int_plot_mean$end=='mean',], aes(x=treatment, y=value)) +
  geom_line(stat = "identity", position = "identity", size = 4, aes(color = sig)) +
  scale_color_manual(values = c("grey60", "grey20"), guide = "none")+
  #geom_label(x=3.3, y=0.15, label = "A", label.size = 0, size =10)+
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(data =out_int_plot_mean[out_int_plot_mean$end=='mean',], aes(x=treatment, y=value), 
             size = 3, colour = "white") +
  theme_bw() +
  labs(y = expression(paste("Mean effect size (Hedges' ", italic("d"), ")", sep = "")),
       x = "Treatment") +
  theme(axis.text.x = element_text(size = 11, colour = "black"), 
        axis.title.x = element_text(size=13, vjust = 0.3), 
        axis.text.y = element_text(size = 10, colour = "black"), 
        axis.title.y= element_text(size=13, vjust = 2),
        plot.margin = margin(0.5, 0.5, 0.1, 0.5, "cm"))  

int_plot<-data.frame(table(d_cl$int_cat_dir)) %>%
  mutate(int_cat_dir=Var1, Prop = Freq/sum(Freq), x = "x")
int_plot$int_cat_dir<-factor(int_plot$int_cat_dir, levels = c("Synergistic +","Antagonistic +", "Additive", "Antagonistic -","Synergistic -"))
fig2b<-ggplot(data = int_plot, aes(x =x, y = Prop, fill = int_cat_dir)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual("     INV&GEC\nInteraction Type", values = c("#BE3526", "#D8867D", "#DDDDDD", "#67A3C2","#016699")[5:1]) + # "breaks" flips order in legend
  theme_bw() +
  #geom_text(y=0.9, x=1.5, label = "B", size =10)+
  labs(y = "Proportion of cases")+
  theme(axis.title.x = element_blank(),  # this is weird
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10, colour = "black"),
        axis.title.y = element_text(size = 13, vjust = 2),
        axis.ticks.x = element_blank(),
        legend.position = "right",
        plot.margin = margin(0.5, 0.5, 1.1, 1, "cm")) # top, right, bottom, left


plot_grid(fig2a, fig2b, nrow = 1, ncol = 2, rel_widths = c(1/2, 1/2))


# Figure 3: GEC effects ----
out_gc<-data.frame(rbind(cj_gc[[1]], cj_gc[[2]], cj_gc[[3]])) %>%
  select(-sd.study, sd)
out_gc_means<-data.frame(g.temp = out_gc$b0, g.Drought = out_gc$b0+out_gc$DROU, g.Nitrogen = out_gc$b0+out_gc$NITR, 
                         i.temp = out_gc$b0+out_gc$I, i.Drought = out_gc$b0+out_gc$I+out_gc$DROU+out_gc$I.DROU, i.Nitrogen = out_gc$b0+out_gc$I+out_gc$NITR+out_gc$I.NITR, 
                         ig.temp = out_gc$b0+out_gc$IG, ig.Drought = out_gc$b0+out_gc$IG+out_gc$DROU+out_gc$IG.DROU, ig.Nitrogen = out_gc$b0+out_gc$IG+out_gc$NITR+out_gc$IG.NITR)

out_gc_plot<-data.frame(mean = apply(out_gc_means, 2, mean), 
                        lower = apply(out_gc_means, 2, function(x) quantile(x, probs = c(.025))),
                        upper = apply(out_gc_means, 2, function(y) quantile(y, probs = c(.975))),
                        name = names(out_gc_means),
                        beta = factor(rep(c("Warming", "Drought","Nitrogen"), 3),  
                                      levels = c("Drought","Nitrogen","Warming")), 
                        treatment = rep(c("GEC","INV","INV&GEC"), each = 3))
out_gc_plot$sig<-ifelse(out_gc_plot$lower<0&out_gc_plot$upper>0, "not", "sig")
out_gc_plot<-out_gc_plot %>% gather(end, value, c(1:3))
out_gc_plot$treatment<- factor(out_gc_plot$treatment, levels = c("GEC","INV","INV&GEC"))

gc_cl<-data.frame(n.cases = table(d_cl$gc_factor), n.studies = tapply(d_cl$studyID, d_cl$gc_factor, function(x) length(unique(x))))
names(gc_cl)<-c("gc_factor", "n.cases", "n.studies")

gc_labels = gc_cl%>%filter(gc_factor%in%c("drought", "nitrogen", "temperature"))
gc_labels$beta<-c("Drought","Nitrogen","Warming")
gc_labels$label<- paste("n cases = ", gc_labels$n.cases, "\nn studies = ", gc_labels$n.studies, sep ="")
gc_labels$value<-max(out_gc_plot$value)

fig3a<-ggplot(data = out_gc_plot[!out_gc_plot$end=='mean',], aes(x=treatment, y=value)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(stat = "identity", position = "identity", size = 4,  aes(color = sig)) +
  scale_color_manual(values = c("grey60", "grey20"), guide = "none")+
  geom_point(data =out_gc_plot[out_gc_plot$end=='mean',], aes(x=treatment, y=value), 
             size = 3, colour = "white") +
  geom_label(y = -6, x = 1.5, size = 3.5, label.size = 0, data = gc_labels, aes(label = label)) +
  facet_grid(.~beta)+
  theme_bw() +
  labs(y = expression(paste("Mean effect size (Hedges' ", italic("d"), ")", sep = ""))) +
  theme(axis.text.x = element_text(size = 11, colour = "black"), 
        axis.text.y = element_text(size = 10, colour = "black"), axis.title.y= element_text(size=13),
        axis.title.x = element_blank(),
        plot.margin = margin(0.5, 0.5, 0.5, 0.8, "cm")) # top, right, bottom, left

d_cl_gc<-d_cl[d_cl$gc_factor%in%c('temperature','drought','nitrogen'),]
type_gc_plot<-data.frame(table(d_cl_gc$int_cat_dir, d_cl_gc$gc_factor)) %>%
  group_by(Var2) %>% mutate(sum = sum(Freq), prop = Freq/sum) %>% data.frame() %>% select(int_cat_dir=Var1, gc_factor=Var2, prop=prop)
type_gc_plot$int_cat_dir<-factor(type_gc_plot$int_cat_dir, levels = c("Synergistic +","Antagonistic +", "Additive", "Antagonistic -","Synergistic -"))
type_gc_plot$gc_factor<-as.character(type_gc_plot$gc_factor)
type_gc_plot$gc_factor[type_gc_plot$gc_factor=="drought"]<-"Drought"
type_gc_plot$gc_factor[type_gc_plot$gc_factor=="nitrogen"]<-"Nitrogen"
type_gc_plot$gc_factor[type_gc_plot$gc_factor=="temperature"]<-"Warming"
fig3b<-
  ggplot(data = type_gc_plot, aes(x = gc_factor, y = prop, fill = int_cat_dir)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual("     INV&GEC\nInteraction Type", values = c("#BE3526", "#D8867D", "#DDDDDD", "#67A3C2","#016699"),
                    breaks=c("Synergistic +","Antagonistic +", "Additive", "Antagonistic -","Synergistic -")[5:1]) +
  theme_bw() +
  xlab("Global environmental change")+
  ylab("Proportion of cases")+
  #scale_x_discrete(labels = c("Drought", "Nitrogen", "Warming"))+
  theme(axis.text.x = element_text(size = 11, colour = "black"), axis.title.x = element_text(size=13, vjust = 0.5), 
        axis.text.y = element_text(size = 10, colour = "black"), axis.title.y= element_text(size=13, hjust = 0.5),
        plot.margin = margin(0.2, 0.5, 0.5, 0.5, "cm"))+  # top, right, bottom, left
  theme(legend.position = "bottom")

plot_grid(fig3a, fig3b, nrow = 2, ncol = 1, rel_heights = c(4/9,5/9)) 
# output at ~8.5X11"


# Figure 4: Response class effects ----
# excluding behavior and reproduction (small n's)
out_rclass<-data.frame(rbind(cj_rclass[[1]], cj_rclass[[2]], cj_rclass[[3]])) %>%
  select(-sd, -sd.study)
out_rclass_means<-data.frame(g.abun = out_rclass$b0+out_rclass$ABUN, g.allo = out_rclass$b0+out_rclass$ALLO, 
                             g.biom = out_rclass$b0, g.div = out_rclass$b0+out_rclass$DIV,#g.gro = out_rclass$b0+out_rclass$GRO,
                             g.nutr = out_rclass$b0+out_rclass$NUTR, g.PHYSI = out_rclass$b0+out_rclass$PHYSI, 
                             g.size = out_rclass$b0+out_rclass$SIZE,g.surv = out_rclass$b0+out_rclass$SURV,
                             i.abun = out_rclass$b0+out_rclass$I+out_rclass$ABUN+out_rclass$I.ABUN, i.allo = out_rclass$b0+out_rclass$I+out_rclass$ALLO+out_rclass$I.ALLO, 
                             i.biom = out_rclass$b0+out_rclass$I, i.div = out_rclass$b0+out_rclass$I+out_rclass$DIV+out_rclass$I.DIV, #i.gro = out_rclass$b0+out_rclass$I+out_rclass$GRO+out_rclass$I.GRO,
                             i.nutr = out_rclass$b0+out_rclass$I+out_rclass$NUTR+out_rclass$I.NUTR, i.PHYSI = out_rclass$b0+out_rclass$I+out_rclass$PHYSI+out_rclass$I.PHYSI, 
                             i.size = out_rclass$b0+out_rclass$I+out_rclass$SIZE+out_rclass$I.SIZE, i.surv = out_rclass$b0+out_rclass$I+out_rclass$SURV+out_rclass$I.SURV,
                             ig.abun = out_rclass$b0+out_rclass$IG+out_rclass$ABUN+out_rclass$IG.ABUN, ig.allo = out_rclass$b0+out_rclass$IG+out_rclass$ALLO+out_rclass$IG.ALLO, 
                             ig.biom = out_rclass$b0+out_rclass$IG, ig.div = out_rclass$b0+out_rclass$IG+out_rclass$DIV+out_rclass$IG.DIV, #ig.gro = out_rclass$b0+out_rclass$IG+out_rclass$GRO+out_rclass$IG.GRO,
                             ig.nutr = out_rclass$b0+out_rclass$IG+out_rclass$NUTR+out_rclass$IG.NUTR, ig.PHYSI = out_rclass$b0+out_rclass$IG+out_rclass$PHYSI+out_rclass$IG.PHYSI, 
                             ig.size = out_rclass$b0+out_rclass$IG+out_rclass$SIZE+out_rclass$IG.SIZE, ig.surv = out_rclass$b0+out_rclass$IG+out_rclass$SURV+out_rclass$IG.SURV)

out_rclass_plot<-data.frame(mean = apply(out_rclass_means, 2, mean), 
                            lower = apply(out_rclass_means, 2, function(x) quantile(x, probs = c(.025))),
                            upper = apply(out_rclass_means, 2, function(y) quantile(y, probs = c(.975))),
                            name = names(out_rclass_means),
                            beta = factor(rep(c('Abundance','Allocation','Biomass','Diversity',
                                                'Nutrient','Physiology', 'Size','Survival'), 3),
                                          levels = c('Nutrient', 'Diversity','Abundance','Allocation',
                                                     'Biomass','Size','Survival','Physiology')), 
                            treatment = rep(c("GEC","INV","INV*\nGEC"), each = nrow(rc)-2))
out_rclass_plot$sig<-ifelse(out_rclass_plot$lower<0&out_rclass_plot$upper>0, "not", "sig")
out_rclass_plot<-out_rclass_plot %>% gather(end, value, c(1:3))
out_rclass_plot$treatment<- factor(out_rclass_plot$treatment, levels = c("GEC","INV","INV*\nGEC"))

rc_cl<-data.frame(n.cases = table(d_cl$response_class), n.studies = tapply(d_cl$studyID, d_cl$response_class, function(x) length(unique(x))))
names(rc_cl)<-c("response_class", "n.cases", "n.studies")

rc_labels = rc_cl%>%filter(!response_class%in%c('behavior','reproduction'))
rc_labels$beta<-factor(c('Abundance','Allocation','Biomass','Diversity',
                  'Nutrient','Physiology', 'Size','Survival'), levels = c('Nutrient', 'Diversity','Abundance','Allocation',
                                                                          'Biomass','Size','Survival','Physiology'))
rc_labels$label<- paste("n cases = ", rc_labels$n.cases, "\nn studies = ", rc_labels$n.studies, sep ="")
rc_labels$value<-max(out_rclass_plot$value)



fig4a<-ggplot(data = out_rclass_plot[!out_rclass_plot$end=='mean',], aes(x=treatment, y=value)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(stat = "identity", position = "identity", size = 4,  aes(color = sig)) +
  scale_color_manual(values = c("grey60", "grey20"), guide = "none")+
  geom_point(data =out_rclass_plot[out_rclass_plot$end=='mean',], aes(x=treatment, y=value), 
             size = 3, colour = "white") +
  geom_label(y = -9, x = 1.65, size = 3.2, label.size = 0, data = rc_labels, aes(label = label), alpha = 0.8) +
  facet_grid(.~beta)+
  ylim(-10, 6) +
  theme_bw() +
  labs(y = expression(paste("Mean effect size (Hedges' ", italic("d"), ")", sep = ""))) +
  theme(axis.text.x = element_text(size = 9, colour = "black"), 
        axis.text.y = element_text(size = 10, colour = "black"), axis.title.y= element_text(size=13),
        axis.title.x = element_blank(),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) # top, right, bottom, left

d_cl_rc<-d_cl[!d_cl$response_class%in%c('behavior','reproduction'),]
type_rc_plot<-data.frame(table(d_cl_rc$int_cat_dir, d_cl_rc$response_class)) %>%
  group_by(Var2) %>% mutate(sum = sum(Freq), prop = Freq/sum) %>% data.frame() %>% select(int_cat_dir=Var1, rc_factor=Var2, prop=prop)
type_rc_plot$int_cat_dir<-factor(type_rc_plot$int_cat_dir, levels = c("Synergistic +","Antagonistic +", "Additive", "Antagonistic -","Synergistic -"))
type_rc_plot$response_class[type_rc_plot$rc_factor=="abundance"]<-"Abundance"
type_rc_plot$response_class[type_rc_plot$rc_factor=="allocation"]<-"Allocation"
type_rc_plot$response_class[type_rc_plot$rc_factor=="biomass"]<-"Biomass"
type_rc_plot$response_class[type_rc_plot$rc_factor=="diversity"]<-"Diversity"
type_rc_plot$response_class[type_rc_plot$rc_factor=="nutrient"]<-"Nutrient"
type_rc_plot$response_class[type_rc_plot$rc_factor=="physiology"]<-"Physiology"
type_rc_plot$response_class[type_rc_plot$rc_factor=="size"]<-"Size"
type_rc_plot$response_class[type_rc_plot$rc_factor=="survival"]<-"Survival"
type_rc_plot$response_class<-factor(type_rc_plot$response_class, levels = c('Nutrient', 'Diversity','Abundance','Allocation',
                                                                         'Biomass','Size','Survival','Physiology'))
fig4b<-
  ggplot(data = type_rc_plot, aes(x = response_class, y = prop, fill = int_cat_dir)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual("     INV&GEC\nInteraction Type", values = c("#BE3526", "#D8867D", "#DDDDDD", "#67A3C2","#016699"),
                    breaks=c("Synergistic +","Antagonistic +", "Additive", "Antagonistic -","Synergistic -")[5:1]) +
  theme_bw() +
  xlab("Response class")+
  ylab("Proportion of cases")+
  theme(axis.text.x = element_text(size = 11, colour = "black"), 
        axis.title.x = element_text(size=13, vjust = 0.5), 
        axis.text.y = element_text(size = 10, colour = "black"),
        axis.title.y= element_text(size=13, vjust = 2),
        plot.margin = margin(0.5, 0.5, 0.5, 0.6, "cm"))+
  theme(legend.position = "bottom")

plot_grid(fig4a, fig4b, nrow = 2, ncol = 1, rel_heights = c(4/9,5/9)) 

# output at 12X9.5"



# 12. Supplementary figures ----

# Fig. S2.1. Outliers ----
ggplot(data = test_plot, aes(x=treatment, y=d)) +
  geom_point(position = position_jitter(width = 0.03, height = 0),
             size = 2, alpha = 0.4, aes(color = over)) +
  scale_color_manual("Inclusion", values = c("black", "red"))+
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw()+
  labs(x = "Treatment", y = "Hedges' d")+
  theme(axis.text.x = element_text(size = 12, colour = "black"), 
        axis.text.y = element_text(size = 11, colour = "black"), 
        axis.title.y= element_text(size=14),
        axis.title.x = element_text(size=14))

# Fig S3.1. Lollipops ----

gc<-d_cl %>% group_by(gc_factor) %>% summarize(n.study = length(unique(studyID)), n.obs = n())%>%
  pivot_longer(cols = -1, names_to = "unit", values_to = "number") %>% data.frame()
gc$gc_factor[gc$gc_factor=="temperature"]<-"warming"
#gc$gc_factor[gc$gc_factor=="drought"]<-"Drought"
#gc$gc_factor[gc$gc_factor=="nitrogen"]<-"Nitrogen"
gc$gc_factor<-factor(gc$gc_factor, levels = unique(gc$gc_factor)[6:1])
figS1a<- ggplot(data = gc, aes(x = number, y = gc_factor, group = unit)) +
  geom_segment(aes(x = 0, xend = number, y = gc_factor, yend = gc_factor, size = unit)) +
  scale_size_manual(values = c(0.5,0.8), guide = "none") + 
  geom_point(aes(color = unit), size = 3) +
  geom_label(x= max(gc$number), y = 6, hjust = 0.9, 
             size = 6, label = "A", label.size = 0) +
  scale_color_manual("", values = c("#D93B0A","#016699"), labels = c("Number of observations","Number of studies"))+
  theme_bw()+
  ylab("Global environmental change") +
  xlab("N") +
  theme(axis.title.y = element_text(size = 12, vjust = 2),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "none",
        plot.margin = margin(0.2, 0.5, 0.5, 0.8, "cm"))

inv<-d_cl %>% group_by(mechanism_broad) %>% summarize(n.study = length(unique(studyID)), n.obs = n())%>%
  pivot_longer(cols = -1, names_to = "unit", values_to = "number") %>% data.frame()
inv$mechanism_broad[inv$mechanism_broad=="chemical/physical"]<-"chemical/\nphysical"
#inv$mechanism_broad[inv$mechanism_broad=="competition"]<-"Competition"
#inv$mechanism_broad[inv$mechanism_broad=="predation"]<-"Predation"
inv$mechanism_broad<-factor(inv$mechanism_broad, levels = unique(inv$mechanism_broad)[3:1])
figS1b<-ggplot(data = inv, aes(x = number, y = mechanism_broad, group = unit)) +
  geom_segment(aes(x = 0, xend = number, y = mechanism_broad, yend = mechanism_broad, size = unit)) +
  scale_size_manual(values = c(0.5,0.8), guide = "none") + 
  geom_point(aes(color = unit), size = 3) +
  scale_color_manual("", values = c("#D93B0A","#016699"), labels = c("Number of observations","Number of studies"))+
  geom_label(x=max(inv$number), y = 3.3, hjust = 0.9, 
             size = 6, label = "B", label.size = 0) +
  theme_bw()+
  ylab("Invasion mechanism") +
  xlab("N") +
  theme(axis.title.y = element_text(size = 12, vjust = 2),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "none",
        plot.margin = margin(0.2, 0.5, 0.5, 0.5, "cm"))

set<-d_cl %>% group_by(setting) %>% summarize(n.study = length(unique(studyID)), n.obs = n())%>%
  pivot_longer(cols = -1, names_to = "unit", values_to = "number") %>% data.frame()
#set$setting[set$setting=="freshwater"]<-"Freshwater"
#set$setting[set$setting=="marine"]<-"Marine"
#set$setting[set$setting=="terrestrial"]<-"Terrestrial"
set$setting<-factor(set$setting, levels = unique(set$setting)[3:1])
figS1c<- ggplot(data = set, aes(x = number, y = setting, group = unit)) +
  geom_segment(aes(x = 0, xend = number, y = setting, yend = setting, size = unit)) +
  scale_size_manual(values = c(0.5,0.8), guide = "none") + 
  geom_point(aes(color = unit), size = 3) +
  scale_color_manual("", values = c("#D93B0A","#016699"), labels = c("Number of observations","Number of studies"))+
  theme_bw()+
  geom_label(x=max(set$number), y = 3.3, hjust = 0.9, 
             size = 6, label = "C", label.size = 0) +
  ylab("Ecosystem setting") +
  xlab("N") +
  theme(axis.title.y = element_text(size = 12, vjust = 2),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "none",
        plot.margin = margin(0.2, 0.5, 0.5, 0.7, "cm"))

resp<-d_cl %>% group_by(response_class) %>% summarize(n.study = length(unique(studyID)), n.obs = n())%>%
  pivot_longer(cols = -1, names_to = "unit", values_to = "number") %>% data.frame()
#resp$response_class<-factor(rep(c('Abundance','Allocation','Behavior','Biomass','Diversity','Nutrient','Physiology', 'Reproduction','Size','Survival'), each = 2),
#                             levels = c('Abundance','Allocation','Behavior','Biomass','Diversity','Nutrient','Physiology', 'Reproduction','Size','Survival')[10:1])
resp$response_class<-factor(resp$response_class, levels = unique(resp$response_class)[10:1])
figS1d<- ggplot(data = resp, aes(x = number, y = response_class, group = unit)) +
  geom_segment(aes(x = 0, xend = number, y = response_class, yend = response_class, size = unit)) +
  scale_size_manual(values = c(0.5,0.8), guide = "none") + 
  geom_point(aes(color = unit), size = 3) +
  scale_color_manual("", values = c("#D93B0A","#016699"), labels = c("Number of cases","Number of studies"))+
  theme_bw()+
  geom_label(x=max(resp$number), y = 10, hjust = 0.9, 
             size = 6, label = "D", label.size = 0) +
  ylab("Response class") +
  xlab("N") +
  theme(axis.title.y = element_text(size = 12, vjust = 2),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "none",
        plot.margin = margin(0.2, 0.5, 0.5, 0.5, "cm")) #c(0.65, 0.5))

figS1.1<- plot_grid(figS1a, figS1b, figS1c, nrow = 3, ncol = 1, rel_heights = c(5/13, 4/13, 4/13))
figS1A<-plot_grid(figS1.1, figS1d, nrow = 1, ncol = 2) #, rel_widths = c(2/5, 3/5)


# continent, exp type, inv taxon, response scale 
exp<-d_cl %>% group_by(study_type) %>% summarize(n.study = length(unique(studyID)), n.obs = n())%>%
  pivot_longer(cols = -1, names_to = "unit", values_to = "number") %>% data.frame()
exp$study_type[exp$study_type=="lab/greenhouse"]<-"lab/\ngreenhouse"
#exp$study_type[exp$study_type=="field"]<-"Field"
#exp$study_type[exp$study_type=="mesocosm"]<-"Mesocosm"
exp$study_type<-factor(exp$study_type, levels = unique(exp$study_type)[3:1])
figS1e<- ggplot(data = exp, aes(x = number, y = study_type, group = unit)) +
  geom_segment(aes(x = 0, xend = number, y = study_type, yend = study_type, size = unit)) +
  scale_size_manual(values = c(0.5,0.8), guide = "none") + 
  geom_point(aes(color = unit), size = 3) +
  scale_color_manual("", values = c("#D93B0A","#016699"), labels = c("Number of observations","Number of studies"))+
  theme_bw()+
  geom_label(x=max(exp$number), y = 3.1, hjust = 0.9, 
             size = 6, label = "E", label.size = 0) +
  ylab("Experiment type") +
  xlab("N") +
  theme(axis.title.y = element_text(size = 12, vjust = 2),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "none",
        plot.margin = margin(0.2, 0.5, 0.5, 0.55, "cm"))

cont<-d_cl %>% group_by(continent) %>% summarize(n.study = length(unique(studyID)), n.obs = n())%>%
  pivot_longer(cols = -1, names_to = "unit", values_to = "number") %>% data.frame()
cont$continent<-factor(cont$continent, levels = unique(cont$continent)[7:1])
figS1f<- ggplot(data = cont, aes(x = number, y = continent, group = unit)) +
  geom_segment(aes(x = 0, xend = number, y = continent, yend = continent, size = unit)) +
  scale_size_manual(values = c(0.5,0.8), guide = "none") + 
  geom_point(aes(color = unit), size = 3) +
  scale_color_manual("", values = c("#D93B0A","#016699"), labels = c("Number of observations","Number of studies"))+
  theme_bw()+
  geom_label(x=max(cont$number), y = 7, hjust = 0.9, 
             size = 6, label = "F", label.size = 0) +
  ylab("Continent") +
  xlab("N") +
  theme(axis.title.y = element_text(size = 12, vjust = 2),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "none",
        plot.margin = margin(0.2, 0.5, 0.5, 0.5, "cm"))

rs<-d_cl %>% group_by(response_scale) %>% summarize(n.study = length(unique(studyID)), n.obs = n())%>%
  pivot_longer(cols = -1, names_to = "unit", values_to = "number") %>% data.frame()
#rs$response_scale[rs$response_scale=="community"]<-"Community"
#rs$response_scale[rs$response_scale=="ecosystem"]<-"Ecosystem"
#rs$response_scale[rs$response_scale=="species"]<-"Species"
rs$response_scale<-factor(rs$response_scale, levels = unique(rs$response_scale)[3:1])
figS1g<- ggplot(data = rs, aes(x = number, y = response_scale, group = unit)) +
  geom_segment(aes(x = 0, xend = number, y = response_scale, yend = response_scale, size = unit)) +
  scale_size_manual(values = c(0.5,0.8), guide = "none") + 
  geom_point(aes(color = unit), size = 3) +
  scale_color_manual("", values = c("#D93B0A","#016699"), labels = c("Number of observations","Number of studies"))+
  theme_bw()+
  geom_label(x=max(rs$number), y = 3.1, hjust = 0.9, 
             size = 6, label = "G", label.size = 0) +
  ylab("Response scale") +
  xlab("N") +
  theme(axis.title.y = element_text(size = 12, vjust = 2),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "none",
        plot.margin = margin(0.2, 0.5, 0.5, 0.5, "cm"))

tax<-d_cl %>% group_by(species_taxon) %>% summarize(n.study = length(unique(studyID)), n.obs = n())%>%
  pivot_longer(cols = -1, names_to = "unit", values_to = "number") %>% data.frame()
#tax$species_taxon<-factor(rep(c('Annelid', 'Crustacean', 'Fish', 'Insect', 'Mollusc', 'Oomycete', 'Plant'), each = 2),
#                          levels = c('Annelid', 'Crustacean', 'Fish', 'Insect', 'Mollusc', 'Oomycete', 'Plant')[7:1])
tax$species_taxon<-factor(tax$species_taxon, levels = unique(tax$species_taxon)[7:1])
figS1h<- ggplot(data = tax, aes(x = number, y = species_taxon, group = unit)) +
  geom_segment(aes(x = 0, xend = number, y = species_taxon, yend = species_taxon, size = unit)) +
  scale_size_manual(values = c(0.5,0.8), guide = "none") + 
  geom_point(aes(color = unit), size = 3) +
  scale_color_manual("", values = c("#D93B0A","#016699"), labels = c("Number of\n cases\n","Number of\n studies"))+
  theme_bw()+
  geom_label(x=max(tax$number), y = 7, hjust = 0.9, 
             size = 6, label = "H", label.size = 0) +
  ylab("Invader taxon") +
  xlab("N") +
  theme(axis.title.y = element_text(size = 12, vjust = 2),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = c(0.6, 0.5),
        plot.margin = margin(0.2, 0.5, 0.5, 0.5, "cm"))

figS1.2<- plot_grid(figS1e, figS1f, nrow = 2, ncol = 1, rel_heights = c(2/5, 3/5))
figS1.3<- plot_grid(figS1g, figS1h, nrow = 2, ncol = 1, rel_heights = c(2/5, 3/5))
figS1B<-plot_grid(figS1.2, figS1.3, nrow = 1, ncol = 2) #, rel_widths = c(2/5, 3/5)

plot_grid(figS1A, figS1B, nrow = 1, ncol =2)


# Fig S3.2: relationships btw setting, gc, trophic ----

# gc versus ecosystem setting
gc_broad_setting<-group_by(d_cl, setting, gc_factor) %>%
  summarise(n = length(unique(studyID)))%>% filter(gc_factor%in%c('temperature', 'drought', 'nitrogen'))
gc_broad_setting$gc_factor[gc_broad_setting$gc_factor=='temperature']<-'warming'
# plot
figS2a<-
  ggplot(data = gc_broad_setting, aes(x = gc_factor, y = n, fill = setting)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("skyblue", "blue","forestgreen"), "Ecosystem\nsetting")+
  labs(y = "Number of studies", x = "Global environmental change")+
  geom_text(x=3.4, y = 42, hjust = 0.9, 
            size = 6, label = "A") +
  theme_bw()+
  theme(legend.position = "bottom")

# mech X GC
mech_broad_gc_factor<-group_by(d_cl, gc_factor, mechanism_broad) %>%
  summarise(n = length(unique(studyID)))%>% filter(gc_factor%in%c('temperature', 'drought', 'nitrogen'))
mech_broad_gc_factor$gc_factor[mech_broad_gc_factor$gc_factor=='temperature']<-'warming'
mech_broad_gc_factor$mechanism_broad[mech_broad_gc_factor$mechanism_broad=="chemical/physical"]<-"chemical/\nphysical"
mech_broad_gc_factor$mechanism_broad<-factor(mech_broad_gc_factor$mechanism_broad, levels = c('predation', 'competition', 'chemical/\nphysical'))

# plot
figS2b<-
  ggplot(data = mech_broad_gc_factor, aes(x = gc_factor, y = n, fill = mechanism_broad)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("darkturquoise", "orange", "orchid"), "Invasion\nmechanism")+
  labs(y = "Number of studies", x = "Global environmental change")+
  geom_text(x=3.4, y = 44, hjust = 0.9, 
            size = 6, label = "B") +
  theme_bw()+
  theme(legend.position = "bottom",
        plot.margin = margin(0.2, 1, 0.2, 1, "cm"))


# trophic level versus GC
troph_gc_factor<-group_by(d_cl, gc_factor,trophic) %>%
  summarise(n = length(unique(studyID)))%>% filter(gc_factor%in%c('temperature', 'drought', 'nitrogen'))
troph_gc_factor$gc_factor[troph_gc_factor$gc_factor=='temperature']<-'warming'
# plot
figS2c<-
  ggplot(data = troph_gc_factor, aes(x = gc_factor, y = n, fill = trophic)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("#D93B0A","#016699"), "Trophic\nlevel")+
  labs(y = "Number of studies", x = "Global environmental change")+
  geom_text(x=3.4, y = 42, hjust = 0.9, 
            size = 6, label = "C") +
  theme_bw()+
  theme(legend.position = "bottom",
        plot.margin = margin(0.2, 1, 0.2, 1, "cm"))

plot_grid(figS2a, figS2b, figS2c, nrow = 1, ncol = 3)


# Fig S3.3: Publication bias  ----

# funnel plots of effect sizes versus precision

# with raw data and residuals from mean model
c2 = coda.samples(jm_int, variable.names=c("residual"),
                  n.iter = n.iter, thin = 1)
# these are: inv, gc, inv+gc 
c2_out<-data.frame(rbind(c2[[1]], c2[[2]], c2[[3]])) 
# plot betas
res<-data.frame(mean = apply(c2_out, 2, mean), 
                point = rep(1:nrow(d_cl), 3),
                treatment = rep(c("INV", "GEC", "IG"), each = nrow(d_cl)))%>%
  pivot_wider(names_from = treatment, values_from = mean)


funnel_data<-data.frame(studyId = d_cl$studyID, d_inv=d_cl$d_inv,d_gc=d_cl$d_gc,d_inv_gc=d_cl$d_inv_gc,
                         inv = res$INV, gc = res$GEC, ig = res$IG,
                         n_inv=d_cl$n_inv+d_cl$n_control, n_gc=d_cl$n_gc+d_cl$n_control, n_inv_gc=d_cl$n_inv_gc+d_cl$n_control,
                         prec_inv = 1/d_cl$d_var_inv, prec_gc = 1/d_cl$d_var_gc, prec_inv_gc = 1/d_cl$d_var_inv_gc,
                         var_inv = d_cl$d_var_inv, var_gc = d_cl$d_var_gc, var_inv_gc = d_cl$d_var_inv_gc)

cor_inv<-cor.test(funnel_data$d_inv, funnel_data$prec_inv, method = "spearman")
figS3B<-ggplot(data = funnel_data, aes(x = d_inv, y = prec_inv))+
  geom_point()+
  geom_vline(xintercept = mean(funnel_data$d_inv))+
  theme_bw()+
  labs(x = "INV effect size (Hedges' d)", 
       y = "Precision in Hedges' d (1/Variance)")+
  geom_label(x = 16, y = max(funnel_data$prec_inv)*9.5/10, size = 6, label.size = 0, label = "B")+
  geom_label(x = 16, y = max(funnel_data$prec_inv)*8.5/10, size = 4, label.size = 0,  
             label = paste("rho = ", round(cor_inv$estimate, 2), 
                           "\np = ", round(cor_inv$p.value, 3)))

cor_gc<-cor.test(funnel_data$d_gc, funnel_data$prec_gc, method = "spearman")
figS3A<-ggplot(data = funnel_data, aes(x = d_gc, y = prec_gc))+
  geom_point()+
  geom_vline(xintercept = mean(funnel_data$d_gc))+
  theme_bw()+
  labs(x = "GEC effect size (Hedges' d)", 
       y = "Precision in Hedges' d (1/Variance)")+
  geom_label(x = 23, y = max(funnel_data$prec_gc)*9.5/10, size = 6, label.size = 0, label = "A")+
  geom_label(x = 23, y = max(funnel_data$prec_gc)*8.5/10, size = 4, label.size = 0,  
             label = paste("rho = ", round(cor_gc$estimate, 2), 
                           "\np = ", round(cor_gc$p.value, 3)))

cor_inv_gc<-cor.test(funnel_data$d_inv_gc, funnel_data$prec_inv_gc, method = "spearman")
figS3C<-ggplot(data = funnel_data, aes(x = d_inv_gc, y = prec_inv_gc))+
  geom_point()+
  geom_vline(xintercept = mean(funnel_data$d_inv_gc))+
  theme_bw()+
  labs(x = "INV&GEC effect size (Hedges' d)", 
       y = "Precision in Hedges' d (1/Variance)")+
  geom_label(x = 19, y = max(funnel_data$prec_inv_gc)*9.5/10, size = 6, label.size = 0, label = "C")+
  geom_label(x = 19, y = max(funnel_data$prec_inv_gc)*8.5/10, size = 4, label.size = 0,  
             label = paste("rho = ", round(cor_inv_gc$estimate, 2), 
                           "\np = ", round(cor_inv_gc$p.value, 3)))




cor_inv2<-cor.test(funnel_data$inv, funnel_data$prec_inv, method = "spearman")
figS3E<-ggplot(data = funnel_data, aes(x = inv, y = prec_inv))+
  geom_point()+
  geom_vline(xintercept = mean(funnel_data$inv))+
  theme_bw()+
  labs(x = "INV Residual from model-estimated mean", 
       y = "Precision in Hedges' d (1/Variance)")+
  geom_label(x = 11, y = max(funnel_data$prec_inv)*9.5/10, size = 6, label.size = 0, label = "E")+
  geom_label(x = 11, y = max(funnel_data$prec_inv)*8.5/10, size = 4, label.size = 0,  
             label = paste("rho = ", round(cor_inv2$estimate, 2), 
                           "\np = ", round(cor_inv2$p.value, 3)))

cor_gc2<-cor.test(funnel_data$gc, funnel_data$prec_gc, method = "spearman")
figS3D<-ggplot(data = funnel_data, aes(x = gc, y = prec_gc))+
  geom_point()+
  geom_vline(xintercept = mean(funnel_data$gc))+
  theme_bw()+
  labs(x = "GEC Residual from model-estimated mean", 
       y = "Precision in Hedges' d (1/Variance)")+
  geom_label(x = 15, y = max(funnel_data$prec_gc)*9.5/10, size = 6, label.size = 0, label = "D")+
  geom_label(x = 15, y = max(funnel_data$prec_gc)*8.5/10, size = 4, label.size = 0,  
             label = paste("rho = ", round(cor_gc2$estimate, 2), 
                           "\np = ", round(cor_gc2$p.value, 3)))

cor_inv_gc2<-cor.test(funnel_data$ig, funnel_data$prec_inv_gc, method = "spearman")
figS3F<-ggplot(data = funnel_data, aes(x = ig, y = prec_inv_gc))+
  geom_point()+
  geom_vline(xintercept = mean(funnel_data$ig))+
  theme_bw()+
  labs(x = "INV&GEC Residual from model-estimated mean", 
       y = "Precision in Hedges' d (1/Variance)")+
  geom_label(x = 10, y = max(funnel_data$prec_inv_gc)*9.5/10, size = 6, label.size = 0, label = "F")+
  geom_label(x = 10, y = max(funnel_data$prec_inv_gc)*8.5/10, size = 4, label.size = 0,  
             label = paste("rho = ", round(cor_inv_gc2$estimate, 2), 
                           "\np = ", round(cor_inv_gc2$p.value, 3)))

plot_grid(figS3A, figS3B, figS3C, figS3D, figS3E, figS3F, nrow = 2, ncol = 3)



# Fig S3.4: comparison of overall treatment effects ----
# (differences between treatments)
out_int_difs<-data.frame(inv.gc = out_int$I, invgc.gc = out_int$IG, 
                         inv.invgc = out_int$I-out_int$IG) # I think????
out_int_plot_difs<-data.frame(mean = apply(out_int_difs, 2, mean), 
                              lower = apply(out_int_difs, 2, function(x) quantile(x, probs = c(.025))),
                              upper = apply(out_int_difs, 2, function(y) quantile(y, probs = c(.975))),
                              treatment = c("INV vs GEC","INV&GEC vs GEC", "INV vs INV&GEC"))
out_int_plot_difs$sig<-ifelse(out_int_plot_difs$lower<0&out_int_plot_difs$upper>0, "not", "sig")
out_int_plot_difs<-out_int_plot_difs%>%gather(end, value, 1:3)
#write.csv(out_int_plot_difs, "./Results_9.21/int_difs_plot.csv", row.names=FALSE)

ggplot(data = out_int_plot_difs[!out_int_plot_difs$end=='mean',], aes(x=value, y=treatment)) +
  geom_line(stat = "identity", position = "identity", size = 4, aes(colour = sig)) +
  scale_color_manual(values = c("grey60", "grey20"), guide = "none")+
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(data =out_int_plot_difs[out_int_plot_difs$end=='mean',], aes(x=value, y=treatment), 
             size = 3, colour = "white") +
  theme_bw() +
  labs(x = "Effect size comparisons (mean +/- 95% credible interval)") +
  theme(axis.text.x = element_text(size = 13, colour = "black"), axis.title.x = element_text(size=15, vjust = 0.5), 
        axis.text.y = element_text(size = 13, colour = "black"), axis.title.y=element_blank(),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))  


# Fig S3.5: means and interaction types with certain benefit and plant invaders datasets ----

# certain benefit dataset 
out_int_ben<-data.frame(rbind(cj_int_ben[[1]], cj_int_ben[[2]], cj_int_ben[[3]])) %>%
  select(-sd.study, -sd)
out_mean<-data.frame(Gmean=out_int_ben$b0, Imean=out_int_ben$b0+out_int_ben$I, IGmean=out_int_ben$b0+out_int_ben$IG)
out_int_ben_plot_mean<-data.frame(mean = apply(out_mean, 2, mean), 
                                  lower = apply(out_mean, 2, function(x) quantile(x, probs = c(.025))),
                                  upper = apply(out_mean, 2, function(y) quantile(y, probs = c(.975))),
                                  treatment = c("GEC","INV","INV&GEC")) 
out_int_ben_plot_mean$sig<-ifelse(out_int_ben_plot_mean$lower<0&out_int_ben_plot_mean$upper>0, "not", "sig")
out_int_ben_plot_mean<-out_int_ben_plot_mean %>% gather(end, value, c(1:3))
out_int_ben_plot_mean$treatment<- factor(out_int_ben_plot_mean$treatment, levels = c("GEC","INV","INV&GEC"))

figS5a<-ggplot(data = out_int_ben_plot_mean[!out_int_ben_plot_mean$end=='mean',], aes(x=treatment, y=value)) +
  geom_line(stat = "identity", position = "identity", size = 4, aes(color = sig)) +
  scale_color_manual(values = c("grey60", "grey20"), guide = "none")+
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(data =out_int_ben_plot_mean[out_int_ben_plot_mean$end=='mean',], aes(x=treatment, y=value), 
             size = 3, colour = "white") +
  theme_bw() +
  labs(y = "Mean effect size (Hedges' d)",
       x = "Treatment", 
       title = "A         Cases with confident benefit direction") +
  theme(axis.text.x = element_text(size = 11, colour = "black"), 
        axis.title.x = element_text(size=14, vjust = 0.3), 
        axis.text.y = element_text(size = 10, colour = "black"), 
        axis.title.y= element_text(size=14, vjust = 2),
        plot.margin = margin(0.5, 0.5, 0.1, 0.5, "cm"), # top, right, bottom, left
        plot.title= element_text(hjust = 0.1, size = 16))  

int_ben_plot<-data.frame(table(d_ben$int_cat_dir)) %>%
  mutate(int_cat_dir=Var1, Prop = Freq/sum(Freq), x = "x")
int_ben_plot$int_cat_dir<-factor(int_ben_plot$int_cat_dir, levels = c("Synergistic +","Antagonistic +", "Additive", "Antagonistic -","Synergistic -"))
figS5b<-ggplot(data = int_ben_plot, aes(x =x, y = Prop, fill = int_cat_dir)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual("     INV&GEC\nInteraction Type", values = c("#BE3526", "#D8867D", "#DDDDDD", "#67A3C2","#016699")[5:1]) + # "breaks" flips order in legend
  theme_bw() +
  labs(y = "Proportion of cases", title = "B")+
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 11, colour = "black"),
        axis.title.y = element_text(size = 14),
        axis.ticks.x = element_blank(),
        plot.title= element_text(hjust = 0.1, size = 16),
        legend.position = "right",
        plot.margin = margin(0.5, 0.5, 1, 0.5, "cm")) # top, right, bottom, left


figS5.1<-plot_grid(figS5a, figS5b, nrow = 1, ncol = 2, rel_widths = c(5/8, 3/8))

# ... plant invaders only ----

out_int_pl<-data.frame(rbind(cj_int_pl[[1]], cj_int_pl[[2]], cj_int_pl[[3]])) %>%
  select(-sd.study, -sd)
out_mean<-data.frame(Gmean=out_int_pl$b0, Imean=out_int_pl$b0+out_int_pl$I, IGmean=out_int_pl$b0+out_int_pl$IG)
out_int_pl_plot_mean<-data.frame(mean = apply(out_mean, 2, mean), 
                                 lower = apply(out_mean, 2, function(x) quantile(x, probs = c(.025))),
                                 upper = apply(out_mean, 2, function(y) quantile(y, probs = c(.975))),
                                 treatment = c("GEC","INV","INV&GEC")) 
out_int_pl_plot_mean$sig<-ifelse(out_int_pl_plot_mean$lower<0&out_int_pl_plot_mean$upper>0, "not", "sig")
out_int_pl_plot_mean<-out_int_pl_plot_mean %>% gather(end, value, c(1:3))
out_int_pl_plot_mean$treatment<- factor(out_int_pl_plot_mean$treatment, levels = c("GEC","INV","INV&GEC"))

figS5c<-ggplot(data = out_int_pl_plot_mean[!out_int_pl_plot_mean$end=='mean',], aes(x=treatment, y=value)) +
  geom_line(stat = "identity", position = "identity", size = 4, aes(color = sig)) +
  scale_color_manual(values = c("grey60", "grey20"), guide = "none")+
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(data =out_int_pl_plot_mean[out_int_pl_plot_mean$end=='mean',], aes(x=treatment, y=value), 
             size = 3, colour = "white") +
  theme_bw() +
  labs(y = "Mean effect size (Hedges' d)",
       x = "Treatment", title = "C        Cases with plant invasions") +
  theme(axis.text.x = element_text(size = 11, colour = "black"), 
        axis.title.x = element_text(size=14, vjust = 0.3), 
        axis.text.y = element_text(size = 10, colour = "black"), 
        axis.title.y= element_text(size=14, vjust = 2),
        plot.margin = margin(0.5, 0.5, 0.1, 0.5, "cm"), # top, right, bottom, left
        plot.title= element_text(hjust = 0.1, size = 16))  

int_pl_plot<-data.frame(table(d_pl$int_cat_dir)) %>%
  mutate(int_cat_dir=Var1, Prop = Freq/sum(Freq), x = "x")
int_pl_plot$int_cat_dir<-factor(int_pl_plot$int_cat_dir, levels = c("Synergistic +","Antagonistic +", "Additive", "Antagonistic -","Synergistic -"))
figS5d<-ggplot(data = int_pl_plot, aes(x =x, y = Prop, fill = int_cat_dir)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual("     INV&GEC\nInteraction Type", values = c("#BE3526", "#D8867D", "#DDDDDD", "#67A3C2","#016699")[5:1]) + # "breaks" flips order in legend
  theme_bw() +
  labs(y = "Proportion of cases", title = "D")+
  theme(axis.title.x = element_blank(),  # this is weird
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 11, colour = "black"),
        axis.title.y = element_text(size = 14),
        axis.ticks.x = element_blank(),
        plot.title= element_text(hjust = 0.1, size = 16),
        legend.position = "right",
        plot.margin = margin(0.5, 0.5, 1, 0.5, "cm")) # top, right, bottom, left


figS5.2<-plot_grid(figS5c, figS5d, nrow = 1, ncol = 2, rel_widths = c(5/8, 3/8))
plot_grid(figS5.1, figS5.2, nrow = 2, ncol = 1)


# Fig S3.6: Results of full model ----

out_main<-data.frame(rbind(cj_main[[1]], cj_main[[2]], cj_main[[3]])) %>%
  select(-g.sd.study,-i.sd.study,-ig.sd.study, -g.sd, -i.sd, -ig.sd)
names(out_main)
out_main_plot<-data.frame(mean = apply(out_main, 2, mean), 
                          lower = apply(out_main, 2, function(x) quantile(x, probs = c(.025))),
                          upper = apply(out_main, 2, function(y) quantile(y, probs = c(.975))),
                          beta = factor(rep(c("intercept", "freshwater", "marine",
                                              "field", "mesocosm",
                                              "CO2", "O2", "drought","nitrogen", "pH", 
                                              "chemical/physical", "predation",
                                              "abundance", "allocation","behavior", "diversity", 
                                              "nutrient", "physiology", "reproduction", 
                                              "size", "survival"),3),
                                        levels = c("intercept", "freshwater", "marine",
                                                   "field", "mesocosm",
                                                   "CO2", "O2", "drought","nitrogen", "pH", 
                                                   "chemical/physical", "predation",
                                                   "abundance", "allocation","behavior", "diversity", 
                                                   "nutrient", "physiology", "reproduction", 
                                                   "size", "survival")),
                          treatment = rep(c("GEC","INV","INV&GEC"), each = 21))
out_main_plot$sig<-ifelse(out_main_plot$lower<0&out_main_plot$upper>0, "not", "sig")
out_main_plot$cat[out_main_plot$beta == "intercept"]<- "Intercept"
out_main_plot$cat[out_main_plot$beta%in%c("drought","nitrogen","CO2","O2","pH")]<- "GEC"
out_main_plot$cat[out_main_plot$beta%in%c("chemical/physical", "predation")]<- "MECH"
out_main_plot$cat[out_main_plot$beta%in%c("freshwater", "marine")]<- "SETT"
out_main_plot$cat[out_main_plot$beta%in%c("field", "mesocosm")]<- "EXP"
out_main_plot$cat[out_main_plot$beta%in%c("abundance", "allocation","behavior", "diversity",
                                          "nutrient", "physiology", "reproduction", 
                                          "size", "survival")]<- "RC"
out_main_plot$color<-factor(paste(out_main_plot$cat, out_main_plot$sig),
                            levels = c("Intercept not", "Intercept sig", "GEC not", "GEC sig","MECH sig",
                                       "MECH not", "SETT not", "EXP not", "RC not", "RC sig"))

out_main_plot<-out_main_plot%>%gather(end, value, 1:3)
out_main_plot$treatment<- factor(out_main_plot$treatment, levels = c("INV","GEC","INV&GEC"))
# colors:
# intercept = grey (#D9D9D9), darkgrey (#999999) change to grey60, grey20
# GEC = green (#E4E9D3), darkgreen (#769022)
# MECH = orange (#F6D3B5), darkorange(#D93B0A)
# SETT = purple (#DACCE5)
# EXP = red (#F2D7D4)
# RC = blue (#CCE0EB), darkblue (#016699)

inv_plot<-out_main_plot[out_main_plot$treatment=="INV",]
gc_plot<-out_main_plot[out_main_plot$treatment=="GEC",]
ig_plot<-out_main_plot[out_main_plot$treatment=="INV&GEC",]
unique(gc_plot$color)
unique(inv_plot$color)
unique(ig_plot$color)

figS6a<-ggplot(data = gc_plot[!gc_plot$end=='mean',], aes(x=value, y=beta)) +
  geom_line(stat = "identity", position = "identity", size = 4, aes(colour = color)) +
  scale_color_manual(values = c("grey60", "#E4E9D3", "#769022", "#F6D3B5",
                                "#DACCE5", "#F2D7D4", "#CCE0EB","#016699"), guide = "none")+
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(data =gc_plot[gc_plot$end=='mean',], aes(x=value, y=beta), 
             size = 3, colour = "white") +
  geom_label(y = 21, x=max(gc_plot$value), 
             vjust = 0.9, hjust = 0.9, label="A", size = 8, label.size = 0)+
  theme_bw() +
  ggtitle("GEC")+
  labs(x = "Effect size +/- 95% credible interval") +
  theme(axis.title.y=element_blank())

figS6b<-ggplot(data = inv_plot[!inv_plot$end=='mean',], aes(x=value, y=beta)) +
  geom_line(stat = "identity", position = "identity", size = 4, aes(colour = color)) +
  scale_color_manual(values = c("grey20", "#E4E9D3", "#D93B0A","#F6D3B5",
                                "#DACCE5",  "#F2D7D4", "#CCE0EB", "#016699"), guide = "none")+
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(data =inv_plot[inv_plot$end=='mean',], aes(x=value, y=beta), 
             size = 3, colour = "white") +
  geom_label(y = 21, x=max(inv_plot$value), 
             vjust = 0.9, hjust = 0.9, label="B", size = 8, label.size = 0)+
  theme_bw() +
  ggtitle("INV")+
  labs(x = "Effect size +/- 95% credible interval") +
  theme(axis.title.y=element_blank())

figS6c<-ggplot(data = ig_plot[!ig_plot$end=='mean',], aes(x=value, y=beta)) +
  geom_line(stat = "identity", position = "identity", size = 4, aes(colour = color)) +
  scale_color_manual(values = c("grey60", "#E4E9D3", "#769022", "#F6D3B5",
                                "#DACCE5", "#F2D7D4", "#CCE0EB", "#016699"), guide = "none")+
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(data =ig_plot[ig_plot$end=='mean',], aes(x=value, y=beta), 
             size = 3, colour = "white") +
  geom_label(y = 21, x=max(ig_plot$value), 
             vjust = 0.9, hjust = 0.9, label="C", size = 8, label.size = 0)+
  theme_bw() +
  ggtitle("INV&GEC")+
  labs(x = "Effect size +/- 95% credible interval") +
  theme(axis.title.y=element_blank())

plot_grid(figS6a, figS6b, figS6c, nrow = 1, ncol = 3)



# Fig S3.7: mean and int cat comparison across mechanism, setting, exp type ----

# .... invasion mechanism ----

out_mech<-data.frame(rbind(cj_mech[[1]], cj_mech[[2]], cj_mech[[3]]))%>%
  select(-sd.study, sd)
out_mech2<-data.frame(g.COMP = out_mech$b0, g.PRED = out_mech$b0+out_mech$PRED, g.CHEM = out_mech$b0+out_mech$CHEM, #g.STR = out_mech$b0+out_mech$STR, 
                      i.COMP = out_mech$b0+out_mech$I, i.PRED = out_mech$b0+out_mech$I+out_mech$PRED+out_mech$I.PRED, i.CHEM = out_mech$b0+out_mech$I+out_mech$CHEM+out_mech$I.CHEM, #i.STR = out_mech$b0+out_mech$I+out_mech$STR+out_mech$I.STR, 
                      ig.COMP = out_mech$b0+out_mech$IG, ig.PRED = out_mech$b0+out_mech$IG+out_mech$PRED+out_mech$IG.PRED, ig.CHEM = out_mech$b0+out_mech$IG+out_mech$CHEM+out_mech$IG.CHEM)#, ig.STR = out_mech$b0+out_mech$IG+out_mech$STR+out_mech$IG.STR)
out_mech_plot<-data.frame(mean = apply(out_mech2, 2, mean), 
                          lower = apply(out_mech2, 2, function(x) quantile(x, probs = c(.025))),
                          upper = apply(out_mech2, 2, function(y) quantile(y, probs = c(.975))),
                          name = names(out_mech2),
                          beta = factor(rep(c("competition", "predation", "chemical/\nphysical"), 3), # ,  "structural"
                                        levels = c("competition", "predation", "chemical/\nphysical")),
                          treatment = rep(c("GEC","INV","INV+\nGEC"), each = nrow(mech)))
out_mech_plot$sig<-ifelse(out_mech_plot$lower<0&out_mech_plot$upper>0, "not", "sig")
out_mech_plot<-out_mech_plot%>%gather(end, value, 1:3)
out_mech_plot$treatment<- factor(out_mech_plot$treatment, levels = c("GEC","INV","INV+\nGEC"))
#write.csv(out_mech_plot, "./Results_9.21/mech_plot.csv", row.names=FALSE)

mech_labels<-data.frame(n.cases = table(d_cl$mechanism_broad), n.studies = tapply(d_cl$studyID, d_cl$mechanism_broad, function(x) length(unique(x))))
names(mech_labels)<-c("mechanism_broad", "n.cases", "n.studies")
mech_labels$beta<-c("chemical/\nphysical", "competition", "predation")
mech_labels$label<- paste("n cases = ", mech_labels$n.cases, "\nn studies = ", mech_labels$n.studies, sep ="")
mech_labels$letter<-c("","","A")
mech_labels$treatment<-3.5
mech_labels$value<-max(out_mech_plot$value)


figS7a<-ggplot(data = out_mech_plot, aes(x=treatment, y=value)) +
  geom_line(data = out_mech_plot[out_mech_plot$end%in%c("lower","upper"),], aes(x=treatment, y=value, color = sig),
            stat = "identity", position = "identity", 
            size = 4,  alpha = 0.7) +
  scale_color_manual(values = c("grey45", "black"), guide = "none")+
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(data =out_mech_plot[out_mech_plot$end=='mean',], aes(x=treatment, y=value), 
             size = 2, colour = "white", 
             #alpha = 0.5, 
             position = "identity") +
  # geom_point(data =out_mech_plot2[out_mech_plot2$end=='mean',], aes(x=treatment, y=value), 
  #           size = 4, colour = "black", shape = 21, position = "identity") +
  geom_label(y = -3.25, x = 1.8, size = 3, label.size = 0, alpha = 0.5,
             data = mech_labels, aes(label = label)) +
  geom_text( vjust=0.5, hjust = 1.1, size = 6, 
             data = mech_labels, aes(label = letter)) +
  labs(y = "Mean effect size (Hedges' d)")+
  facet_grid(.~beta)+
  theme_bw() +
  theme(axis.text.x = element_text(size = 9, colour = "black"), 
        axis.text.y = element_text(size = 10, colour = "black"), 
        #axis.title.y= element_text(size=14),
        #axis.title.x= element_text(size=14),
        axis.title.x = element_blank(),
        #axis.title.y = element_blank(),
        plot.margin = margin(0.5, 0.5, 0.5, 0.8, "cm")) # top, right, bottom, left

type_mech_plot<-data.frame(table(d_cl$int_cat_dir, d_cl$mechanism_broad)) %>%
  group_by(Var2) %>% mutate(sum = sum(Freq), prop = Freq/sum) %>% data.frame() %>% select(int_cat_dir=Var1, mechanism_broad=Var2, prop=prop)
type_mech_plot$int_cat_dir<-factor(type_mech_plot$int_cat_dir, levels = c("Synergistic +","Antagonistic +", "Additive", "Antagonistic -","Synergistic -"))
type_mech_plot$mechanism_broad<-as.character(type_mech_plot$mechanism_broad)
type_mech_plot$mechanism_broad[type_mech_plot$mechanism_broad=="chemical_physical"]<-"chemical/\nphysical"
figS7b<-
  ggplot(data = type_mech_plot, aes(x = mechanism_broad, y = prop, fill = int_cat_dir)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual("     INV&GEC\nInteraction Type", values = c("#BE3526", "#D8867D", "#DDDDDD", "#67A3C2","#016699"),
                    breaks=c("Synergistic +","Antagonistic +", "Additive", "Antagonistic -","Synergistic -")[5:1]) +
  theme_bw() +
  ylim(0, 1.05)+
  geom_text(x=length(unique(d_cl$mechanism_broad))+0.5, y = 1.05, 
            vjust=0.5, hjust = 1.1, size = 6, label = "B") +
  xlab("Invasion mechanism")+
  ylab("Proportion of cases")+
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))+  # top, right, bottom, left
  theme(legend.position = "bottom")

# .... setting ----
out_sett<-data.frame(rbind(cj_sett[[1]], cj_sett[[2]], cj_sett[[3]]))%>%
  select(-sd.study, sd)
out_sett2<-data.frame(g.TERR = out_sett$b0, g.FRESH = out_sett$b0+out_sett$FRESH, g.MARINE = out_sett$b0+out_sett$MARINE, 
                      i.TERR = out_sett$b0+out_sett$I, i.FRESH = out_sett$b0+out_sett$I+out_sett$FRESH+out_sett$I.FRESH, i.MARINE = out_sett$b0+out_sett$I+out_sett$MARINE+out_sett$I.MARINE, 
                      ig.TERR = out_sett$b0+out_sett$IG, ig.FRESH = out_sett$b0+out_sett$IG+out_sett$FRESH+out_sett$IG.FRESH, ig.MARINE = out_sett$b0+out_sett$IG+out_sett$MARINE+out_sett$IG.MARINE)
out_sett_plot<-data.frame(mean = apply(out_sett2, 2, mean), 
                          lower = apply(out_sett2, 2, function(x) quantile(x, probs = c(.025))),
                          upper = apply(out_sett2, 2, function(y) quantile(y, probs = c(.975))),
                          beta = factor(rep(c("terrestrial", "freshwater", "marine"), 3),
                                        levels = c("freshwater", "marine","terrestrial")),
                          treatment = rep(c("GEC","INV","INV+\nGEC"), each = nrow(sett)))
out_sett_plot$sig<-ifelse(out_sett_plot$lower<0&out_sett_plot$upper>0, "not", "sig")
out_sett_plot<-out_sett_plot%>%gather(end, value, 1:3)
out_sett_plot$treatment<- factor(out_sett_plot$treatment, levels = c("GEC","INV","INV+\nGEC"))
#write.csv(out_sett_plot, "./Results_9.21/setting_plot.csv", row.names=FALSE)


sett_labels<-data.frame(n.cases = table(d_cl$setting), n.studies = tapply(d_cl$studyID, d_cl$setting, function(x) length(unique(x))))
names(sett_labels)<-c("setting", "n.cases", "n.studies")
sett_labels$beta<-sett_labels$setting
sett_labels$label<- paste("n cases = ", sett_labels$n.cases, "\nn studies = ", sett_labels$n.studies, sep ="")
sett_labels$letter<-c("","","C")
sett_labels$treatment<-3.5
sett_labels$value<-max(out_sett_plot$value)

figS7c<-ggplot(data = out_sett_plot, aes(x=treatment, y=value)) +
  geom_line(data = out_sett_plot[out_sett_plot$end%in%c("lower","upper"),], aes(x=treatment, y=value, color = sig),
            stat = "identity", position = "identity", 
            size = 4,  alpha = 0.7) +
  scale_color_manual(values = c("grey45", "black"), guide = "none")+
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(data =out_sett_plot[out_sett_plot$end=='mean',], aes(x=treatment, y=value), 
             size = 2, colour = "white", 
             #alpha = 0.5, 
             position = "identity") +
  # geom_point(data =out_sett_plot2[out_sett_plot2$end=='mean',], aes(x=treatment, y=value), 
  #           size = 4, colour = "black", shape = 21, position = "identity") +
  geom_label(y = -3.2, x = 1.8, size = 3, label.size = 0, alpha = 0.5,
             data = sett_labels, aes(label = label)) +
  geom_text( vjust=0.5, hjust = 1.1, size = 6, 
             data = sett_labels, aes(label = letter)) +
  facet_grid(.~beta)+
  theme_bw() +
  theme(axis.text.x = element_text(size = 9, colour = "black"), 
        axis.text.y = element_text(size = 10, colour = "black"), 
        #axis.title.y= element_text(size=14),
        #axis.title.x= element_text(size=14),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = margin(0.5, 0.5, 0.5,0.8, "cm")) # top, right, bottom, left

type_sett_plot<-data.frame(table(d_cl$int_cat_dir, d_cl$setting)) %>%
  group_by(Var2) %>% mutate(sum = sum(Freq), prop = Freq/sum) %>% data.frame() %>% select(int_cat_dir=Var1, setting=Var2, prop=prop)
type_sett_plot$int_cat_dir<-factor(type_sett_plot$int_cat_dir, levels = c("Synergistic +","Antagonistic +", "Additive", "Antagonistic -","Synergistic -"))
figS7d<-
  ggplot(data = type_sett_plot, aes(x = setting, y = prop, fill = int_cat_dir)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual("     INV&GEC\nInteraction Type", values = c("#BE3526", "#D8867D", "#DDDDDD", "#67A3C2","#016699"),
                    breaks=c("Synergistic +","Antagonistic +", "Additive", "Antagonistic -","Synergistic -")[5:1]) +
  theme_bw() +
  ylim(0, 1.05)+
  geom_text(x=length(unique(d_cl$setting))+0.5, y = 1.05, 
            vjust=0.5, hjust = 1.1, size = 6, label = "D") +
  xlab("Ecosystem setting")+
  ylab("Proportion of cases")+
  theme(axis.title.y= element_blank(),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))+  # top, right, bottom, left
  theme(legend.position = "bottom")


# .... exp type ----

out_exp<-data.frame(rbind(cj_exp[[1]], cj_exp[[2]], cj_exp[[3]]))%>%
  select(-sd.study, sd)
out_exp2<-data.frame(g.MESO = out_exp$b0, g.LAB = out_exp$b0+out_exp$LAB, g.FIELD = out_exp$b0+out_exp$FIELD, 
                     i.MESO = out_exp$b0+out_exp$I, i.LAB = out_exp$b0+out_exp$I+out_exp$LAB+out_exp$I.LAB, i.FIELD = out_exp$b0+out_exp$I+out_exp$FIELD+out_exp$I.FIELD,
                     ig.MESO = out_exp$b0+out_exp$IG, ig.LAB = out_exp$b0+out_exp$IG+out_exp$LAB+out_exp$IG.LAB, ig.FIELD = out_exp$b0+out_exp$IG+out_exp$FIELD+out_exp$IG.FIELD)
out_exp_plot<-data.frame(mean = apply(out_exp2, 2, mean), 
                         lower = apply(out_exp2, 2, function(x) quantile(x, probs = c(.025))),
                         upper = apply(out_exp2, 2, function(y) quantile(y, probs = c(.975))),
                         beta = factor(rep(c("mesocosm", "lab/\ngreenhouse", "field"), 3),
                                       levels = c( "field", "lab/\ngreenhouse","mesocosm")),
                         treatment = rep(c("GEC","INV","INV+\nGEC"), each = 3))
out_exp_plot$sig<-ifelse(out_exp_plot$lower<0&out_exp_plot$upper>0, "not", "sig")
out_exp_plot<-out_exp_plot%>%gather(end, value, 1:3)
out_exp_plot$treatment<- factor(out_exp_plot$treatment, levels = c("GEC","INV","INV+\nGEC"))
#write.csv(out_exp_plot, "./Results_9.21/exp_plot.csv", row.names=FALSE)


exp_labels<-data.frame(n.cases = table(d_cl$study_type), n.studies = tapply(d_cl$studyID, d_cl$study_type, function(x) length(unique(x))))
names(exp_labels)<-c("study_type", "n.cases", "n.studies")
exp_labels$beta<-c("field", "lab/\ngreenhouse","mesocosm")
exp_labels$label<- paste("n cases = ", exp_labels$n.cases, "\nn studies = ", exp_labels$n.studies, sep ="")
exp_labels$letter<-c("","","E")
exp_labels$treatment<-3.5
exp_labels$value<-max(out_exp_plot$value)

figS7e<-ggplot(data = out_exp_plot, aes(x=treatment, y=value)) +
  geom_line(data = out_exp_plot[out_exp_plot$end%in%c("lower","upper"),], aes(x=treatment, y=value, color = sig),
            stat = "identity", position = "identity", 
            size = 4,  alpha = 0.7) +
  scale_color_manual(values = c("grey45", "black"), guide = "none")+
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(data =out_exp_plot[out_exp_plot$end=='mean',], aes(x=treatment, y=value), 
             size = 2, colour = "white", 
             #alpha = 0.5, 
             position = "identity") +
  # geom_point(data =out_exp_plot2[out_exp_plot2$end=='mean',], aes(x=treatment, y=value), 
  #           size = 4, colour = "black", shape = 21, position = "identity") +
  geom_label(y = -3.35, x = 1.8, size = 3, label.size = 0, alpha = 0.5,
             data = exp_labels, aes(label = label)) +
  geom_text( vjust=0.5, hjust = 1.1, size = 6, 
             data = exp_labels, aes(label = letter)) +
  facet_grid(.~beta)+
  theme_bw() +
  theme(axis.text.x = element_text(size = 9, colour = "black"), 
        axis.text.y = element_text(size = 10, colour = "black"), 
        #axis.title.y= element_text(size=14),
        #axis.title.x= element_text(size=14),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = margin(0.5, 0.5, 0.5, 0.8, "cm")) # top, right, bottom, left


type_exp_plot<-data.frame(table(d_cl$int_cat_dir, d_cl$study_type)) %>%
  group_by(Var2) %>% mutate(sum = sum(Freq), prop = Freq/sum) %>% data.frame() %>% select(int_cat_dir=Var1, study_type=Var2, prop=prop)
type_exp_plot$int_cat_dir<-factor(type_exp_plot$int_cat_dir, levels = c("Synergistic +","Antagonistic +", "Additive", "Antagonistic -","Synergistic -"))
type_exp_plot$study_type<-as.character(type_exp_plot$study_type)
type_exp_plot$study_type[type_exp_plot$study_type=="lab/greenhouse"]<-"lab/\ngreenhouse"
figS7f<-
  ggplot(data = type_exp_plot, aes(x = study_type, y = prop, fill = int_cat_dir)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual("     INV&GEC\nInteraction Type", values = c("#BE3526", "#D8867D", "#DDDDDD", "#67A3C2","#016699"),
                    breaks=c("Synergistic +","Antagonistic +", "Additive", "Antagonistic -","Synergistic -")[5:1]) +
  theme_bw() +
  ylim(0, 1.05)+
  geom_text(x=length(unique(d_cl$study_type))+0.5, y = 1.05, 
            vjust=0.5, hjust = 1.1, size = 6, label = "F") +
  xlab("Experiment type")+
  ylab("Proportion of cases")+
  theme(axis.title.y= element_blank(),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))+  # top, right, bottom, left
  theme(legend.position = "bottom")

figS7.1<-plot_grid(figS7a, figS7b, nrow = 2, ncol = 1, rel_heights = c(4/9,5/9))
figS7.2<-plot_grid(figS7c, figS7d, nrow = 2, ncol = 1, rel_heights = c(4/9,5/9))
figS7.3<-plot_grid(figS7e, figS7f, nrow = 2, ncol = 1, rel_heights = c(4/9,5/9))
plot_grid(figS7.1, figS7.2, figS7.3, nrow = 1, ncol = 3)


# Fig S3.8: full GC results ----

out_gc<-data.frame(rbind(cj_gc[[1]], cj_gc[[2]], cj_gc[[3]])) %>%
  select(-sd.study, sd)

out_gc_means<-data.frame(g.temp = out_gc$b0, g.drought = out_gc$b0+out_gc$DROU, g.nitrogen = out_gc$b0+out_gc$NITR, g.co2 = out_gc$b0+out_gc$CO2, g.o2 = out_gc$b0+out_gc$O2, g.pH = out_gc$b0+out_gc$PH, 
                         i.temp = out_gc$b0+out_gc$I, i.drought = out_gc$b0+out_gc$I+out_gc$DROU+out_gc$I.DROU, i.nitrogen = out_gc$b0+out_gc$I+out_gc$NITR+out_gc$I.NITR, 
                         i.co2 = out_gc$b0+out_gc$I+out_gc$CO2+out_gc$I.CO2 , i.o2 = out_gc$b0+out_gc$I+out_gc$O2+out_gc$I.O2, i.pH = out_gc$b0+out_gc$I+out_gc$PH+out_gc$I.PH, 
                         ig.temp = out_gc$b0+out_gc$IG, ig.drought = out_gc$b0+out_gc$IG+out_gc$DROU+out_gc$IG.DROU, ig.nitrogen = out_gc$b0+out_gc$IG+out_gc$NITR+out_gc$IG.NITR, 
                         ig.co2 = out_gc$b0+out_gc$IG+out_gc$CO2+out_gc$IG.CO2 , ig.o2 = out_gc$b0+out_gc$IG+out_gc$O2+out_gc$IG.O2, ig.pH = out_gc$b0+out_gc$IG+out_gc$PH+out_gc$IG.PH)

out_gc_plot<-data.frame(mean = apply(out_gc_means, 2, mean), 
                        lower = apply(out_gc_means, 2, function(x) quantile(x, probs = c(.025))),
                        upper = apply(out_gc_means, 2, function(y) quantile(y, probs = c(.975))),
                        name = names(out_gc_means),
                        beta = factor(rep(c("warming", "drought","nitrogen","CO2","O2","pH"), 3), # 
                                      levels = c("CO2", "drought","nitrogen","O2", "pH", "warming")), # 
                        treatment = rep(c("GEC","INV","INV&\nGEC"), each = 6))
out_gc_plot$sig<-ifelse(out_gc_plot$lower<0&out_gc_plot$upper>0, "not", "sig")
out_gc_plot<-out_gc_plot%>%gather(end, value, 1:3)
out_gc_plot$treatment<- factor(out_gc_plot$treatment, levels = c("GEC","INV","INV&\nGEC"))
#write.csv(out_gc_plot, "./Results_9.21/gc_all_plot.csv", row.names=FALSE)

gc_plot<-data.frame(name = rep(d_cl$gc_factor, 3), beta = "",
                    treatment = factor(rep(c("GEC","INV","INV&\nGEC"), each = nrow(d_cl)),levels = c("GEC","INV","INV&\nGEC")),
                    sig = "", end = "raw", value = c(d_cl$d_gc, d_cl$d_inv, d_cl$d_inv_gc))
gc_plot$beta<-gc_plot$name
gc_plot$beta[gc_plot$name=="temperature"]<-"warming"
#gc_plot<-gc_plot[,c(1,6,2:5)]
out_gc_plot2<-data.frame(rbind(out_gc_plot, gc_plot))


gc_labels<-data.frame(n.cases = table(d_cl$gc_factor), n.studies = tapply(d_cl$studyID, d_cl$gc_factor, function(x) length(unique(x))))
names(gc_labels)<-c("gc_factor", "n.cases", "n.studies")
gc_labels$beta<-c("CO2", "drought","nitrogen","O2", "pH", "warming")
gc_labels$label<- paste("n cases = ", gc_labels$n.cases, "\nn studies = ", gc_labels$n.studies, sep ="")
gc_labels$letter<-""
gc_labels$letter[gc_labels$beta=="Warming"]<-"A"
gc_labels$treatment<-3.5
gc_labels$value<-max(out_gc_plot2$value)

figS8a<-ggplot(data = out_gc_plot, aes(x=treatment, y=value)) +
  geom_point(data = out_gc_plot2[out_gc_plot2$end=='raw',], 
             aes(x = treatment, y = value), 
             position = position_jitter(width = 0.03, height = 0),
             size = 1, color = "#016699", alpha = 0.4) +
  geom_line(data = out_gc_plot2[out_gc_plot2$end%in%c("lower","upper"),], aes(x=treatment, y=value, color = sig),
            stat = "identity", position = "identity", 
            size = 4,  alpha = 0.8) +
  scale_color_manual(values = c("grey45", "black"), guide = "none")+
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(data =out_gc_plot2[out_gc_plot2$end=='mean',], aes(x=treatment, y=value), 
             size = 2, colour = "white", 
             #alpha = 0.5, 
             position = "identity") +
  # geom_point(data =out_gc_plot2[out_gc_plot2$end=='mean',], aes(x=treatment, y=value), 
  #           size = 4, colour = "black", shape = 21, position = "identity") +
  geom_label(y = -22, x = 1.8, size = 3, label.size = 0, alpha = 0.5,
             data = gc_labels, aes(label = label)) +
  facet_grid(.~beta)+
  theme_bw() +
  labs(y = "Effect size (Hedges' d)", title = "A          Effect sizes, full dataset") +
  theme(axis.text.x = element_text(size = 9, colour = "black"), 
        axis.text.y = element_text(size = 10, colour = "black"), 
        axis.title.y= element_text(size=12),
        plot.title = element_text(size=15),
        axis.title.x = element_blank())#,
#plot.margin = margin(0.5, 0.5, 0.8, 0.5, "cm")) # top, right, bottom, left

type_gc_plot<-data.frame(table(d_cl$int_cat_dir, d_cl$gc_factor)) %>%
  group_by(Var2) %>% mutate(sum = sum(Freq), prop = Freq/sum) %>% data.frame() %>% select(int_cat_dir=Var1, gc_factor=Var2, prop=prop)
type_gc_plot$int_cat_dir<-factor(type_gc_plot$int_cat_dir, levels = c("Synergistic +","Antagonistic +", "Additive", "Antagonistic -","Synergistic -"))
type_gc_plot2<-data.frame(table(d_cl_ints$int_cat_dir, d_cl_ints$gc_factor)) %>%
  group_by(Var2) %>% mutate(sum = sum(Freq), prop = Freq/sum) %>% data.frame() %>% select(int_cat_dir=Var1, gc_factor=Var2, prop=prop)
type_gc_plot2$int_cat_dir<-factor(type_gc_plot2$int_cat_dir, levels = c("Synergistic +","Antagonistic +", "Additive", "Antagonistic -","Synergistic -"))
type_gc_plot$gc_factor<-as.character(type_gc_plot$gc_factor)
type_gc_plot2$gc_factor<-as.character(type_gc_plot2$gc_factor)
type_gc_plot$gc_factor[type_gc_plot$gc_factor=="temperature"]<-"warming"
type_gc_plot2$gc_factor[type_gc_plot2$gc_factor=="temperature"]<-"warming"
figS8b<-
  ggplot(data = type_gc_plot, aes(x = gc_factor, y = prop, fill = int_cat_dir)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual("     INV&GEC\nInteraction Type", values = c("#BE3526", "#D8867D", "#DDDDDD", "#67A3C2","#016699"),
                    breaks=c("Synergistic +","Antagonistic +", "Additive", "Antagonistic -","Synergistic -")[5:1]) +
  theme_bw() +
  ggtitle("B          Interaction types, full dataset")+
  ylab("Proportion of cases")+
  theme(axis.text.x= element_text(size = 10, colour = "black"), 
        axis.text.y = element_text(size = 10, colour = "black"), 
        axis.title.y= element_text(size=12),
        plot.title = element_text(size=15),
        axis.title.x = element_blank()) +
  #plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))+  # top, right, bottom, left
  theme(legend.position = "bottom")

type_gc2<-data.frame(table(d_cl_ints$gc_factor)) 
type_gc2$Var1<-as.character(type_gc2$Var1)
type_gc2$Var1[type_gc2$Var1=="temperature"]<-"warming"
type_gc2$labels<-paste(type_gc2$Var1, "\nn = ", type_gc2$Freq, sep = "")
type_gc_plot2<-merge(type_gc_plot2, type_gc2, by.x = "gc_factor", by.y = "Var1")
figS8c<-
  ggplot(data = type_gc_plot2, aes(x = labels, y = prop, fill = int_cat_dir)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("#BE3526", "#D8867D", "#DDDDDD", "#67A3C2","#016699"),
                    breaks=c("Synergistic +","Antagonistic +", "Additive", "Antagonistic -","Synergistic -")[5:1]) +
  theme_bw() +
  ggtitle("C          Interaction types, reduced dataset")+
  xlab("Global environmental change")+
  ylab("Proportion of cases")+
  theme(axis.text.x= element_text(size = 10, colour = "black"), 
        axis.text.y = element_text(size = 10, colour = "black"), 
        axis.title.x= element_text(size=12),
        axis.title.y= element_text(size=12),
        plot.title = element_text(size=15))+
  #plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))+  # top, right, bottom, left
  theme(legend.position = "none")

#plot_grid(figS8a, figS8b, figS8c, nrow = 3, ncol = 1, rel_heights = c(7/20, 6/20, 7/20))
plot_grid(figS8a, figS8b, figS8c, nrow = 3, ncol = 1, rel_heights = c(6/20, 7/20, 6/20))


# Fig S3.9: full response class results ----

oout_rclass<-data.frame(rbind(cj_rclass[[1]], cj_rclass[[2]], cj_rclass[[3]])) %>%
  select(-sd, -sd.study)
names(out_rclass)

out_rclass_means<-data.frame(g.abun = out_rclass$b0+out_rclass$ABUN, g.allo = out_rclass$b0+out_rclass$ALLO, 
                             g.beh = out_rclass$b0+out_rclass$BEH,g.biom = out_rclass$b0,  g.div = out_rclass$b0+out_rclass$DIV,#g.gro = out_rclass$b0+out_rclass$GRO,
                             g.nutr = out_rclass$b0+out_rclass$NUTR, g.PHYSI = out_rclass$b0+out_rclass$PHYSI, g.REPR = out_rclass$b0+out_rclass$REPR, 
                             g.size = out_rclass$b0+out_rclass$SIZE,g.surv = out_rclass$b0+out_rclass$SURV,
                             i.abun = out_rclass$b0+out_rclass$I+out_rclass$ABUN+out_rclass$I.ABUN, i.allo = out_rclass$b0+out_rclass$I+out_rclass$ALLO+out_rclass$I.ALLO, 
                             i.beh = out_rclass$b0+out_rclass$I+out_rclass$BEH+out_rclass$I.BEH, i.biom = out_rclass$b0+out_rclass$I, i.div = out_rclass$b0+out_rclass$I+out_rclass$DIV+out_rclass$I.DIV, #i.gro = out_rclass$b0+out_rclass$I+out_rclass$GRO+out_rclass$I.GRO,
                             i.nutr = out_rclass$b0+out_rclass$I+out_rclass$NUTR+out_rclass$I.NUTR, i.PHYSI = out_rclass$b0+out_rclass$I+out_rclass$PHYSI+out_rclass$I.PHYSI, i.REPR = out_rclass$b0+out_rclass$I+out_rclass$REPR+out_rclass$I.REPR, 
                             i.size = out_rclass$b0+out_rclass$I+out_rclass$SIZE+out_rclass$I.SIZE, i.surv = out_rclass$b0+out_rclass$I+out_rclass$SURV+out_rclass$I.SURV,
                             ig.abun = out_rclass$b0+out_rclass$IG+out_rclass$ABUN+out_rclass$IG.ABUN, ig.allo = out_rclass$b0+out_rclass$IG+out_rclass$ALLO+out_rclass$IG.ALLO, 
                             ig.beh = out_rclass$b0+out_rclass$IG+out_rclass$BEH+out_rclass$IG.BEH, ig.biom = out_rclass$b0+out_rclass$IG, ig.div = out_rclass$b0+out_rclass$IG+out_rclass$DIV+out_rclass$IG.DIV, #ig.gro = out_rclass$b0+out_rclass$IG+out_rclass$GRO+out_rclass$IG.GRO,
                             ig.nutr = out_rclass$b0+out_rclass$IG+out_rclass$NUTR+out_rclass$IG.NUTR, ig.PHYSI = out_rclass$b0+out_rclass$IG+out_rclass$PHYSI+out_rclass$IG.PHYSI, ig.REPR = out_rclass$b0+out_rclass$IG+out_rclass$REPR+out_rclass$IG.REPR, 
                             ig.size = out_rclass$b0+out_rclass$IG+out_rclass$SIZE+out_rclass$IG.SIZE, ig.surv = out_rclass$b0+out_rclass$IG+out_rclass$SURV+out_rclass$IG.SURV)

out_rclass_plot<-data.frame(mean = apply(out_rclass_means, 2, mean), 
                            lower = apply(out_rclass_means, 2, function(x) quantile(x, probs = c(.025))),
                            upper = apply(out_rclass_means, 2, function(y) quantile(y, probs = c(.975))),
                            name = names(out_rclass_means),
                            beta = factor(rep(rc$response_class, 3)), 
                            treatment = rep(c("GEC","INV","INV&\nGEC"), each = nrow(rc)))
out_rclass_plot$sig<-ifelse(out_rclass_plot$lower<0&out_rclass_plot$upper>0, "not", "sig")
out_rclass_plot<-out_rclass_plot%>%gather(end, value, 1:3)
out_rclass_plot$treatment<- factor(out_rclass_plot$treatment, levels = c("GEC","INV","INV&\nGEC"))
#write.csv(out_rclass_plot, "./Results_9.21/rclass_all_plot.csv", row.names=FALSE)

rc_plot<-data.frame(name = "", beta = rep(d_cl$response_class, 3), 
                    treatment = factor(rep(c("GEC","INV","INV&\nGEC"), each = nrow(d_cl)),levels = c("GEC","INV","INV&\nGEC")),
                    end = "raw", value = c(d_cl$d_gc, d_cl$d_inv, d_cl$d_inv_gc), sig = "")

out_rclass_plot2<-data.frame(rbind(out_rclass_plot, rc_plot))

rc_labels<-data.frame(n.cases = table(d_cl$response_class), n.studies = tapply(d_cl$studyID, d_cl$response_class, function(x) length(unique(x))))
names(rc_labels)<-c("response_class", "n.cases", "n.studies")
rc_labels$beta<-rc_labels$response_class
rc_labels$label<- paste("n cases = ", rc_labels$n.cases, "\nn studies = ", rc_labels$n.studies, sep ="")
rc_labels$letter<-""
rc_labels$letter[rc_labels$response_class=="survival"]<-"A"
rc_labels$treatment<-3.5
rc_labels$value<-max(out_rclass_plot2$value)

figS9a<-ggplot(data = out_rclass_plot, aes(x=treatment, y=value)) +
  geom_point(data = out_rclass_plot2[out_rclass_plot2$end=='raw',], 
             aes(x = treatment, y = value), 
             position = position_jitter(width = 0.03, height = 0),
             size = 1, color = "#769022", alpha = 0.4) +
  geom_line(data = out_rclass_plot2[out_rclass_plot2$end%in%c("lower","upper"),], aes(x=treatment, y=value, color = sig),
            stat = "identity", position = "identity", 
            size = 4,  alpha = 0.7) +
  scale_color_manual(values = c("grey45", "black"), guide = "none")+
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(data =out_rclass_plot2[out_rclass_plot2$end=='mean',], aes(x=treatment, y=value), 
             size = 2, colour = "white", 
             #alpha = 0.5, 
             position = "identity") +
  # geom_point(data =out_rclass_plot2[out_rclass_plot2$end=='mean',], aes(x=treatment, y=value), 
  #           size = 4, colour = "black", shape = 21, position = "identity") +
  geom_label(y = -20, x = 1.8, size = 3, label.size = 0, alpha = 0.5,
             data = rc_labels, aes(label = label)) +
  facet_grid(.~beta)+
  theme_bw() +
  labs(y = "Effect size (Hedges' d)", title = "A          Effect sizes, full dataset", 
       x = "Treatment") +
  theme(axis.text.x = element_text(size = 9, colour = "black"), 
        axis.text.y = element_text(size = 10, colour = "black"), 
        axis.title.y= element_text(size=12),
        axis.title.x= element_text(size=12, vjust = 0.5),
        plot.title = element_text(size=15))
#axis.title.x = element_blank(),
#plot.margin = margin(0.2, 0.5, 1, 0.5, "cm")) # top, right, bottom, left

type_rc_plot<-data.frame(table(d_cl$int_cat_dir, d_cl$response_class)) %>%
  group_by(Var2) %>% mutate(sum = sum(Freq), prop = Freq/sum) %>% data.frame() %>% select(int_cat_dir=Var1, response_class=Var2, prop=prop)
type_rc_plot$int_cat_dir<-factor(type_rc_plot$int_cat_dir, levels = c("Synergistic +","Antagonistic +", "Additive", "Antagonistic -","Synergistic -"))
type_rc_plot2<-data.frame(table(d_cl_ints$int_cat_dir, d_cl_ints$response_class)) %>%
  group_by(Var2) %>% mutate(sum = sum(Freq), prop = Freq/sum) %>% data.frame() %>% select(int_cat_dir=Var1, response_class=Var2, prop=prop)
type_rc_plot2$int_cat_dir<-factor(type_rc_plot2$int_cat_dir, levels = c("Synergistic +","Antagonistic +", "Additive", "Antagonistic -","Synergistic -"))
type_rc_plot2$response_class<-factor(type_rc_plot2$response_class,
                                      levels = levels(type_rc_plot$response_class))
figS9b<-
  ggplot(data = type_rc_plot, aes(x = response_class, y = prop, fill = int_cat_dir)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual("     INV&GEC\nInteraction Type", values = c("#BE3526", "#D8867D", "#DDDDDD", "#67A3C2","#016699"),
                    breaks=c("Synergistic +","Antagonistic +", "Additive", "Antagonistic -","Synergistic -")[5:1]) +
  theme_bw() +
  #xlab("Response class")+
  ylab("Proportion of cases")+
  ggtitle("B          Interaction types, full dataset")+
  theme(axis.title.y = element_text(size=12),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 10, colour = "black"), 
        plot.title = element_text(size=15),
        axis.text.x= element_text(size = 9, colour = "black")) +
  theme(legend.position = "bottom")
#guides(fill = guide_legend(reverse=T))

type_rc2<-data.frame(table(d_cl_ints$response_class)) 
type_rc2$labels<-paste(type_rc2$Var1, "\nn = ", type_rc2$Freq, sep = "")
type_rc_plot2<-merge(type_rc_plot2, type_rc2, by.x = "response_class", by.y = "Var1")
figS9c<-
  ggplot(data = type_rc_plot2, aes(x = labels, y = prop, fill = int_cat_dir)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual("     INV&GEC\nInteraction Type", values = c("#BE3526", "#D8867D", "#DDDDDD", "#67A3C2","#016699"),
                    breaks=c("Synergistic +","Antagonistic +", "Additive", "Antagonistic -","Synergistic -")[5:1]) +
  theme_bw() +
  xlab("Response class")+
  ylab("Proportion of cases")+
  ggtitle("C          Interaction types, reduced dataset")+
  theme(axis.text.x= element_text(size = 10, colour = "black"), 
        axis.text.y = element_text(size = 10, colour = "black"), 
        axis.title.y= element_text(size=12),
        plot.title = element_text(size=15),
        axis.title.x = element_blank()) +
  theme(legend.position = "none") 
#guides(fill = guide_legend(reverse=T))

plot_grid(figS9a, figS9b, figS9c, nrow = 3, ncol = 1, rel_heights = c(6/20, 7/20, 6/20))

