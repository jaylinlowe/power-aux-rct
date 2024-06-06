# script: 01-make-data
# author: Adam Sales / Charlotte Mann
# original date: Feb 4, 2022
# purpose: Create combined HS and MS AEIS data

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)


#==========================================================================================#
                                  ## CTA STUDY DATA ##
#==========================================================================================#


# read in and append CTA study High School data
h1 <- read_csv('raw-data/RAND/H1_algebra_rcal_20121119_fieldid.csv')
h2 <- read_csv('raw-data/RAND/H2_algebra_rcal_20121119_fieldid.csv')
hs <- rbind(h1[,intersect(names(h1),names(h2))],h2[,intersect(names(h1),names(h2))])

# read in and append CTA study Middle School data
m1 <- read_csv('raw-data/RAND/M1_algebra_rcal_20121119_fieldid.csv')
m2 <- read_csv('raw-data/RAND/M2_algebra_rcal_20121119_fieldid.csv')
ms <- rbind(m1[,intersect(names(m1),names(m2))],m2[,intersect(names(m1),names(m2))])

# filter to schools in Texas
tx <- hs %>%
  bind_rows(ms) %>%
  filter(state=='TX')
table(tx$grdlvl)

grd.xwalk <- tx %>%
  select(pair, grdlvl) %>%
  distinct()

# list of school pairs with school names
exp.schools <- read_csv('raw-data/RAND/PairCodesComplete.csv')

# data for schools in the paired list and the full study data
schools <- subset(exp.schools, pair%in%tx$pair)

# 2007-2008 campus level reference file from AEIS data
cref <- read_csv('raw-data/campus-ref/cref78.dat')

# clean up school names to merge
cref <- subset(cref,DISTRICT==101912)
cref$CAMPNAME <- tolower(cref$CAMPNAME)
schools$sch_name <- tolower(schools$sch_name)
schools$CAMPUS <- cref$CAMPUS[pmatch(schools$sch_name,cref$CAMPNAME)]

# fix campus names to match between datasets
schools$CAMPUS[schools$sch_name=="contemporary learning center h s" ] <- cref$CAMPUS[cref$CAMPNAME=="contemporary lrn ctr h s"]
schools$CAMPUS[schools$sch_name=="houston academy for intnl studies"] <- cref$CAMPUS[cref$CAMPNAME=="houston academy for international"]
schools$CAMPUS[schools$sch_name=="newcomer hs"] <- cref$CAMPUS[cref$CAMPNAME=="liberty h s"]
schools$CAMPUS[schools$sch_name=='gregory-lincoln ed cntr'] <- cref$CAMPUS[cref$CAMPNAME=='gregory-lincoln ed ctr'&cref$GRDTYPE=='M']

#https://en.wikipedia.org/wiki/Liberty_High_School_(Houston)

# join the publicly available information onto the study data to 
# create the master list of study schools with basic information
schools <- merge(schools,cref,by='CAMPUS',all.x=TRUE,all.y=FALSE) %>%
  left_join(grd.xwalk, by = "pair")

## newcomer high school is a third school in a "pair" and doesn't have outcome data

schools <- schools %>%
  filter(sch_name != "newcomer hs")

## there are now 44 schools in 22 pairs
## there are 12 high schools and 32 middle schools
table(schools$grdlvl)

#==========================================================================================#
                          ## PUBLICLY AVAILABLE TEXAS DATA ##
#==========================================================================================#

#*************************        Test (TAK) Scores               *************************#

# 2008-2009 school year information about the schools (most importantly the grade)
cref <- read_csv('raw-data/campus-ref/cref89.dat') %>%
  select(CAMPUS, COUNTY, GRDTYPE, GRDSPAN)

## Grades 9 & 10 2008-2009 TAKS test data
taks910 <- read_csv('raw-data/outcomes/y89/taks910_89.dat',na='.',col_types=cols(CAMPUS='c'))

## Grades 7 & 8 2008-2009 TAKS test data
taks78 <- read_csv('raw-data/outcomes/y89/taks78_89.dat',na='.',col_types=cols(CAMPUS='c'))

## combine the grade datasets (the only overlapping variable is the campus ID)
taks <- taks78 %>%
  full_join(taks910, by = "CAMPUS")

## rename variables of interest for high school outcomes - 9th grade mathematics 2008 and 2009 TAKS met standard
names(taks) <- gsub('(C)([ABHFEMW])(009TM)(0[89])(R)','outh\\2\\4',names(taks))

## rename variables of interest for middle school outcomes - 8th grade mathematics 2008 and 2009 TAKS met standard
names(taks) <- gsub('(C)([ABHFEMW])(008TM)(0[89])(R)','outm\\2\\4',names(taks))

## according to the documentation, masking values of -4 for TAKS outcomes indicate that the percentage was essentially 100 and -3 indicates
## that the percentage was essentially 0 so these outcomes needed to be masked. values of -99, -1, and -2 indicate that the value was masked and should
## be considered as missing. (https://rptsvr1.tea.texas.gov/perfreport/aeis/2008/masking.html)

taks <- taks %>%
  select(CAMPUS, starts_with("out")) %>%
  mutate(across(!CAMPUS, ~ifelse(is.na(.x), 1,0), .names = "{.col}_na")) %>% #create indicators that it is missing from the start - not masked
  mutate(across(!CAMPUS, ~case_when(.x == -4 ~ 100,
                                    .x == -3 ~ 0,
                                    .x == -1 ~ NA_real_,
                                    .x == -2 ~ NA_real_,
                                    .x == -99 ~ NA_real_,
                                    TRUE ~ .x)))

## subset to schools that have overall 2008 or 2009 mathematics scores
# 3,084 out of the 8,322 schools in the raw data have this information
idx <- !is.na(taks$outhA08)|!is.na(taks$outhA09)|!is.na(taks$outmA08)|!is.na(taks$outmA09)
taks <- taks[idx,]

#*************************        PRETREATMENT COVARIATES             ***********************#

## save vector of campuses that have TAKS information
camp <- taks$CAMPUS

# create a data frame with the school ID as the only variable
# this is a vector of the campuses that have the outcome data
covs <- data.frame(CAMPUS=camp)

# loop through all of the covariate files and merge by school ID
#  the data from earlier years does not include column names, so must be treated differently
for(i in 3:7){
  yr <- paste0(i,i+1)
  files <- list.files(paste0('raw-data/y',yr),pattern='.dat')
  print(yr)
  for(f in files){
    fname <- str_replace(f, "\\.dat", "")
    print(fname)
    
    suppressWarnings(suppressMessages(newdat <- read_csv(paste0('raw-data/y',yr,'/',f),col_names=FALSE,na=c('','.'))))
   
     # if no column names, replace the dataset name in the front instead of "X"
    names(newdat)[names(newdat)!='CAMPUS'] <- str_replace(names(newdat)[names(newdat)!='CAMPUS'],"X",paste0(fname, "_"))
    
    if(newdat[1,1]=='CAMPUS')
      suppressMessages(newdat <- read_csv(paste0('raw-data/y',yr,'/',f),col_names=TRUE,na=c('.','')))
    
    # add the year and dataset to the variable name 
    names(newdat)[names(newdat)!='CAMPUS'] <- paste0(names(newdat)[names(newdat)!='CAMPUS'],paste0("_", yr))
    
    # set first name as "CAMPUS"
    names(newdat)[1] <- 'CAMPUS'
   
    # print a quick check
    print(newdat[1:2,1:6])
    
    # join additional covariates
    covs <- left_join(covs,newdat,'CAMPUS')
  }
}

save(covs,taks,schools,file='data/temp/rawCovs.RData')

#*************************        SUBSET COVARIATES         ***********************#
#load("data/temp/rawCovs.Rdata")

covs0 <- covs

# check out variable structure
typ <- rep(NA, ncol(covs))
for(i in 1:ncol(covs)){
  typ[i] <- class(covs[,i])
}

check <- covs[,typ == "logical"]

# there are a number of covariates that are just missing entirely, drop these off the bat for simplicity
na.prop <- sapply(covs,function(x) mean(is.na(x)))

# now there are 8420 variables
covs <- covs[,na.prop <.97]

## Masking
## according to the documentation, masking values of -4 for TAKS outcomes indicate that the percentage was essentially 100 and -3 indicates
## that the percentage was essentially 0 so these outcomes needed to be masked. values of -99, -1, and -2 indicate that the value was masked and should
## be considered as missing. (https://rptsvr1.tea.texas.gov/perfreport/aeis/2008/masking.html)

covs <- covs %>%
  mutate(across(!CAMPUS, ~case_when(.x == -4 ~ 100,
                                    .x == -3 ~ 0,
                                    .x == -1 ~ NA_real_,
                                    .x == -2 ~ NA_real_,
                                    .x == -99 ~ NA_real_,
                                    TRUE ~ .x)))

hist(sapply(covs,function(x) mean(is.na(x))))

## subset covariates to those with at least 60% or more non-missing values for either HSs or MSs
## reduces number of covariates to 5541 from 8422

# consider high schools as those that aren't missing one of the grade 9 scores
# note: the school ids are in the same order in the "covs" and "taks" dataset
hs.sub <- covs[!is.na(taks$outhA08)|!is.na(taks$outhA09),]
keep.hs <- sapply(hs.sub,function(x) mean(is.na(x))<0.4)

ms.sub <- covs[!is.na(taks$outmA08)|!is.na(taks$outmA09),]
keep.ms <- sapply(ms.sub,function(x) mean(is.na(x))<0.4)

keep <- keep.ms == TRUE | keep.hs == TRUE

#keeping the pretest variables - the variables for the black subgroup would be dropped if only considered number
#of missing values
# now there are 5543 variables
keep[which(str_detect(names(covs), '(C)([ABHFE])(00[89]TM)(07)(R_67)'))] <- TRUE
covs <- covs[,keep]

## removes 13 covariates that take the values of 100 for all unless they are na
check <- covs[,sapply(covs,n_distinct,na.rm=TRUE)<=1]
covs <- covs[,sapply(covs,n_distinct,na.rm=TRUE)>1]

#************************ REMOVE COVARIATES WITH MANY DISSENTERS ***********************#

Mode <- function(x){
  if(n_distinct(x)>100) return(NA)
  if(is.factor(x)) levels(x)[which.max(table(x))]
  unique(na.omit(x))[which.max(table(x))]
}

modes <- sapply(covs,Mode)

## almost all 1 value?
dissenters <- sapply(1:ncol(covs),function(i) ifelse(is.na(modes[i]),NA, mean(covs[[i]]==modes[i],na.rm=TRUE)))

dissenters[is.na(dissenters)] <- 0

## reduces number of covariates to 5108
covs <- covs[,dissenters<0.1]

#************************ IDENTIFY PRETEST VARIABLES ***********************#

# pretest is 2007
names(covs) <- gsub('(C)([ABHFE])(009TM)(07)(R_67)','preh\\2',names(covs))
names(covs) <- gsub('(C)([ABHFE])(008TM)(07)(R_67)','prem\\2',names(covs))

pretest <- select(covs,CAMPUS,starts_with('pre'))

# these 228 schoools don't have any pretest info
check <- pretest[apply(pretest[2:11],1, function(x) mean(is.na(x)) >.9),]
# & they include a mixture of available posttest info
check2 <- taks %>%
  filter(CAMPUS %in% check$CAMPUS) %>%
  select(CAMPUS,starts_with("out"))

#************************ CHECK THAT OUTCOME ISNT IN COVARIATES ***********************#

## make sure the outcome isn't accidentally included in the dataset
cors08 <- sapply(dplyr::select(covs,-CAMPUS), function(x) cor(taks$outhA08,x,method='spearman',use='pairwise'))
print(max(cors08,na.rm=TRUE))
print(which.max(cors08))

cors09 <- sapply(dplyr::select(covs,-CAMPUS), function(x) cor(taks$outhA09,x,method='spearman',use='pairwise'))
print(max(cors09,na.rm=TRUE))
print(which.max(cors09))

cors08 <- sapply(dplyr::select(covs,-CAMPUS), function(x) cor(taks$outmA08,x,method='spearman',use='pairwise'))
print(max(cors08,na.rm=TRUE))
print(which.max(cors08))

cors09 <- sapply(dplyr::select(covs,-CAMPUS), function(x) cor(taks$outmA09,x,method='spearman',use='pairwise'))
print(max(cors09,na.rm=TRUE))
print(which.max(cors09))


#************************ MISSING VALUE IMPUTATION  ***********************#

# to handle missing values:
#       1. mean imputation
#       2. indicators for missing values

covsMeanImp <- covs %>%
  mutate(across(where(is.numeric), ~case_when(is.na(.x) ~ 1,
                                              TRUE ~ 0),
                .names = "{.col}_mis")) %>%
  mutate(across(where(is.numeric), ~case_when(is.na(.x) ~ mean(.x, na.rm = TRUE),
                                               TRUE ~ .x)))
  

#************************ Middle School / High School  ***********************#

### Checking out the distinction between middle shcool and high school - doesn't matter for this analysis
hs.sub <- covsMeanImp$CAMPUS[!is.na(taks$outhA08)|!is.na(taks$outhA09)]
ms.sub <- covsMeanImp$CAMPUS[!is.na(taks$outmA08)|!is.na(taks$outmA09)]

#392 schools have both 8th and 9th grade as outcomes
both.outs <- intersect(hs.sub, ms.sub)

# create a list of campus names and grade range information
check <- read_csv('raw-data/campus-ref/cref89.dat') %>%
  select(CAMPUS, COUNTY, GRDTYPE, GRDSPAN)

table(check[check$CAMPUS %in% both.outs, ]$GRDTYPE, check[check$CAMPUS %in% both.outs, ]$GRDSPAN)

#************************ DISTRICT & CAMPUS INFORMATION  ***********************#

## add district and general campus information as covariates
## use the 2008-2009 information since this should include all schools that have outcome scores
dist <- read_csv('raw-data/district0708.csv',skip=2,col_types=cols(.default='c'))
cref89 <- read_csv('raw-data/campus-ref/cref89.dat',col_types=cols(.default='c'))

#also want the "campus rating" information, but this should be pre-treatment
cref67 <- read_csv('raw-data/campus-ref/cref67.dat',col_types=cols(.default='c'))


######### quick check of how different cref 67 and cref 89 are #############
check <- cref89 %>%
  left_join(cref67, by = "CAMPUS") %>%
  mutate(countyind = COUNTY.x != COUNTY.y & !is.na(COUNTY.y),
         districtind = DISTRICT.x != DISTRICT.y & !is.na(DISTRICT.y),
         grdtypind = GRDTYPE.x != GRDTYPE.y & !is.na(GRDTYPE.y),
         cratingind = C_RATING.x != C_RATING.y & !is.na(C_RATING.y))

# as you would expect, the counties and districts all align, but the rating differs for 40% of schools
# and the gradetype differs for about 3% of schools. We will definitely use the pre-treatment rating
mean(check$countyind)
mean(check$districtind)
mean(check$cratingind)
mean(check$grdtypind)
###############################################################################

cref67 <- cref67 %>%
  select(CAMPUS, C_RATING_67 = C_RATING)

dist <- dist %>%
  mutate(DISTRICT =  sprintf('%06d', as.numeric(District))) %>%
  select(-District, -Description)

# join all of the data and fill missing values with "na" for district type (DTYPE) and campus rating (C_RATING_67)
# start with the covariates, because keeps the relevant campuses
refCovs <- covsMeanImp %>%
  select(CAMPUS) %>%
  left_join(cref89, by = "CAMPUS") %>%
  left_join(dist, by = "DISTRICT") %>%
  select(CAMPUS, DISTRICT, COUNTY, REGION, GRDTYPE, GRDSPAN, CFLCHART, DTYPE = Type) %>%
  left_join(cref67, by = "CAMPUS") %>%
  mutate(across(DTYPE:C_RATING_67, ~case_when(is.na(.x) ~ "na",
                                              TRUE ~ .x)))

# create a variable indicating which TAKS outcomes are available for each school (only 9th, only 8th, or both)
grades <- taks %>%
  mutate(OUTTYPE = case_when((!is.na(outmA08) | !is.na(outmA09)) & (is.na(outhA08) & is.na(outhA09)) ~ "M",
                             (!is.na(outhA08) | !is.na(outhA09)) & (is.na(outmA08) & is.na(outmA09)) ~ "H",
                             !is.na(outmA08) | !is.na(outhA08) | !is.na(outmA09) | !is.na(outhA09) ~ "B",
                             TRUE ~ "none")) %>%
  select(CAMPUS, OUTTYPE)

grd.xwalk <- grades %>%
  left_join(refCovs, by = "CAMPUS") %>%
  select(CAMPUS, GRDTYPE, GRDSPAN, OUTTYPE)
  
save(grd.xwalk, file = "data/xwalk/grade_xwalk.Rdata")

#==========================================================================================#
            ## DISTINGUISH BETWEEN EXPERIMENTAL AND REMNANT DATA & SAVE ##
#==========================================================================================#

# combine numeric and descriptive covariates (which are all character/factor variables)

covs <- refCovs %>%
  left_join(grades, by = "CAMPUS") %>%
  left_join(covsMeanImp, by = "CAMPUS")

covsRem <- subset(covs,!CAMPUS%in%schools$CAMPUS)
covsE <- covs[match(schools$CAMPUS,covs$CAMPUS),]

## add post and pretest values to the experimental data
schools <- schools %>%
  left_join(taks, by = "CAMPUS") %>%
  left_join(pretest, by = "CAMPUS") %>%
  select(-sch_name, -PAIRCAMP, -PAIRNAME) %>%
  rename(C_RATING_78 = C_RATING)

## save remnant outcomes
outRem <- taks %>%
  filter(CAMPUS %in% covsRem$CAMPUS)

## indicate that these are unscaled
covsRem_noscale <- covsRem
covsE_noscale <- covsE

## save all of the data
save(schools, outRem,
     covsRem_noscale, covsE_noscale,
     file='data/HS_MSdata.RData')

##############################################################################################
                      ################# END OF FILE  #####################
##############################################################################################