
setwd ("M:/Elena - R/Multi-state  cancer")

library(here)
library(readxl)
library(dplyr)
library(RPostgreSQL)
library(tidyverse)
library(lubridate)
library(pastecs)
library(data.table)
library(mstate)

 
 db <- dbConnect(RPostgreSQL::PostgreSQL(),
                 dbname = "postgres20v2",
                 port = 5432,
                 host = "10.80.192.22", 
                 user = "omop", 
                 password = "omop")

# dates ####
# 1) the date for which we'll identify variables of interest
# only up to the date, not after
healthy.start.date<-as.Date("29/02/2020", 
                            "%d/%m/%Y")
# 2) the date at which time at risk stats
tar.start.date<-as.Date("01/03/2020", 
                        "%d/%m/%Y")
#3) the date at which time at risk ends for administrative censoring
tar.end.date<-as.Date("06/05/2020", 
                      "%d/%m/%Y")



###### ####### #### 
# Identying patients prevously diagnosed with cancer 
# + create the variable cancer_status: cancer-free / <1y since diagnosis / 1-5y />5y

# ICD10 codes for all cancers
excel<-here("ConceptSets", "cancer_types_codes.xlsx")
## In this sheet, we have the codes for any malingancy exlcuding non melanoma
## note: ICD10 codes are in "long format": eg. C53.42
all_cancer_codes <- read_excel(excel, sheet = "all_cancers")
##  In this sheet, we have ICD10 codes for each specific cancer type and if it is a primary or secondary cancer
## Here, ICD10 codes are in "short format" (only numbers prior to the point) e.g. C53)
cancer_types_codes <- read_excel(excel, sheet = "types")

working.codes <- all_cancer_codes  %>% 
                select(icd10_code) %>%
                pull()

rm(all_cancer_codes)

# lookup in database



# condition_occurrence cdm table
cancer_exc_non_melanoma <- tbl(db, sql("SELECT * FROM omop20v2.condition_occurrence")) %>%  
                          filter(condition_source_value %in% working.codes) %>%  
                          collect() 
#nb a lot of codes to look up, so takes a while ....

# total number of records
nrow(cancer_exc_non_melanoma) #663479

# total number of individuals
length(unique(cancer_exc_non_melanoma$person_id)) # 486350

# exclude cancer observations after healthy start date
cancer_exc_non_melanoma <- cancer_exc_non_melanoma %>%
  filter(condition_start_date < tar.start.date)

nrow(cancer_exc_non_melanoma) 
length(unique(cancer_exc_non_melanoma$person_id)) # 483 479 
# There is more than one record per patient

# keep only first cancer diagnosis per person
first_cancer <- cancer_exc_non_melanoma %>% 
  group_by(person_id) %>%
  filter(condition_start_date == min(condition_start_date)) %>%
  distinct()

nrow(first_cancer) # 534 440


# add a new variable with ICD10 codes removing the numbers after the point - will use that to identify cancer types
# eg. C52.3 --> C52 (we have enough with that)
first_cancer$icd10 <- data.frame(do.call("rbind", strsplit(as.character(first_cancer$condition_source_value), ".", fixed = TRUE)))[,1]

# create variable for years  since diagnosis prior to index date
first_cancer$days<- as.numeric(difftime(healthy.start.date, 
                                        first_cancer$condition_start_date,
                                        units="days"))

first_cancer$years <- first_cancer$days/365.25
summary(first_cancer$years)

# create a categorical variable with years since diagnosis
first_cancer$c_status <- ifelse(first_cancer$days < 365.25, "< 1 year", 
                                     ifelse(between(first_cancer$days,365.25,(365.25*5)),"1-5 years",
                                            ifelse(first_cancer$days>(365.25*5),">5years",NA)))


#  exclude secondary cancers 
# codes for primary_cancers
working.codes<- cancer_types_codes %>% 
                filter(primary=="Yes") %>% 
                select(cancer_code) %>%
                pull()
  
#  DF with only primary cancers
first_cancer_prim <- first_cancer %>%  
                  filter(icd10 %in% working.codes) %>%  
                  collect()  %>%
                  select (person_id, condition_start_date,icd10, years, c_status, condition_source_value) %>%
                  distinct()
  
length(unique(first_cancer_prim$person_id)) # 478285
nrow (first_cancer_prim) #  484699
  
rm(first_cancer)

# some patients have more than one cancer diagnosed on the same day

# Some ICD10 codes do not idenitfy the cancer site
# ICD10 C76  Malignant neoplasm of other and ill-defined sites /C80 Malignant neoplasm without specification of site
# If a patient has more than one code the same day, include only those that are informative

first_cancer_prim$useless_codes <- ifelse(first_cancer_prim$icd10 == "C76" | first_cancer_prim$icd10=="C80", 1, 0)

first_cancer_prim <- arrange(first_cancer_prim, useless_codes)

# identify patients with duplicated id
first_cancer_prim <-first_cancer_prim %>%
  mutate(dup=duplicated(person_id))

table(first_cancer_prim$dup)  # 6414 DUP


# remove those with duplicated observations in which one cancer code stands for "non-specified cancer"
# that way, we will only keep the specific cancer subtype information
first_cancer_prim <- first_cancer_prim %>%
filter(!(useless_codes==1 & dup==TRUE )) %>%
  select(person_id, condition_start_date, icd10, years, c_status, condition_source_value)

nrow(first_cancer_prim) # 482297

first_cancer_prim <- first_cancer_prim %>%
  mutate(dup=duplicated(person_id))

table(first_cancer_prim$dup) # 4012 
## note: duplicated id do not identify patients repeated (some ids are repeated more than 2 times)

# identify patients with duplicated id
duplicated.ids <- first_cancer_prim %>%
  filter(duplicated(person_id) == TRUE) %>%
           select(person_id, c_status) %>%
          distinct() 

nrow(duplicated.ids) #3919 Persons duplicated


table(first_cancer_prim$person_id %in% duplicated.ids$person_id) # 474366 p do not have a dup

# exclude this patients from first_prim_cancer, will add them later

first_cancer_prim <- first_cancer_prim %>%
  filter(!(person_id %in% duplicated.ids$person_id)) %>%
  collect %>%
  select(person_id, condition_start_date, icd10, years, c_status, condition_source_value) 

# check we have one row per person
nrow(first_cancer_prim)/length(unique(first_cancer_prim$person_id))
nrow(first_cancer_prim) # 474366


# generate a df with the cancer description for each cancer
first_cancer_prim <- first_cancer_prim %>% 
                     left_join(cancer_types_codes%>% 
                     select(cancer_code, cancer_code_description, cancer_category),
                     by=c("icd10" = "cancer_code"))

rm(cancer_types_codes)

# add patients with more than one cancer recorded
nrow(first_cancer_prim) + nrow(duplicated.ids)
first_cancer_prim <- rbind(first_cancer_prim, duplicated.ids) 
nrow(first_cancer_prim) # 478285 // unique ids with primary cancers
rm(duplicated.ids)

# check it is okay - patients that were duplicated before will have missing values for cancer codes
sum(is.na(first_cancer_prim$icd10)) # 3919

# replace those missing values 
# icd10 code 98 will be mire than one primary cancer
first_cancer_prim$icd10 <- ifelse(is.na(first_cancer_prim$icd10),
                                  98,
                                  first_cancer_prim$icd10)

first_cancer_prim$cancer_code_description <- ifelse(is.na(first_cancer_prim$cancer_code_description), 
                                                    "More than one primary cancer", 
                                                    first_cancer_prim$cancer_code_description)

first_cancer_prim$cancer_category<- ifelse(is.na(first_cancer_prim$cancer_category), 
                                                    "More than one primary cancer", 
                                                    first_cancer_prim$cancer_category)

sum(is.na(first_cancer_prim$icd10))

# add column indicating which cancer patients should be excluded from the analysis (patients with secondary cancers)
all_cancer_ids <- cancer_exc_non_melanoma %>%
select(person_id) %>%
  distinct()

first_cancer_prim <- first_cancer_prim %>%
  as.data.frame()

cancer_patients <- full_join(first_cancer_prim, all_cancer_ids)


nrow(cancer_patients)
length(unique(cancer_patients$person_id))

table(cancer_patients$c_status, useNA = "always")

# replace c_status as "exclude" to exclude them later
cancer_patients$c_status <- ifelse(is.na(cancer_patients$c_status), "Exclude", cancer_patients$c_status)

rm(all_cancer_ids, first_cancer_prim, cancer_exc_non_melanoma)

# save so can start from here if needed
#save.image('1 data prep/up_to_cancer_patients.RData')
# load('1 data prep/up_to_cancer_patients.RData')


##################################

 db <- dbConnect(RPostgreSQL::PostgreSQL(),
                 dbname = "postgres20v2",
                 port = 5432,
                 host = "10.80.192.22", 
                 user = "omop", 
                 password = "omop")
 
### join this cancer population dataframe with the Study population 

# Collect from person and observation table
# To have individuals
person <- tbl(db, sql("SELECT * FROM omop20v2.person")) %>% 
          select("person_id" , "gender_concept_id", "year_of_birth", "month_of_birth", "day_of_birth","location_id") %>% 
          collect()

# To have observation period table
observation_period <- tbl(db, sql("SELECT * FROM omop20v2.observation_period")) %>% 
                     select("person_id", "observation_period_start_date", "observation_period_end_date") %>% 
                      collect()

person <- person %>% 
          left_join(observation_period, 
          by="person_id") # 7887308

rm(observation_period)  

# location
location.db<-tbl(db, sql("SELECT * FROM omop20v2.location")) 

location.names<-location.db %>% 
  select(location_id, location_source_value) %>% 
  collect()

location.names<-location.names %>% 
  rename(region=location_source_value)

person<-person %>% 
  left_join(location.names,
            by=c("location_id"))

rm(location.db,location.names)

# observation table
observation<- tbl(db, sql("SELECT * FROM omop20v2.observation")) 

# get nationality from observation table
# 3 ways to idenity observations for nationality observation_id>=43788485 
# OR observation_concept_id == 4087925
# OR observation_source_value == "agr_nacionalitat" ## best way would be that one but all work
nationality <- observation %>%
  filter(observation_source_value == "agr_nacionalitat") %>%
  collect()

# hospitalisations in 2020
any.hospital<-tbl(db, sql("SELECT * FROM omop20v2.visit_occurrence")) %>% 
  filter(visit_concept_id==262) %>% 
  filter(year(visit_start_date)=="2020") %>% 
  collect()

# gp visits in 2020
gp.visits<-tbl(db, sql("SELECT * FROM omop20v2.visit_occurrence")) %>% 
  filter(visit_concept_id==9202) %>% 
  filter(year(visit_start_date)=="2020") %>% 
  collect()

care_home<-observation %>% 
  filter(observation_concept_id=="44791364") %>% 
  select(person_id) %>% 
  mutate(care_home="Yes") %>%
  collect()


## Smoking and Medea
# concept table
concept<- tbl(db, sql("SELECT * FROM omop20v2.concept"))

# smoking data
smoking <- tbl(db, sql("SELECT * FROM omop20v2.measurement")) %>% 
  filter( measurement_source_value	=="tab") 

 smoking<-smoking %>% 
  left_join(concept ,
            by=c("value_as_concept_id"="concept_id")) %>% 
            collect() %>% 
            select(person_id, measurement_concept_id,concept_name,measurement_date)

#rm(concept)

# MEDEA data 
MEDEA <- observation %>% 
  filter(observation_source_value	=="medea") %>% 
  collect()

# Alcohol
alcohol <- observation %>%
  filter(observation_source_value == "ALRIS") %>%
  collect()

# covidmultistatecohorts table
# this is the table generated using cohort diagnostics
#covidmultistatecohorts_db<-tbl(db, sql("SELECT * FROM results20v2.covidmultistatecohorts")) %>% 
 # collect()

covidmultistatecohorts_db<-tbl(db, sql("SELECT * FROM results20v2.CovidMultiStateCohortsCancer")) %>% 
  collect()


## Add cancer status
person <-person %>% 
  left_join(cancer_patients %>% 
              select(person_id, c_status, years, icd10, condition_start_date, condition_source_value, cancer_code_description, cancer_category) %>%
              distinct() %>% 
              collect()) %>%
  mutate(c_status = coalesce(c_status, "cancer-free"))

person <- person %>% 
  rename(data_cancer_dx = condition_start_date)

# save so can start from here if needed
#  save.image(file='1 data prep/prior_study_pop.RData')

 # load('1 data prep/prior_study_pop.RData')

###### #######
######  Identify study population -------
# Collect from person table ------

# start from entire population from cdm
exclusion_table <- tibble(N_current=nrow(person), exclusion_reason=NA)

#only adults, get age
person$dob<- paste(person$year_of_birth, 
                   person$month_of_birth, 
                   person$day_of_birth, sep="-") %>% ymd() # %>% as.Date()
# age as of 1st March
person<-person %>% 
  mutate(age=floor(as.numeric(difftime(healthy.start.date,
                                       dob,
                                       units="days"))/365.25))

length(unique(person$person_id)) # 7887308
person<-person%>%
  filter(age>=18) # 6698681

exclusion_table<-rbind(exclusion_table,
                       c(nrow(person),
                         "Younger than 18"))

# only those with observation start date pior to or on healthy.start.date -----
person<-person %>% 
  filter(observation_period_start_date<=healthy.start.date)

exclusion_table<-rbind(exclusion_table,
                       c(nrow(person), 
                         paste0("Observations start after ", healthy.start.date)))

# only those with observation end date on or after 1st March -----
person<-person %>% 
  filter(observation_period_end_date>=tar.start.date)

# also exclude those who died befor start date
# (observation period end date does not capture all of these at the moment)
# exlude the 15 deads, index found at line 1181 approx
person<-person %>% 
  anti_join(
    covidmultistatecohorts_db %>% 
      filter(cohort_definition_id==303) %>% 
      filter(cohort_start_date<={{healthy.start.date}}) %>% 
      select(subject_id),
    by=c("person_id"="subject_id"))

exclusion_table<-rbind(exclusion_table,
                       c(nrow(person), 
                         paste0("Observations end before ", tar.start.date)))

# for healthy pop, exclude if less than a year of prior history -----
sum(person$observation_period_start_date > tar.start.date-years(1))  #  104022

person<-person %>% 
  filter(observation_period_start_date <= tar.start.date-years(1))

exclusion_table<-rbind(exclusion_table,
                       c(nrow(person), 
                         paste0("Less than a year of prior history")))



# exclude if first diagnosed with a secondary cancers
person <- person %>%
  filter(c_status!="Exclude")

exclusion_table<-rbind(exclusion_table,
                       c(nrow(person), 
                         paste0("Diagnosed with secondary cancer before primary")))

# check we don't have cancers diagnosed before birth
nrow(person$years >person$age)

# gender
#8507 male
#8532 female
person$gender<-ifelse(person$gender_concept_id==8507, "Male",
                      ifelse(person$gender_concept_id==8532, "Female", NA ))
#table(person$gender, useNA = "always")

###### #######
# Comorbidities and states cohorts



## ATLAS cohorts ------
# States
#339	[MultiStateCovid] COVID-19 diagnosis 2020
#303	[MultiStateCovid] Mortality 2020
#344	[MultiStateCovid] COVID-19 positive test 2020
#488  [MultiStateCovid] COVID-19 hospitalisation 2020 3 day limit (laboratory confirmed 3d max after hospitalization)
#491  [MultiStateCovid] Hospital Acquired infection: diagnosed 4d or more after admission

# Comorbidities
# 331	[MultiStateCovid] Autoimmune condition
# 337	[MultiStateCovid] Chronic kidney disease - single diagnosis
# 333	[MultiStateCovid] COPD
# 334	[MultiStateCovid] Dementia
# 315	[MultiStateCovid] Heart disease
# 316	[MultiStateCovid] Hyperlipidemia
# 312	[MultiStateCovid] Hypertension
# 336	[MultiStateCovid] Type 2 Diabetes Mellitus
# 345 [MultiStateCovid] Obesity, latest event before 1st March 2020

#table(covidmultistatecohorts_db$cohort_definition_id)
covid.diagnosis <- covidmultistatecohorts_db %>% 
  filter(cohort_definition_id==339)

covid_hospitalised <- covidmultistatecohorts_db %>% 
  filter(cohort_definition_id==488)

death <- covidmultistatecohorts_db %>% 
  filter(cohort_definition_id==303)

covid.positive <- covidmultistatecohorts_db %>% 
  filter(cohort_definition_id==344)

# comorbidity cohorts- add indicator to person table 
# start with A (from Atlas)
person<-person %>% 
  left_join(covidmultistatecohorts_db %>% 
              filter(cohort_definition_id==331) %>% 
              select(subject_id) %>% 
              mutate(a_autoimmune_condition=1) %>% 
              distinct() %>% 
              collect(),
            by=c("person_id"="subject_id"))

person<- person %>% 
  left_join(covidmultistatecohorts_db %>% 
              filter(cohort_definition_id==337) %>% 
              select(subject_id) %>% 
              mutate(a_chronic_kidney_disease=1) %>% 
              distinct() %>% 
              collect(),
            by=c("person_id"="subject_id"))

person <- person %>% 
  left_join(covidmultistatecohorts_db %>% 
              filter(cohort_definition_id==333) %>% 
              select(subject_id) %>% 
              mutate(a_copd=1) %>% 
              distinct() %>% 
              collect(),
            by=c("person_id"="subject_id"))

person <- person %>% 
  left_join(covidmultistatecohorts_db %>% 
              filter(cohort_definition_id==334) %>% 
              select(subject_id) %>% 
              mutate(a_dementia=1) %>% 
              distinct() %>% 
              collect(),
            by=c("person_id"="subject_id"))

person <- person %>% 
  left_join(covidmultistatecohorts_db %>% 
              filter(cohort_definition_id==315) %>% 
              select(subject_id) %>% 
              mutate(a_heart_disease=1) %>% 
              distinct() %>% 
              collect(),
            by=c("person_id"="subject_id"))

person<-person %>% 
  left_join(covidmultistatecohorts_db %>% 
              filter(cohort_definition_id==316) %>% 
              select(subject_id) %>% 
              mutate(a_hyperlipidemia=1) %>% 
              distinct() %>% 
              collect(),
            by=c("person_id"="subject_id"))

person<-person %>% 
  left_join(covidmultistatecohorts_db %>% 
              filter(cohort_definition_id==312) %>% 
              select(subject_id) %>% 
              mutate(a_hypertension=1) %>% 
              distinct() %>% 
              collect(),
            by=c("person_id"="subject_id"))

person<-person %>% 
  left_join(covidmultistatecohorts_db %>% 
              filter(cohort_definition_id==336) %>% 
              select(subject_id) %>% 
              mutate(a_t2_diabetes=1) %>% 
              distinct() %>% 
              collect(),
            by=c("person_id"="subject_id"))

person<-person %>% 
  left_join(covidmultistatecohorts_db %>% 
              filter(cohort_definition_id==345) %>% 
              select(subject_id) %>% 
              mutate(a_obesity=1) %>% 
              distinct() %>% 
              collect(),
            by=c("person_id"="subject_id"))

 # person<-person %>% 
# left_join(covidmultistatecohorts_db %>% 
#             filter(cohort_definition_id==319) %>% 
#             select(subject_id) %>% 
#              mutate(a_malignancy=1) %>% 
#              distinct() %>% 
#              collect(),
#            by=c("person_id"="subject_id"))

# hospital-acquired infection
person<-person %>% 
  left_join(covidmultistatecohorts_db %>% 
              filter(cohort_definition_id==491) %>% 
              select(subject_id) %>% 
              mutate(a_hai=1) %>% 
              distinct() %>% 
              collect(),
            by=c("person_id"="subject_id"))


# rm(covidmultistatecohorts_db)

# 


# if missing a value (ie not included in cohort), zero
person<-person %>% 
  mutate(a_autoimmune_condition=ifelse(is.na(a_autoimmune_condition),0,a_autoimmune_condition)) %>% 
  mutate(a_chronic_kidney_disease=ifelse(is.na(a_chronic_kidney_disease),0,a_chronic_kidney_disease)) %>% 
  mutate(a_copd=ifelse(is.na(a_copd),0,a_copd)) %>% 
  mutate(a_dementia=ifelse(is.na(a_dementia),0,a_dementia)) %>% 
  mutate(a_heart_disease=ifelse(is.na(a_heart_disease),0,a_heart_disease)) %>% 
  mutate(a_hyperlipidemia=ifelse(is.na(a_hyperlipidemia),0,a_hyperlipidemia))%>% 
  mutate(a_hypertension=ifelse(is.na(a_hypertension),0,a_hypertension))%>% 
  mutate(a_t2_diabetes=ifelse(is.na(a_t2_diabetes),0,a_t2_diabetes))%>% 
  mutate(a_obesity=ifelse(is.na(a_obesity),0,a_obesity)) %>%
#  mutate(a_malignancy=ifelse(is.na(a_malignancy),0,a_malignancy)) %>%
  mutate(a_hai=ifelse(is.na(a_hai),0,a_hai))

# add cancer types variables
table(person$c_status) 

# first hematological malignancies
hematological.codes <- c("C81", "C82",	"C83",	"C84",	"C85",	"C86",	"C88",	"C90",
                         "C91",	"C92",	"C93",	"C94",	"C95",	"C96")

person$hematological <- ifelse(person$icd10 %in% hematological.codes, 1, 0)
person$nhl<- ifelse(person$cancer_category =="Non-Hodgkin lymphoma", 1, 0)
person$leukemia <- ifelse(person$cancer_category =="Leukemia", 1, 0)
person$myeloma <- ifelse(person$cancer_category =="Multiple myeloma", 1, 0)
person$hodgkin <- ifelse(person$cancer_category =="Hodgkin lymphoma", 1, 0)

# adding C96 as NHL # as Martina's paper
person$nhl_others <- ifelse((person$cancer_category =="Non-Hodgkin lymphoma" | person$icd10 == "C96"), 1, 0)
person$other_hematological <-ifelse((person$hematological==1 & 
                                      person$nhl==0 & 
                                      person$leukemia==0 & 
                                      person$myeloma==0 & 
                                      person$hodgkin==0),1,0)

# solid cancers - using previously validated cancer definitions
# protate C61
# lung C34
# bladder C67
# stomach  C16
# cervix C53
# corpus uteri C54
# ovary C56
# kidney C64
# liver C22
# pancreas C25
# thyroid C73
#  larynx C32


person$breast <- ifelse(person$cancer_category =="Breast", 1, 0)
person$prostate <- ifelse(person$icd10=="C61", 1, 0)

colorectal.codes <- c("C18", "C19", "C20", "C21") 
person$colorectal <- ifelse(person$icd10 %in% colorectal.codes, 1, 0)
person$bladder <- ifelse(person$icd10=="C67", 1, 0)
person$lung <- ifelse(person$icd10=="C34", 1, 0)
person$lung_trachea <- ifelse((person$icd10=="C34" | person$icd10=="C33"), 1, 0) 
person$uterus <- ifelse((person$icd10=="C54" |person$icd10=="C55"), 1, 0)
person$kidney <- ifelse(person$icd10=="C64", 1, 0)
person$melanoma <- ifelse(person$cancer_category =="Melanoma of skin", 1, 0)
person$thyroid <- ifelse(person$icd10=="C73", 1, 0)
person$lopc <- ifelse(person$cancer_category =="Lip, oral cavity and pharynx", 1, 0)
person$brain<- ifelse(person$cancer_category =="Eye and SNC", 1, 0)

pituitary.codes <- c("C75.1", "C75.2", "C75.3")
person$brain_pit<- ifelse((person$cancer_category =="Eye and SNC" | person$condition_source_value %in% pituitary.codes),
                           1, 0) # add pituitary C75.1-C75.3
person$stomach <- ifelse(person$icd10=="C16", 1, 0)
person$ovary<- ifelse(person$icd10=="C56", 1, 0)
person$larynx <- ifelse(person$icd10=="C32", 1, 0)
person$liver <- ifelse(person$icd10=="C22", 1, 0)
person$cervix <- ifelse(person$icd10=="C53", 1, 0)
person$pancreas <- ifelse(person$icd10=="C25", 1, 0)
person$esophagus <- ifelse(person$icd10=="C15", 1, 0)
person$gallbladder <- ifelse((person$icd10=="C23" |person$icd10== "C24"), 1, 0)
person$testis<- ifelse(person$icd10=="C62", 1, 0)
person$bone <- ifelse((person$icd10=="C40" | person$icd10=="C41") , 1, 0)
person$unspecified <- ifelse(person$icd10=="C76" | person$icd10 =="C80", 1, 0)
person$more_one <- ifelse(person$cancer_category =="More than one primary cancer", 1, 0)
person$cancer <- ifelse(person$c_status== "cancer-free",0,1)

# variable for solid cancer, hematological cancer, free 
# note: more than one cancer is included in solid cancer
person <- person %>%
 mutate(solid=ifelse((hematological==0 & cancer==1),1,0)) %>%
mutate(hem_solid=ifelse(hematological==1,"Hematological",ifelse(solid==1, "Solid", "cancer-free")))  

# variable or other solid cancers
person <- person %>%
  mutate(other_solid=ifelse(solid==1 &
                            breast==0 &
                            prostate==0 & 
                            colorectal==0 & 
                            bladder==0 &
                            lung_trachea==0 &
                            uterus==0 & 
                            kidney==0 & 
                            melanoma==0 &
                            thyroid==0 & 
                            lopc==0 &
                            brain_pit==0 &
                            stomach==0 & 
                            ovary==0 & 
                            larynx==0 & 
                            liver==0 &
                            cervix==0 & 
                            esophagus==0 & 
                            pancreas==0 & 
                            gallbladder==0 &
                            testis==0 & 
                            bone==0 & 
                            unspecified==0 & 
                            more_one ==0, 1,0))


rm(hematological.codes, colorectal.codes, pituitary.codes)

# replace missing values with 0s

person <- person%>%
          mutate_at(vars(hematological:other_solid), ~replace(., is.na(.), 0))


## smoking status -----

# individuals have more than one record
length(unique(smoking$person_id))/ nrow(smoking)

#values from before 2006?
smoking$days<- as.numeric(difftime(smoking$measurement_date,
                                   as.Date("2006/01/01"),
                                   units="days")) 
sum(smoking$days<0)
sum(smoking$days<0)/nrow(smoking)
# drop these
smoking<-smoking %>% 
  filter(smoking$days>=0)


# drop values after start date
smoking$days<- as.numeric(difftime(smoking$measurement_date,
                                   healthy.start.date))
sum(smoking$days>0)
smoking<-smoking %>% 
  filter(smoking$days<=0)

# smoking %>% 
# ggplot()+
#   geom_histogram(aes(measurement_date))

# keep most recent record for an individual
smoking<-smoking %>% 
  arrange(person_id, desc(measurement_date)) %>% 
  group_by(person_id) %>% 
  mutate(seq=1:length(person_id)) 

smoking <- smoking %>% 
  filter(seq==1) %>% 
  select(-seq)

# add four variants to person
# smoking based on all time, prior 5, prior 2, and prior 1

smoking$days<- as.numeric(difftime(smoking$measurement_date,
                                   healthy.start.date,
                                   units="days") )
hist(smoking$days/365.25)

# smoking 

smoke_5y<- smoking %>% 
  filter(days>-(365.25*5))
smoke_2y<- smoking %>% 
  filter(days>-(365.25*2))
smoke_1y<- smoking %>% 
  filter(days>-(365.25))


person<-person %>% 
  left_join(smoking %>% 
              select(person_id, concept_name, measurement_date) %>%
              rename(smoke.all_time=concept_name,
                     smoke.measurement_date=measurement_date))
person<-person %>% 
  left_join(smoke_5y %>% 
              select(person_id, concept_name) %>%
              rename(smoke.5y=concept_name))
person<-person %>% 
  left_join(smoke_2y %>% 
              select(person_id, concept_name) %>%
              rename(smoke.2y=concept_name))
person<-person %>% 
  left_join(smoke_1y %>% 
              select(person_id, concept_name) %>%
              rename(smoke.1y=concept_name))

prop.table(table(person$smoke.all_time, useNA = "always"))
prop.table(table(person$smoke.5y, useNA = "always"))
prop.table(table(person$smoke.2y, useNA = "always"))
prop.table(table(person$smoke.1y, useNA = "always"))

rm(smoke_1y, smoke_2y, smoke_5y, smoking)

## MEDEA -----
# U1 is quintile 1 of MEDEA which is the least deprived areas, 
# U5 is quintile 5 and represent the most deprived areas. "R" is or rural areas for which we cannot
# calculate MEDEA. And "U" means a person is assigned to a urban area but the quintile of MEDEA is missing.

# no individuals have more than one record
length(unique(MEDEA$person_id))/ nrow(MEDEA)

# drop values "" as missing
MEDEA<-MEDEA %>% 
  filter(value_as_string!="")

# dates
hist(year(MEDEA$observation_date))

# add to person
person<-person %>% 
  left_join(MEDEA %>% 
              select(person_id, value_as_string) %>% 
              rename(medea=value_as_string)  )
rm(MEDEA)

# set "U" to missing
prop.table(table(person$medea, useNA = "always"))

person<-person %>% 
  mutate(medea=ifelse(medea=="U", NA, medea))

prop.table(table(person$medea, useNA = "always"))


# change missing data to new category
person <- person %>%
  mutate(medea=ifelse(is.na(medea), "Missing", as.character(medea)),
         smoke.all_time=ifelse(is.na(smoke.all_time), "Missing", smoke.all_time)) 

## add nationality 
person<-person %>% 
  left_join(nationality %>% 
              select(person_id, value_as_string) %>% 
              rename(nationality=value_as_string))

# create new categories for nationality
person$nationality_cat <- ifelse(person$nationality== "Espanya", "Spain", NA)
person$nationality_cat <- ifelse(str_detect(person$nationality, "Am�rica"), "Central & South America", person$nationality_cat)
person$nationality_cat <- ifelse(str_detect(person$nationality, "Carib"), "Central & South America", person$nationality_cat)
person$nationality_cat <- ifelse(str_detect(person$nationality, "Am�rica del Nord"), "North America", person$nationality_cat)
person$nationality_cat <- ifelse(str_detect(person$nationality, "Europa"), "Europe", person$nationality_cat)
person$nationality_cat <- ifelse(str_detect(person$nationality, "Europa oriental"), "Eastern Europe", person$nationality_cat)
person$nationality_cat <- ifelse(str_detect(person$nationality, "frica"), "Africa", person$nationality_cat)
person$nationality_cat <- ifelse(str_detect(person$nationality, "�sia"), "Asia", person$nationality_cat)
person$nationality_cat <- ifelse(str_detect(person$nationality, "Melan�sia"), "Oceania", person$nationality_cat)
person$nationality_cat <- ifelse(str_detect(person$nationality, "Polin�sia"), "Oceania", person$nationality_cat)
person$nationality_cat <- ifelse(str_detect(person$nationality, "Micron�sia"), "Oceania", person$nationality_cat)
person$nationality_cat <- ifelse(str_detect(person$nationality, "Austr�lia i Nova Zelanda��"), "Oceania", person$nationality_cat)
person$nationality_cat <- ifelse(str_detect(person$nationality, "Austr�lia i Nova Zelanda"), "Oceania", person$nationality_cat)

# add oceania to asia
person$nationality_cat <- ifelse(str_detect(person$nationality_cat, "Oceania"), "Asia", person$nationality_cat)
person$nationality_cat <- ifelse(str_detect(person$nationality_cat, "Asia"), "Asia & Oceania", person$nationality_cat)

# join north america with western europe
person$nationality_cat <- ifelse((person$nationality_cat=="North America" |
                                   person$nationality_cat=="Europe"), "Europe & North America", person$nationality_cat)

rm(nationality)

# alcohol
# keep most recent observation per person
alcohol <- alcohol %>% 
  arrange(person_id, desc(observation_date)) %>% 
  group_by(person_id) %>% 
  mutate(seq=1:length(person_id)) 

alcohol <- alcohol %>% 
  filter(seq==1) %>% 
  select(-seq)

## categories for alcohol
# no risk -> 4125550 (Concept id) -> "Very low"
#  low risk -> 4267416 (Concept id) -> "Low"
# high risk -> 4328749 (Concept id) -> "High
alcohol <- alcohol %>%
  mutate(alcohol=ifelse(value_as_concept_id==4125550, "No risk", 
                        ifelse(value_as_concept_id==4267416 ,"Low risk",
                               ifelse(value_as_concept_id==4328749 ,"High risk", NA))))

person<-person %>% 
  left_join(alcohol %>% 
              select(person_id, alcohol)) 

# add missing category
person <- person %>%
  mutate(alcohol=ifelse(is.na(alcohol), "Missing", alcohol))

rm(alcohol)

## variable for age at cancer diagnosis
# years since cancer diagnosis
summary(person$years)

# some observations are very extreme - cancers diagnosed more than 80y ago

# check no one was dx with cancer before date of birth 
person$dob <- paste(person$day_of_birth, 
                    person$month_of_birth, 
                    person$year_of_birth, 
                    sep="-") %>% dmy() %>%  as.Date()

nrow(person %>% filter(data_cancer_dx<dob)) # 0

# variable for age when diagnosed
person$age_cancer_dx <- round(((person$data_cancer_dx-person$dob)/365.25), digits=2) %>% as.numeric()
person$dob <- NULL

# save.image(file='1 data prep/prior_transitions.RData')
#  load('1 data prep/prior_transitions.RData')


  
########## 

# each cohort individuals should be unique ----
nrow(person)/length(person$person_id)
nrow(covid.diagnosis)/length(covid.diagnosis$subject_id)
nrow(covid_hospitalised)/length(covid_hospitalised$subject_id)

# Exclusions
# for healthy pop, exclude if prior postitive test for covid ----

covid.positive.before_start <- covid.positive %>% 
                              filter(cohort_start_date<tar.start.date) # 1 patient

person <- person %>% 
        anti_join(covid.positive.before_start %>% select(subject_id),
            by=c("person_id"="subject_id")) 

exclusion_table<-rbind(exclusion_table,
                       c(nrow(person), 
                         paste0("With COVID-19 test prior to ",tar.start.date)))


# for healthy pop, exclude if prior diagnosis of covid -----
diag.before.march<-covid.diagnosis %>% 
  filter(cohort_start_date<tar.start.date) 
nrow(diag.before.march) # # 312

person<-person %>% 
  anti_join(diag.before.march,
            by=c("person_id"="subject_id"))

exclusion_table<-rbind(exclusion_table,
                       c(nrow(person), 
                         paste0("With COVID-19 diagnosis prior to ",tar.start.date)))


# are diagnoses linked to primary care visits ----
a<-covid.diagnosis %>% 
  left_join(gp.visits,
            by=c("subject_id"="person_id"))
a<-  a %>% 
  filter(cohort_start_date==visit_start_date) %>% 
  select(subject_id) %>% 
  distinct()

nrow(a)/ nrow(covid.diagnosis)

# for healthy pop, exclude if prior hospitalisation with covid -----
hosp.before.march<-covid_hospitalised %>% 
  filter(cohort_start_date<tar.start.date) 

nrow(hosp.before.march) #16 patients

person<-person %>% 
  anti_join(hosp.before.march,
            by=c("person_id"="subject_id"))

exclusion_table<-rbind(exclusion_table,
                       c(nrow(person), 
                         paste0("With COVID-19 hospitalisation prior to ",tar.start.date)))


# for healthy pop, exclude if in hospital on 1st March -----
hospi_before_start <- any.hospital %>% 
  filter(visit_end_date>= tar.start.date) %>% # end date from march 1st
  filter(visit_start_date<= tar.start.date)%>%  # and start  date from march 1st
  select(person_id)%>% 
  distinct() 

nrow(hospi_before_start) #  1248

person<-person %>% 
  anti_join(hospi_before_start %>% select("person_id"),
            by="person_id")

exclusion_table<-rbind(exclusion_table,
                       c(nrow(person), 
                         paste0("Hospitalisation on ",tar.start.date)))


# get length of hospitalisation
# concatanate into CIPS
# first, create index for visits - overlapping visits given same index
hospitalisations <-  any.hospital %>% 
                     arrange(person_id, visit_start_date) %>%
                     group_by(person_id) %>%
                     mutate(indx = c(0, 
                                     cumsum(as.numeric(lead(visit_start_date)) >
                                     cummax(as.numeric(visit_end_date)))[-n()]))  %>%
                    select(person_id, visit_start_date, visit_end_date, indx)

# next, put visits together - min start, max end date from overlapping visits 
hospitalisations<- hospitalisations %>% 
                   group_by(person_id, indx) %>%
                   summarise(visit_start_date = min(visit_start_date), 
                   visit_end_date = max(visit_end_date)) %>% 
                   select(-indx)

# get CIP of index hospitalisation

hospitalisations <- covid_hospitalised %>% 
                   select(subject_id,cohort_start_date) %>% 
                    left_join(hospitalisations, 
                              by=c("subject_id"="person_id"))

hospitalisations <- hospitalisations %>% 
                    mutate(visit_interval=interval(visit_start_date,visit_end_date)) %>% 
                    mutate(within=ifelse((cohort_start_date %within% visit_interval)==TRUE, "Yes", "No"  ))

hospitalisations <- hospitalisations %>% 
                    mutate(los=as.numeric(difftime(
                    visit_end_date,
                    visit_start_date, 
                    units="days")))

hospitalisations <- hospitalisations %>%
                    select(subject_id, cohort_start_date, visit_start_date, visit_end_date, los, within) %>%
                    distinct()

hospitalisations <- hospitalisations %>% 
                    filter(within=="Yes") %>%
                    distinct()

# check we only have the first hospitalisation per person
nrow(hospitalisations)
length(unique(hospitalisations$subject_id))

## add los to covid_hospitalised
covid_hospitalised <- covid_hospitalised %>% 
                      left_join(hospitalisations %>%
                      select(subject_id, los),
                      by="subject_id")

##table with hospitalisations 0 days 
covid_hospi_0 <- covid_hospitalised %>%
                filter(los==0)

# keep only hospitalisations >0 days
covid_hospitalised <- covid_hospitalised %>% 
                   filter(los>0) # now 14236 hospitalisations
  

rm(any.hospital)
rm(hospitalisations)

# exlude those living in a care home ----
# individuals in a care home
person<- person %>% 
         anti_join(care_home %>%
         select("person_id"),
             by="person_id")


exclusion_table<-rbind(exclusion_table,
                       c(nrow(person), 
                         paste0("Care h ",tar.start.date)))


rm(observation, care_home)

# for covid diagnosis, remove diagnosis if it came after hospitalisation  -----
# given the multistate model structure, we're looking for diagnoses before hospitalisation
diag.after.hosp <- covid.diagnosis %>% 
                   inner_join(covid_hospitalised,
                   by="subject_id",
                   suffix = c(".diag", ".hosp")) # those in hospitalised

diag.after.hosp <- diag.after.hosp %>% 
                   mutate(days=as.numeric(difftime(
                                          cohort_start_date.diag, 
                                          cohort_start_date.hosp, 
                                          units="days")))

sum(diag.after.hosp$days>=0) #5969

diag.after.hosp<-diag.after.hosp %>% 
                 filter(days>=0) %>%  # diag after hospitalisation
                 select(subject_id)

covid.diagnosis<-covid.diagnosis %>% 
                 anti_join(diag.after.hosp,
                  by="subject_id")

rm(diag.after.hosp)

# check diagnosis after death
diag.after.death <- covid.diagnosis %>% 
  inner_join(death,
             by="subject_id",
             suffix = c(".diag", ".death")) # those in death

diag.after.death <- diag.after.death %>% 
  mutate(days=as.numeric(difftime(
    cohort_start_date.diag, 
    cohort_start_date.death, 
    units="days")))

diag.after.death<-diag.after.death %>% 
  filter(days>0) %>%  # diag after death
  select(subject_id)

covid.diagnosis<-covid.diagnosis %>% 
  anti_join(diag.after.death,
            by="subject_id")

rm(diag.after.death)



# # check cancers diagnosed before SIDIAP started data collection -
# the date of dx for those might be more prone to error 

# the date when SIDIAP began collecting data
start.sidiap <- as.Date("01/01/2006",
                        "%d/%m/%Y" )

nrow(person %>% filter(data_cancer_dx < start.sidiap)) # 52276
# cancers diagnosed before start observation period
nrow(person %>% filter(data_cancer_dx<observation_period_start_date)) # 54365

rm(start.sidiap)



##### 
# covid.data -----


# our row per person dataframe 
covid.diagnosis<-covid.diagnosis %>% 
                 select("subject_id",  "cohort_start_date")

names(covid.diagnosis)<-c("person_id", 
                          "covid_diagnosis_date")

# combine person and diagnosis
covid.data<-person %>% 
            left_join(covid.diagnosis,
            by="person_id")
rm(person)

#combine with hospitalised
# only persons with at least one night stay
nrow(covid_hospitalised) # 14236

covid_hospitalised <- covid_hospitalised %>% 
  select("subject_id",  "cohort_start_date", "los")

names(covid_hospitalised)<-c("person_id", 
                             "covid_hospitalised_date", "los_hospi")
covid.data<- covid.data %>% 
  left_join(covid_hospitalised,
            by="person_id")

rm(covid_hospitalised)


# add deaths -----
# only those in covid.data

# before start date
death.before.start <- death %>% 
                      filter(cohort_start_date<={{healthy.start.date}}) %>% 
                      inner_join(covid.data ,
                      by=c("subject_id"="person_id"))

nrow(death.before.start) #0

 covid.data<-covid.data %>% 
             anti_join(death.before.start %>% select(subject_id),
             by=c("person_id"="subject_id"))

exclusion_table<-rbind(exclusion_table,
                       c(nrow(covid.data), 
                         paste0("check (=0) Death before ",healthy.start.date)))

save(exclusion_table, file='1 data prep/exclusion.RData')

rm(exclusion_table)


# any deaths after tar end?
sum(death$cohort_start_date>{{tar.end.date}}) #0


covid.data<-covid.data %>% 
            left_join(death %>% 
              select(subject_id,cohort_start_date) %>% 
              rename(death_date=cohort_start_date),
            by=c("person_id"="subject_id"))



# check that there were no events after 5th May -----
sum(covid.data$covid_diagnosis_date>tar.end.date, na.rm = T) #0
sum(covid.data$covid_hospitalised_date>tar.end.date, na.rm = T) #0
sum(covid.data$death_date>tar.end.date, na.rm = T) #0
# all looks ok



# add indicator variable  #####
covid.data$covid_diagnosis_status<-ifelse(!is.na(covid.data$covid_diagnosis_date),
                                          1,0)
covid.data$covid_hospitalised_status<-ifelse(!is.na(covid.data$covid_hospitalised_date),
                                             1,0)

covid.data$death_status<-ifelse(is.na(covid.data$death_date),
                                0,1)

# censor at 5th May or end of observation period, whichever comes first -----

# if event, date of event
# if no event,  censor at tar.end.date or end of observation period, whichever comes first
covid.data<-covid.data %>%
  mutate(covid_diagnosis_date=if_else(covid_diagnosis_status==1,
                                      covid_diagnosis_date, 
                                      if_else(observation_period_end_date < {{tar.end.date}},
                                              observation_period_end_date, {{tar.end.date}})))

covid.data<-covid.data %>%
  mutate(covid_hospitalised_date=if_else(covid_hospitalised_status==1,
                                         covid_hospitalised_date, 
                                         if_else(observation_period_end_date < {{tar.end.date}},
                                                 observation_period_end_date, {{tar.end.date}})))
covid.data<-covid.data %>%
  mutate(death_date=if_else(death_status==1,
                            death_date, 
                            if_else(observation_period_end_date < {{tar.end.date}},
                                    observation_period_end_date, {{tar.end.date}})))


# time to event -----

# time from 
covid.data<-covid.data %>% 
  mutate(covid_diagnosis_time= as.numeric(difftime(covid_diagnosis_date,
                                                   {{healthy.start.date}}, 
                                                   units="days")))
covid.data<-covid.data %>% 
  mutate(covid_hospitalised_time= as.numeric(difftime(covid_hospitalised_date,
                                                      {{healthy.start.date}}, 
                                                      units="days")))
covid.data<-covid.data %>% 
  mutate(death_time= as.numeric(difftime(death_date,
                                         {{healthy.start.date}}, 
                                         units="days")))
# quantile(covid.data$covid_diagnosis_time)
# quantile(covid.data$covid_hospitalised_time)
# quantile(covid.data$death_time)

# do we have transitions on the same day  -------
# eg someone getting diagnosed and dying on the same day

nrow(covid.data %>% 
       filter(covid_diagnosis_status==1) %>% 
       filter(covid_hospitalised_status==1) %>% 
       filter(covid_diagnosis_time==covid_hospitalised_time))

nrow(covid.data %>% 
       filter(covid_diagnosis_status==1) %>% 
       filter(death_status==1) %>% 
       filter(covid_diagnosis_time==death_time))  # 41 deaths on same day as diagnosis

nrow(covid.data %>% 
       filter(covid_hospitalised_status==1) %>% 
       filter(death_status==1) %>% 
       filter(covid_hospitalised_time==death_time)) # 1 death on same date as hospitalised

# deaths on the same day as diagnosis: drop diagnosis  -------
# where diagnoses are seen on the same day as a diagnosis, we can not be sure about the meaning of this diagnosis
# (it does not necessarily mean someone was diagnosed in the morning and died in the afternoon, but could also be 
# directly tied to the reporting of death)
# so we will drop these diagnoses

diag.death<-covid.data %>% 
  filter(covid_diagnosis_status==1) %>% 
  filter(death_status==1) %>% 
  filter(covid_diagnosis_time==death_time) #41individuals

diag.death$covid_diagnosis_status<-0

covid.data<-covid.data %>% 
             anti_join(diag.death %>% 
              select(person_id),
               by="person_id")

covid.data<-rbind(covid.data, diag.death)

nrow(covid.data %>% 
       filter(covid_diagnosis_status==1) %>% 
       filter(death_status==1) %>% 
       filter(covid_diagnosis_time==death_time))  




## Length of hospitalisation is at least one day
# so death should not occur on hospitalization day
# there is an individual that died on the same day as hospitalization but was discharged a day after being hospitalized
# we add 0.5 to the death

hosp.death<-covid.data %>% 
            filter(covid_hospitalised_status==1) %>% 
            filter(death_status==1) %>% 
            filter(covid_hospitalised_time==death_time)

 nrow(hosp.death) #1
 
 

hosp.death$death_time<-hosp.death$death_time+0.5

covid.data<-covid.data %>% 
            anti_join(hosp.death %>% 
            select(person_id),
            by="person_id")

covid.data<-rbind(covid.data, hosp.death)

nrow(covid.data %>% 
       filter(covid_hospitalised_status==1) %>% 
       filter(death_status==1) %>% 
       filter(covid_hospitalised_time==death_time)) 



## exploring how many persons died with 0 days of hospitalisation 
a <- covid.data %>%
  filter(death_status==1)

b <- left_join(a,
               covid_hospi_0,
               by=c("person_id"="subject_id")) %>%
       select(-(age:age_cancer_dx))

table(b$cohort_definition_id) # 1058
table(b$covid_diagnosis_status, b$cohort_definition_id) # Of this, 224 did not have an outpatient diagnosis, 834 did
table(b$covid_diagnosis_status, b$cohort_definition_id, b$cancer) # among those diagnosed with COVID-19. 173 had cancer (20.7%)

write.csv2(b, "results/exploring_outpatient_deaths.csv")
b <- left_join(covid.data,
               covid_hospi_0,
               by=c("person_id"="subject_id"))
# how many persons went to the hospital for 0 days
table(b$cohort_definition_id) # 4446 
table(b$covid_diagnosis_status, b$cohort_definition_id) # of this, 4005 had an outpatient covid diagnosis


# create variable for cancer_groups - to use for Multiple imputation
# generate a dataframe including only the dummy variables we are interested in (and the ID)
# create also a new dummy variable for individuals without cancer
create_cancer_group <- covid.data %>%
  mutate(no_cancer=ifelse(cancer==0, 1, 0)) %>%
  select(person_id, nhl:more_one, other_solid, -nhl_others, -brain,-lung, no_cancer)

# create variable - each category name will have the name of the dummy variable which has the maximum value 
# -1 to not consider person_id
create_cancer_group$cancer_group <-  names(create_cancer_group[,-1])[max.col(create_cancer_group[,-1])]
create_cancer_group <- create_cancer_group %>%
  select(person_id, cancer_group)

covid.data <- covid.data %>%
  left_join(create_cancer_group, by="person_id")
# tidy up workspace ----- 

rm(list= ls()[!(ls() %in% c('covid.data'))])

## years since cancer diagnosis (c_status) in 2 groups - for solid cancers
covid.data <- covid.data %>%
  mutate(c_status_2cat = if_else(c_status == "< 1 year" | c_status == "1-5 years", "<5 years",
                            if_else(c_status == ">5years", ">5years", "cancer-free")))


# save so can start from here if needed
# save.image(file='1 data prep/up_to_covid.RData')

 # load('1 data prep/up_to_covid.RData')




## set up for cause specific survival models------ 

tmat <- matrix(NA, 4, 4)
dimnames(tmat) <- list(from = c("healthy","diagnosed","hospitalised","death"), 
                       to = c("healthy","diagnosed","hospitalised","death"))
tmat[1, 2:4]<- 1:3
tmat[2, 3:4]<- 4:5
tmat[3, 4]<- 6

r<-msprep(time = c(NA, 
                   "covid_diagnosis_time",
                   "covid_hospitalised_time", 
                   "death_time"), 
          status = c(NA, 
                     "covid_diagnosis_status",
                     "covid_hospitalised_status",
                     "death_status"), 
          id="person_id",
          data = as.data.frame(covid.data),  
          trans = tmat)

events(r)

# add age, age group, gender, cancer status, etc ------
covid.data<-covid.data %>% 
  mutate(age_gr=ifelse(age<18, 
                       "Under 18",
                       ifelse(age>=18 &
                                age<=39, 
                              "18 to 39",
                              ifelse(age>=40 &
                                       age<=59, 
                                     "40 to 59",
                                     ifelse(age>=60 &
                                              age<=69, 
                                            "60 to 69",
                                            ifelse(age>=70 &
                                                     age<=79, 
                                                   "70 to 79",      
                                                   ifelse(age>=80, 
                                                          "80 or older",  
                                                          NA)))))))
table(covid.data$age_gr, useNA = "always")

covid.data <- covid.data %>%
  mutate(los_hospi_cat= ifelse(los_hospi<=3, "3 days or less",
                               ifelse(los_hospi<=7, ">3 to 7 days",
                                      ifelse(los_hospi<=14, ">7 to 14days",
                                             ifelse(los_hospi<=30, ">14 to 30 days", "More than 30 days")))))

  # as factors----
covid.data$age_gr <- factor(covid.data$age_gr, 
                            levels = c("Under 18","18 to 39", "40 to 59", "60 to 69",
                                       "70 to 79", "80 or older"))
covid.data$gender <- factor(covid.data$gender, 
                            levels = c("Male", "Female"))

covid.data$c_status<- factor(covid.data$c_status, 
                           levels = c("cancer-free", ">5years", "1-5 years", "< 1 year"))

covid.data$c_status_2cat <- factor(covid.data$c_status_2cat, 
                             levels = c("cancer-free", ">5years", "<5 years"))

covid.data$medea <- factor(covid.data$medea,
                           levels = c("U1", "U2", "U3", "U4", "U5", "R", "Missing")) 

covid.data$smoke.all_time <-   factor(covid.data$smoke.all_time, 
                               levels = c("Never smoker", "Former smoker", "Current some day smoker", "Missing")) 

covid.data$hem_solid <- factor(covid.data$hem_solid,
                               levels = c("cancer-free", "Solid", "Hematological"))

covid.data$alcohol <- factor(covid.data$alcohol,
                               levels = c("No risk", "Low risk", "High risk", "Missing"))
  
covid.data$nationality_cat <- factor(covid.data$nationality_cat,
                             levels = c("Spain", "Europe & North America", "Eastern Europe",
                                        "Central & South America", "Asia & Oceania", "Africa"))


covid.data$los_hospi_cat <-   factor(covid.data$los_hospi_cat, 
                                     levels =   c("3 days or less", 
                                    ">3 to 7 days",
                                    ">7 to 14days",
                                    ">14 to 30 days",
                                    "More than 30 days")) 

  # add vars to r dataframes ----
  r <- as.data.frame(r) %>% 
       left_join(covid.data,
              by="person_id")


# split r by transition ------
r.healthy.diagnosis <- r %>%
  filter(trans==1) %>% # transition from healthy to diagnosis
  select(-from, -to, -trans, -Tstart, -Tstop)

r.healthy.hospitalised <- r %>%
  filter(trans==2) %>% # transition from healthy to hospitalised
  select(-from, -to, -trans, -Tstart, -Tstop)

r.healthy.death<-r %>%
  filter(trans==3) %>% # transition from healthy to death
  select(-from, -to, -trans, -Tstart, -Tstop)

r.diagnosis.hospitalised<-r %>%
  filter(trans==4) %>% # traonsition from diagnosis to hospitalised
  select(-from, -to, -trans, -Tstart, -Tstop)

r.diagnosis.death<-r %>%
  filter(trans==5) %>% # transition from diagnosis to death
  select(-from, -to, -trans, -Tstart, -Tstop)

r.hospitalised.death<-r %>%
  filter(trans==6) %>% # transition from hospitalised to death
  select(-from, -to, -trans, -Tstart, -Tstop)


# add variables for estimating cumulative incidence-----

# from healthy
healthy_c.event<-rbind(
  r.healthy.diagnosis %>% 
    filter(status==1) %>% 
    select(person_id, time) %>% 
    mutate(healthy_c.event=1),
  r.healthy.hospitalised %>% 
    filter(status==1) %>% 
    select(person_id,time) %>% 
    mutate(healthy_c.event=2),
  r.healthy.death %>% 
    filter(status==1) %>% 
    select(person_id,time) %>% 
    mutate(healthy_c.event=3)) %>% 
  rename(healthy_c.time=time)
#nrow(healthy_c.event)/length(unique(healthy_c.event$person_id))

# add those without event 
healthy_c.event<-rbind(healthy_c.event,
                       r.healthy.diagnosis %>% 
                         anti_join(healthy_c.event, by="person_id") %>% 
                         select(person_id, time) %>% 
                         mutate(healthy_c.event=0) %>% 
                         rename(healthy_c.time=time))
#nrow(healthy_c.event)/length(unique(healthy_c.event$person_id))

covid.data <-  covid.data %>% 
  left_join(healthy_c.event, by="person_id")


# from diagnosis
diagnosis_c.event<-rbind(
  r.diagnosis.hospitalised %>% 
    filter(status==1) %>% 
    select(person_id, time) %>% 
    mutate(diagnosis_c.event=1),
  r.diagnosis.death %>% 
    filter(status==1) %>% 
    select(person_id,time) %>% 
    mutate(diagnosis_c.event=2)) %>% 
  rename(diagnosis_c.time=time)
#nrow(diagnosis_c.event)/length(unique(diagnosis_c.event$person_id))
# add those without event 
diagnosis_c.event<-rbind(diagnosis_c.event,
                         r.diagnosis.hospitalised %>% 
                           anti_join(diagnosis_c.event, by="person_id") %>% 
                           select(person_id, time) %>% 
                           mutate(diagnosis_c.event=0) %>% 
                           rename(diagnosis_c.time=time))
#nrow(diagnosis_c.event)/length(unique(diagnosis_c.event$person_id))

covid.data <-
  covid.data %>% 
  left_join(diagnosis_c.event, by="person_id")






# exclude those with zero days time at risk from specific transitions-----

# we don't have anyone with zero days time at risk for the first set
# of transitions by design 
# everyone has tar from starting state
sum(r.healthy.diagnosis$time==0)
sum(r.healthy.hospitalised$time==0)
sum(r.healthy.death$time==0)

# but we can have people with no time at risk for the later transitions
# i.e. someone who got a diagnosis on the last day of follow up
# so, for our cause-specific survival data we use for subsequent modelling
# we'll drop that individual from that specific dataframe
# (but, note, we're still capturing and reporting their diagnosis, as per the example above)
sum(r.diagnosis.hospitalised$time==0)
r.diagnosis.hospitalised<-r.diagnosis.hospitalised %>% 
  filter(time>0)

sum(r.diagnosis.death$time==0)
r.diagnosis.death<-r.diagnosis.death %>% 
  filter(time>0)

sum(r.hospitalised.death$time==0)
r.hospitalised.death<-r.hospitalised.death %>% 
  filter(time>0)



# save -----
save(list=c("covid.data",
            "r",
            "r.diagnosis.death",
            "r.diagnosis.hospitalised",
            "r.healthy.death"  ,
            "r.healthy.diagnosis",
            "r.healthy.hospitalised",
            "r.hospitalised.death" ),
     file = "1 data prep/working.data.Rda")

