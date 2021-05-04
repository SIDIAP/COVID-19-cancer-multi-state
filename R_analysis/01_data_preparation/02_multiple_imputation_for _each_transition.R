
# packages ------
library(dplyr)
library(ggplot2)
library(rms)

setwd("M:/Elena - R/Multi-state  cancer/")

# data -----
load("M:/Elena - R/Multi-state  cancer/1 data prep/working.data.Rda")


e <- eapply(.GlobalEnv, function(x) {
  x <- x %>%
  mutate(medea = na_if(medea, "Missing")) %>%
  mutate(smoke.all_time = na_if(smoke.all_time, "Missing"))
  output <- x
}
)

nms <- names(e)
for ( n in nms ){
  assign(n, e[[n]])
}
### run multiple imputation ------
# we have missing data in smoking, and MEDEA
sum(is.na(covid.data$smoke.all_time))/nrow(covid.data)
sum(is.na(covid.data$medea))/nrow(covid.data)

r.healthy.diagnosis$medea<-droplevels(r.healthy.diagnosis$medea)
r.healthy.hospitalised$medea<-droplevels(r.healthy.hospitalised$medea)
r.diagnosis.hospitalised$medea<-droplevels(r.diagnosis.hospitalised$medea)
r.diagnosis.death$medea<-droplevels(r.diagnosis.death$medea)
r.hospitalised.death$medea<-droplevels(r.hospitalised.death$medea)

r.healthy.diagnosis$smoke.all_time<-droplevels(r.healthy.diagnosis$smoke.all_time)
r.healthy.hospitalised$smoke.all_time<-droplevels(r.healthy.hospitalised$smoke.all_time)
r.diagnosis.hospitalised$smoke.all_time<-droplevels(r.diagnosis.hospitalised$smoke.all_time)
r.diagnosis.death$smoke.all_time<-droplevels(r.diagnosis.death$smoke.all_time)
r.hospitalised.death$smoke.all_time<-droplevels(r.hospitalised.death$smoke.all_time)



# 
print("Getting r.healthy.diagnosis.imp")
start<-Sys.time()
r.healthy.diagnosis.imp <- aregImpute(~
    time+ status+ cancer_group+
          gender+age+
           smoke.all_time+ medea + a_autoimmune_condition+
        a_chronic_kidney_disease+a_copd+
        a_dementia+ a_heart_disease+
        a_hyperlipidemia+ a_hypertension+
         a_t2_diabetes + a_obesity, 
           r.healthy.diagnosis,
           n.impute=5,  
           B=5, nk=0,
           match='closest',
           type="pmm")
save(list=c("r.healthy.diagnosis.imp"),
     file = "1 data prep/r.healthy.diagnosis.imp.RData")


## From healthy to hospitalised
Sys.time()-start
print("Getting r.healthy.hospitalised.imp")
start<-Sys.time()
r.healthy.hospitalised.imp <- aregImpute(~
           time+ status+ cancer_group+
           gender+age+
           smoke.all_time+ medea + a_autoimmune_condition+
           a_chronic_kidney_disease+a_copd+
           a_dementia+ a_heart_disease+
           a_hyperlipidemia+ a_hypertension+
             a_t2_diabetes + a_obesity, 
           r.healthy.hospitalised,
           n.impute=5,  
           B=5, nk=0,
           match='closest',
           type="pmm")
save(list=c("r.healthy.hospitalised.imp"),
     file = "1 data prep/r.healthy.hospitalised.imp.RData")
Sys.time()-start



# From healthy to death
print("Getting r.healthy.death.imp")
start<-Sys.time()
r.healthy.death.imp<-aregImpute(~
                                  time+ status+ cancer_group+
                                  gender+age+
                                  smoke.all_time+ medea + a_autoimmune_condition+
                                  a_chronic_kidney_disease+a_copd+
                                  a_dementia+ a_heart_disease+
                                  a_hyperlipidemia+ a_hypertension+
                                  a_t2_diabetes + a_obesity,                                         
                                r.healthy.hospitalised,
                                n.impute=5,  
                                B=5, nk=0,
                                match='closest',
                                type="pmm")
save(list=c("r.healthy.death.imp"),
     file = "1 data prep/r.healthy.death.imp.RData")
Sys.time()-start



## From diagnosed to hospitalised
print("Getting r.diagnosis.hospitalised.imp")
start<-Sys.time()

r.diagnosis.hospitalised.imp <- aregImpute(~
                                           time+ status+ cancer_group+
                                           gender+age+
                                           smoke.all_time+ medea + a_autoimmune_condition+
                                           a_chronic_kidney_disease+a_copd+
                                           a_dementia+ a_heart_disease+
                                           a_hyperlipidemia+ a_hypertension+
                                            a_t2_diabetes + a_obesity,      
           r.diagnosis.hospitalised,
           n.impute=5,  
           B=5, nk=0,
           match='closest',
           type="pmm")
save(list=c("r.diagnosis.hospitalised.imp"),
     file = "1 data prep/r.diagnosis.hospitalised.imp.RData")

Sys.time()-start

## From diagnosed to death

print("Getting r.diagnosis.death.imp")
start<-Sys.time()
r.diagnosis.death.imp<-aregImpute(~
                                    time+ status+ cancer_group+
                                    gender+age+
                                    smoke.all_time+ medea + a_autoimmune_condition+
                                    a_chronic_kidney_disease+a_copd+
                                    a_dementia+ a_heart_disease+
                                    a_hyperlipidemia+ a_hypertension+
                                     a_t2_diabetes + a_obesity,
           r.diagnosis.death,
           n.impute=5,  
           B=5, nk=0,
           match='closest',
           type="pmm")
save(list=c("r.diagnosis.death.imp"),
     file = "1 data prep/r.diagnosis.death.imp.RData")
Sys.time()-start


## From hospitalised to death

## replace cancer_group testits to other solid - because there is only one individual with testis cancer and this gives an error
r.hospitalised.death <- r.hospitalised.death  %>%
  mutate(cancer_group=ifelse(cancer_group=="testis", "other_solid", cancer_group))


print("Getting r.hospitalised.death.imp")
start<-Sys.time()
r.hospitalised.death.imp<-aregImpute(~
                                       time+ status+ cancer_group+
                                       gender+age+
                                       smoke.all_time+ medea + a_autoimmune_condition+
                                       a_chronic_kidney_disease+a_copd+
                                       a_dementia+ a_heart_disease+
                                       a_hyperlipidemia+ a_hypertension+
                                        a_t2_diabetes + a_obesity,
           r.hospitalised.death,
           n.impute=5, 
           B=5, nk=0,
           match='closest',
           type="pmm")
save(list=c("r.hospitalised.death.imp"),
     file = "1 data prep/r.hospitalised.death.imp.RData")
Sys.time()-start




















