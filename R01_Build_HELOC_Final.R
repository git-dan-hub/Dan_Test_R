
rm(list=ls())
#CLEAR CONSOLE
cat("\014") 

library(RODBC)
suppressMessages(suppressWarnings(library(lubridate)))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(suppressWarnings(library(foreach)))
suppressMessages(suppressWarnings(library(ROCR)))
suppressMessages(suppressWarnings(library(boot)))
suppressMessages(suppressWarnings(library(ggplot2)))
suppressMessages(suppressWarnings(library(grid)))
suppressMessages(suppressWarnings(library(gridExtra)))


###############################################################################
## pull data (note, EXCLUDE_FLAG was redefined in the the View)
## TO DO: CHECK FIT FOR MISSING SCORES LATER

## CHECK THESE ASSUMPTIONS:
## FOR PULs, I EXCLUDE ACCOUNT OPENED PRIOR TO '2012-04-30'

VENDSQL  <- odbcConnect("RCPDatamart")

raw.df   <- as.data.frame(sqlQuery(VENDSQL,
                                   "
                                   SELECT *
                                   FROM [RCPTemporary].[ann2021].[vHELOCBanding] --**** 2021 VIEW, WithModel!!!!! ****
                                   WHERE
                                   [caExclusionFlag] = 0  --Only removes 89 bads
                                   ")) #END OF mydata

table(raw.df$caExclusionFlag, raw.df$defaultCount)
table(raw.df$year, raw.df$defaultCount)

all.df <-
  raw.df %>%
  transmute(
    AccountNumber               = accountNumberJoin,
    accountNumberJoin,
    PostDate                    = year,
    ModelYear                   = as.numeric(year),
    DefaultFlag                 = caPerformanceFlag,
    prf                         = caPerformanceFlag,
    GoodFlag                    = 1 - caPerformanceFlag,
    defaultCount                = caPerformanceFlag,
    
    recordCount                 = 1,
    LOGIT                       = caLogit_PROD,
    PD                          = caPD_PROD,
    
    # LOGIT_PREV                = caLogit_PREV,
    # PD_PREV                   = caPD_PREV,
    # 
    # LOGIT                     = caLogit,
    # PD                        = caPD,
    
    KG                          = caGood,
    
    prf_FACTOR                  = factor(prf),
    facility                    = facility,
    #CUSTOMER_MONTHS_ON_BOOKS_v2 = caCUSTOMER_MONTHS_ON_BOOK_v2,
    #MODEL_BKRPT_v2              = BKRPT,
    #BAL_PERCT                   = caBal_perct,
    #TERM_IN_MONTHS              = TERM_IN_MONTHS,
    #MONTHS_ON_BOOK              = MONTHS_ON_BOOK,
    #caEOLFlag                   = caEOLFlag,
    
    
    FICO  = FICO,
    BKRPT = BKRPT,
    age   = age
    
  ) %>%
  na.omit() %>%  #NEEDED FOR HOSMER TEST to indicate good model; Include then TEST AUC Drops
  as.data.frame()

  
    
audit.df <- raw.df
min(all.df$PostDate)

max(all.df$PostDate)

table(all.df$prf)


###############################################################################
## setup environment, constants, and functions

# clear environment
# rm(list=ls())

# constants
# last.build.year <- 2017 #ObservationDate
# last.build.date <- '2018-06-30'

# build.start.date <- "2014-09-30"  #New for 2021: Don't start model build until 2014; drop observations prior to 2014!!
build.start.date <- "2011"  #New for 2021: Don't start model build until 2014; drop observations prior to 2014!!
build.end.date   <- "2018"


valid.start.date <- "2019" #Performance Year
valid.end.date   <- "2020" #Performance Year - COVID started in 4/2020; data cuts of as of 6/2020

# functions
'%ni%' <- Negate('%in%')

# lapply(audit.df, class)

##################### BINNING ##########################################################
###############################################################################
###############################################################################
## prep data for model validation; create build/validation data splits

build.df <- all.df  %>%
  filter(PostDate >= build.start.date)  %>%
  filter(PostDate <= build.end.date) 

valid.df <- all.df  %>%
  filter(PostDate >= valid.start.date)  %>%
  filter(PostDate <= valid.end.date) 

table(build.df$prf)
table(build.df$PostDate)

table(valid.df$prf)
table(valid.df$PostDate)
##################### OR RATIO ##############################
set.seed(121)
# 
# build.df$random   <- runif(dim(build.df)[[1]])
# # # head(audit.df)
# # train.df <- subset(build.df, random>0.3)
# # test.df  <- subset(build.df, random<=0.3)
# 
train.df <- build.df
test.df <- valid.df
# 
# table(build.df$prf)
# table(test.df$prf)


########################################################################
library(smbinning)
result = smbinning(df=train.df, y="prf", x="age", p=0.05)
result$bands
smbinning.plot(result, option="dist")
train.df <- smbinning.gen(train.df, result, chrname = "binnedAge")
test.df  <- smbinning.gen(test.df, result, chrname = "binnedAge")

smbinning.sql(result)
# case
# when age <= 2.55 then '01: age <= 2.55'
# when age <= 8.91 then '02: age <= 8.91'
# when age > 8.91 then '03: age > 8.91'
# when age Is Null then '04: age Is Null'
# else '99: Error' end

table(train.df$binnedAge, train.df$prf)


tmp.df <- train.df %>%
          group_by(roundedAge = round(age,0)) %>%
          summarise(badrate_by_age = sum(defaultCount)/sum(recordCount))
# View(tmp.df)

ggplot(tmp.df, aes(roundedAge, badrate_by_age))+
      geom_point()+
      labs(
        x = "Age (Rounded)",
        y = "Bad Rate"
      )

############################################ REFERENCE ##############################
############################################ RUN LM (LASSO) #########################
library(lars)

library(mlr) #needed for impute()

################### Loglikelihood Variable Selection ###################
pos <- '1' #Outcome considered positive
outcome <- 'prf_FACTOR'

vars <- setdiff(colnames(train.df),
                c(outcome,'random'))

# numericVars <- vars[sapply(train.df[,vars],class) %in% c('numeric','integer') ]
# numericVars <- numericVars [numericVars %in% c("age", "FICO", "BKRPT")]
# "facility"

predVars         <- c("age", "FICO", "BKRPT", "binnedAge") #Just options

########################################################################
# If still get error, check that there are no NA records
findna.df <- train.df %>% select(all_of(predVars))
names(findna.df)[sapply(findna.df, anyNA)]

# findna.df <- train.df %>% select(all_of(predVars))
# names(findna.df)[sapply(findna.df, anyNA)]

library(mlr) #needed for impute()
# IMPUTE TRAIN
mean_FICO <- mean(train.df$FICO , trim = 0, na.rm = TRUE)
mean_FICO
# 773.8327
imp                  <- impute(train.df,cols = list(FICO  = imputeMean()))
                        # train.df[is.na(train.df$FICO), c("accountNumberJoin", "PostDate", "FICO")]
                        train.df[train.df$accountNumberJoin==915020866 & train.df$PostDate == '2011', c("accountNumberJoin", "PostDate", "FICO")]
imp.df               <- imp$data
train.df             <- imp.df
                        train.df[train.df$accountNumberJoin==915020866 & train.df$PostDate == '2011', c("accountNumberJoin", "PostDate", "FICO")]


mean_BKRPT<- mean(train.df$BKRPT , trim = 0, na.rm = TRUE)
mean_BKRPT
# 755.5388
imp                  <- impute(train.df,cols = list(BKRPT = imputeMean()))
                        # train.df[is.na(train.df$BKRPT), c("accountNumberJoin", "PostDate", "BKRPT")]
                        train.df[train.df$accountNumberJoin==915022102 & train.df$PostDate == '2011', c("accountNumberJoin", "PostDate", "BKRPT")]
imp.df               <- imp$data
train.df             <- imp.df
                        train.df[train.df$accountNumberJoin==915022102 & train.df$PostDate == '2011', c("accountNumberJoin", "PostDate", "BKRPT")]

# IMPUTE TEST
imp                  <- impute(test.df,cols = list(FICO  = mean_FICO))
                        # test.df[is.na(test.df$FICO), c("accountNumberJoin", "PostDate", "FICO")]
                        test.df[test.df$accountNumberJoin==8920001409 & test.df$PostDate == '2020', c("accountNumberJoin", "PostDate", "FICO")]
test.df              <- imp$data
                        test.df[test.df$accountNumberJoin==8920001409 & test.df$PostDate == '2020', c("accountNumberJoin", "PostDate", "FICO")]
                        

imp                  <- impute(test.df,cols = list(BKRPT = mean_BKRPT))
test.df              <- imp$data


dim(train.df)
dim(test.df)

findna.df <- train.df %>% select(all_of(predVars))
names(findna.df)[sapply(findna.df, anyNA)]

x <- data.matrix(subset(train.df, select = predVars))
y <-   as.matrix(subset(train.df, select = prf ))
###############################################################################
###############################################################################
## BuILD LOGIT

#######################
## This Year's Model ##
# #######################
# train.df$TIMES_LATE_30_DAYS_3P <- ifelse(train.df$TIMES_LATE_30_DAYS >3, 3, train.df$TIMES_LATE_30_DAYS)
# test.df$TIMES_LATE_30_DAYS_3P  <- ifelse(test.df$TIMES_LATE_30_DAYS >3,  3, test.df$TIMES_LATE_30_DAYS)
# 
# table(train.df$TIMES_LATE_30_DAYS_3P)
# table(test.df$TIMES_LATE_30_DAYS_3P)

dim(train.df)
dim(test.df)

featureSet1 <- c(#"facility",
                 "FICO"
                 ,"age"
                 #,"binnedAge"
                 ,"BKRPT"
)

logit.df     <- subset(train.df, select = c(featureSet1,"prf"))

library (wrapr)
fmla1       <- mk_formula("prf", featureSet1)
logit.model <- glm(fmla1, family=binomial(link="logit"), data=logit.df)
summary(logit.model)
coef(   logit.model)

library(car)
car::vif(logit.model)

#variable importance
library(caret)
varImp(logit.model)

pseudoRsq = (1-logit.model$deviance/logit.model$null.deviance)
pseudoRsq


library(ResourceSelection)
hoslem.test(logit.model$y, logit.model$fitted)


# save.image("C:/Users/ptom/Desktop/23. CRM Annual PD Build/all_data.RData")
# load("C:/Users/ptom/Desktop/23. CRM Annual PD Build/all_data.RData")


### ROC for This Year's Model ###
train.df$pred <- predict(logit.model, newdata = train.df, type = 'response')
test.df$pred  <- predict(logit.model, newdata = test.df,  type = 'response')


fcn_calcAUC <- function (predcol, outcol){
  perf <- ROCR::performance(ROCR::prediction(predcol, outcol==pos), 'auc')
  as.numeric(perf@y.values)
}
fcn_calcAUC(train.df[,'pred'],train.df[,'prf'])
# 0.8209816 #omitted NAs
fcn_calcAUC(test.df[,'pred'],test.df[,'prf'])
# 0.7455513

dim(train.df)
dim(test.df)

#######################
## Last Year's Model ##
#######################

featureSetPrev <- c(
                    "FICO"
                    ,"BKRPT"
)

library (wrapr) #mk_formula()
library(car)    #vif()
logit.df       <- subset(train.df, select = c(featureSetPrev, "prf"))
fmlaPrev       <- mk_formula("prf", featureSetPrev)
previous.model <- glm(fmlaPrev, family=binomial(link="logit"), data=logit.df)
summary(previous.model)
coef(previous.model)

# car::vif(previous.model)


#variable importance
library(caret)
varImp(previous.model)

pseudoRsq = (1-previous.model$deviance/previous.model$null.deviance)
pseudoRsq
# ###############################################################################
### ROC for Last Year's Model ###
train.df$pred.prev <- predict(previous.model, newdata = train.df, type = 'response')
test.df$pred.prev  <- predict(previous.model, newdata = test.df,  type = 'response')


fcn_calcAUC <- function (predcol, outcol){
  perf <- ROCR::performance(ROCR::prediction(predcol, outcol==pos), 'auc')
  as.numeric(perf@y.values)
}
fcn_calcAUC(train.df[,'pred.prev'],train.df[,'prf'])
# 0.8132614 #Omitted NAs
fcn_calcAUC(test.df[,'pred.prev'],test.df[,'prf'])
# 0.699198

library(pROC)

roc1=roc(train.df$prf ~ train.df$pred.prev) #Black Line is Prev Yr's Model
roc2=roc(train.df$prf ~ train.df$pred)      #Green Line is This Yr's Model

plot(roc1)
plot(roc2, add=TRUE, col='green')

library(WVPlots)
ROCPlot(train.df, xvar='pred',      truthVar = 'prf', truthTarget = '1', title = 'Proposed Model')
ROCPlot(train.df, xvar='pred.prev', truthVar = 'prf', truthTarget = '1', title = 'Proposed Model')

train.df$proposed_model_form <- train.df$pred
train.df$current_model_form  <- train.df$pred.prev

test.df$proposed_model_form <- test.df$pred
test.df$current_model_form  <- test.df$pred.prev

WVPlots::ROCPlotPair(train.df, "pred",                "pred.prev",          "prf", '1', title="PUL ROC Curve Comparison")
WVPlots::ROCPlotPair(train.df, "proposed_model_form", "current_model_form", "prf", '1', title="PUL ROC Curve Comparison on Train Sample")
WVPlots::ROCPlotPair(test.df,  "proposed_model_form", "current_model_form", "prf", '1', title="PUL ROC Curve Comparison on Test Sample")
# 
# ############################## VALIDATION SCRIPT ####################################################
# # validate iteratively over model years beyond the last build year
# 
# # constants
# last.build.year <- 2017 #ObservationDate
# last.build.date <- build.end.date
# 
# model.data.years <- sort(unique(build.df$ModelYear))
# model.data.years.valid <- model.data.years[model.data.years > 2017]
# 
# model.data.lst <-
#   sapply(
#     as.character(model.data.years.valid),
#     FUN=
#       function(my.year) {
#         my.year <- as.numeric(my.year)
#         
#         list(
#           'build'=train.df %>% filter(ModelYear <= last.build.year),
#           'valid'=test.df %>% filter(ModelYear == my.year)
#         )
#       },
#     simplify=F,
#     USE.NAMES=T
#   )
# 
# 
# # ###############################################################################
# ### ROC for This Year's Model ###
# train.df$pred <- predict(logit.model, newdata = train.df, type = 'response')
# test.df$pred  <- predict(logit.model, newdata = test.df,  type = 'response')
# 
# fcn_calcAUC <- function (predcol, outcol){
#   perf <- ROCR::performance(ROCR::prediction(predcol, outcol==pos), 'auc')
#   as.numeric(perf@y.values)
# }
# fcn_calcAUC(train.df[,'pred'],train.df[,'prf'])
# fcn_calcAUC(test.df[,'pred'],test.df[,'prf'])
# 
# library(WVPlots)
# WVPlots::ROCPlot(train.df, xvar = 'pred', truthVar = 'prf', truthTarget = '1', title = 'Training Sample Performance')
# WVPlots::ROCPlot(test.df,  xvar = 'pred', truthVar = 'prf', truthTarget = '1', title = 'Testing Sample Performance')
# 
# library(sigr)
# sigr::calcAUC(test.df$pred, test.df$prf=='1')   
# table(train.df$prf)
# table(test.df$prf)
# 
# names(train.df)[sapply(train.df, anyNA)]
# names(test.df)[sapply(test.df, anyNA)]
# 
# library(caret)
# varImp(logit.model)
# 
# # ###############################################################################
# ### ROC for Last Year's Model ###
# train.df$pred.prev <- predict(previous.model, newdata = train.df, type = 'response')
# test.df$pred.prev  <- predict(previous.model, newdata = test.df,  type = 'response')
# 
# 
# fcn_calcAUC <- function (predcol, outcol){
#   perf <- ROCR::performance(ROCR::prediction(predcol, outcol==pos), 'auc')
#   as.numeric(perf@y.values)
# }
# fcn_calcAUC(train.df[,'pred.prev'],train.df[,'prf'])
# # [1] 0.7500264
# fcn_calcAUC(test.df[,'pred.prev'],test.df[,'prf'])
# # 0.7496357
# 
# library(WVPlots)
# ROCPlot(train.df, xvar = 'pred', truthVar = 'prf', truthTarget = '1', title = 'Training Sample Performance')
# ROCPlot(test.df,  xvar = 'pred', truthVar = 'prf', truthTarget = '1', title = 'Testing Sample Performance')
# 
# library(sigr)
# sigr::calcAUC(test.df$pred, test.df$prf=='1')  
# table(test.df$prf)
# table(test.df$pred)
# ?sigr::calcAUC
