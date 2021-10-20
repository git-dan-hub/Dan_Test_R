# CMD: In VM, to get list of packages, call in command prompt
# (1) >cd C:\R\Package
# (2) >dir
# (3) Copy and paste in Notepad and then parse in Excel

# To check library location
# .libPaths()
# myPaths <- .libPaths()   
# myPaths <- c(myPaths[2], myPaths[1])  
# .libPaths(myPaths)  

# UPDATE R VERSION
# R.Version()
# install.packages("installr"); library( installr); updateR()
# R.Version()

#CLEAR ENVIRONMENT
rm(list=ls())
#CLEAR CONSOLE
cat("\014") 

# install.packages("RODBC")
# install.packages("lubridate")
# install.packages("tidyverse")
# install.packages("foreach")
# install.packages("ROCR")
# install.packages("boot")
# install.packages("ggplot2")
# install.packages("grid")
# install.packages("gridExtra")

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
## setup environment, constants, and functions

# clear environment
rm(list=ls())

# build.start.date <- "2014-09-30"  #New for 2021: Don't start model build until 2014; drop observations prior to 2014!!
build.start.date <- 2011  #New for 2021: Don't start model build until 2011; with Bal Info
build.end.date   <- 2011


valid.start.date <- 2019 #Performance Year
valid.end.date   <- 2020 #Performance Year - COVID started in 4/2020; data cuts of as of 6/2020

# functions
'%ni%' <- Negate('%in%')


###############################################################################
## HELOCl data (note, EXCLUDE_FLAG was redefined in the the View)
## TO DO: CHECK FIT FOR MISSING SCORES LATER

## CHECK THESE ASSUMPTIONS:
## FOR HELOCs, I EXCLUDE ACCOUNT OPENED PRIOR TO '2014-04-30' *** NEW FOR 2021 (Used to start 4/30/2012) ****

VENDSQL <- odbcConnect("RCPDatamart")

mydata.df   <- as.data.frame(sqlQuery(VENDSQL,
                                   "
                                   SELECT *
                                   FROM [RCPTemporary].[ann2021].[vHELOCBanding_WithModel] --**** 2021 VIEW, WithModel!!!!! ****
                                   WHERE
                                   [caExclusionFlag] = 0  --Only removes 6 observations
                                   ")) #END OF mydata

raw.df <- mydata.df

# colnames(raw.df)

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
    
    LOGIT_PROD                  = caLogit_PROD,
    PD_PROD                     = caPD_PROD,

    LOGIT_PREV                  = caLogit_PREV,
    PD_PREV                     = caPD_PREV,

    
    LOGIT_PREV_2016P            = caLogit_PREV_2016P,
    PD_PREV_2016P               = caPD_PREV_2016P,
    
    LOGIT_PREV_2017P            = caLogit_PREV_2017P,
    PD_PREV_2017P               = caPD_PREV_2017P,
    
    LOGIT                       = caLogit,
    PD                          = caPD,
    
    LOGIT_BIN                   = caLogit_BIN,
    PD_BIN                      = caPD_BIN,
    
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
    age   = age,
    LoanAge = age
    
  ) %>%
  #na.omit() %>%  #NEEDED FOR HOSMER TEST to indicate good model; Include then TEST AUC Drops
  as.data.frame()

###############################################################################
## create new variables and prepare various modeling data sets

model.all.df <- all.df 

model.build.df <- model.all.df
model.valid.df <- model.all.df
# model.valid.df <- model.all.df[is.na(model.all.df$FICO), ]
# test <- is.na(model.all.df$FICO)
# 
# model.no_na.df <- model.all.df %>%
#                 na.omit() 

###############################################################################
## prep data for model validation; create build/validation data splits

model.data.years <- sort(unique(model.all.df$ModelYear))
model.data.years.valid <- model.data.years[model.data.years > build.end.date]


# model.data.years <- sort(unique(model.all.df$PostDate))
# model.data.years.valid <- model.data.years[model.data.years > last.build.date]

# validate iteratively over model years beyond the last build year
model.data.lst <-
  sapply(
    as.character(model.data.years.valid),
    # as.character(model.data.years),
    FUN=
      function(my.year) {
        my.year <- as.numeric(my.year)
        
        list(
          'build'=model.all.df %>% filter(ModelYear <= build.end.date),
          # 'valid'=model.all.df %>% filter(ModelYear > build.end.date)
          # 'build'=model.build.df %>% filter(PostDate <= last.build.date),
          'valid'=model.valid.df %>% filter(ModelYear == my.year)
        )
      },
    simplify=F,
    USE.NAMES=T
  )


head(model.data.lst)

foreach(
  my.year=names(model.data.lst),
  .combine=rbind
) %do% {
  build.df <- model.data.lst[[my.year]]$build
  valid.df <- model.data.lst[[my.year]]$valid
}
# 
# table(build.df$ModelYear)
# table(valid.df$ModelYear)
# table(valid.df$prf)


library(ROCR)
pos <- '1' #Outcome considered positive
fcn_calcAUC <- function (predcol, outcol){
  perf <- ROCR::performance(ROCR::prediction(predcol, outcol==pos), 'auc')
  as.numeric(perf@y.values)
}

fcn_calcAUC(build.df[,"PD"],build.df[,"DefaultFlag"])
fcn_calcAUC(valid.df[,"PD"],valid.df[,"DefaultFlag"])

fcn_calcAUC(build.df[,"PD_BIN"],build.df[,"DefaultFlag"])
fcn_calcAUC(valid.df[,"PD_BIN"],valid.df[,"DefaultFlag"])


fcn_calcAUC(build.df[,"PD_PREV"],build.df[,"DefaultFlag"])
fcn_calcAUC(valid.df[,"PD_PREV"],valid.df[,"DefaultFlag"])

fcn_calcAUC(build.df[,"PD_PREV_2016P"],build.df[,"DefaultFlag"])
fcn_calcAUC(valid.df[,"PD_PREV_2016P"],valid.df[,"DefaultFlag"])

fcn_calcAUC(build.df[,"PD_PREV_2017P"],build.df[,"DefaultFlag"])
fcn_calcAUC(valid.df[,"PD_PREV_2017P"],valid.df[,"DefaultFlag"])


fcn_calcAUC(build.df[,"PD_PROD"],build.df[,"DefaultFlag"])
fcn_calcAUC(valid.df[,"PD_PROD"],valid.df[,"DefaultFlag"])

sum(!complete.cases(valid.df))
head(valid.df)
table(is.na(valid.df$FICO), valid.df$prf)

# Why are FICOs missing?

# 0    1
# FALSE 6466   18
# TRUE   138    6

# 
# 
# # PTOM # #####################
# # CUSTOMER_MONTHS_ON_BOOKS_v2 
# # MODEL_BKRPT_v2 
# # BAL_PERCT 
# x <- build.df$BAL_PERCT 
# n <- 5
# qx <- quantile(x,seq(0,1,1/n))
# qx
# ###############################################################################
# ## look at stability index for defaults and model variables

psi.factor <-
  function(cx,cy) {
    fx <- table(cx)/length(cx)
    fy <- table(cy)/length(cy)
    sum((fx-fy)*log(fx/fy))
  }

psi.dr.df <-
  foreach(
    my.year=names(model.data.lst),
    .combine=rbind
  ) %do% {
    build.df <- model.data.lst[[my.year]]$build
    valid.df <- model.data.lst[[my.year]]$valid
    
    data.frame(
      ObservationYear=my.year,
      DefaultRate=psi.factor(build.df$DefaultFlag, valid.df$DefaultFlag)
    )
  }


# poHELOCation default stability is very stable over time
ggplot(psi.dr.df %>% gather(Variable, PSI, -ObservationYear)) +
  aes(x=ObservationYear, y=PSI, group=Variable, color=Variable) +
  geom_point() +
  geom_line() +
  geom_rect(
    data=
      data.frame(
        xmin=rep(-Inf,3),
        xmax=rep(+Inf,3),
        ymin=c(-Inf, 0.10, 0.25),
        ymax=c(0.10, 0.25, +Inf),
        fill=
          factor(
            c("Stable", "Marginally Stable", "Unstable"),
            levels=c("Unstable", "Marginally Stable", "Stable")
          )
      ),
    aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=fill),
    alpha=0.20, color=NA, inherit.aes=FALSE
  ) +
  scale_fill_manual(
    values=
      c(
        "Stable"="green",
        "Marginally Stable"="yellow",
        "Unstable"="red"
      )
  ) +
  labs(
    x='Performance Year',
    y='Stability Index',
    fill="Stability",
    color='Stability Index',
    title="HELOC: Stability Index (for Defaults vs. Non-Defaults)"
  )

psi.dr.df


## look at stability index for model variables
## Needed for SSI
psi <-
  function(x,y,n) {
    qx <- quantile(x,seq(0,1,1/n))
    cx <- cut(x, breaks=qx)
    cy <- cut(y, breaks=qx)
    fx <- table(cx)/length(cx)
    fy <- table(cy)/length(cy)
    sum((fx-fy)*log(fx/fy))
  }

# THERE ARE NAs NEED TO RUN In OMIT NA Version

# psi.var.df <-
#   foreach(
#     my.year=names(model.data.lst),
#     .combine=rbind
#   ) %do% {
#     build.df <- model.data.lst[[my.year]]$build
#     valid.df <- model.data.lst[[my.year]]$valid
#     
#     data.frame(
#       ObservationYear=my.year,
#       age=psi(build.df$age,     valid.df$age, 4)#,
#       FICO=psi(build.df$FICO,   valid.df$FICO, 10),
#       BKRPT=psi(build.df$BKRPT, valid.df$BKRPT, 10)
#     )
#   }
# 
# ggplot(psi.var.df %>% gather(Variable, PSI, -ObservationYear)) +
#   aes(x=ObservationYear, y=PSI, group=Variable, color=Variable) +
#   geom_point() +
#   geom_line() +
#   geom_rect(
#     data=
#       data.frame(
#         xmin=rep(-Inf,3),
#         xmax=rep(+Inf,3),
#         ymin=c(-Inf, 0.10, 0.25),
#         ymax=c(0.10, 0.25, +Inf),
#         fill=
#           factor(
#             c("Stable", "Marginally Stable", "Unstable"),
#             levels=c("Unstable", "Marginally Stable", "Stable")
#           )
#       ),
#     aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=fill),
#     alpha=0.20, color=NA, inherit.aes=FALSE
#   ) +
#   scale_fill_manual(
#     values=
#       c(
#         "Stable"="green",
#         "Marginally Stable"="yellow",
#         "Unstable"="red"
#       )
#   ) +
#   labs(
#     x="Observation Year",
#     y="Stability Index",
#     fill="Stability",
#     title="HELOC: Stability Index (for Model Variables)"
#   )
# 
# psi.var.df




ssi.pd.df <-
  foreach(
    my.year=names(model.data.lst),
    .combine=rbind
  ) %do% {
    build.df <- model.data.lst[[my.year]]$build
    valid.df <- model.data.lst[[my.year]]$valid
    
    data.frame(
      ObservationYear=my.year,
      PD=psi(build.df$PD, valid.df$PD, 10)
      
    )
  }


ggplot(ssi.pd.df %>% gather(Variable, PSI, -ObservationYear)) +
  aes(x=ObservationYear, y=PSI, group=Variable, color=Variable) +
  geom_point() +
  geom_line() +
  geom_rect(
    data=
      data.frame(
        xmin=rep(-Inf,3),
        xmax=rep(+Inf,3),
        ymin=c(-Inf, 0.10, 0.25),
        ymax=c(0.10, 0.25, +Inf),
        fill=
          factor(
            c("Stable", "Marginally Stable", "Unstable"),
            levels=c("Unstable", "Marginally Stable", "Stable")
          )
      ),
    aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=fill),
    alpha=0.20, color=NA, inherit.aes=FALSE
  ) +
  scale_fill_manual(
    values=
      c(
        "Stable"="green",
        "Marginally Stable"="yellow",
        "Unstable"="red"
      )
  ) +
  labs(
    x="Observation Year",
    y="Stability Index",
    fill="Stability",
    title="HELOC: Stability Index (for Model Variables)"
  )

ssi.pd.df
##############################################################
## collect historical prediction data

## model.coefs=NULL => build a model
collect.prediction.data <-
  function(model.data.lst, model.formula, model.coefs=NULL) {
    sapply(
      names(model.data.lst),
      FUN=
        function(build.year) {
          build.df <- model.data.lst[[build.year]]$build
          valid.df <- model.data.lst[[build.year]]$valid
          
          if (is.null(model.coefs)) {
            my.model <-
              glm(
                model.formula,
                data=build.df,
                family=binomial(logit)
              )
            
            model.coefs <- coef(my.model)
          } else {
            my.model=NULL
          }
          
          get.logit.preds <-
            function(model.formula, model.coefs, data.df) {
              model.df <- model.frame(model.formula, data.df)
              model.df[,1] <- 1  # intercept
              sapply(
                Reduce(
                  `+`,
                  mapply(
                    `*`,
                    model.df,
                    model.coefs,
                    SIMPLIFY=F
                  )
                ),
                FUN=function(x) { 1/(1+exp(-x)) }
              )
            }
          
          build.actual <- build.df$DefaultFlag
           build.pred   <- get.logit.preds(model.formula, model.coefs, build.df)
          
          #PTOM ################################ BEWARE####################### MISSING VALUES 
          #build.pred   <- replace_na(get.logit.preds(model.formula, model.coefs, build.df), log(0.0849/(1-0.0849)))
          
          if (nrow(valid.df) > 0) {
            valid.actual  <- valid.df$DefaultFlag
            
            if (is.null(my.model)) {
              valid.pred    <- get.logit.preds(model.formula, model.coefs, valid.df)
              valid.lowerCI <- NULL
              valid.upperCI <- NULL
            } else {
              my.pred       <- predict(my.model, newdata=valid.df, type="link", se.fit=TRUE)
              valid.pred    <- my.model$family$linkinv(my.pred$fit)
              q.val <- qnorm(0.975)
              valid.lowerCI <- my.model$family$linkinv(my.pred$fit-q.val*my.pred$se.fit)
              valid.upperCI <- my.model$family$linkinv(my.pred$fit+q.val*my.pred$se.fit)
            }
          } else {
            valid.actual  <- NULL
            valid.pred    <- NULL
            valid.lowerCI <- NULL
            valid.upperCI <- NULL
          }
          
          list(
            'model'=my.model,
            'formula'=model.formula,
            'build.actual'=build.actual,
            'valid.actual'=valid.actual,
            'build.pred'=build.pred,
            'valid.pred'=valid.pred,
            'valid.lowerCI'=valid.lowerCI,
            'valid.upperCI'=valid.upperCI
          )
        },
      simplify=F,
      USE.NAMES=T
    )
  }




## old model
# prediction.data.old <-
#   collect.prediction.data(
#     model.data.lst,
#     DefaultFlag ~ CUSTOMER_MONTHS_ON_BOOKS_v2 + MODEL_BKRPT_v2 + BAL_PERCT,
#     model.coefs=c(-0.76194401, -0.00459511, 0.812687955, -0.003085686)
#   )  



###############################################################################
## (1/2) PROD MODEL model stability

prediction.data.old <-
  collect.prediction.data(
    model.data.lst,
    DefaultFlag ~ LOGIT,
    model.coefs=c(0, 1)
  )  

prediction.data <- list()
prediction.data[['HELOC_NEW Model: FICO + BKRPT + Loan Age']] <- prediction.data.old ###### PTOM - CHANGES CHART TITLES #########

############## COMPARISON ROC CURVE LOGIT (GREEN) VS LOGIT_PROD (BLACK) ###############
library(pROC)

roc1=pROC::roc(model.data.lst[['2019']]$valid$DefaultFlag ~ model.data.lst[['2019']]$valid$LOGIT_PROD)      #Black Line is Prev Yr's Model
roc2=pROC::roc(model.data.lst[['2019']]$valid$DefaultFlag ~ model.data.lst[['2019']]$valid$LOGIT)           #Green Line is This Yr's Model

plot(roc1,
     main = "HELOC_NEW (green) vs HELOC_OLD (black) on 2019 Test sample")
plot(roc2, add=TRUE, col='green')

library(pROC)

roc1=pROC::roc(model.data.lst[['2020']]$valid$DefaultFlag ~ model.data.lst[['2020']]$valid$LOGIT_PROD)      #Black Line is Prev Yr's Model
roc2=pROC::roc(model.data.lst[['2020']]$valid$DefaultFlag ~ model.data.lst[['2020']]$valid$LOGIT)           #Green Line is This Yr's Model

plot(roc1,
     main = "HELOC_NEW (green) vs HELOC_OLD (black) on 2020 Test sample")
plot(roc2, add=TRUE, col='green')
############## COMPARISON ROC CURVE LOGIT (GREEN) VS LOGIT_PROD (BLACK) ###############

ssi.pd.df <-
  foreach(
    my.model=names(prediction.data),
    .combine=rbind
  ) %do% {
    foreach(
      my.year=names(prediction.data[[my.model]]),
      .combine=rbind
    ) %do% {
      build.pred <- prediction.data[[my.model]][[my.year]]$build.pred
      qx <- quantile(build.pred, seq(0,1,1/10))
      qx[1]  <- 0
      qx[11] <- 1
      build.q <- cut(build.pred, breaks=qx)
      
      valid.pred <- prediction.data[[my.model]][[my.year]]$valid.pred
      data.frame(
        Model=my.model,
        ObservationYear=as.numeric(my.year),
        SSI=ifelse(is.null(valid.pred), NA, psi.factor(build.q, cut(valid.pred, breaks=qx))),
        Q00=qx[1], Q01=qx[2],  Q02=qx[3],  Q03=qx[4],
        Q04=qx[5], Q05=qx[6],  Q06=qx[7],  Q07=qx[8],
        Q08=qx[9], Q09=qx[10], Q10=qx[11]
      )
    }
  }

ggplot(
  ssi.pd.df %>% select(-SSI) %>% gather(CutName, CutPoint, -Model, -ObservationYear)
) +
  aes(x=ObservationYear, y=log(CutPoint), group=CutName, color=CutName) +
  geom_point() +
  geom_line() +
  scale_x_continuous(
    breaks=ssi.pd.df$ObservationYear,
    minor_breaks=ssi.pd.df$ObservationYear
  ) +
  labs(
    x='Performance Year',
    y="log(PD)",
    color="Cut Point",
    title="HELOC: PD Model Cutpoints"
  ) +
  facet_wrap(~Model)

ggplot(ssi.pd.df) +
  aes(x=ObservationYear, y=SSI) +
  geom_point() +
  geom_line() +
  
  geom_rect(
    data=
      data.frame(
        xmin=rep(-Inf,3),
        xmax=rep(+Inf,3),
        ymin=c(-Inf, 0.10, 0.25),
        ymax=c(0.10, 0.25, +Inf),
        fill=
          factor(
            c("Stable", "Marginally Stable", "Unstable"),
            levels=c("Unstable", "Marginally Stable", "Stable")
          )
      ),
    aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=fill),
    alpha=0.20, color=NA, inherit.aes=FALSE
  ) +
  scale_fill_manual(
    values=
      c(
        "Stable"="green",
        "Marginally Stable"="yellow",
        "Unstable"="red"
      )
  ) +
  scale_x_continuous(
    breaks=ssi.pd.df$ObservationYear,
    minor_breaks=ssi.pd.df$ObservationYear
  ) +
  labs(
    x='Performance Year',
    y="Stability Index",
    fill="Stability",
    title="HELOC: PD Model Stability Index"
  ) +
  facet_wrap(~Model)

ssi.pd.df

#########PTOM

###############################################################################
## (2/2) REFRESHED MODEL model stability

prediction.data.old <-
  collect.prediction.data(
    model.data.lst,
    DefaultFlag ~ LOGIT_PREV_2016P,
    model.coefs=c(0, 1)
  )  

prediction.data <- list()
prediction.data[['Refreshed Model: FICO']] <- prediction.data.old ###### PTOM - CHANGES CHART TITLES #########

###############################################################################
## (2/2) REFRESHED MODEL model stability

prediction.data.old <-
  collect.prediction.data(
    model.data.lst,
    DefaultFlag ~ LOGIT_PREV_2017P,
    model.coefs=c(0, 1)
  )  

prediction.data <- list()
prediction.data[['Refreshed Model: FICO']] <- prediction.data.old ###### PTOM - CHANGES CHART TITLES #########

###############################################################################

prediction.data.old <-
  collect.prediction.data(
    model.data.lst,
    DefaultFlag ~ LOGIT_PROD,
    model.coefs=c(0, 1)
  )  

prediction.data <- list()
prediction.data[['HELOC_OLD Baseline In-Production Model: FICO + BKRPT']] <- prediction.data.old ###### PTOM - CHANGES CHART TITLES #########



ssi.pd.df <-
  foreach(
    my.model=names(prediction.data),
    .combine=rbind
  ) %do% {
    foreach(
      my.year=names(prediction.data[[my.model]]),
      .combine=rbind
    ) %do% {
      build.pred <- prediction.data[[my.model]][[my.year]]$build.pred
      qx <- quantile(build.pred, seq(0,1,1/10))
      qx[1]  <- 0
      qx[11] <- 1
      build.q <- cut(build.pred, breaks=qx)
      
      valid.pred <- prediction.data[[my.model]][[my.year]]$valid.pred
      data.frame(
        Model=my.model,
        ObservationYear=as.numeric(my.year),
        SSI=ifelse(is.null(valid.pred), NA, psi.factor(build.q, cut(valid.pred, breaks=qx))),
        Q00=qx[1], Q01=qx[2],  Q02=qx[3],  Q03=qx[4],
        Q04=qx[5], Q05=qx[6],  Q06=qx[7],  Q07=qx[8],
        Q08=qx[9], Q09=qx[10], Q10=qx[11]
      )
    }
  }

ggplot(
  ssi.pd.df %>% select(-SSI) %>% gather(CutName, CutPoint, -Model, -ObservationYear)
) +
  aes(x=ObservationYear, y=log(CutPoint), group=CutName, color=CutName) +
  geom_point() +
  geom_line() +
  scale_x_continuous(
    breaks=ssi.pd.df$ObservationYear,
    minor_breaks=ssi.pd.df$ObservationYear
  ) +
  labs(
    x='Performance Year',
    y="log(PD)",
    color="Cut Point",
    title="HELOC: PD Model Cutpoints"
  ) +
  facet_wrap(~Model)

ggplot(ssi.pd.df) +
  aes(x=ObservationYear, y=SSI) +
  geom_point() +
  geom_line() +
  
  geom_rect(
    data=
      data.frame(
        xmin=rep(-Inf,3),
        xmax=rep(+Inf,3),
        ymin=c(-Inf, 0.10, 0.25),
        ymax=c(0.10, 0.25, +Inf),
        fill=
          factor(
            c("Stable", "Marginally Stable", "Unstable"),
            levels=c("Unstable", "Marginally Stable", "Stable")
          )
      ),
    aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=fill),
    alpha=0.20, color=NA, inherit.aes=FALSE
  ) +
  scale_fill_manual(
    values=
      c(
        "Stable"="green",
        "Marginally Stable"="yellow",
        "Unstable"="red"
      )
  ) +
  scale_x_continuous(
    breaks=ssi.pd.df$ObservationYear,
    minor_breaks=ssi.pd.df$ObservationYear
  ) +
  labs(
    x='Performance Year',
    y="Stability Index",
    fill="Stability",
    title="HELOC: PD Model Stability Index"
  ) +
  facet_wrap(~Model)

ssi.pd.df



###############################################################################
## Brier Score / Spiegelhalter Test

st.df <-
  foreach(
    my.model=names(prediction.data),
    .combine=rbind
  ) %do% {
    foreach(
      my.year=names(prediction.data[[my.model]]),
      .combine=rbind
    ) %do% {
      build.actual <- as.numeric(prediction.data[[my.model]][[my.year]]$build.actual)
      build.pred   <- prediction.data[[my.model]][[my.year]]$build.pred
      
      build.n    <- length(build.pred)
      build.mse  <- mean((build.pred - build.actual)**2)
      build.mean <- sum(build.pred * (1 - build.pred)) / build.n
      build.var  <- sum((1 - 2*build.pred)**2 * build.pred * (1 - build.pred)) / (build.n ** 2)
      build.st   <- (build.mse - build.mean) / sqrt(build.var)
      
      valid.actual <- as.numeric(prediction.data[[my.model]][[my.year]]$valid.actual)
      valid.pred   <- prediction.data[[my.model]][[my.year]]$valid.pred
      
      if (length(valid.actual) > 0) {
        valid.n    <- length(valid.pred)
        valid.mse  <- mean((valid.pred - valid.actual)**2)
        valid.mean <- sum(valid.pred * (1 - valid.pred)) / valid.n
        valid.var  <- sum((1 - 2*valid.pred)**2 * valid.pred * (1 - valid.pred)) / (valid.n ** 2)
        valid.st   <- (valid.mse - valid.mean) / sqrt(valid.var)
      } else {
        valid.st <- NA
      }
      
      data.frame(
        Model=my.model,
        ObservationYear=as.numeric(my.year),
        Build.ST=build.st,
        Build.Pval=2*(1-pnorm(abs(build.st))),
        Valid.Brier=valid.mse,
        Valid.ST=valid.st,
        Valid.Pval=2*(1-pnorm(abs(valid.st)))
      )
    }
  }

ggplot(st.df) +
  aes(x=ObservationYear) +
  geom_rect(
    data=
      data.frame(
        xmin=c(-Inf,-Inf,-Inf,-Inf,-Inf),
        xmax=c(+Inf,+Inf,+Inf,+Inf,+Inf),
        ymin=c( -Inf, -2.57, -1.96, +1.96, +2.57),
        ymax=c(-2.57, -1.96, +1.96, +2.57,  +Inf),
        fill=
          factor(
            c("Poor", "Marginal", "Well", "Marginal", "Poor"),
            levels=c("Well", "Marginal", "Poor")
          )
      ),
    aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=fill),
    alpha=0.20, color=NA, inherit.aes=FALSE
  ) +
  geom_point(aes(y=Build.ST, color='Build'), shape=21, size=3) +
  geom_point(aes(y=Valid.ST, color='Test'), shape=21, size=3) +
  scale_color_manual(values=c("Build"="blue", "Test"="red")) +
  scale_fill_manual(
    values=
      c(
        "Well"="green",
        "Marginal"="yellow",
        "Poor"="red"
      )
  ) +
  scale_x_continuous(
    breaks=st.df$ObservationYear,
    minor_breaks=st.df$ObservationYear
  ) +
  labs(
    x='Performance Year',
    y='Spiegelhalter Score',
    fill="Calibration",
    color="Data Sample",
    title="HELOC: Spiegelhalter Test"
  ) +
  facet_wrap(~Model)

st.df



###############################################################################
## ROC / AUC / CAP / Gini (= 2*AUC-1)

roc.df <-
  foreach(
    my.model=names(prediction.data),
    .combine=rbind
  ) %do% {
    foreach(
      my.year=names(prediction.data[[my.model]]),
      .combine=rbind
    ) %do% {
      build.actual <- prediction.data[[my.model]][[my.year]]$build.actual
      build.pred   <- prediction.data[[my.model]][[my.year]]$build.pred
      
      valid.actual <- prediction.data[[my.model]][[my.year]]$valid.actual
      valid.pred   <- prediction.data[[my.model]][[my.year]]$valid.pred
      
      default.count <- sum(as.numeric(valid.actual) == '1')
      
      build.auc <- unlist(ROCR::performance(ROCR::prediction(build.pred, build.actual), 'auc')@y.values)
      
      
      library(pROC)
      
      roc1=pROC::roc(build.actual ~ build.pred)      #Black Line is Prev Yr's Model
      roc2=pROC::roc(valid.actual ~ valid.pred)      #Green Line is This Yr's Model
      
      plot(roc1)
      plot(roc2, add=TRUE, col='green')
      
      if (length(valid.actual) > 0) {
        valid.auc <- unlist(ROCR::performance(ROCR::prediction(valid.pred, valid.actual), 'auc')@y.values)
        auc.var   <- valid.auc*(1-valid.auc)/default.count
      } else {
        valid.auc <- NA
        auc.var   <- NA
      }
      
      q.val <- qnorm(0.975)
      
      data.frame(
        Model=my.model,
        ObservationYear=as.numeric(my.year),
        BuildAUC=build.auc,
        ValidAUC=valid.auc,
        LowerCI=valid.auc - q.val*sqrt(auc.var),
        UpperCI=valid.auc + q.val*sqrt(auc.var)
      )
    }
  }

ggplot(roc.df) +
  aes(x=ObservationYear) +
  geom_rect(
    data=
      data.frame(
        xmin=c(-Inf,-Inf,-Inf),
        xmax=c(+Inf,+Inf,+Inf),
        ymin=c(0.70, 0.60, -Inf),
        ymax=c(+Inf, 0.70, 0.60),
        fill=
          factor(
            c("Excellent", "Marginal", "Poor"),
            levels=c("Excellent", "Marginal", "Poor")
          )
      ),
    aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=fill),
    alpha=0.20, color=NA, inherit.aes=FALSE
  ) +
  geom_errorbar(width=0.1, aes(ymin=LowerCI, ymax=UpperCI)) +
  geom_point(aes(y=BuildAUC, color='Build'), shape=21, size=3) +
  geom_point(aes(y=ValidAUC, color='Test'), shape=21, size=3) +
  scale_x_continuous(
    breaks=roc.df$ObservationYear,
    minor_breaks=roc.df$ObservationYear
  ) +
  scale_fill_manual(
    values=
      c(
        "Excellent"="green",
        "Marginal"="yellow",
        "Poor"="red"
      )
  ) +
  scale_color_manual(values=c("Build"="blue", "Test"="red")) +
  labs(
    x='Performance Year',
    y='AUC',
    fill='Performance',
    color="Data Sample",
    title="HELOC: AUC Performance"
  ) +
  facet_wrap(~Model)

roc.df



###############################################################################
## KS

ks.df <-
  foreach(
    my.model=names(prediction.data),
    .combine=rbind
  ) %do% {
    foreach(
      my.year=names(prediction.data[[my.model]]),
      .combine=rbind
    ) %do% {
      build.actual <- as.numeric(prediction.data[[my.model]][[my.year]]$build.actual)
      build.pred   <- prediction.data[[my.model]][[my.year]]$build.pred
      build.split  <- split(build.pred, build.actual)
      build.ks <-
        suppressWarnings(
          ks.test(build.split$'1', build.split$'0', alternative='two.sided')$statistic
        )
      
      valid.actual <- as.numeric(prediction.data[[my.model]][[my.year]]$valid.actual)
      valid.pred   <- prediction.data[[my.model]][[my.year]]$valid.pred
      
      if (length(valid.actual) > 0) {
        valid.split <- split(valid.pred, valid.actual)
        valid.ks <-
          suppressWarnings(
            ks.test(valid.split$'1', valid.split$'0', alternative='two.sided')$statistic
          )
        
        ks.boot.fun <-
          function(valid.preds, valid.idx, valid.actuals) {
            my.valid.actual <- valid.actuals[valid.idx]
            my.valid.pred   <- valid.preds[valid.idx]
            my.valid.split  <- split(my.valid.pred, my.valid.actual)
            
            suppressWarnings(
              ks.test(my.valid.split$'1', my.valid.split$'0', alternative='two.sided')$statistic
            )
          }
        
        boot.results <-
          boot(
            valid.pred,
            statistic=ks.boot.fun,
            R=1000,
            strata=valid.actual,
            valid.actuals=valid.actual
          )
        
        stat.boot <- boot.ci(boot.results, conf=0.95, type='perc')
        
        valid.lower <- stat.boot$percent[4]
        valid.upper <- stat.boot$percent[5]
      } else {
        valid.ks    <- NA
        valid.lower <- NA
        valid.upper <- NA
      }
      
      data.frame(
        Model=my.model,
        ObservationYear=as.numeric(my.year),
        BuildKS=build.ks,
        ValidKS=valid.ks,
        LowerCI=valid.lower,
        UpperCI=valid.upper
      )
    }
  }

ggplot(ks.df) +
  aes(x=ObservationYear) +
  geom_rect(
    data=
      data.frame(
        xmin=c(-Inf,-Inf,-Inf),
        xmax=c(+Inf,+Inf,+Inf),
        ymin=c(0.40, 0.20, -Inf),
        ymax=c(+Inf, 0.40, 0.20),
        fill=
          factor(
            c("Excellent", "Marginal", "Poor"),
            levels=c("Excellent", "Marginal", "Poor")
          )
      ),
    aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=fill),
    alpha=0.20, color=NA, inherit.aes=FALSE
  ) +
  geom_errorbar(width=0.4, aes(ymin=LowerCI, ymax=UpperCI)) +
  geom_point(aes(y=BuildKS, color='Build'), shape=21, size=3) +
  geom_point(aes(y=ValidKS, color='Test'), shape=21, size=3) +
  scale_x_continuous(
    breaks=ks.df$ObservationYear,
    minor_breaks=ks.df$ObservationYear
  ) +
  scale_fill_manual(
    values=
      c(
        "Excellent"="green",
        "Marginal"="yellow",
        "Poor"="red"
      )
  ) +
  scale_color_manual(values=c("Build"="blue", "Test"="red")) +
  labs(
    x='Performance Year',
    y='KS',
    fill="Lift",
    color="Data Sample",
    title="HELOC: KS Lift"
  ) +
  facet_wrap(~Model)

ks.df



###############################################################################
## PD confidence intervals

pd.confint.df <-
  foreach(
    my.model=names(prediction.data),
    .combine=rbind
  ) %do% {
    foreach(
      my.year=names(prediction.data[[my.model]]),
      .combine=rbind
    ) %do% {
      valid.actual  <- prediction.data[[my.model]][[my.year]]$valid.actual
      valid.pred    <- prediction.data[[my.model]][[my.year]]$valid.pred
      valid.lowerCI <- prediction.data[[my.model]][[my.year]]$valid.lowerCI
      valid.upperCI <- prediction.data[[my.model]][[my.year]]$valid.upperCI
      valid.n       <- length(valid.actual)
      
      if (length(valid.actual) > 0) {
        data.frame(
          Model=my.model,
          ObservationYear=as.numeric(my.year),
          ActualDR=sum(valid.actual)/valid.n,
          ExpectedDR=sum(valid.pred)/valid.n,
          LowerCI=sum(valid.lowerCI)/valid.n,
          UpperCI=sum(valid.upperCI)/valid.n
        )
      } else {
        data.frame(
          Model=my.model,
          ObservationYear=as.numeric(my.year),
          ActualDR=NA,
          ExpectedDR=NA,
          LowerCI=NA,
          UpperCI=NA
        )
      }
    }
  }

ggplot(pd.confint.df) +
  aes(x=ObservationYear) +
  geom_point(aes(y=ExpectedDR, fill='RCP Model'), shape=21, size=3) +
  geom_point(aes(y=ActualDR, fill='Actual'), shape=21, size=3) +
  ylim(0,0.03) +
  scale_x_continuous(
    breaks=pd.confint.df$ObservationYear,
    minor_breaks=pd.confint.df$ObservationYear
  ) +
  scale_fill_manual(values=c("Old RCP Model"="white", "Actual"="red", "Challenger"="yellow")) +
  labs(
    x='Performance Year',
    y='Default Rate',
    fill="Data Sample",
    title="HELOC: Actual Default Rate vs. Model Estimate(s)"
  ) +
  facet_wrap(~Model)

pd.confint.df






