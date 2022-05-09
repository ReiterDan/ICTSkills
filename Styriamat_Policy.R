#Weiterbildung von IKT-Kompetenzen als Gelegenheit oder Notwendigkeit für (bessere) Erwerbschancen?

#Read Dataset

setwd("C:/Users/reiterd/Documents/Daniel Reiter/Universität Graz/FiWi und VWL Institut/Styriamat Projekt/AES/AES 2016")
A <- read.csv("EU_AES_2016_MICRODATA_V2.0.CSV")

View(A)

#Country Selection  -> only Austria

A1 <- subset(A, COUNTRY %in% c("AT") & HATLEVEL >-1)

View(A1)


install.packages("data.table")
library(data.table)

#Degree of Urbanisation (1=Cities; 2=Towns and suburbs; 3=Rural areas)

summary(A1$DEG_URB)

A1$DEG_URB <- as.factor(A1$DEG_URB)
summary(A1$DEG_URB)


#Female Dummy

A1$Female = 0
A1$Female <- ifelse(A1$SEX == 2, 1, 0)

A1$Female <- as.factor(A1$Female)

summary(A1$Female)


#Age und Age^2

summary(A1$AGE)

A1$Age <- A1$AGE
A1$Agesqrt <- A1$Age^2


#Children (0-13 years old) living in the household

A1$Children = 0
A1$Children <- ifelse(A1$HHNBPERS_0_13 > 0, 1, 0)

A1$Children <- as.factor(A1$Children)

summary(A1$Children)


#Migration Background (1=National; 2=another EU country; 3=non-EU country)

A1$MigrationB <- as.factor(A1$BIRTHPLACE)

summary(A1$MigrationB)
levels(A1$MigrationB)


#Migration Background (First and Second Generation as well as Country of Origin)

 #Country of birth father (1=National; 2=another EU country; 3=non-EU country)

A1$MigrationB_father <- as.numeric(A1$BIRTHFATHER)

summary(A1$MigrationB_father)


 #Country of birth mother (1=National; 2=another EU country; 3=non-EU country)

A1$MigrationB_mother <- as.numeric(A1$BIRTHMOTHER)

summary(A1$MigrationB_mother)


#Migration Background Respondent (Country of Origin) -> 0 if both parents born in the respondent's country of residence (native); 1 if at least one parent born in in the respondent's country of residence (MB): 1 = home country, 2 = another EU27 country, 3 = born outside EU; 2 if at least one parent born in another EU country (MB EU country); 3 if both parents born outside EU (MB outside EU);

setDT(A1)[MigrationB_father == 1 & MigrationB_mother == 1, MB_Country := "0"]
A1[MigrationB_father == 1 & MigrationB_mother == 2 | MigrationB_father == 1 & MigrationB_mother == 3 | MigrationB_father == 3 & MigrationB_mother == 1 | MigrationB_father == 2 & MigrationB_mother == 1, MB_Country := "1"]
A1[MigrationB_father == 2 & MigrationB_mother == 2 | MigrationB_father == 2 & MigrationB_mother == 3 | MigrationB_father == 3 & MigrationB_mother == 2, MB_Country := "2"]
A1[MigrationB_father == 3 & MigrationB_mother == 3, MB_Country := "3"]

A1$MB_Country <- as.factor(A1$MB_Country)

summary(A1$MB_Country)


#Migration Background Respondent (Distinction between 1st and 2nd Generation as well as Country of Origin) ->  0 = base (natives); 1 = 2nd gen (at least one parent born in the country of the respondents residence); 2 = 1st gen (born in another EU27 country); 3 = 1st gen (born outside EU);

setDT(A1)[MB_Country == "0" & MigrationB == "1" | MB_Country == "1" & MigrationB == "1", MB_Generation := "0"]
A1[MB_Country == "2" & MigrationB == "1" | MB_Country == "3" & MigrationB == "1", MB_Generation := "1"]
A1[MB_Country == "2" & MigrationB == "2" | MB_Country == "3" & MigrationB == "2", MB_Generation := "2"]
A1[MB_Country == "2" & MigrationB == "3" | MB_Country == "3" & MigrationB == "3", MB_Generation := "3"]

A1$MB_Generation <- as.factor(A1$MB_Generation)

levels(A1$MB_Generation)
summary(A1$MB_Generation)
View(A1)


#Years of residence (1 = max 1 year; 2 = between 2 and 10 years; 3 = more than 10 years)

install.packages("dplyr")
library(dplyr)

summary(A1$RESTIME)

A1$Years_since_residence <- na_if(A1$RESTIME, -2)

summary(A1$Years_since_residence)


#Marital Status

A1$Partnership = 0
A1$Partnership <- ifelse(A1$MARSTADEFACTO == 1, 1, 0)

A1$Partnership <- as.factor(A1$Partnership)

summary(A1$Partnership)


#Highest Level of Educational Attainment (1 = Primary Education; 2 = Upper secondary education; 3 = Short-cycle tertiary education;  4 = Bachelor's degree at most or equivalent)

summary(A1$HATLEVEL)

setDT(A1)[HATLEVEL == 100, Education_Level := "1"]
A1[HATLEVEL == 300, Education_Level := "2"]
A1[HATLEVEL == 500 | HATLEVEL == 600, Education_Level := "3"]

A1$Education_Level <- as.factor(A1$Education_Level)

levels(A1$Education_Level)
summary(A1$Education_Level)


#Occupation according to ISCO-08 (Skill-Level 2 = High skilled; Skill-Level 1 = Medium skilled; Skill-Level 0 = Low skilled)

summary(A1$JOBISCO)
class(A1$JOBISCO)

setDT(A1)[JOBISCO == "OC1" | JOBISCO == "OC11" | JOBISCO == "OC12" | JOBISCO == "OC13" | JOBISCO == "OC14", ISCO := "1"]
A1[JOBISCO == "OC2" | JOBISCO == "OC20" | JOBISCO == "OC21" | JOBISCO == "OC22" | JOBISCO == "OC23" | JOBISCO == "OC24" | JOBISCO == "OC25" | JOBISCO == "OC26", ISCO := "2"]
A1[JOBISCO == "OC3" | JOBISCO == "OC30" | JOBISCO == "OC31" | JOBISCO == "OC32" | JOBISCO == "OC33" | JOBISCO == "OC34" | JOBISCO == "OC35", ISCO := "3"]
A1[JOBISCO == "OC4" | JOBISCO == "OC40" | JOBISCO == "OC41" | JOBISCO == "OC42" | JOBISCO == "OC43" | JOBISCO == "OC44", ISCO := "4"]
A1[JOBISCO == "OC5" | JOBISCO == "OC51" | JOBISCO == "OC52" | JOBISCO == "OC53" | JOBISCO == "OC54", ISCO := "5"]
A1[JOBISCO == "OC6" | JOBISCO == "OC61" | JOBISCO == "OC62" | JOBISCO == "OC63", ISCO := "6"]
A1[JOBISCO == "OC7" | JOBISCO == "OC70" | JOBISCO == "OC71" | JOBISCO == "OC72" | JOBISCO == "OC73" | JOBISCO == "OC74" | JOBISCO == "OC75", ISCO := "7"]
A1[JOBISCO == "OC8" | JOBISCO == "OC81" | JOBISCO == "OC82" | JOBISCO == "OC83", ISCO := "8"]
A1[JOBISCO == "OC9" | JOBISCO == "OC91" | JOBISCO == "OC92" | JOBISCO == "OC93" | JOBISCO == "OC94" | JOBISCO == "OC95" | JOBISCO == "OC96", ISCO := "9"]
A1[JOBISCO == "oc0"| JOBISCO == "OC01" | JOBISCO == "OC02" | JOBISCO == "OC03", ISCO := "10"]
A1[JOBISCO == -1 | JOBISCO == -2, ISCO := "0"]

A1$ISCO <- as.factor(A1$ISCO)

levels(A1$ISCO)
summary(A1$ISCO)


#ISCO dummies for further Estimation

A1$ISCO_dummy_0 = 0
A1$ISCO_dummy_0 <- ifelse(A1$ISCO == 1,0,0)

A1$ISCO_dummy_1 = 0
A1$ISCO_dummy_1 <- ifelse(A1$ISCO == 1,1,0)

A1$ISCO_dummy_2 = 0
A1$ISCO_dummy_2 <- ifelse(A1$ISCO == 2,1,0)

A1$ISCO_dummy_3 = 0
A1$ISCO_dummy_3 <- ifelse(A1$ISCO == 3,1,0)

A1$ISCO_dummy_4 = 0
A1$ISCO_dummy_4 <- ifelse(A1$ISCO == 4,1,0)

A1$ISCO_dummy_5 = 0
A1$ISCO_dummy_5 <- ifelse(A1$ISCO == 5,1,0)

A1$ISCO_dummy_6 = 0
A1$ISCO_dummy_6 <- ifelse(A1$ISCO == 6,1,0)

A1$ISCO_dummy_7 = 0
A1$ISCO_dummy_7 <- ifelse(A1$ISCO == 7,1,0)

A1$ISCO_dummy_8 = 0
A1$ISCO_dummy_8 <- ifelse(A1$ISCO == 8,1,0)

A1$ISCO_dummy_9 = 0
A1$ISCO_dummy_9 <- ifelse(A1$ISCO == 9,1,0)

A1$ISCO_dummy_10 = 0
A1$ISCO_dummy_10 <- ifelse(A1$ISCO == 10,1,0)


#Economic activity of the local unit (Information and communication = 1; Agriculture = 2; Manufacturing = 3; Construction = 4; Wohlesale and retail trade = 5; financial and insurance activities = 6; real estate activities = 7; Professional, scientific, technical, administration and support services = 8; public administration = 9; other services = 10)

summary(A1$LOCNACE)
class(A1$LOCNACE)

setDT(A1)[LOCNACE == "J", Economic_activity := "1"]
A1[LOCNACE == "A", Economic_activity := "2"]
A1[LOCNACE == "B"| LOCNACE == "C" | LOCNACE == "C" | LOCNACE == "D" | LOCNACE == "E" | LOCNACE =="B_E", Economic_activity := "3"]
A1[LOCNACE == "F", Economic_activity := "4"]
A1[LOCNACE == "G" | LOCNACE == "H" | LOCNACE == "I", Economic_activity := "5"]
A1[LOCNACE == "K", Economic_activity := "6"]
A1[LOCNACE == "L", Economic_activity := "7"]
A1[LOCNACE == "M" | LOCNACE == "N" | LOCNACE == "L_N", Economic_activity := "8"]
A1[LOCNACE == "O" | LOCNACE == "P" | LOCNACE == "Q", Economic_activity := "9"]
A1[LOCNACE == "R" | LOCNACE == "S" | LOCNACE == "T" | LOCNACE == "U" | LOCNACE == "R_U", Economic_activity := "10"]
A1[LOCNACE == -1 | LOCNACE == -2, Economic_activity := "0"]

A1$Economic_activity <- as.factor(A1$Economic_activity)

levels(A1$Economic_activity)
summary(A1$Economic_activity)


#Economic Activity dummies for further Estimation

A1$Economic_activity_dummy_0 = 0
A1$Economic_activity_dummy_0 <- ifelse(A1$Economic_activity == 1,0,0)

A1$Economic_activity_dummy_1 = 0
A1$Economic_activity_dummy_1 <- ifelse(A1$Economic_activity == 1,1,0)

A1$Economic_activity_dummy_2 = 0
A1$Economic_activity_dummy_2 <- ifelse(A1$Economic_activity == 2,1,0)

A1$Economic_activity_dummy_3 = 0
A1$Economic_activity_dummy_3 <- ifelse(A1$Economic_activity == 3,1,0)

A1$Economic_activity_dummy_4 = 0
A1$Economic_activity_dummy_4 <- ifelse(A1$Economic_activity == 4,1,0)

A1$Economic_activity_dummy_5 = 0
A1$Economic_activity_dummy_5 <- ifelse(A1$Economic_activity == 5,1,0)

A1$Economic_activity_dummy_6 = 0
A1$Economic_activity_dummy_6 <- ifelse(A1$Economic_activity == 6,1,0)

A1$Economic_activity_dummy_7 = 0
A1$Economic_activity_dummy_7 <- ifelse(A1$Economic_activity == 7,1,0)

A1$Economic_activity_dummy_8 = 0
A1$Economic_activity_dummy_8 <- ifelse(A1$Economic_activity == 8,1,0)

A1$Economic_activity_dummy_9 = 0
A1$Economic_activity_dummy_9 <- ifelse(A1$Economic_activity == 9,1,0)

A1$Economic_activity_dummy_10 = 0
A1$Economic_activity_dummy_10 <- ifelse(A1$Economic_activity == 10,1,0)


#ICT competencies

summary(A1$FEDFIELD)
A1$FEDFIELD <- as.factor(A1$FEDFIELD)

levels(A1$FEDFIELD)
summary(A1$FEDFIELD)

#levels(A1$FEDFIELD)[levels(A1$FEDFIELD) == -1] <- NA
#levels(A1$FEDFIELD)[levels(A1$FEDFIELD) == -2] <- NA

#summary(A1$FEDFIELD)


summary(A1$NFEFIELD1)
A1$NFEFIELD1 <- as.factor(A1$NFEFIELD1)

levels(A1$NFEFIELD1)
summary(A1$NFEFIELD1)

#levels(A1$NFEFIELD1)[levels(A1$NFEFIELD1) == -1] <- NA
#levels(A1$NFEFIELD1)[levels(A1$NFEFIELD1) == -2] <- NA

#summary(A1$NFEFIELD1)

summary(A1$NFEFIELD2)
A1$NFEFIELD2 <- as.factor(A1$NFEFIELD2)

levels(A1$NFEFIELD2)
summary(A1$NFEFIELD2)

#levels(A1$NFEFIELD2)[levels(A1$NFEFIELD2) == -1] <- NA
#levels(A1$NFEFIELD2)[levels(A1$NFEFIELD2) == -2] <- NA

#summary(A1$NFEFIELD2)


 #Formal ICT learning activity

A1$Formal_ICT = 0
A1$Formal_ICT <- ifelse(A1$FEDFIELD == 6, 1, 0)

A1$Formal_ICT <- as.factor(A1$Formal_ICT)

summary(A1$Formal_ICT)


 #Non-Formal ICT learning activity

A1$Nonformal_ICT = 0
A1$Nonformal_ICT <- ifelse(A1$NFEFIELD1 == 6 | A1$NFEFIELD2 == 6, 1, 0)

A1$Nonformal_ICT <- as.factor(A1$Nonformal_ICT)

summary(A1$Nonformal_ICT)


 #Formal and non-Formal ICT learning activity combined

A1$ICT_Training = 0
A1$ICT_Training <- ifelse(A1$NFEFIELD1 == 6 | A1$NFEFIELD2 == 6 | A1$FEDFIELD == 6, 1, 0)

A1$ICT_Training <- as.factor(A1$ICT_Training)

summary(A1$ICT_Training)



#Dependent Variables

 #Income Qintile

summary(A1$HHINCOME)

A1$HHINCOMEQINTILE <- as.factor(A1$HHINCOME)

summary(A1$HHINCOMEQINTILE)
levels(A1$HHINCOMEQINTILE)



 #Main current labour status (1 = Unemployed; 2 = Part time; 3 = Full time; 0 = Others including no answer)

summary(A1$MAINSTAT)

setDT(A1)[MAINSTAT == 11, Labour_Status := "2"]
A1[MAINSTAT == 12, Labour_Status := "1"]
A1[MAINSTAT == 31 | MAINSTAT == 32 | MAINSTAT == 33 | MAINSTAT == 34 | MAINSTAT == 35 | MAINSTAT == 36 | MAINSTAT == -1 | MAINSTAT == 20, Labour_Status := "0"]

A1$Labour_Status <- as.factor(A1$Labour_Status)

levels(A1$Labour_Status)
summary(A1$Labour_Status)



 #Employment Dummy

A1$Employment_Dummy = 0
A1$Employment_Dummy <- ifelse(A1$MAINSTAT == 12 | A1$MAINSTAT == 11, 1, 0)

A1$Employment_Dummy <- as.numeric(A1$Employment_Dummy)

summary(A1$Employment_Dummy)



 #Outcome of Formal and non-Formal learning activities


  #Outcome Formal training

A1$FEDOUTCOME_1 <- as.factor(A1$FEDOUTCOME_1)
summary(A1$FEDOUTCOME_1)

A1$FED_new_Job = 0
A1$FED_new_Job <- ifelse(A1$FEDOUTCOME_1 == 1, 1, 0)
A1$FED_new_Job <- as.factor(A1$FED_new_Job)
summary(A1$FED_new_Job)


A1$FEDOUTCOME_2 <- as.factor(A1$FEDOUTCOME_2)
summary(A1$FEDOUTCOME_2)

A1$FED_higher_Salary = 0
A1$FED_higher_Salary <- ifelse(A1$FEDOUTCOME_2 == 1, 1, 0)
A1$FED_higher_Salary <- as.factor(A1$FED_higher_Salary)
summary(A1$FED_higher_Salary)


A1$FEDOUTCOME_3 <- as.factor(A1$FEDOUTCOME_3)
summary(A1$FEDOUTCOME_3)

A1$FED_job_promotion = 0
A1$FED_job_promotion <- ifelse(A1$FEDOUTCOME_3 == 1, 1, 0)
A1$FED_job_promotion <- as.factor(A1$FED_job_promotion)
summary(A1$FED_job_promotion)


  #Outcome non-formal training

A1$NFEOUTCOME1_1 <- as.factor(A1$NFEOUTCOME1_1)
summary(A1$NFEOUTCOME1_1)

A1$NFE1_new_Job = 0
A1$NFE1_new_Job <- ifelse(A1$NFEOUTCOME1_1 == 1, 1, 0)
A1$NFE1_new_Job <- as.factor(A1$NFE1_new_Job)
summary(A1$NFE1_new_Job)


A1$NFEOUTCOME1_2 <- as.factor(A1$NFEOUTCOME1_2)
summary(A1$NFEOUTCOME1_2)

A1$NFE1_higher_Salary = 0
A1$NFE1_higher_Salary <- ifelse(A1$NFEOUTCOME1_2 == 1, 1, 0)
A1$NFE1_higher_Salary <- as.factor(A1$NFE1_higher_Salary)
summary(A1$NFE1_higher_Salary)


A1$NFEOUTCOME1_3 <- as.factor(A1$NFEOUTCOME1_3)
summary(A1$NFEOUTCOME1_3)

A1$NFE1_job_promotion = 0
A1$NFE1_job_promotion <- ifelse(A1$NFEOUTCOME1_3 == 1, 1, 0)
A1$NFE1_job_promotion <- as.factor(A1$NFE1_job_promotion)
summary(A1$NFE1_job_promotion)




A1$NFEOUTCOME2_1 <- as.factor(A1$NFEOUTCOME2_1)
summary(A1$NFEOUTCOME2_1)

A1$NFE2_new_Job = 0
A1$NFE2_new_Job <- ifelse(A1$NFEOUTCOME2_1 == 1, 1, 0)
A1$NFE2_new_Job <- as.factor(A1$NFE2_new_Job)
summary(A1$NFE2_new_Job)


A1$NFEOUTCOME2_2 <- as.factor(A1$NFEOUTCOME2_2)
summary(A1$NFEOUTCOME2_2)

A1$NFE2_higher_Salary = 0
A1$NFE2_higher_Salary <- ifelse(A1$NFEOUTCOME2_2 == 1, 1, 0)
A1$NFE2_higher_Salary <- as.factor(A1$NFE2_higher_Salary)
summary(A1$NFE2_higher_Salary)


A1$NFEOUTCOME2_3 <- as.factor(A1$NFEOUTCOME2_3)
summary(A1$NFEOUTCOME2_3)

A1$NFE2_job_promotion = 0
A1$NFE2_job_promotion <- ifelse(A1$NFEOUTCOME2_3 == 1, 1, 0)
A1$NFE2_job_promotion <- as.factor(A1$NFE2_job_promotion)
summary(A1$NFE2_job_promotion)


 #Outcome of formal and non-formal ICT training combined

A1$New_Job = 0
A1$New_Job <- ifelse(A1$FEDOUTCOME_1 == 1 | A1$NFEOUTCOME1_1 == 1 | A1$NFEOUTCOME2_1 == 1, 1, 0)
A1$New_Job <- as.numeric(A1$New_Job)
summary(A1$New_Job)


A1$higher_Salary = 0
A1$higher_Salary <- ifelse(A1$FEDOUTCOME_2 == 1 | A1$NFEOUTCOME1_2 == 1 | A1$NFEOUTCOME2_2 == 1, 1, 0)
A1$higher_Salary <- as.numeric(A1$higher_Salary)
summary(A1$higher_Salary)


A1$Job_Promotion = 0
A1$Job_Promotion <- ifelse(A1$FEDOUTCOME_3 == 1 | A1$NFEOUTCOME1_3 == 1 | A1$NFEOUTCOME2_3 == 1, 1, 0)
A1$Job_Promotion <- as.numeric(A1$Job_Promotion)
summary(A1$Job_Promotion)



#ESTIMATES

 #Step 1: Ordered Probit Model Estimation (dependent variable: Labour status)

install.packages("estimatr")
library(estimatr)

install.packages("MASS")
library(MASS)

install.packages("mfx")
library(mfx)


polr <- function(formula, data, weights, start, ..., subset,
                 na.action, contrasts = NULL, Hess = FALSE,
                 model = TRUE,
                 method = c("logistic", "probit", "cloglog", "cauchit"))
{
  logit <- function(p) log(p/(1 - p))
  
  fmin <- function(beta) {
    theta <- beta[pc + 1L:q]
    gamm <- c(-Inf, cumsum(c(theta[1L], exp(theta[-1L]))), Inf)
    eta <- offset
    if (pc > 0)
      eta <- eta + drop(x %*% beta[1L:pc])
    pr <- pfun(gamm[y + 1] - eta) - pfun(gamm[y] - eta)
    if (all(pr > 0))
      -sum(wt * log(pr))
    else Inf
  }
  
  gmin <- function(beta)
  {
    jacobian <- function(theta) { ## dgamma by dtheta matrix
      k <- length(theta)
      etheta <- exp(theta)
      mat <- matrix(0 , k, k)
      mat[, 1] <- rep(1, k)
      for (i in 2:k) mat[i:k, i] <- etheta[i]
      mat
    }
    theta <- beta[pc + 1L:q]
    gamm <- c(-Inf, cumsum(c(theta[1L], exp(theta[-1L]))), Inf)
    eta <- offset
    if(pc > 0) eta <- eta + drop(x %*% beta[1L:pc])
    pr <- pfun(gamm[y+1] - eta) - pfun(gamm[y] - eta)
    p1 <- dfun(gamm[y+1] - eta)
    p2 <- dfun(gamm[y] - eta)
    g1 <- if(pc > 0) t(x) %*% (wt*(p1 - p2)/pr) else numeric(0)
    xx <- .polrY1*p1 - .polrY2*p2
    g2 <- - t(xx) %*% (wt/pr)
    g2 <- t(g2) %*% jacobian(theta)
    if(all(pr > 0)) c(g1, g2) else rep(NA, pc+q)
  }
  
  m <- match.call(expand.dots = FALSE)
  method <- match.arg(method)
  pfun <- switch(method, logistic = plogis, probit = pnorm,
                 cloglog = pgumbel, cauchit = pcauchy)
  dfun <- switch(method, logistic = dlogis, probit = dnorm,
                 cloglog = dgumbel, cauchit = dcauchy)
  if(is.matrix(eval.parent(m$data)))
    m$data <- as.data.frame(data)
  m$start <- m$Hess <- m$method <- m$model <- m$... <- NULL
  m[[1L]] <- as.name("model.frame")
  m <- eval.parent(m)
  Terms <- attr(m, "terms")
  x <- model.matrix(Terms, m, contrasts)
  xint <- match("(Intercept)", colnames(x), nomatch=0L)
  n <- nrow(x)
  pc <- ncol(x)
  cons <- attr(x, "contrasts") # will get dropped by subsetting
  if(xint > 0) {
    x <- x[, -xint, drop=FALSE]
    pc <- pc - 1
  } else warning("an intercept is needed and assumed")
  wt <- model.weights(m)
  if(!length(wt)) wt <- rep(1, n)
  offset <- model.offset(m)
  if(length(offset) <= 1) offset <- rep(0, n)
  y <- model.response(m)
  if(!is.factor(y)) stop("response must be a factor")
  lev <- levels(y)
  if(length(lev) <= 2) stop("response must have 3 or more levels")
  y <- unclass(y)
  q <- length(lev) - 1
  Y <- matrix(0, n, q)
  .polrY1 <- col(Y) == y
  .polrY2 <- col(Y) == y - 1
  if(missing(start)) {
    # try something that should always work -tjb
    u <- as.integer(table(y))
    u <- (cumsum(u)/sum(u))[1:q]
    zetas <-
      switch(method,
             "logistic"= qlogis(u),
             "probit"=   qnorm(u),
             "cauchit"=  qcauchy(u),
             "cloglog"=  -log(-log(u)) )
    s0 <- c(rep(0,pc),zetas[1],log(diff(zetas)))
    
    ##         # try logistic/probit regression on 'middle' cut
    ##         q1 <- length(lev) %/% 2
    ##         y1 <- (y > q1)
    ##         X <- cbind(Intercept = rep(1, n), x)
    ##         fit <-
    ##             switch(method,
    ##                    "logistic"= glm.fit(X, y1, wt, family = binomial(), offset = offset),
    ##                    "probit" = glm.fit(X, y1, wt, family = binomial("probit"), offset = offset),
    ##                    ## this is deliberate, a better starting point
    ##                    "cloglog" = glm.fit(X, y1, wt, family = binomial("probit"), offset = offset),
    ##                    "cauchit" = glm.fit(X, y1, wt, family = binomial("cauchit"), offset = offset))
    ##         if(!fit$converged)
    ##             stop("attempt to find suitable starting values failed")
    ##         coefs <- fit$coefficients
    ##         if(any(is.na(coefs))) {
    ##             warning("design appears to be rank-deficient, so dropping some coefs")
    ##             keep <- names(coefs)[!is.na(coefs)]
    ##             coefs <- coefs[keep]
    ##             x <- x[, keep[-1L], drop = FALSE]
    ##             pc <- ncol(x)
    ##           }
    ##         spacing <- logit((1L:q)/(q+1)) # just a guess
    ##         if(method != "logistic") spacing <- spacing/1.7
    ##         gammas <- -coefs[1L] + spacing - spacing[q1]
    ##         thetas <- c(gammas[1L], log(diff(gammas)))
    ##         s0 <- c(coefs[-1L], thetas)
  } else if(length(start) != pc + q)
    stop("'start' is not of the correct length")
  else {
    s0 <- if(pc > 0) c(start[seq_len(pc+1)], log(diff(start[-seq_len(pc)])))
    else c(start[1L], log(diff(start)))
  }
  res <- optim(s0, fmin, gmin, method="BFGS", hessian = Hess, ...)
  beta <- res$par[seq_len(pc)]
  theta <- res$par[pc + 1L:q]
  zeta <- cumsum(c(theta[1L],exp(theta[-1L])))
  deviance <- 2 * res$value
  niter <- c(f.evals=res$counts[1L], g.evals=res$counts[2L])
  names(zeta) <- paste(lev[-length(lev)], lev[-1L], sep="|")
  if(pc > 0) {
    names(beta) <- colnames(x)
    eta <- offset + drop(x %*% beta)
  } else eta <- offset + rep(0, n)
  
  cumpr <- matrix(pfun(matrix(zeta, n, q, byrow=TRUE) - eta), , q)
  fitted <- t(apply(cumpr, 1L, function(x) diff(c(0, x, 1))))
  dimnames(fitted) <- list(row.names(m), lev)
  fit <- list(coefficients = beta, zeta = zeta, deviance = deviance,
              fitted.values = fitted, lev = lev, terms = Terms,
              df.residual = sum(wt) - pc - q, edf = pc + q, n = sum(wt),
              nobs = sum(wt),
              call = match.call(), method = method,
              convergence = res$convergence, niter = niter, lp = eta)
  if(Hess) {
    dn <- c(names(beta), names(zeta))
    H <- res$hessian
    dimnames(H) <- list(dn, dn)
    fit$Hessian <- H
  }
  if(model) fit$model <- m
  fit$na.action <- attr(m, "na.action")
  fit$contrasts <- cons
  fit$xlevels <- .getXlevels(Terms, m)
  class(fit) <- "polr"
  fit
}

print.polr <- function(x, ...)
{
  if(!is.null(cl <- x$call)) {
    cat("Call:\n")
    dput(cl, control=NULL)
  }
  if(length(coef(x))) {
    cat("\nCoefficients:\n")
    print(coef(x), ...)
  } else {
    cat("\nNo coefficients\n")
  }
  cat("\nIntercepts:\n")
  print(x$zeta, ...)
  cat("\nResidual Deviance:", format(x$deviance, nsmall=2), "\n")
  cat("AIC:", format(x$deviance + 2*x$edf, nsmall=2), "\n")
  if(nzchar(mess <- naprint(x$na.action))) cat("(", mess, ")\n", sep="")
  if(x$convergence > 0)
    cat("Warning: did not converge as iteration limit reached\n")
  invisible(x)
}

vcov.polr <- function(object, ...)
{
  jacobian <- function(theta) { ## dgamma by dtheta matrix
    k <- length(theta)
    etheta <- exp(theta)
    mat <- matrix(0 , k, k)
    mat[, 1] <- rep(1, k)
    for (i in 2:k) mat[i:k, i] <- etheta[i]
    mat
  }
  
  if(is.null(object$Hessian)) {
    message("\nRe-fitting to get Hessian\n")
    utils::flush.console()
    object <- update(object, Hess=TRUE,
                     start=c(object$coefficients, object$zeta))
  }
  vc <- ginv(object$Hessian)
  pc <- length(coef(object))
  gamma <- object$zeta
  z.ind <- pc + seq_along(gamma)
  theta <- c(gamma[1L], log(diff(gamma)))
  J <- jacobian(theta)
  A <- diag(pc + length(gamma))
  A[z.ind, z.ind] <- J
  V <- A %*% vc %*% t(A)
  structure(V,  dimnames = dimnames(object$Hessian))
}

summary.polr <- function(object, digits = max(3, .Options$digits - 3),
                         correlation = FALSE, ...)
{
  cc <- c(coef(object), object$zeta)
  pc <- length(coef(object))
  q <- length(object$zeta)
  coef <- matrix(0, pc+q, 3, dimnames=list(names(cc),
                                           c("Value", "Std. Error", "t value")))
  coef[, 1] <- cc
  vc <- vcov(object)
  coef[, 2] <- sd <- sqrt(diag(vc))
  coef[, 3] <- coef[, 1]/coef[, 2]
  object$coefficients <- coef
  object$pc <- pc
  object$digits <- digits
  if(correlation)
    object$correlation <- (vc/sd)/rep(sd, rep(pc+q, pc+q))
  class(object) <- "summary.polr"
  object
}

print.summary.polr <- function(x, digits = x$digits, ...)
{
  if(!is.null(cl <- x$call)) {
    cat("Call:\n")
    dput(cl, control=NULL)
  }
  coef <- format(round(x$coefficients, digits=digits))
  pc <- x$pc
  if(pc > 0) {
    cat("\nCoefficients:\n")
    print(x$coefficients[seq_len(pc), , drop=FALSE], quote = FALSE,
          digits = digits, ...)
  } else {
    cat("\nNo coefficients\n")
  }
  cat("\nIntercepts:\n")
  print(coef[(pc+1):nrow(coef), , drop=FALSE], quote = FALSE,
        digits = digits, ...)
  cat("\nResidual Deviance:", format(x$deviance, nsmall=2), "\n")
  cat("AIC:", format(x$deviance + 2*x$edf, nsmall=2), "\n")
  if(nzchar(mess <- naprint(x$na.action))) cat("(", mess, ")\n", sep="")
  if(!is.null(correl <- x$correlation)) {
    cat("\nCorrelation of Coefficients:\n")
    ll <- lower.tri(correl)
    correl[ll] <- format(round(correl[ll], digits))
    correl[!ll] <- ""
    print(correl[-1, -ncol(correl)], quote = FALSE, ...)
  }
  invisible(x)
}

predict.polr <- function(object, newdata, type=c("class","probs"), ...)
{
  if(!inherits(object, "polr")) stop("not a \"polr\" object")
  type <- match.arg(type)
  if(missing(newdata)) Y <- object$fitted
  else {
    newdata <- as.data.frame(newdata)
    Terms <- delete.response(object$terms)
    m <- model.frame(Terms, newdata, na.action = function(x) x,
                     xlev = object$xlevels)
    if (!is.null(cl <- attr(Terms, "dataClasses")))
      .checkMFClasses(cl, m)
    X <- model.matrix(Terms, m, contrasts = object$contrasts)
    xint <- match("(Intercept)", colnames(X), nomatch=0L)
    if(xint > 0) X <- X[, -xint, drop=FALSE]
    n <- nrow(X)
    q <- length(object$zeta)
    eta <- drop(X %*% object$coefficients)
    pfun <- switch(object$method, logistic = plogis, probit = pnorm,
                   cloglog = pgumbel, cauchit = pcauchy)
    cumpr <- matrix(pfun(matrix(object$zeta, n, q, byrow=TRUE) - eta), , q)
    Y <- t(apply(cumpr, 1L, function(x) diff(c(0, x, 1))))
    dimnames(Y) <- list(rownames(X), object$lev)
  }
  if(missing(newdata) && !is.null(object$na.action))
    Y <- napredict(object$na.action, Y)
  switch(type, class = {
    Y <- factor(max.col(Y), levels=seq_along(object$lev),
                labels=object$lev)
  }, probs = {})
  drop(Y)
}

extractAIC.polr <- function(fit, scale = 0, k = 2, ...)
{
  edf <- fit$edf
  c(edf, deviance(fit) + k * edf)
}

model.frame.polr <- function(formula, ...)
{
  dots <- list(...)
  nargs <- dots[match(c("data", "na.action", "subset"), names(dots), 0)]
  if(length(nargs) || is.null(formula$model)) {
    m <- formula$call
    m$start <- m$Hess <- m$... <- NULL
    m[[1L]] <- as.name("model.frame")
    m[names(nargs)] <- nargs
    if (is.null(env <- environment(formula$terms))) env <- parent.frame()
    data <- eval(m, env)
    if(!is.null(mw <- m$weights)) {
      nm <- names(data)
      nm[match("(weights)", nm)] <- as.character(mw)
      names(data) <- nm
    }
    data
  } else formula$model
}

pgumbel <- function(q, loc = 0, scale = 1, lower.tail = TRUE)
{
  q <- (q - loc)/scale
  p <- exp(-exp(-q))
  if (!lower.tail) 1 - p else p
}

dgumbel <- function (x, loc = 0, scale = 1, log = FALSE)
{
  x <- (x - loc)/scale
  d <- log(1/scale) - x - exp(-x)
  d[is.nan(d)] <- -Inf                # -tjb
  if (!log) exp(d) else d
}

anova.polr <- function (object, ..., test = c("Chisq", "none"))
{
  test <- match.arg(test)
  dots <- list(...)
  if (length(dots) == 0L)
    stop('anova is not implemented for a single "polr" object')
  mlist <- list(object, ...)
  nt <- length(mlist)
  dflis <- sapply(mlist, function(x) x$df.residual)
  s <- order(dflis, decreasing = TRUE)
  mlist <- mlist[s]
  if (any(!sapply(mlist, inherits, "polr")))
    stop('not all objects are of class "polr"')
  ns <- sapply(mlist, function(x) length(x$fitted.values))
  if(any(ns != ns[1L]))
    stop("models were not all fitted to the same size of dataset")
  rsp <- unique(sapply(mlist, function(x) paste(formula(x)[2L])))
  mds <- sapply(mlist, function(x) paste(formula(x)[3L]))
  dfs <- dflis[s]
  lls <- sapply(mlist, function(x) deviance(x))
  tss <- c("", paste(1L:(nt - 1), 2:nt, sep = " vs "))
  df <- c(NA, -diff(dfs))
  x2 <- c(NA, -diff(lls))
  pr <- c(NA, 1 - pchisq(x2[-1L], df[-1L]))
  out <- data.frame(Model = mds, Resid.df = dfs, Deviance = lls,
                    Test = tss, Df = df, LRtest = x2, Prob = pr)
  names(out) <- c("Model", "Resid. df", "Resid. Dev", "Test",
                  "   Df", "LR stat.", "Pr(Chi)")
  if (test == "none") out <- out[, 1L:6]
  class(out) <- c("Anova", "data.frame")
  attr(out, "heading") <-
    c("Likelihood ratio tests of ordinal regression models\n",
      paste("Response:", rsp))
  out
}

polr.fit <- function(x, y, wt, start, offset, method)
{
  logit <- function(p) log(p/(1 - p))
  
  fmin <- function(beta) {
    theta <- beta[pc + 1L:q]
    gamm <- c(-Inf, cumsum(c(theta[1L], exp(theta[-1L]))), Inf)
    eta <- offset
    if (pc > 0)
      eta <- eta + drop(x %*% beta[1L:pc])
    pr <- pfun(gamm[y + 1] - eta) - pfun(gamm[y] - eta)
    if (all(pr > 0))
      -sum(wt * log(pr))
    else Inf
  }
  
  gmin <- function(beta)
  {
    jacobian <- function(theta) { ## dgamma by dtheta matrix
      k <- length(theta)
      etheta <- exp(theta)
      mat <- matrix(0 , k, k)
      mat[, 1] <- rep(1, k)
      for (i in 2:k) mat[i:k, i] <- etheta[i]
      mat
    }
    theta <- beta[pc + 1L:q]
    gamm <- c(-Inf, cumsum(c(theta[1L], exp(theta[-1L]))), Inf)
    eta <- offset
    if(pc > 0) eta <- eta + drop(x %*% beta[1L:pc])
    pr <- pfun(gamm[y+1] - eta) - pfun(gamm[y] - eta)
    p1 <- dfun(gamm[y+1] - eta)
    p2 <- dfun(gamm[y] - eta)
    g1 <- if(pc > 0) t(x) %*% (wt*(p1 - p2)/pr) else numeric(0)
    xx <- .polrY1*p1 - .polrY2*p2
    g2 <- - t(xx) %*% (wt/pr)
    g2 <- t(g2) %*% jacobian(theta)
    if(all(pr > 0)) c(g1, g2) else rep(NA, pc+q)
  }
  
  pfun <- switch(method, logistic = plogis, probit = pnorm,
                 cloglog = pgumbel, cauchit = pcauchy)
  dfun <- switch(method, logistic = dlogis, probit = dnorm,
                 cloglog = dgumbel, cauchit = dcauchy)
  n <- nrow(x)
  pc <- ncol(x)
  lev <- levels(y)
  if(length(lev) <= 2L) stop("response must have 3 or more levels")
  y <- unclass(y)
  q <- length(lev) - 1L
  Y <- matrix(0, n, q)
  .polrY1 <- col(Y) == y
  .polrY2 <- col(Y) == y - 1L
  # pc could be 0.
  s0 <- if(pc > 0) c(start[seq_len(pc+1)], diff(start[-seq_len(pc)]))
  else c(start[1L], diff(start))
  res <- optim(s0, fmin, gmin, method="BFGS")
  beta <- res$par[seq_len(pc)]
  theta <- res$par[pc + 1L:q]
  zeta <- cumsum(c(theta[1L],exp(theta[-1L])))
  deviance <- 2 * res$value
  names(zeta) <- paste(lev[-length(lev)], lev[-1L], sep="|")
  if(pc > 0) {
    names(beta) <- colnames(x)
    eta <- drop(x %*% beta)
  } else {
    eta <- rep(0, n)
  }
  list(coefficients = beta, zeta = zeta, deviance = deviance)
}

profile.polr <- function(fitted, which = 1L:p, alpha = 0.01,
                         maxsteps = 10, del = zmax/5, trace = FALSE, ...)
{
  Pnames <- names(B0 <- coefficients(fitted))
  pv0 <- t(as.matrix(B0))
  p <- length(Pnames)
  if(is.character(which)) which <- match(which, Pnames)
  summ <- summary(fitted)
  std.err <- summ$coefficients[, "Std. Error"]
  mf <- model.frame(fitted)
  n <- length(Y <- model.response(mf))
  O <- model.offset(mf)
  if(!length(O)) O <- rep(0, n)
  W <- model.weights(mf)
  if(length(W) == 0L) W <- rep(1, n)
  OriginalDeviance <- deviance(fitted)
  X <- model.matrix(fitted)[, -1L, drop=FALSE] # drop intercept
  zmax <- sqrt(qchisq(1 - alpha, 1))
  profName <- "z"
  prof <- vector("list", length=length(which))
  names(prof) <- Pnames[which]
  start <- c(fitted$coefficients, fitted$zeta)
  for(i in which) {
    zi <- 0
    pvi <- pv0
    Xi <- X[,  - i, drop = FALSE]
    pi <- Pnames[i]
    for(sgn in c(-1, 1)) {
      if(trace) {
        message("\nParameter:", pi, c("down", "up")[(sgn + 1)/2 + 1])
        utils::flush.console()
      }
      step <- 0
      z <- 0
      ## LP is the linear predictor including offset.
      ## LP <- X %*% fitted$coef + O
      while((step <- step + 1) < maxsteps && abs(z) < zmax) {
        bi <- B0[i] + sgn * step * del * std.err[i]
        o <- O + X[, i] * bi
        fm <- polr.fit(x = Xi, y = Y, wt = W, start = start[-i],
                       offset = o, method = fitted$method)
        ri <- pv0
        ri[, names(coef(fm))] <- coef(fm)
        ri[, pi] <- bi
        pvi <- rbind(pvi, ri)
        zz <- fm$deviance - OriginalDeviance
        if(zz > - 1e-3) zz <- max(zz, 0)
        else stop("profiling has found a better solution, so original fit had not converged")
        z <- sgn * sqrt(zz)
        zi <- c(zi, z)
      }
    }
    si <- order(zi)
    prof[[pi]] <- structure(data.frame(zi[si]), names = profName)
    prof[[pi]]$par.vals <- pvi[si, ]
  }
  val <- structure(prof, original.fit = fitted, summary = summ)
  class(val) <- c("profile.polr", "profile")
  val
}

confint.polr <- function(object, parm, level = 0.95, trace = FALSE, ...)
{
  pnames <- names(coef(object))
  if(missing(parm)) parm <- seq_along(pnames)
  else if(is.character(parm))  parm <- match(parm, pnames, nomatch = 0L)
  message("Waiting for profiling to be done...")
  utils::flush.console()
  object <- profile(object, which = parm, alpha = (1. - level)/4.,
                    trace = trace)
  confint(object, parm=parm, level=level, trace=trace, ...)
}

confint.profile.polr <-
  function(object, parm = seq_along(pnames), level = 0.95, ...)
  {
    of <- attr(object, "original.fit")
    pnames <- names(coef(of))
    if(is.character(parm))  parm <- match(parm, pnames, nomatch = 0L)
    a <- (1-level)/2
    a <- c(a, 1-a)
    pct <- paste(round(100*a, 1), "%")
    ci <- array(NA, dim = c(length(parm), 2L),
                dimnames = list(pnames[parm], pct))
    cutoff <- qnorm(a)
    for(pm in parm) {
      pro <- object[[ pnames[pm] ]]
      if(length(pnames) > 1L)
        sp <- spline(x = pro[, "par.vals"][, pm], y = pro[, 1])
      else sp <- spline(x = pro[, "par.vals"], y = pro[, 1])
      ci[pnames[pm], ] <- approx(sp$y, sp$x, xout = cutoff)$y
    }
    drop(ci)
  }

logLik.polr <- function(object, ...)
  structure(-0.5 * object$deviance, df = object$edf, class = "logLik")

simulate.polr <- function(object, nsim = 1, seed = NULL, ...)
{
  if(!is.null(object$model) && any(model.weights(object$model) != 1))
    stop("weighted fits are not supported")
  
  rgumbel <- function(n, loc = 0, scale = 1) loc - scale*log(rexp(n))
  
  ## start the same way as simulate.lm
  if(!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
    runif(1)                     # initialize the RNG if necessary
  if(is.null(seed))
    RNGstate <- get(".Random.seed", envir = .GlobalEnv)
  else {
    R.seed <- get(".Random.seed", envir = .GlobalEnv)
    set.seed(seed)
    RNGstate <- structure(seed, kind = as.list(RNGkind()))
    on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
  }
  rfun <- switch(object$method, logistic = rlogis, probit = rnorm,
                 cloglog = rgumbel, cauchit = rcauchy)
  eta <- object$lp
  n <- length(eta)
  res <- cut(rfun(n*nsim, eta),
             c(-Inf, object$zeta, Inf),
             labels = colnames(fitted(object)),
             ordered_result = TRUE)
  val <- split(res, rep(seq_len(nsim), each=n))
  names(val) <- paste("sim", seq_len(nsim), sep="_")
  val <- as.data.frame(val)
  if (!is.null(nm <- rownames(fitted(object)))) row.names(val) <- nm
  attr(val, "seed") <- RNGstate
  val
}



install.packages("erer")
library(erer)


Model1 <- polr(Labour_Status ~ Age + Agesqrt + Children + Female + Partnership + Education_Level + MB_Generation + Overall_ICT, data = A1, Hess = T, method = "logistic")
summary(Model1)
AMEModel1 <- ocME(w = Model1)
AMEModel1


#Data cleaning

A1 <- na.omit(A1, cols = c("Employment_Dummy", "Female", "Age", "Partnership", "Education_Level", "Economic_activity", "ISCO", "MB_Generation", "Children", "ICT_Training"))


 #Step 2: Probit Estimation (Outcome: Overall Employment Whole Sample)

Model2 <- glm(Employment_Dummy ~ Age + Agesqrt + Children + Female + Partnership + MB_Generation + Education_Level + ICT_Training, data = A1, family = quasibinomial(link = "probit"))
summary(Model2)
AMEModel2_Scalar <- mean(dnorm(predict(Model2, type = "link")))
AMEModel2 <- AMEModel2_Scalar*coef(Model2)
print(AMEModel2)


install.packages("stargazer")
library(stargazer)


 #Subsetting Male and Female

A1_Female <- subset(A1, Female == 1)
A1_Male <- subset(A1, Female == 0) 


 #Step 3: Probit Estimation (Overall Employment Male Sample)

Model2_Male <- glm(Employment_Dummy ~ Age + Agesqrt + Children + Partnership + MB_Generation + Education_Level + ICT_Training, data = A1_Male, family = quasibinomial(link = "probit"))
summary(Model2_Male)
AMEModel2_Male_Scalar <- mean(dnorm(predict(Model2_Male, type = "link")))
AMEModel2_Male <- AMEModel2_Male_Scalar*coef(Model2_Male)
print(AMEModel2_Male)


 #Step 4: Probit Estimation (Overall Employment Female Sample)

Model2_Female <- glm(Employment_Dummy ~ Age + Agesqrt + Children + Partnership + MB_Generation + Education_Level + ICT_Training, data = A1_Female, family = quasibinomial(link = "probit"))
summary(Model2_Female)
AMEModel2_Female_Scalar <- mean(dnorm(predict(Model2_Female, type = "link")))
AMEModel2_Female <- AMEModel2_Female_Scalar*coef(Model2_Female)
print(AMEModel2_Female)


stargazer(Model2, Model2_Male, Model2_Female, coef = list(AMEModel2, AMEModel2_Male, AMEModel2_Female), title = "Results Overall Employment", type = "html", out = "ResultsProbitOverall.htm", align = TRUE)




 #Step 5: Probit Estimamtion (Outcome: New Job Whole Sample)

Model3 <- glm(New_Job ~ Age + Agesqrt + Children + Partnership + MB_Generation + Education_Level + Economic_activity + ICT_Training, data = A1, family = quasibinomial(link = "probit"))
summary(Model3)
AMEModel3_Scalar <- mean(dnorm(predict(Model3, type = "link")))
AMEModel3 <- AMEModel3_Scalar*coef(Model3)
print(AMEModel3)


 #Step 6: Probit Estimamtion (Outcome: New Job Male Sample)

Model3_Male <- glm(New_Job ~ Age + Agesqrt + Children + Partnership + MB_Generation + Education_Level + Economic_activity + ICT_Training, data = A1_Male, family = quasibinomial(link = "probit"))
summary(Model3_Male)
AMEModel3_Male_Scalar <- mean(dnorm(predict(Model3_Male, type = "link")))
AMEModel3_Male <- AMEModel3_Male_Scalar*coef(Model3_Male)
print(AMEModel3_Male)


 #Step 7: Probit Estimamtion (Outcome: New Job Female Sample)

Model3_Female <- glm(New_Job ~ Age + Agesqrt + Children + Partnership + MB_Generation + Education_Level + Economic_activity + ICT_Training, data = A1_Female, family = quasibinomial(link = "probit"))
summary(Model3_Female)
AMEModel3_Female_Scalar <- mean(dnorm(predict(Model3_Female, type = "link")))
AMEModel3_Female <- AMEModel3_Female_Scalar*coef(Model3_Female)
print(AMEModel3_Female)


 #Step 8: Probit Estimamtion (Outcome: higher Salary Whole Sample)

Model4 <- glm(higher_Salary ~ Age + Agesqrt + Children + Partnership + MB_Generation + Education_Level + Economic_activity + ICT_Training, data = A1, family = quasibinomial(link = "probit"))
summary(Model4)
AMEModel4_Scalar <- mean(dnorm(predict(Model4, type = "link")))
AMEModel4 <- AMEModel4_Scalar*coef(Model3)
print(AMEModel4)


 #Step 9: Probit Estimamtion (Outcome: higher Salary Male Sample)

Model4_Male <- glm(higher_Salary ~ Age + Agesqrt + Children + Partnership + MB_Generation + Education_Level + Economic_activity + ICT_Training, data = A1_Male, family = quasibinomial(link = "probit"))
summary(Model4_Male)
AMEModel4_Male_Scalar <- mean(dnorm(predict(Model4_Male, type = "link")))
AMEModel4_Male <- AMEModel4_Male_Scalar*coef(Model4_Male)
print(AMEModel4_Male)


 #Step 10: Probit Estimamtion (Outcome: higher Salary Female Sample)

Model4_Female <- glm(higher_Salary ~ Age + Agesqrt + Children + Partnership + MB_Generation + Education_Level + Economic_activity + ICT_Training, data = A1_Female, family = quasibinomial(link = "probit"))
summary(Model4_Female)
AMEModel4_Female_Scalar <- mean(dnorm(predict(Model4_Female, type = "link")))
AMEModel4_Female <- AMEModel4_Female_Scalar*coef(Model4_Female)
print(AMEModel4_Female)


 #Step 11: Probit Estimamtion (Outcome: Job Promotion Whole Sample)

Model5 <- glm(Job_Promotion ~ Age + Agesqrt + Children + Partnership + MB_Generation + Education_Level + Economic_activity + ICT_Training, data = A1, family = quasibinomial(link = "probit"))
summary(Model5)
AMEModel5_Scalar <- mean(dnorm(predict(Model5, type = "link")))
AMEModel5 <- AMEModel5_Scalar*coef(Model5)
print(AMEModel5)


 #Step 12: Probit Estimamtion (Outcome: Job Promotion Male Sample)

Model5_Male <- glm(Job_Promotion ~ Age + Agesqrt + Children + Partnership + MB_Generation + Education_Level + Economic_activity + ICT_Training, data = A1_Male, family = quasibinomial(link = "probit"))
summary(Model5_Male)
AMEModel5_Male_Scalar <- mean(dnorm(predict(Model5_Male, type = "link")))
AMEModel5_Male <- AMEModel5_Male_Scalar*coef(Model5_Male)
print(AMEModel5_Male)


 #Step 13: Probit Estimamtion (Outcome: Job Promotion Female Sample)

Model5_Female <- glm(Job_Promotion ~ Age + Agesqrt + Children + Partnership + MB_Generation + Education_Level + Economic_activity + ICT_Training, data = A1_Female, family = quasibinomial(link = "probit"))
summary(Model5_Female)
AMEModel5_Female_Scalar <- mean(dnorm(predict(Model5_Female, type = "link")))
AMEModel5_Female <- AMEModel5_Female_Scalar*coef(Model5_Female)
print(AMEModel5_Female)


stargazer(Model3, Model3_Male, Model3_Female, Model4, Model4_Male, Model4_Female, Model5, Model5_Male, Model5_Female, coef = list(AMEModel3, AMEModel3_Male, AMEModel3_Female, AMEModel4, AMEModel4_Male, AMEModel4_Female, AMEModel5, AMEModel5_Male, AMEModel5_Female), title = "Results Benefits through ICT Training", type = "html", out = "ResultsProbitBenefits.htm", align = TRUE)




 #Step 14: Linear Probability Estimamtion (Outcome: New Job Whole Sample) - Styria specific

Model6 <- lm(New_Job ~ Age + Agesqrt + Children + Partnership + MB_Generation + Education_Level + Economic_activity + ICT_Training*ISCO_dummy_2 + ICT_Training*ISCO_dummy_3 + ICT_Training*ISCO_dummy_5 + ICT_Training*ISCO_dummy_7, data = A1)
coeftest(Model6, vcov. = vcovHC)
coefModel6 <- coeftest(Model6, vcov. = vcovHC)


 #Step 15: Linear Probability Estimamtion (Outcome: New Job Male Sample) - Styria specific

Model6_Male <- lm(New_Job ~ Age + Agesqrt + Children + Partnership + MB_Generation + Education_Level + Economic_activity + ICT_Training*ISCO_dummy_2 + ICT_Training*ISCO_dummy_3 + ICT_Training*ISCO_dummy_7, data = A1_Male)
coeftest(Model6_Male, vcov. = vcovHC)
coefModel6_Male <- coeftest(Model6_Male, vcov. = vcovHC)


 #Step 16: Linear Probability Estimamtion (Outcome: New Job Female Sample) - Styria specific

Model6_Female <- lm(New_Job ~ Age + Agesqrt + Children + Partnership + MB_Generation + Education_Level + Economic_activity + ICT_Training*ISCO_dummy_2 + ICT_Training*ISCO_dummy_3 + ICT_Training*ISCO_dummy_5, data = A1_Female)
coeftest(Model6_Female, vcov. = vcovHC)
coefModel6_Female <- coeftest(Model6_Female, vcov. = vcovHC)


 #Step 17: Linear Probability Estimamtion (Outcome: Higher Salary Whole Sample) - Styria specific

Model7 <- lm(higher_Salary ~ Age + Agesqrt + Children + Partnership + MB_Generation + Education_Level + Economic_activity + ICT_Training*ISCO_dummy_2 + ICT_Training*ISCO_dummy_3 + ICT_Training*ISCO_dummy_5 + ICT_Training*ISCO_dummy_7, data = A1)
coeftest(Model7, vcov. = vcovHC)
coefModel7 <- coeftest(Model7, vcov. = vcovHC)


 #Step 18: Linear Probability Estimamtion (Outcome: Higher Salary Male Sample) - Styria specific

Model7_Male <- lm(higher_Salary ~ Age + Agesqrt + Children + Partnership + MB_Generation + Education_Level + Economic_activity + ICT_Training*ISCO_dummy_2 + ICT_Training*ISCO_dummy_3 + ICT_Training*ISCO_dummy_7, data = A1_Male)
coeftest(Model7_Male, vcov. = vcovHC)
coefModel7_Male <- coeftest(Model7_Male, vcov. = vcovHC)


 #Step 19: Linear Probability Estimamtion (Outcome: Higher Salary Female Sample) - Styria specific

Model7_Female <- lm(higher_Salary ~ Age + Agesqrt + Children + Partnership + MB_Generation + Education_Level + Economic_activity + ICT_Training*ISCO_dummy_2 + ICT_Training*ISCO_dummy_3 + ICT_Training*ISCO_dummy_5, data = A1_Female)
coeftest(Model7_Female, vcov. = vcovHC)
coefModel7_Female <- coeftest(Model7_Female, vcov. = vcovHC)


 #Step 20: Linear Probability Estimamtion (Outcome: Job Promotion Whole Sample) - Styria specific

Model8 <- lm(Job_Promotion ~ Age + Agesqrt + Children + Partnership + MB_Generation + Education_Level + Economic_activity + ICT_Training*ISCO_dummy_2 + ICT_Training*ISCO_dummy_3 + ICT_Training*ISCO_dummy_5 + ICT_Training*ISCO_dummy_7, data = A1)
coeftest(Model8, vcov. = vcovHC)
coefModel8 <- coeftest(Model8, vcov. = vcovHC)


 #Step 21: Linear Probability Estimamtion (Outcome: Job Promotion Male Sample) - Styria specific

Model8_Male <- lm(Job_Promotion ~ Age + Agesqrt + Children + Partnership + MB_Generation + Education_Level + Economic_activity + ICT_Training*ISCO_dummy_2 + ICT_Training*ISCO_dummy_3 + ICT_Training*ISCO_dummy_7, data = A1_Male)
coeftest(Model8_Male, vcov. = vcovHC)
coefModel8_Male <- coeftest(Model8_Male, vcov. = vcovHC)


 #Step 22: Linear Probability Estimamtion (Outcome: Job Promotion Female Sample) - Styria specific

Model8_Female <- lm(Job_Promotion ~ Age + Agesqrt + Children + Partnership + MB_Generation + Education_Level + Economic_activity + ICT_Training*ISCO_dummy_2 + ICT_Training*ISCO_dummy_3 + ICT_Training*ISCO_dummy_5, data = A1_Female)
coeftest(Model8_Female, vcov. = vcovHC)
coefModel8_Female <- coeftest(Model8_Female, vcov. = vcovHC)


stargazer(Model6, Model6_Male, Model6_Female, Model7, Model7_Male, Model7_Female, Model8, Model8_Male, Model8_Female, coef = list(coefModel6, coefModel6_Male, coefModel6_Female, coefModel7, coefModel7_Male, coefModel7_Female, coefModel8, coefModel8_Male, coefModel8_Female), title = "Results Benefits through ICT Training - Styria specific", type = "html", out = "ResultsLPMBenefits_Styria.htm", align = TRUE)




#Descriptive Statistics

install.packages("Weighted.Desc.Stat")
library(Weighted.Desc.Stat)

install.packages("spatstat")
library(spatstat)

install.packages("Hmisc")
library(Hmisc)


#Employment rate by Gender

#Male

employment_rate_1Gen_Male <- table(A1_1Gen_Male$Employment_Dummy)
employment_rate_1Gen_Male <- prop.table(employment_rate_1Gen_Male, margin = NULL)
View(employment_rate_1Gen_Male)

#Female

employment_rate_1Gen_Female <- table(A1_1Gen_Female$Employment_Dummy)
employment_rate_1Gen_Female <- prop.table(employment_rate_1Gen_Female, margin = NULL)
View(employment_rate_1Gen_Female)


#Age by Gender

#Male

w.mean(A1_1Gen_Male$Age)

#Female

w.mean(A1_1Gen_Female$Age)



