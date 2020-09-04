#setwd("D:/Matma/2gi_stopien/Kruk/projekt")
load("KrukUWr2019.RData")
library(data.table)
#library(randomForest)
              
variables_events <- c(
                      "NumberOfCalls",
                      "NumberOfLettersSent",
                      "NumberOfVisits",
                      "NumberOfCallsWithClient",
                      "NumberOfVisitsWithClient",
                      "NumberOfLettersReceived",
                      "NumberOfAgreementConcluded",
                      "NumberOfAgreementSigned",
                      "TransferToLegalProcess")

#summary(events)

#uzupelniamy wartosci NA poprzez 0
for (variable in variables_events) 
{
  eval(parse(text=paste("events[is.na(",variable,"), ",variable,":= 0.0]",sep="")))
}

#ucinamy wplaty do nieujemnych
events[PaymentAmount < 0, PaymentAmount := 0.0]
#tam gdzie wplata nie byla odnotowana dajemy 0 na liczbie i kwocie wplat
events[is.na(PaymentAmount), `:=` (NumberOfPayment = 0.0, PaymentAmount = 0.0)]

anyNA(events)

events[NumberOfLettersSent == 0, NumberOfLettersReceived := 0.0]

events[NumberOfLettersSent < NumberOfLettersReceived, NumberOfLettersReceived := NumberOfLettersSent]

## dyskontowanie wplat

rate <- 0.05

events[, PaymentAmount_d := PaymentAmount/(1 + rate/Month)^Month]

#events_agr[, on = "CaseId"]

events_agr <- events[ , .(
                          NOfCalls12M = sum(NumberOfCalls),
                          NOfCallsWithClient12M = sum(NumberOfCallsWithClient),
                          NOfCallsWithClientByCalls12M = sum(NumberOfCallsWithClient)/max(sum(NumberOfCalls), 1),
                          NOfLettersSent12M = sum(NumberOfLettersSent),
                          NOfLettersReceived12M = sum(NumberOfLettersReceived),
                          NOfLettersReceivedBySent12M = sum(NumberOfLettersReceived)/max(sum(NumberOfLettersSent), 1),
                          NOfVisits12M = sum(NumberOfVisits),
                          NOfVisitsWithClient12M = sum(NumberOfVisitsWithClient),
                          NOfVisitsWithClientByVisits12M = sum(NumberOfVisitsWithClient)/max(sum(NumberOfVisits), 1),
                          NOfAgreementSigned12M = sum(NumberOfAgreementSigned),
                          NOfAgreementConcluded12M = sum(NumberOfAgreementConcluded),
                          NOfAgreementConcludedBySigned12M = sum(NumberOfAgreementConcluded)/max(sum(NumberOfAgreementConcluded), 1),
                          TransferToLegalProcess = max(TransferToLegalProcess),
                          NOfPayments12M = sum(NumberOfPayment),
                          PaymentsAmount12M = sum(PaymentAmount),
                          PaymentsAmount_d12M = sum(PaymentAmount_d),
                          MeanPayment12M = sum(PaymentAmount)/max(sum(NumberOfPayment), 1)
                        ), by = CaseId]



#summary(events_agr)

save(events_agr, file = "events_agr.rdata")


####CASES#####################
#w skrocie:
#LoanAmount - podzielone na dwie czesci: 1.gdzie produkt to credit card - przypisanie wartosci principal
#2. gdzie produkt to cash loan - regresja liniowa obliczona na zbiorze kompletnym ze zmienna principal
#Other - ekspercko - z rownania TOA=principal+Interest+Other
#D_ContractDateToImportDate - mediana
#M_LastPaymentToImportDate - losowanie z rozkladu
#LastPaymentAmount - mediana (wzgledem grup M_LastPaymentToImportDate)
#Gender - firmy to teraz faceci
#Land - losowy przypisanie
#GDPPerCapita i MeanSalary - srednie wedlug przypisanego landu
#DPD - srednia wedlug zakresu TOA (zakres co 10K)
#Bailiff + ClosedExecution + ExternalAgency- je?li by?y razem 3 braki, to wszystkie dosta?y 0 (to wszystkie przypadki NA dla ExternalAgency)
#Jesli Bailiff i ClosedExecution mialy razem NA, to dostaly oba 0
#PopulationInCity - drzewo losowe

summary(cases)
anyNA(cases$LoanAmount)
unique(cases$Product)

summary(cases$LoanAmount)

#LOAN AMOUNT

###z laborek: karty kredytowe
#### w przypadku kart kredytowych warto?c LoanAmount mo?emy zastapi? Principal #####

cases[Product == "Credit card" & is.na(LoanAmount), LoanAmount := Principal]
anyNA(cases[Product == "Credit card", LoanAmount]) # sprawdzamy czy w tym zbiorze zostaly NA

summary(cases$LoanAmount)

### LoanAmount:cash 

#princ=cases[Product == "Cash loan" & is.na(LoanAmount)==0,Principal]
#loan=cases[Product == "Cash loan" & is.na(LoanAmount)==0,LoanAmount]

cash=lm(LoanAmount~Principal, data = cases[Product == "Cash loan" & is.na(LoanAmount)==0])

#pred=cases[Product == "Cash loan" & is.na(LoanAmount)==1,Principal]*1.058 +1816.559
pred <- predict.lm(object = cash, newdata = cases[Product == "Cash loan" & is.na(LoanAmount)==1])

cases[Product == "Cash loan" & is.na(LoanAmount), LoanAmount := pred]

summary(cases)

#OTHER

# ISNULL from SQL
gcisna <- function (x, xSubst) {
  x[is.na(x)] <- xSubst
  return(x)
}

###uzupelnienie Other
cases[, TOAsum:=Principal + Interest + gcisna(Other,0)]
cases[, .(abs(TOAsum -TOA))][V1 > 1e-2, ][order(V1)]
cases[, `:=`(Other=TOA - (Principal + Interest), TOAsum=NULL)]
summary(cases$Other)




#D_ContractDateToImportDate - mediana *wyklad - malo przypadkow - 39)

cases[!is.na(D_ContractDateToImportDate), .(
  NAcount=cases[, .N] - .N,
  Med=median(D_ContractDateToImportDate),
  Avg=mean(D_ContractDateToImportDate))] 

# Te 39 przypadki to raczej niewiele znacz?ca kosmetyka w tej puli spraw
cases[is.na(D_ContractDateToImportDate), 
      D_ContractDateToImportDate:=median(cases$D_ContractDateToImportDate, na.rm=T)]
#


tmp <- cases[!is.na(M_LastPaymentToImportDate), 
             .(Pr=.N/cases[!is.na(M_LastPaymentToImportDate), .N]), 
             by=M_LastPaymentToImportDate]
tmp <- tmp[order(M_LastPaymentToImportDate)]
tmp

# Wygenerowanie nowych warto?ci  
newValues <- sample(tmp$M_LastPaymentToImportDate, 
                    size=cases[is.na(M_LastPaymentToImportDate), .N], 
                    prob=tmp$Pr, replace=TRUE)  

# Por?wnanie rozk?ad?w
#tmp$PrNewVal <- as.integer(table(newValues))/
#  cases[is.na(M_LastPaymentToImportDate), .N]
#tmp

#plot(tmp$PrNewVal/tmp$Pr - 1, 
#     ylab="Percent Deviance", xlab="M_LastPaymentToImportDate")
#abline(h=0, lty=3)

cases[is.na(M_LastPaymentToImportDate), M_LastPaymentToImportDate:=newValues]

#anyNA(cases$M_LastPaymentToImportDate)

# Ze zmienn? M_LastPaymentToImportDate powi?zana jest zmienna LastPaymentAmount
# warto zachowa? zale?no?ci pomi?dzy nimi
rm(tmp)
tmp <- cases[, .(Med=median(LastPaymentAmount, na.rm=TRUE))
             , by=M_LastPaymentToImportDate][order(M_LastPaymentToImportDate)]

cases <- cases[tmp, on="M_LastPaymentToImportDate"]
cases[is.na(LastPaymentAmount), LastPaymentAmount:=Med]
cases[, Med:=NULL]

#anyNA(cases$LastPaymentAmount)

# Swiadome przypisanie firm m??czyznom (przyda si? nam w modelu)
cases[is.na(Gender), Gender:="MALE"]
# Wiek dla firm to mediana wieku mezczyzn
cases[Age == -1, Age:=cases[Age != -1 & Gender == "MALE", median(Age)]] 

# Swiadome losowe przypisanie Landu
cases[is.na(Land), Land:=sample(1:37, cases[is.na(Land), .N], replace=TRUE)]
# oraz uzupe?nienie GDPPerCapita i MeanSalary (wzgl?dem Landu)
cases <- cases[cases[, .(
  GDP=mean(GDPPerCapita, na.rm=T), 
  MS=mean(MeanSalary, na.rm=T)), by=Land], on="Land"]
cases[is.na(GDPPerCapita), `:=`(
  GDPPerCapita=GDP,
  MeanSalary=MS)]
cases[, `:=`(GDP=NULL, MS=NULL)]   

#! ################# uzupe?nianie DPD mediana

#library(corrplot)
#corrplot(cor(cases[,.(TOA, Principal, Interest, Other, DPD)], use="pairwise.complete.obs", method="pearson"), order = "hclust", tl.col='black', tl.cex=.75)

cases[is.na(DPD), DPD:= median(cases[!is.na(DPD), DPD])]

###

####Bailiff + ClosedExecution + ExternalAgency############## uzupelnienie zerami
cases[is.na(Bailiff)&is.na(ClosedExecution)&is.na(ExternalAgency),`:=` (Bailiff = 0, ClosedExecution = 0,ExternalAgency=0) ]

cases[is.na(Bailiff)&is.na(ClosedExecution),`:=` (Bailiff = 0, ClosedExecution = 0)]
#summary(cases)
#cases[(Bailiff==0)&(ClosedExecution==1),]#brak


#! ######################################################## Uzupe?nianie modelem

summary(cases)

cases[, `:=`(
  ExternalAgency=as.factor(ExternalAgency),
  Bailiff=as.factor(Bailiff),
  ClosedExecution=as.factor(ClosedExecution),
  Product=as.factor(Product),
  Gender=as.factor(Gender),
  Land=as.factor(Land)
)]

# Bierzemy zar?wno karty jak i po?yczki, wi?c dla u?atwienia pomijamy LoanAmount
casesTmp <- copy(cases[!is.na(PopulationInCity), 
                       .SD, .SDcols=setdiff(names(cases), c("CaseId", "LoanAmount"))])

n <- casesTmp[, .N]
train <- sample(1:n, 0.6*n)
trnSet <- casesTmp[train, ]
tstSet <- casesTmp[-train, ]

# Uwaga: Drzewa losowe to tylko jeden z mo?liwych modeli, kt?ry mo?na u?y? do 
# imputacji danych

library(h2o)
h2o.init(nthreads = -1)

trnSet.hex <- as.h2o(trnSet)
tstSet.hex <- as.h2o(tstSet)

variables <- setdiff(names(trnSet),"PopulationInCity")

rndF <- h2o.randomForest(y = "PopulationInCity",
                         x = variables,
                         training_frame = trnSet.hex,
                         validation_frame = tstSet.hex,
                         ntrees = 500,
                         mtries = 6,
                         min_rows = 200,
                         seed = 1234)

#h2o.saveModel(object = rndF, path = getwd(), force = TRUE)
rndF <- h2o.loadModel("DRF_model_R_1579775190358_1")

cases[is.na(PopulationInCity), 
      PopulationInCity:=as.vector(h2o.predict(rndF,
                                              as.h2o(cases[is.na(PopulationInCity), ])))]

#After using H2O
h2o.shutdown(prompt=FALSE)

summary(cases)
anyNA(cases)
#rm(tmp, woNAcases, rndF)


#benchmark per case

benchmark_pcase <- cases[events_agr[, .(CaseId, NPV = PaymentsAmount_d12M*0.8)], on = "CaseId"]


#podział na zbiory testowy, walidacyjny, testowy ( na pewno można mądrzej to zrobić)

n <- benchmark_pcase[, .N]
set.seed(10)
benchmark_pcase[, Set := sample(1:3, n, replace = TRUE, prob = c(0.5, 0.25, 0.25))]

#1 - train
#2 - validation
#3 - test

##############################################


#konwersja 
benchmark_pcase[, `:=` (Credit = ifelse(Product == "Credit card", 1L, 0L),
                        Female = ifelse(Gender == "FEMALE", 1L, 0L),
                        Product = NULL,
                        Gender = NULL)]

#for(v in names(benchmark_pcase))
#{#v<-"ExternalAgency"
#  eval(parse(text = paste("benchmark_pcase[, ", v, " := as.numeric(", v, ")]", sep = "")))
#}


#-------------------------------------
#WOE

#WOE dla Land

library(InformationValue)

Y <- as.factor(benchmark_pcase[events_agr, on = "CaseId"][Set == 1,
                                                          ifelse(NOfPayments12M > 2, 1, 0)])

Land_WOE <- as.data.table(WOETable(X=as.factor(benchmark_pcase[Set == 1, Land]), Y=Y))
Land_WOE <- Land_WOE[, .(Land = CAT, Land_WOE = WOE)]
#WOE(X=as.factor(benchmark_pcase[Set == 1, Land]), Y=Y)

benchmark_pcase <- benchmark_pcase[Land_WOE, on = "Land"]
benchmark_pcase[, Land := NULL]

#WOE dla M_LastPaymentToImportDate ???

benchmark_pcase[M_LastPaymentToImportDate == 50, M_LastPaymentToImportDate_d :="50"]
benchmark_pcase[M_LastPaymentToImportDate != 50,
                M_LastPaymentToImportDate_d := cut(M_LastPaymentToImportDate,
                                                   c(0, 10, 20, 30, Inf), include.lowest = T)]


#table(X)

M_LastPaymentToImportDate_WOE <- as.data.table(WOETable(X = as.factor(benchmark_pcase[Set == 1, M_LastPaymentToImportDate_d]),
                                                        Y = Y))
M_LastPaymentToImportDate_WOE <- M_LastPaymentToImportDate_WOE[, .(M_LastPaymentToImportDate_d = CAT,
                                                                   M_LastPaymentToImportDate_WOE = WOE)]

benchmark_pcase <- benchmark_pcase[M_LastPaymentToImportDate_WOE,
                                   on = "M_LastPaymentToImportDate_d"][order(CaseId)]

benchmark_pcase[, `:=` (M_LastPaymentToImportDate = NULL,
                        M_LastPaymentToImportDate_d = NULL)]

#SAVE

save(benchmark_pcase, file = "benchmark_pcase.RData")
