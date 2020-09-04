
library(data.table)
#getwd()
#setwd(...)
load("benchmark_pcase.rdata")

# dzielenie na portfele

#zl - zwykłe losowanie
#dzp - duże zadłużenie początkowe TOA
#mzp - małe zadłużenie początkowe TOA
#cl - tylko sprawy Cash loan
#cc - tylko sprawy Credit card
#dDPD - długi czas DPD
#mDPD - mały czas DPD
#b - sprawa skierowana do komornika
#nb - sprawa nie skierowana do komornika
#dzp+dDPD
#dzp+mDPD
#mzp+dDPD
#mzp+mDPD
#dzp+dDPD+b
#dzp+mDPD+b
#mzp+dDPD+b
#mzp+mDPD+b
#dzp+dDPD+nb
#dzp+mDPD+nb
#mzp+dDPD+nb
#mzp+mDPD+nb


Portfolios <- data.table()
r <- c(1,0.5,0.5)
n <- 1000 #liczba spraw w portfelu

for (s in 1:3) {
  #s<-1
  portfel <- "portfolio"
  
  sprawy <- benchmark_pcase[Set == s, 1]  
  
  #Zwykłe Losowanie
  print(paste("Zwykle losowanie, s=",s))
  for(i in 1:(50*r[s]))
  {#i<-1
    Portfolios <- rbind(Portfolios,
                        data.table(CaseId = sample(sprawy[, CaseId], n, replace = TRUE), 
                                   PortfolioId = paste(portfel, s, i, "zl", sep= "_")))
  } 
  
  #sprawy z dużym zadłużeniem całkowitym TOA
  print(paste("Duze TOA, s=",s))
  sprawy <- benchmark_pcase[TOA > quantile(benchmark_pcase$TOA,0.75) & Set == s, 1]
  
  for(i in 1:(50*r[s]))
  {
    Portfolios <- rbind(Portfolios,
                        data.table(CaseId = sample(sprawy[, CaseId], n, replace = TRUE),
                                   PortfolioId = paste(portfel, s, i, "dzp", sep= "_")))
  }
  
  #sprawy z małym zadłużeniem  całkowitym TOA
  print(paste("Male TOA, s=",s))
  sprawy <- benchmark_pcase[TOA < quantile(benchmark_pcase$TOA,0.25) & Set == s, 1]
  
  for(i in 1:(50*r[s]))
  {
    Portfolios <- rbind(Portfolios, 
                        data.table(CaseId = sample(sprawy[, CaseId], n, replace = TRUE),
                                   PortfolioId = paste(portfel, s, i, "mzp", sep= "_")))
  }
  
  #tylko Cash loan
  print(paste("Cash loan, s=",s))
  sprawy <- benchmark_pcase[Credit == 0 & Set == s, 1]
  
  for(i in 1:(50*r[s]))
  {
    Portfolios <- rbind(Portfolios,
                        data.table(CaseId = sample(sprawy[, CaseId], n, replace = TRUE),
                                   PortfolioId = paste(portfel, s, i, "cl", sep= "_")))
  }
  
  #tylko Credit card
  print(paste("Credit card, s=",s))
  sprawy <- benchmark_pcase[Credit == 1 & Set == s, 1]
  
  for(i in 1:(50*r[s]))
  {
    Portfolios <- rbind(Portfolios,
                        data.table(CaseId = sample(sprawy[, CaseId], n, replace = TRUE),
                                   PortfolioId = paste(portfel, s, i, "cc", sep= "_")))
  }
  
  #sprawy z dużym DPD
  print(paste("duze DPD, s=",s))
  sprawy <- benchmark_pcase[DPD > quantile(benchmark_pcase$DPD,0.75) & Set == s, 1]
  
  for(i in 1:(50*r[s]))
  {
    Portfolios <- rbind(Portfolios,
                        data.table(CaseId = sample(sprawy[, CaseId], n, replace = TRUE),
                                   PortfolioId = paste(portfel, s, i, "dDPD", sep= "_")))
  }
  
  #sprawy z małym DPD
  print(paste("male DPD, s=",s))
  sprawy <- benchmark_pcase[DPD < quantile(benchmark_pcase$DPD,0.25) & Set == s, 1]
  
  for(i in 1:(50*r[s]))
  {
    Portfolios <- rbind(Portfolios,
                        data.table(CaseId = sample(sprawy[, CaseId], n, replace = TRUE),
                                   PortfolioId = paste(portfel, s, i, "mDPD", sep= "_")))
  }  
  
  #sprawy skierowane do komornika
  print(paste("Bailiff, s=",s))
  sprawy <- benchmark_pcase[Bailiff==1 & Set == s, 1]
  
  for(i in 1:(50*r[s]))
  {
    Portfolios <- rbind(Portfolios, 
                        data.table(CaseId = sample(sprawy[, CaseId], n, replace = TRUE),
                                   PortfolioId = paste(portfel, s, i, "b", sep= "_")))
  }
  
  #sprawy nie skierowane do komornika
  print(paste("Bez Bailiff, s=",s))
  sprawy <- benchmark_pcase[Bailiff==0 & Set == s, 1]
  
  for(i in 1:(50*r[s]))
  {
    Portfolios <- rbind(Portfolios,
                        data.table(CaseId = sample(sprawy[, CaseId], n, replace = TRUE),
                                   PortfolioId = paste(portfel, s, i, "nb", sep= "_")))
  }
  
  #sprawy z dużym zadłużeniem  całkowitym TOA i dużym DPD
  print(paste("duze TOA + duze DPD, s=",s))
  sprawy <- benchmark_pcase[DPD > quantile(benchmark_pcase$DPD,0.5) & TOA > quantile(benchmark_pcase$TOA,0.5) & Set == s, 1] # tu zmieniłem już na 50-50
  
  for(i in 1:(50*r[s]))
  {
    Portfolios <- rbind(Portfolios, 
                        data.table(CaseId = sample(sprawy[, CaseId], n, replace = TRUE),
                                   PortfolioId = paste(portfel, s, i, "dzp+dDPD", sep= "_")))
  }
  
  #sprawy z dużym zadłużeniem  całkowitym TOA i małym DPD
  print(paste("duze TOA + male DPD, s=",s))
  sprawy <- benchmark_pcase[DPD < quantile(benchmark_pcase$DPD,0.5) & TOA > quantile(benchmark_pcase$TOA,0.5) & Set == s, 1]
  
  for(i in 1:(50*r[s]))
  {
    Portfolios <- rbind(Portfolios,
                        data.table(CaseId = sample(sprawy[, CaseId], n, replace = TRUE),
                                   PortfolioId = paste(portfel, s, i, "dzp+mDPD", sep= "_")))
  }
  
  #sprawy z małym zadłużeniem  całkowitym TOA i dużym DPD
  print(paste("male TOA + duze DPD, s=",s))
  sprawy <- benchmark_pcase[DPD > quantile(benchmark_pcase$DPD,0.5) & TOA < quantile(benchmark_pcase$TOA,0.5) & Set == s, 1]
  
  for(i in 1:(50*r[s]))
  {
    Portfolios <- rbind(Portfolios, 
                        data.table(CaseId = sample(sprawy[, CaseId], n, replace = TRUE),
                                   PortfolioId = paste(portfel, s, i, "mzp+dDPD", sep= "_")))
  }
  
  #sprawy z małym zadłużeniem  całkowitym TOA i małym DPD
  print(paste("male TOA + male DPD, s=",s))
  sprawy <- benchmark_pcase[DPD < quantile(benchmark_pcase$DPD,0.5) & TOA < quantile(benchmark_pcase$TOA,0.5) & Set == s, 1]
  
  for(i in 1:(50*r[s]))
  {
    Portfolios <- rbind(Portfolios,
                        data.table(CaseId = sample(sprawy[, CaseId], n, replace = TRUE),
                                   PortfolioId = paste(portfel, s, i, "mzp+mDPD", sep= "_")))
  }
  
  #sprawy z dużym zadłużeniem  całkowitym TOA i dużym DPD i oddana do komornika
  print(paste("duze TOA + duze DPD + Bailiff, s=",s))
  sprawy <- benchmark_pcase[Bailiff==1 & DPD > quantile(benchmark_pcase$DPD,0.5) & TOA > quantile(benchmark_pcase$TOA,0.5) & Set == s, 1] # tu zmieniłem już na 50-50
  
  for(i in 1:(50*r[s]))
  {
    Portfolios <- rbind(Portfolios, 
                        data.table(CaseId = sample(sprawy[, CaseId], n, replace = TRUE),
                                   PortfolioId = paste(portfel, s, i, "dzp+dDPD+b", sep= "_")))
  }
  
  #sprawy z dużym zadłużeniem  całkowitym TOA i małym DPD  i oddana do komornika
  print(paste("duze TOA + male DPD + Bailiff, s=",s))
  sprawy <- benchmark_pcase[Bailiff==1 & DPD < quantile(benchmark_pcase$DPD,0.5) & TOA > quantile(benchmark_pcase$TOA,0.5) & Set == s, 1]
  
  for(i in 1:(50*r[s]))
  {
    Portfolios <- rbind(Portfolios, 
                        data.table(CaseId = sample(sprawy[, CaseId], n, replace = TRUE),
                                   PortfolioId = paste(portfel, s, i, "dzp+mDPD+b", sep= "_")))
  }
  
  #sprawy z małym zadłużeniem  całkowitym TOA i dużym DPD i oddana do komornika
  print(paste("male TOA + duze DPD + Bailiff, s=",s))
  sprawy <- benchmark_pcase[Bailiff==1 & DPD > quantile(benchmark_pcase$DPD,0.5) & TOA < quantile(benchmark_pcase$TOA,0.5) & Set == s, 1]
  
  for(i in 1:(50*r[s]))
  {
    Portfolios <- rbind(Portfolios, 
                        data.table(CaseId = sample(sprawy[, CaseId], n, replace = TRUE),
                                   PortfolioId = paste(portfel, s, i, "mzp+dDPD+b", sep= "_")))
  }
  
  #sprawy z małym zadłużeniem  całkowitym TOA i małym DPD i oddana do komornika
  print(paste("male TOA + male DPD + Bailiff, s=",s))
  sprawy <- benchmark_pcase[Bailiff==1 & DPD < quantile(benchmark_pcase$DPD,0.5) & TOA < quantile(benchmark_pcase$TOA,0.5) & Set == s, 1]
  
  for(i in 1:(50*r[s]))
  {
    Portfolios <- rbind(Portfolios,
                        data.table(CaseId = sample(sprawy[, CaseId], n, replace = TRUE),
                                   PortfolioId = paste(portfel, s, i, "mzp+mDPD+b", sep= "_")))
  }
  
  #sprawy z dużym zadłużeniem  całkowitym TOA i dużym DPD i nie oddana do komornika
  print(paste("duze TOA + duze DPD + bez Bailiff, s=",s))
  sprawy <- benchmark_pcase[Bailiff==0 & DPD > quantile(benchmark_pcase$DPD,0.5) & TOA > quantile(benchmark_pcase$TOA,0.5) & Set == s, 1] # tu zmieniłem już na 50-50
  
  for(i in 1:(50*r[s]))
  {
    Portfolios <- rbind(Portfolios,
                        data.table(CaseId = sample(sprawy[, CaseId], n, replace = TRUE),
                                   PortfolioId = paste(portfel, s, i, "dzp+dDPD+nb", sep= "_")))
  }
  
  
  #sprawy z dużym zadłużeniem  całkowitym TOA i małym DPD  i nie oddana do komornika
  print(paste("duze TOA + male DPD + bez Bailiff, s=",s))
  sprawy <- benchmark_pcase[Bailiff==0 & DPD < quantile(benchmark_pcase$DPD,0.5) & TOA > quantile(benchmark_pcase$TOA,0.5) & Set == s, 1]
  
  for(i in 1:(50*r[s]))
  {
    Portfolios <- rbind(Portfolios,
                        data.table(CaseId = sample(sprawy[, CaseId], n, replace = TRUE),
                                   PortfolioId = paste(portfel, s, i, "dzp+mDPD+nb", sep= "_")))
  }
  
  #sprawy z małym zadłużeniem  całkowitym TOA i dużym DPD i nie oddana do komornika
  print(paste("male TOA + duze DPD + bez Bailiff, s=",s))
  sprawy <- benchmark_pcase[Bailiff==0 & DPD > quantile(benchmark_pcase$DPD,0.5) & TOA < quantile(benchmark_pcase$TOA,0.5) & Set == s, 1]
  
  for(i in 1:(50*r[s]))
  {
    Portfolios <- rbind(Portfolios,
                        data.table(CaseId = sample(sprawy[, CaseId], n, replace = TRUE),
                                   PortfolioId = paste(portfel, s, i, "mzp+dDPD+nb", sep= "_")))
  }
  
  
  #sprawy z małym zadłużeniem  całkowitym TOA i małym DPD i nie oddana do komornika
  print(paste("male TOA + male DPD + bez Bailiff, s=",s))
  sprawy <- benchmark_pcase[Bailiff==0 & DPD < quantile(benchmark_pcase$DPD,0.5) & TOA < quantile(benchmark_pcase$TOA,0.5) & Set == s, 1]
  
  for(i in 1:(50*r[s]))
  {
    Portfolios <- rbind(Portfolios,
                        data.table(CaseId = sample(sprawy[, CaseId], n, replace = TRUE),
                                   PortfolioId = paste(portfel, s, i, "mzp+mDPD+nb", sep= "_")))
  }
}

rm(sprawy, i, n, portfel, r, s)

#Agregacja na portfele

cor_variables <- c(
                  "PaymentsAmount12M",
                  "LoanAmount",
                  "TOA",
                  "Principal",
                  "Interest",
                  "Other",
                  "Credit",
                  "D_ContractDateToImportDate",
                  "DPD",
                  "ExternalAgency",
                  "Bailiff",
                  "ClosedExecution",
                  "Land_WOE",
                  "PopulationInCity",
                  "Age",
                  "Female",
                  "LastPaymentAmount",
                  "M_LastPaymentToImportDate_WOE",
                  "GDPPerCapita",
                  "MeanSalary"          
                  )

for(v in cor_variables)
{#v<-"Product"
  eval(parse(text = paste("benchmark_pcase[, ", v, " := as.numeric(", v, ")]")))
}

library(corrplot)
corrplot(cor(benchmark_pcase[, .SD, .SDcols = cor_variables],
             use = "pairwise.complete.obs", method = "pearson"),
         order = "hclust", tl.col = 'black', tl.cex = 0.75)


library(moments)

benchmark <- benchmark_pcase[Portfolios,
                             on = "CaseId"][, .(NPV = sum(NPV),
                                                TOA_sum = sum(TOA),
                                                TOA_avg = mean(TOA),
                                                TOA_sd = sd(TOA),
                                                TOA_skew = skewness(TOA),
                                                TOA_kurt = kurtosis(TOA),
                                                LoanAmount_avg = mean(LoanAmount),
                                                LoanAmount_sd = sd(LoanAmount),
                                                LoanAmount_skew = skewness(LoanAmount),
                                                LoanAmount_kurt = kurtosis(LoanAmount),
                                                Principal_avg = mean(Principal),
                                                Interest_avg = mean(Interest),
                                                Other_avg = mean(Other),
                                                D_ContractDateToImportDate_med = median(D_ContractDateToImportDate),
                                                DPD_med = median(DPD),
                                                ExternalAgency_pshare = mean(as.numeric(ExternalAgency)),
                                                Bailiff_pshare = mean(as.numeric(Bailiff)),
                                                ClosedExecution_pshare = mean(as.numeric(ClosedExecution)),
                                                PopulationInCity_avg = mean(PopulationInCity),
                                                Age_avg = mean(Age),
                                                LastPaymentAmount_avg = mean(LastPaymentAmount),
                                                GDPPerCapita_avg = mean(GDPPerCapita),
                                                MeanSalary_avg = mean(MeanSalary),
                                                Credit_pshare = mean(Credit),
                                                Female_pshare = mean(Female),
                                                Land_WOE_avg = mean(Land_WOE),
                                                M_LastPaymentToImportDate_WOE_avg = mean(M_LastPaymentToImportDate_WOE),
                                                Set = min(Set)),
                                                by = PortfolioId]

#save(benchmark, file = "final_benchmark.rdata")


