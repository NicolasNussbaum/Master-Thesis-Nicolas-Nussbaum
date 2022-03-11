
library(plyr)
library(dplyr)
library(magrittr)
library(qwraps2)
library(readr)
library(gtools)
library(sandwich)
library(readxl)

library(tidyverse)
library(tidyr)
library(purrr)
library(broom)

library(pracma)
library(matrixStats)
library(zoo)
library(fame)


##--> 12/85 as Beginning
options(scipen=999)
Sys.setenv(LANG = "en")

#####Dataset preparation #####

#Import Japan Dataset
Japan_Data_Date <- read.csv("~/Desktop/MA/Data/Japan_Data.csv") 

Japan_Data_Date$fic <- NULL

# Include only stocks in domestic currency
Japan_Data_Date <- Japan_Data_Date[which(Japan_Data_Date$curcdd == "JPY"),]

# Include only common stocks
Japan_Data_Date <- Japan_Data_Date[which(Japan_Data_Date$tpci == 0),]

Japan_Data_Date$curcdd <- NULL
Japan_Data_Date$tpci <- NULL

### Add Inflation column to dataset
Japan_Inflation <- select(read_excel("Desktop/MA/Data/Inflation-data.xlsx"), date, Japan)# Headline CPI; Data from Worldbank: https://www.worldbank.org/en/research/brief/inflation-database ; (Monthly inflation following this methodology: https://www.uvm.edu/~awoolf/classes/spring2005/ec11/calculating_inflation.html)
Japan_Inflation <-  Japan_Inflation %>%mutate(Japan=lag(Japan)) %>%na.omit()

# Remove ADR´s 
Japan_Data_Date$ADR <- substr(Japan_Data_Date$iid, 1,2)
Japan_Data_Date$ADR <- as.numeric(as.character(Japan_Data_Date$ADR))
Japan_Data_Date <- Japan_Data_Date[!Japan_Data_Date$ADR >= 90,]
Japan_Data_Date$ADR <- NULL 

#Create separate identification column
Identification <- Japan_Data_Date[c("gvkey","ggroup", "gind", "naics", "sic","conm")]

#Rename Identification: gvkey to PERMNO
names(Identification)[names(Identification) == 'gvkey'] <- 'PERMNO'
Table1_ID <- Identification[!duplicated(Identification[c("PERMNO")]),]
Table1_ID$gind <- NULL
Table1_ID$naics <- NULL
rm(Identification)

#delete identification
Japan_Data_Date$ggroup  <- NULL
Japan_Data_Date$gind  <- NULL
Japan_Data_Date$naics  <- NULL
Japan_Data_Date$sic  <- NULL

#Exclude rows which only have the dividend information of the secondary issue (div amount of primary issue is copied)

Japan_Data_Date <- Japan_Data_Date[!is.na(Japan_Data_Date$prccd),]

#create merged column to address the problem of multiple issues
Japan_Data_Date$merged <- paste(Japan_Data_Date$gvkey, Japan_Data_Date$iid)

# Replace missing trfd values with last available value
Japan_Data_Date <- as.data.frame(Japan_Data_Date %>% group_by(merged) %>% fill(trfd) %>% fill(trfd))
Japan_Data_Date$trfd[is.na(Japan_Data_Date$trfd)] <- 1

Japan_Data_Date$iid <- NULL
Japan_Data_Date$mktcap <-abs(Japan_Data_Date$prccd)*Japan_Data_Date$cshoc

dummy <- aggregate(mktcap ~ datadate + gvkey, Japan_Data_Date, na.action=na.pass, sum)  
names(Japan_Data_Date)[names(Japan_Data_Date) == 'mktcap'] <- 'indiv_merged_mktcap'

Japan_Data_Date <- join(dummy,Japan_Data_Date,by=c("datadate", "gvkey"), type="inner")

Japan_Data_Date$frct <- Japan_Data_Date$indiv_merged_mktcap / Japan_Data_Date$mktcap
dummy<- Japan_Data_Date %>% group_by(gvkey, datadate) %>% summarise(n = n())

Japan_Data_Date <- join(Japan_Data_Date,dummy,by=c("datadate", "gvkey"), type="inner")

dummy <- Japan_Data_Date[is.na(Japan_Data_Date$frct),]
dummy$frct[is.na(dummy$frct)] <- 1/dummy$n

Japan_Data_Date <- Japan_Data_Date[!is.na(Japan_Data_Date$frct),]
Japan_Data_Date <- rbind(Japan_Data_Date,dummy)

Japan_Data_Date$indiv_merged_mktcap <- NULL
Japan_Data_Date$n <- NULL

Japan_Data_Date$PrAj_1 <- Japan_Data_Date$prccd / Japan_Data_Date$ajexdi
Japan_Data_Date$PrAjTR_1 <- Japan_Data_Date$PrAj_1 * Japan_Data_Date$trfd

library(data.table)

Japan_Data_Date <- as.data.table(Japan_Data_Date)
setkey(Japan_Data_Date, gvkey, datadate)

#Subset the div column
dividend <- Japan_Data_Date[,c("datadate", "gvkey", "div", "prccd", "frct", "merged", "ajexdi")]

Japan_Data_Date <- Japan_Data_Date[!is.infinite(Japan_Data_Date$PrAjTR_1),]
Japan_Data_Date[, return := PrAjTR_1 / shift(PrAjTR_1, fill = NA), by = merged]
Japan_Data_Date$return[is.na(Japan_Data_Date$return)] <- 1

# Remove observations with daily returns >20
Japan_Data_Date <- Japan_Data_Date[which(Japan_Data_Date$return <= 20),]
Japan_Data_Date <- Japan_Data_Date[which(Japan_Data_Date$return >= 0.05),]

Japan_Data_Date$frct_return <- Japan_Data_Date$return * Japan_Data_Date$frct
Japan_Data_Date$return <- NULL
dummy <- aggregate(frct_return ~ datadate + gvkey, Japan_Data_Date, na.action=na.pass,sum)
names(dummy)[names(dummy) == 'frct_return'] <- 'return'

Japan_Data_Date <- join(Japan_Data_Date, dummy, by=c("datadate","gvkey"), type="inner")

Japan_Data_Date <- Japan_Data_Date[!duplicated(Japan_Data_Date[,c("datadate","gvkey")]),]

Japan_Data_Date$Month_Year <- substr(Japan_Data_Date$datadate, 1,6)

#subset volatility dataset
volatility <- select(Japan_Data_Date, datadate, gvkey, return)
names(volatility)[names(volatility) == 'return_dummy'] <- 'return'


Japan_Data_Date$merged <- NULL
Japan_Data_Date$frct <- NULL
Japan_Data_Date$frct_return <- NULL

Japan_Data_Date$PrAj_1  <- NULL
Japan_Data_Date$PrAjTR_1 <- NULL

Japan_Data_Date$ajexdi <- NULL
Japan_Data_Date$trfd <- NULL
Japan_Data_Date$div <- NULL

rm(dummy)

####
# Aggregate the daily returns by month and company to get monthly returns

library(data.table)
dummy <- setDT(Japan_Data_Date)[,prod(return), by=.(Month_Year,gvkey)]

#merge the datasets 
Japan <- join(dummy, Japan_Data_Date, by=c("Month_Year","gvkey"), type="inner")
rm(dummy)

#rename monthly return column in ret
names(Japan)[names(Japan) == 'V1'] <- 'ret'

# Remove the duplicates
Japan <- Japan[!duplicated(Japan[,c("Month_Year","gvkey")])]

#Remove excess columns
Japan$return <- NULL
Japan$datadate <- NULL

# Transform returns into 0.2 = 20% notation as in original Japan dataset
Japan$ret <- Japan$ret -1

# Rename columns to be equivalent to Original Japan dataset
names(Japan)[names(Japan) == 'Month_Year'] <- 'date'
names(Japan)[names(Japan) == 'gvkey'] <- 'PERMNO'
names(Japan)[names(Japan) == 'ret'] <- 'RET'
names(Japan)[names(Japan) == 'conm'] <- 'COMNAM'
names(Japan)[names(Japan) == 'cshoc'] <- 'SHROUT'
names(Japan)[names(Japan) == 'prccd'] <- 'PRC'


#Rename inflation dataset accordingly

names(Japan_Inflation)[names(Japan_Inflation) == 'Japan'] <- 'inflation'

# Add Inflation Data
Japan$date <- as.numeric(as.character(Japan$date))
Original1 <- join(Japan,Japan_Inflation,by=c("date"), type="inner")

#Remove Missing NA´s
Original1 = Original1[!(Original1$RET==""),]

# Only include stocks with at least 60 observations

Peter <- plyr::count(Original1,c('PERMNO')) # disable plyr first, den enable it again

#if not specified, filter function from "stats" package is called
Peter1 <- dplyr::filter(Peter, freq >= 60)
InflaData <- match_df(Original1, Peter1)

rm(Peter, Peter1, Original1, Japan)

# Convert RET column from factor to numeric
InflaData$RET <- as.numeric(as.character(InflaData$RET))

# Add market cap at quarter end to InflaData

Japan_Shares_Outstanding <- select(read.csv("~/Desktop/MA/Data/full_shares_set_global.csv") , gvkey, datadate, cshoi)  # Retreived from quarterly fundamentals compJapantat
Japan_Shares_Outstanding <- transform(Japan_Shares_Outstanding, datadate = as.Date(as.character(datadate), "%Y%m%d"))
Japan_Shares_Outstanding <- as.data.table(Japan_Shares_Outstanding)
library(lubridate)
Japan_Shares_Outstanding <- Japan_Shares_Outstanding[rep(1:.N, each = 12),]
Japan_Shares_Outstanding[, year := datadate]
Japan_Shares_Outstanding[, datadate := {day(datadate) <- 1; datadate}]
Japan_Shares_Outstanding[, datadate := {month(datadate) <- month(datadate) - 0:11; datadate}, by = year]
Japan_Shares_Outstanding$datadate <- gsub("-", "", Japan_Shares_Outstanding$datadate)
Japan_Shares_Outstanding$year <- NULL
Japan_Shares_Outstanding$cshoi <- Japan_Shares_Outstanding$cshoi *1000000
Japan_Shares_Outstanding$datadate <- substr(Japan_Shares_Outstanding$datadate, 1,6)

names(Japan_Shares_Outstanding)[names(Japan_Shares_Outstanding) == 'gvkey'] <- 'PERMNO'
names(Japan_Shares_Outstanding)[names(Japan_Shares_Outstanding) == 'datadate'] <- 'date'

dummy <- InflaData[is.na(InflaData$mktcap),]
dummy <- join(Japan_Shares_Outstanding, dummy, by=c("date","PERMNO"), type="right")
dummy <- unique(setDT(dummy), by = c("PERMNO", "date"))
dummy$mktcap <- dummy$SHROUT *dummy$PRC

dummy2 <- dummy[is.na(dummy$mktcap),]
dummy2$mktcap <- dummy2$cshoi *dummy2$PRC
dummy <- dummy[!is.na(dummy$mktcap),]

dummy <- rbind(dummy2,dummy)
rm(dummy2)
dummy$cshoi <- NULL

InflaData <- InflaData[!is.na(InflaData$mktcap),]
InflaData <- rbind(InflaData,dummy)
InflaData$SHROUT <- NULL
rm(dummy)
rm(Japan_Shares_Outstanding)

### Table 1: Betas, nom/real returns,mapping ####
#Calculate Inflation Betas for every Stock
#

# Run regression for every PERMNO; Solution from: https://stackoverflow.com/questions/54907726/running-multiple-linear-regressions-across-several-columns-of-a-data-frame-in-r
Inflation_Betas <- InflaData %>% select(PERMNO, inflation, RET) %>% na.omit( )%>%
  as_tibble %>%                                                   # first set the data in long format
  nest(RET, inflation) %>%                                        # now nest the dependent and independent factors
  mutate(model = map(data, ~lm(RET ~ inflation, data = .))) %>%   # fit the model using purrr
  mutate(tidy_model = map(model, tidy)) %>%                       # clean the model output with broom
  select(-data, -model) %>%                                       # remove the "untidy" parts
  unnest()


## Numbers for Table1: CAGR nominal and real returns

#Nominal Returns

Table1_Annual_Nominal <- InflaData
Number_Observations <- plyr::count(Table1_Annual_Nominal,c('PERMNO')) # disable plyr first, den enable it again - Calculates Observations per PERMNO

Table1_Annual_Nominal$RET <- Table1_Annual_Nominal$RET+1
Table1_Annual_Nominal <- aggregate(RET ~ PERMNO, Table1_Annual_Nominal, prod) #calculate end value over the full investment period

Table1_Annual_Nominal <- merge(Table1_Annual_Nominal, Number_Observations, by=c("PERMNO"))
Table1_Annual_Nominal$CAGR <- Table1_Annual_Nominal$RET ^ (1 / Table1_Annual_Nominal$freq) #Test @home
Table1_Annual_Nominal$CAGR <- Table1_Annual_Nominal$CAGR ^ 12 # Convert monthly to yearly CAGR
Table1_Annual_Nominal$CAGR <- Table1_Annual_Nominal$CAGR -1
names(Table1_Annual_Nominal)[names(Table1_Annual_Nominal) == 'CAGR'] <- 'Nominal_CAGR'
Table1_Annual_Nominal$RET <- NULL


#Real Returns

Table1_Annual_Real <- InflaData
Table1_Annual_Real$RET <- Table1_Annual_Real$RET+1

Table1_Annual_Real$inflation <- Table1_Annual_Real$inflation+1

Table1_Annual_Real$Real_RET <- (Table1_Annual_Real$RET / Table1_Annual_Real$inflation) -1
Table1_Annual_Real$Real_RET <- Table1_Annual_Real$Real_RET+1

Table1_Annual_Real <- aggregate(Real_RET ~ PERMNO, Table1_Annual_Real, prod) #calculate end value over the full investment period

Table1_Annual_Real <- merge(Table1_Annual_Real, Number_Observations, by=c("PERMNO"))
Table1_Annual_Real$CAGR <- Table1_Annual_Real$Real_RET ^ (1 / Table1_Annual_Real$freq) #Test @home
Table1_Annual_Real$CAGR <- Table1_Annual_Real$CAGR ^ 12 # Convert monthly to yearly CAGR
Table1_Annual_Real$CAGR <- Table1_Annual_Real$CAGR -1
names(Table1_Annual_Real)[names(Table1_Annual_Real) == 'CAGR'] <- 'Real_CAGR'
Table1_Annual_Real$Real_RET <- NULL


### Merge real and nominal tables with inflation beta table

# Combine Tables of Real and Nominal Returns

Table1_Annual_Real$PERMNO <- NULL
Table1_Annual_Real$freq <- NULL
Table1_Real_Nominal <- cbind(Table1_Annual_Nominal, Table1_Annual_Real)
rm(Table1_Annual_Nominal)
rm(Table1_Annual_Real)

# Merge inflabetas with returns to get table 1
Table1_Betas <- Inflation_Betas
Table1_Betas = Table1_Betas[which(Table1_Betas$term == "inflation"),]
Table1_Betas$term <- NULL

Table1 <- merge(Table1_Betas, Table1_Real_Nominal, by=c("PERMNO"))
rm(Table1_Betas)
rm(Table1_Real_Nominal)

### Mapping
sic_mapping <- read_excel("Desktop/MA/Data/SIC_Mapping.xlsx") # DIY job
translation <- read_excel("Desktop/MA/Data/Sector_Translation.xlsx") # from https://www.spglobal.com/marketintelligence/en/documents/112727-gics-mapbook_2018_v3_letter_digitalspreads.pdf

dummy_merge <- merge(Table1_ID, translation, by=c("ggroup"), all.x = TRUE)
Table1_ID <- merge(dummy_merge, sic_mapping, by=c("sic"), all.x = TRUE)
Table1_ID <- Table1_ID[!duplicated(Table1_ID[c("PERMNO")]),]

Table1_ID$sector_gics <- ifelse(is.na(Table1_ID$sector_gics), Table1_ID$sector_sic, Table1_ID$sector_gics) # replace na´s with sic

Table1_ID$sic <- NULL
Table1_ID$ggroup <- NULL
Table1_ID$sector_sic<- NULL

colnames(Table1_ID)[colnames(Table1_ID) == "sector_gics"] <- "sector"

rm(dummy_merge)
rm(sic_mapping)
rm(translation)

Table1 <- merge(Table1, Table1_ID, by=c("PERMNO"))

# Create Table that includes only the stocks wit a significant p value 
Table1_Significant <- Table1[which(Table1$p.value < 0.05),]

Table1_Significant <- merge(Table1_Significant, InflaData, by=c("PERMNO"))
Table1_Significant = Table1_Significant[!duplicated(Table1_Significant$PERMNO),]
Table1_Significant$date <- NULL
Table1_Significant$RET <- NULL
Table1_Significant$COMNAM <- NULL
Table1_Significant$SHROUT <- NULL
Table1_Significant$PRC <- NULL
Table1_Significant$mktcap <- NULL
Table1_Significant$inflation <- NULL

## Extract first and last trading days
last <- InflaData %>% arrange(PERMNO, date) %>% group_by(PERMNO) %>% summarise_all(last)
first <- InflaData %>% arrange(PERMNO, date) %>% group_by(PERMNO) %>% summarise_all(first)
first <- first[, 1:2]
last <- last[, 1:2]
colnames(first)[colnames(first) == "date"] <- "trading_first"
colnames(last)[colnames(last) == "date"] <- "trading_last"
first_last <- merge(first, last, by=c("PERMNO"))

Table1_Significant <- merge(Table1_Significant, first_last, by=c("PERMNO"))
Table1_Significant$trading_first <- substr(Table1_Significant$trading_first, 1,4)
Table1_Significant$trading_last <- substr(Table1_Significant$trading_last, 1,4)
Table1 <- merge(Table1, first_last, by=c("PERMNO"))

rm(first)
rm(last)
rm(first_last)

### Table 2 for the whole portfolio ####
#


# Calculate the nominal annual mean return

Annual_Mean <- select(InflaData, date, PERMNO, PRC, RET, inflation, mktcap)
Annual_Mean <-Annual_Mean %>% drop_na()

# Add 1 to RET 
Annual_Mean$RET <- Annual_Mean$RET + 1

# Calculate the market cap of all stocks in the portfolio
dummy <- aggregate(mktcap ~ date, Annual_Mean, sum)  
colnames(dummy)[colnames(dummy) == "mktcap"] <- "total_mktcap"
Annual_Mean <- merge(dummy,Annual_Mean,by=c("date"))
rm(dummy)


#Calculate the fraction of a stock in the overall portfolio
Annual_Mean <- transform(Annual_Mean, frct_port = mktcap / total_mktcap)

#Calculate the monthly portfolio returns
Annual_Mean$frcXret <- Annual_Mean$frct_port * Annual_Mean$RET
dummy <- aggregate(frcXret ~ date, Annual_Mean, sum)
colnames(dummy)[colnames(dummy) == "frcXret"] <- "tot_port_ret"
Annual_Mean <- merge(dummy,Annual_Mean,by=c("date"))
rm(dummy)
Annual_Mean$inflation <- Annual_Mean$inflation +1
Annual_Mean$real_tot_port_ret <- Annual_Mean$tot_port_ret / Annual_Mean$inflation
Annual_Mean$inflation <- Annual_Mean$inflation -1


##Calculate nominal portfolio CAGR

# Get number of observations
exponent_number <- select(InflaData, date)
exponent_number <- unique(exponent_number) # -> 421

dummy <- select(Annual_Mean, date, tot_port_ret) # extract total portfolio returns for every year
dummy <- unique(dummy)
aggregate(tot_port_ret ~ tot_port_ret, dummy, prod)              # Calcluate Cumulative Return over total time horizon

#Calculate CAGR by taking the n root of the end value
aggregate(tot_port_ret ~ tot_port_ret, dummy, prod)  ^ (1/(nrow(exponent_number)/12)) # Yearly CAGR!
rm(dummy)


##Calculate real portfolio CAGR

dummy <- select(Annual_Mean, date, real_tot_port_ret) # extract total portfolio returns for every year
dummy <- unique(dummy)
aggregate(real_tot_port_ret ~ real_tot_port_ret, dummy, prod)              # Calcluate Cumulative Return over total time horizon

#Calculate CAGR by taking the n root of the end value
aggregate(real_tot_port_ret ~ real_tot_port_ret, dummy, prod)  ^ (1/(nrow(exponent_number)/12)) # Yearly CAGR!
rm(dummy)
rm(exponent_number)

# Calculate the inflation beta for the whole portfolio

Portfolio_Returns <- select(Annual_Mean, date, tot_port_ret, real_tot_port_ret, inflation) # extract total portfolio returns for every year
Portfolio_Returns <- unique(Portfolio_Returns)
Portfolio_Returns$tot_port_ret <- Portfolio_Returns$tot_port_ret -1
summary(lm(formula = tot_port_ret ~ inflation, data = Portfolio_Returns))
reg <-lm(formula = tot_port_ret ~ inflation, data = Portfolio_Returns)

4*(nrow(Portfolio_Returns)/100)^(2/9)
library(sandwich)
library(lmtest)
coeftest(reg, NeweyWest(reg, lag = 6, prewhite = FALSE))

#get summmary statistics for the whole protfolio
summary(Portfolio_Returns) # min, max, median, mean
sapply(Portfolio_Returns, sd, na.rm=TRUE) #standard deviation
sapply(Portfolio_Returns, var, na.rm=TRUE) # variance

library(moments)

skewness(Portfolio_Returns) # skewness
kurtosis(Portfolio_Returns) # kurtosis

# success rate:

nrow(dplyr::filter(Portfolio_Returns, tot_port_ret > inflation)) /nrow(Portfolio_Returns)

# Year distribution of whole portfolio observations

Sample_hist <- plyr::count(Annual_Mean,c('date')) # disable plyr first, den enable it again
plot(Sample_hist$date, Sample_hist$freq)
rm(Sample_hist)

### Table 1b: Sector distribution among quintiles ####
#

# Exclude Intercept Row from Inflation Beta Dataset
sic <- Inflation_Betas
sic = sic[which(sic$term == "inflation"),]

# Create Five Portfolios Based on Inflation Betas -> Make sure the NAISC-Procedure has not changed the data
Table1b_Quantile <- quantile(sic$estimate, probs = seq(0, 1, 1/5))
Q1 <- sic[which(sic$estimate > Table1b_Quantile[5]),] # Q1=highest beta
Q2 <- sic[which(sic$estimate < Table1b_Quantile[5]),]
Q2 <- Q2[which(Q2$estimate > Table1b_Quantile[4]),]
Q3 <- sic[which(sic$estimate < Table1b_Quantile[4]),]
Q3 <- Q3[which(Q3$estimate > Table1b_Quantile[3]),]
Q4 <- sic[which(sic$estimate < Table1b_Quantile[3]),]
Q4 <- Q4[which(Q4$estimate > Table1b_Quantile[2]),]
Q5 <- sic[which(sic$estimate < Table1b_Quantile[2]),]
rm(sic)
rm(Table1b_Quantile)

###Check Industry distribution among the Quintiles

# Overall
prop.table(table(Table1$sector))

#Q1
prop.table(table(match_df(Table1,Q1 , on="PERMNO")$sector))

#Q2
prop.table(table(match_df(Table1,Q2 , on="PERMNO")$sector))

#Q3
prop.table(table(match_df(Table1,Q3 , on="PERMNO")$sector))

#Q4
prop.table(table(match_df(Table1,Q4 , on="PERMNO")$sector))

#Q5
prop.table(table(match_df(Table1,Q5 , on="PERMNO")$sector))

ID <- select(Table1_ID, PERMNO, sector)
ID <- merge(InflaData, ID, by="PERMNO")
tapply(ID$RET, ID$sector, sd)
rm(ID)

###Table 2 : Quintile 1 ########

# Calculate the nominal annual mean return
Q1 <- match_df(InflaData,Q1 , on="PERMNO")

# Add 1 to RET 
Q1$RET <- Q1$RET + 1

# Calculate the market cap of all stocks in the portfolio
dummy <- aggregate(mktcap ~ date, Q1, sum)  
colnames(dummy)[colnames(dummy) == "mktcap"] <- "total_mktcap"
Q1 <- merge(dummy,Q1,by=c("date"))
rm(dummy)


#Calculate the fraction of a stock in the overall portfolio
Q1 <- transform(Q1, frct_port = mktcap / total_mktcap)

#Calculate the monthly portfolio returns
Q1$frcXret <- Q1$frct_port * Q1$RET
dummy <- aggregate(frcXret ~ date, Q1, sum)
colnames(dummy)[colnames(dummy) == "frcXret"] <- "tot_port_ret"
Q1 <- merge(dummy,Q1,by=c("date"))
rm(dummy)
Q1$inflation <- Q1$inflation +1
Q1$real_tot_port_ret <- Q1$tot_port_ret / Q1$inflation
Q1$inflation <- Q1$inflation -1


##Calculate nominal portfolio CAGR

# Get number of observations
exponent_number <- select(InflaData, date)
exponent_number <- unique(exponent_number) # -> 421

dummy <- select(Q1, date, tot_port_ret) # extract total portfolio returns for every year
dummy <- unique(dummy)
aggregate(tot_port_ret ~ tot_port_ret, dummy, prod)              # Calcluate Cumulative Return over total time horizon

#Calculate CAGR by taking the n root of the end value
aggregate(tot_port_ret ~ tot_port_ret, dummy, prod)  ^ (1/(nrow(exponent_number)/12)) # Yearly CAGR!
rm(dummy)


##Calculate real portfolio CAGR

dummy <- select(Q1, date, real_tot_port_ret) # extract total portfolio returns for every year
dummy <- unique(dummy)
aggregate(real_tot_port_ret ~ real_tot_port_ret, dummy, prod)              # Calcluate Cumulative Return over total time horizon

#Calculate CAGR by taking the n root of the end value
aggregate(real_tot_port_ret ~ real_tot_port_ret, dummy, prod)  ^ (1/(nrow(exponent_number)/12)) # Yearly CAGR!
rm(dummy)
rm(exponent_number)

# Calculate the inflation beta for the whole portfolio

Q1_Returns <- select(Q1, date, tot_port_ret, inflation,real_tot_port_ret) # extract total portfolio returns for every year
Q1_Returns <- unique(Q1_Returns)
Q1_Returns$tot_port_ret <- Q1_Returns$tot_port_ret -1
summary(lm(formula = tot_port_ret ~ inflation, data = Q1_Returns))

reg <-lm(formula = tot_port_ret ~ inflation, data = Q1_Returns)
4*(nrow(Q1_Returns)/100)^(2/9)
library(sandwich)
library(lmtest)
coeftest(reg, NeweyWest(reg, lag = 6, prewhite = FALSE))

#get summmary statistics for the whole protfolio
summary(Q1_Returns) # min, max, median, mean
sapply(Q1_Returns, sd, na.rm=TRUE) #standard deviation
sapply(Q1_Returns, var, na.rm=TRUE) # variance

library(moments)

skewness(Q1_Returns) # skewness
kurtosis(Q1_Returns) # kurtosis

# success rate:

nrow(dplyr::filter(Q1_Returns, tot_port_ret > inflation)) /nrow(Q1_Returns)

# Year distribution of Q1 observations

Q1_hist <- plyr::count(Q1,c('date')) # disable plyr first, den enable it again
plot(Q1_hist$date, Q1_hist$freq)
rm(Q1_hist)

###Table 2 : Quintile 2 ########

# Calculate the nominal annual mean return
Q2 <- match_df(InflaData,Q2 , on="PERMNO")

# Add 1 to RET 
Q2$RET <- Q2$RET + 1

# Calculate the market cap of all stocks in the portfolio
dummy <- aggregate(mktcap ~ date, Q2, sum)  
colnames(dummy)[colnames(dummy) == "mktcap"] <- "total_mktcap"
Q2 <- merge(dummy,Q2,by=c("date"))
rm(dummy)


#Calculate the fraction of a stock in the overall portfolio
Q2 <- transform(Q2, frct_port = mktcap / total_mktcap)

#Calculate the monthly portfolio returns
Q2$frcXret <- Q2$frct_port * Q2$RET
dummy <- aggregate(frcXret ~ date, Q2, sum)
colnames(dummy)[colnames(dummy) == "frcXret"] <- "tot_port_ret"
Q2 <- merge(dummy,Q2,by=c("date"))
rm(dummy)
Q2$inflation <- Q2$inflation +1
Q2$real_tot_port_ret <- Q2$tot_port_ret / Q2$inflation
Q2$inflation <- Q2$inflation -1


##Calculate nominal portfolio CAGR

# Get number of observations
exponent_number <- select(InflaData, date)
exponent_number <- unique(exponent_number) # -> 421

dummy <- select(Q2, date, tot_port_ret) # extract total portfolio returns for every year
dummy <- unique(dummy)
aggregate(tot_port_ret ~ tot_port_ret, dummy, prod)              # Calcluate Cumulative Return over total time horizon

#Calculate CAGR by taking the n root of the end value
aggregate(tot_port_ret ~ tot_port_ret, dummy, prod)  ^ (1/(nrow(exponent_number)/12)) # Yearly CAGR!
rm(dummy)


##Calculate real portfolio CAGR

dummy <- select(Q2, date, real_tot_port_ret) # extract total portfolio returns for every year
dummy <- unique(dummy)
aggregate(real_tot_port_ret ~ real_tot_port_ret, dummy, prod)              # Calcluate Cumulative Return over total time horizon

#Calculate CAGR by taking the n root of the end value
aggregate(real_tot_port_ret ~ real_tot_port_ret, dummy, prod)  ^ (1/(nrow(exponent_number)/12)) # Yearly CAGR!
rm(dummy)
rm(exponent_number)

# Calculate the inflation beta for the whole portfolio

Q2_Returns <- select(Q2, date, tot_port_ret, inflation,real_tot_port_ret) # extract total portfolio returns for every year
Q2_Returns <- unique(Q2_Returns)
Q2_Returns$tot_port_ret <- Q2_Returns$tot_port_ret -1
summary(lm(formula = tot_port_ret ~ inflation, data = Q2_Returns))

reg <-lm(formula = tot_port_ret ~ inflation, data = Q2_Returns)
4*(nrow(Q2_Returns)/100)^(2/9)
library(sandwich)
library(lmtest)
coeftest(reg, NeweyWest(reg, lag = 6, prewhite = FALSE))


#get summmary statistics for the whole protfolio
summary(Q2_Returns) # min, max, median, mean
sapply(Q2_Returns, sd, na.rm=TRUE) #standard deviation
sapply(Q2_Returns, var, na.rm=TRUE) # variance

library(moments)

skewness(Q2_Returns) # skewness
kurtosis(Q2_Returns) # kurtosis

# success rate:

nrow(dplyr::filter(Q2_Returns, tot_port_ret > inflation)) /nrow(Q2_Returns)

# Year distribution of Q2 observations

Q2_hist <- plyr::count(Q2,c('date')) # disable plyr first, den enable it again
plot(Q2_hist$date, Q2_hist$freq)
rm(Q2_hist)

###Table 2 : Quintile 3 ########

# Calculate the nominal annual mean return
Q3 <- match_df(InflaData,Q3 , on="PERMNO")

# Add 1 to RET 
Q3$RET <- Q3$RET + 1

# Calculate the market cap of all stocks in the portfolio
dummy <- aggregate(mktcap ~ date, Q3, sum)  
colnames(dummy)[colnames(dummy) == "mktcap"] <- "total_mktcap"
Q3 <- merge(dummy,Q3,by=c("date"))
rm(dummy)


#Calculate the fraction of a stock in the overall portfolio
Q3 <- transform(Q3, frct_port = mktcap / total_mktcap)

#Calculate the monthly portfolio returns
Q3$frcXret <- Q3$frct_port * Q3$RET
dummy <- aggregate(frcXret ~ date, Q3, sum)
colnames(dummy)[colnames(dummy) == "frcXret"] <- "tot_port_ret"
Q3 <- merge(dummy,Q3,by=c("date"))
rm(dummy)
Q3$inflation <- Q3$inflation +1
Q3$real_tot_port_ret <- Q3$tot_port_ret / Q3$inflation
Q3$inflation <- Q3$inflation -1


##Calculate nominal portfolio CAGR

# Get number of observations
exponent_number <- select(InflaData, date)
exponent_number <- unique(exponent_number) # -> 421

dummy <- select(Q3, date, tot_port_ret) # extract total portfolio returns for every year
dummy <- unique(dummy)
aggregate(tot_port_ret ~ tot_port_ret, dummy, prod)              # Calcluate Cumulative Return over total time horizon

#Calculate CAGR by taking the n root of the end value
aggregate(tot_port_ret ~ tot_port_ret, dummy, prod)  ^ (1/(nrow(exponent_number)/12)) # Yearly CAGR!
rm(dummy)


##Calculate real portfolio CAGR

dummy <- select(Q3, date, real_tot_port_ret) # extract total portfolio returns for every year
dummy <- unique(dummy)
aggregate(real_tot_port_ret ~ real_tot_port_ret, dummy, prod)              # Calcluate Cumulative Return over total time horizon

#Calculate CAGR by taking the n root of the end value
aggregate(real_tot_port_ret ~ real_tot_port_ret, dummy, prod)  ^ (1/(nrow(exponent_number)/12)) # Yearly CAGR!
rm(dummy)
rm(exponent_number)

# Calculate the inflation beta for the whole portfolio

Q3_Returns <- select(Q3, date, tot_port_ret, inflation,real_tot_port_ret) # extract total portfolio returns for every year
Q3_Returns <- unique(Q3_Returns)
Q3_Returns$tot_port_ret <- Q3_Returns$tot_port_ret -1
summary(lm(formula = tot_port_ret ~ inflation, data = Q3_Returns))

reg <-lm(formula = tot_port_ret ~ inflation, data = Q3_Returns)
4*(nrow(Q3_Returns)/100)^(2/9)
library(sandwich)
library(lmtest)
coeftest(reg, NeweyWest(reg, lag = 6, prewhite = FALSE))


#get summmary statistics for the whole protfolio
summary(Q3_Returns) # min, max, median, mean
sapply(Q3_Returns, sd, na.rm=TRUE) #standard deviation
sapply(Q3_Returns, var, na.rm=TRUE) # variance

library(moments)

skewness(Q3_Returns) # skewness
kurtosis(Q3_Returns) # kurtosis

# success rate:

nrow(dplyr::filter(Q3_Returns, tot_port_ret > inflation)) /nrow(Q3_Returns)

# Year distribution of Q3 observations

Q3_hist <- plyr::count(Q3,c('date')) # disable plyr first, den enable it again
plot(Q3_hist$date, Q3_hist$freq)
rm(Q3_hist)

###Table 2 : Quintile 4 ########

# Calculate the nominal annual mean return
Q4 <- match_df(InflaData,Q4 , on="PERMNO")

# Add 1 to RET 
Q4$RET <- Q4$RET + 1

# Calculate the market cap of all stocks in the portfolio
dummy <- aggregate(mktcap ~ date, Q4, sum)  
colnames(dummy)[colnames(dummy) == "mktcap"] <- "total_mktcap"
Q4 <- merge(dummy,Q4,by=c("date"))
rm(dummy)


#Calculate the fraction of a stock in the overall portfolio
Q4 <- transform(Q4, frct_port = mktcap / total_mktcap)

#Calculate the monthly portfolio returns
Q4$frcXret <- Q4$frct_port * Q4$RET
dummy <- aggregate(frcXret ~ date, Q4, sum)
colnames(dummy)[colnames(dummy) == "frcXret"] <- "tot_port_ret"
Q4 <- merge(dummy,Q4,by=c("date"))
rm(dummy)
Q4$inflation <- Q4$inflation +1
Q4$real_tot_port_ret <- Q4$tot_port_ret / Q4$inflation
Q4$inflation <- Q4$inflation -1


##Calculate nominal portfolio CAGR

# Get number of observations
exponent_number <- select(InflaData, date)
exponent_number <- unique(exponent_number) # -> 421

dummy <- select(Q4, date, tot_port_ret) # extract total portfolio returns for every year
dummy <- unique(dummy)
aggregate(tot_port_ret ~ tot_port_ret, dummy, prod)              # Calcluate Cumulative Return over total time horizon

#Calculate CAGR by taking the n root of the end value
aggregate(tot_port_ret ~ tot_port_ret, dummy, prod)  ^ (1/(nrow(exponent_number)/12)) # Yearly CAGR!
rm(dummy)


##Calculate real portfolio CAGR

dummy <- select(Q4, date, real_tot_port_ret) # extract total portfolio returns for every year
dummy <- unique(dummy)
aggregate(real_tot_port_ret ~ real_tot_port_ret, dummy, prod)              # Calcluate Cumulative Return over total time horizon

#Calculate CAGR by taking the n root of the end value
aggregate(real_tot_port_ret ~ real_tot_port_ret, dummy, prod)  ^ (1/(nrow(exponent_number)/12)) # Yearly CAGR!
rm(dummy)
rm(exponent_number)

# Calculate the inflation beta for the whole portfolio

Q4_Returns <- select(Q4, date, tot_port_ret, inflation,real_tot_port_ret) # extract total portfolio returns for every year
Q4_Returns <- unique(Q4_Returns)
Q4_Returns$tot_port_ret <- Q4_Returns$tot_port_ret -1
summary(lm(formula = tot_port_ret ~ inflation, data = Q4_Returns))

reg <-lm(formula = tot_port_ret ~ inflation, data = Q4_Returns)
4*(nrow(Q2_Returns)/100)^(2/9)
library(sandwich)
library(lmtest)
coeftest(reg, NeweyWest(reg, lag = 6, prewhite = FALSE))

#get summmary statistics for the whole protfolio
summary(Q4_Returns) # min, max, median, mean
sapply(Q4_Returns, sd, na.rm=TRUE) #standard deviation
sapply(Q4_Returns, var, na.rm=TRUE) # variance

library(moments)

skewness(Q4_Returns) # skewness
kurtosis(Q4_Returns) # kurtosis

# success rate:

nrow(dplyr::filter(Q4_Returns, tot_port_ret > inflation)) /nrow(Q4_Returns)

# Year distribution of Q4 observations

Q4_hist <- plyr::count(Q4,c('date')) # disable plyr first, den enable it again
plot(Q4_hist$date, Q4_hist$freq)
rm(Q4_hist)

###Table 2 : Quintile 5 ########

# Calculate the nominal annual mean return

Q5 <- match_df(InflaData,Q5 , on="PERMNO")

# Add 1 to RET 
Q5$RET <- Q5$RET + 1

# Calculate the market cap of all stocks in the portfolio
dummy <- aggregate(mktcap ~ date, Q5, sum)  
colnames(dummy)[colnames(dummy) == "mktcap"] <- "total_mktcap"
Q5 <- merge(dummy,Q5,by=c("date"))
rm(dummy)


#Calculate the fraction of a stock in the overall portfolio
Q5 <- transform(Q5, frct_port = mktcap / total_mktcap)

#Calculate the monthly portfolio returns
Q5$frcXret <- Q5$frct_port * Q5$RET
dummy <- aggregate(frcXret ~ date, Q5, sum)
colnames(dummy)[colnames(dummy) == "frcXret"] <- "tot_port_ret"
Q5 <- merge(dummy,Q5,by=c("date"))
rm(dummy)
Q5$inflation <- Q5$inflation +1
Q5$real_tot_port_ret <- Q5$tot_port_ret / Q5$inflation
Q5$inflation <- Q5$inflation -1


##Calculate nominal portfolio CAGR

# Get number of observations
exponent_number <- select(InflaData, date)
exponent_number <- unique(exponent_number) # -> 421

dummy <- select(Q5, date, tot_port_ret) # extract total portfolio returns for every year
dummy <- unique(dummy)
aggregate(tot_port_ret ~ tot_port_ret, dummy, prod)              # Calcluate Cumulative Return over total time horizon

#Calculate CAGR by taking the n root of the end value
aggregate(tot_port_ret ~ tot_port_ret, dummy, prod)  ^ (1/(nrow(exponent_number)/12)) # Yearly CAGR!
rm(dummy)


##Calculate real portfolio CAGR

dummy <- select(Q5, date, real_tot_port_ret) # extract total portfolio returns for every year
dummy <- unique(dummy)
aggregate(real_tot_port_ret ~ real_tot_port_ret, dummy, prod)              # Calcluate Cumulative Return over total time horizon

#Calculate CAGR by taking the n root of the end value
aggregate(real_tot_port_ret ~ real_tot_port_ret, dummy, prod)  ^ (1/(nrow(exponent_number)/12)) # Yearly CAGR!
rm(dummy)
rm(exponent_number)

# Calculate the inflation beta for the whole portfolio

Q5_Returns <- select(Q5, date, tot_port_ret, inflation,real_tot_port_ret) # extract total portfolio returns for every year
Q5_Returns <- unique(Q5_Returns)
Q5_Returns$tot_port_ret <- Q5_Returns$tot_port_ret -1
summary(lm(formula = tot_port_ret ~ inflation, data = Q5_Returns))

reg <-lm(formula = tot_port_ret ~ inflation, data = Q5_Returns)
4*(nrow(Q5_Returns)/100)^(2/9)
library(sandwich)
library(lmtest)
coeftest(reg, NeweyWest(reg, lag = 6, prewhite = FALSE))


#get summmary statistics for the whole protfolio
summary(Q5_Returns) # min, max, median, mean
sapply(Q5_Returns, sd, na.rm=TRUE) #standard deviation
sapply(Q5_Returns, var, na.rm=TRUE) # variance

library(moments)

skewness(Q5_Returns) # skewness
kurtosis(Q5_Returns) # kurtosis

# success rate:

nrow(dplyr::filter(Q5_Returns, tot_port_ret > inflation)) /nrow(Q5_Returns)

# Year distribution of Q5 observations

Q5_hist <- plyr::count(Q5,c('date')) # disable plyr first, den enable it again
plot(Q5_hist$date, Q5_hist$freq)
rm(Q5_hist)

###Table 2 : Long-Short Portfolio Q1-Q5 ########

# Calculate the nominal annual mean return

Q1_Q5_Returns <- Q1_Returns
Q1_Q5_Returns$tot_port_ret <- Q1_Q5_Returns$tot_port_ret - Q5_Returns$tot_port_ret
Q1_Q5_Returns$tot_port_ret <- Q1_Q5_Returns$tot_port_ret +1
Q1_Q5_Returns$real_tot_port_ret <- Q1_Q5_Returns$tot_port_ret /(1+Q1_Q5_Returns$inflation)

##Calculate nominal portfolio CAGR

# Get number of observations
aggregate(tot_port_ret ~ tot_port_ret, Q1_Q5_Returns, prod)              # Calcluate Cumulative Return over total time horizon

#Calculate CAGR by taking the n root of the end value
aggregate(tot_port_ret ~ tot_port_ret, Q1_Q5_Returns, prod)  ^ (1/(nrow(Q1_Q5_Returns)/12)) # Yearly CAGR!


##Calculate real portfolio CAGR

aggregate(real_tot_port_ret ~ real_tot_port_ret, Q1_Q5_Returns, prod)              # Calcluate Cumulative Return over total time horizon

#Calculate CAGR by taking the n root of the end value
aggregate(real_tot_port_ret ~ real_tot_port_ret, Q1_Q5_Returns, prod)  ^ (1/(nrow(Q1_Q5_Returns)/12)) # Yearly CAGR!

# Calculate the inflation beta for the whole portfolio


Q1_Q5_Returns$tot_port_ret <- Q1_Q5_Returns$tot_port_ret -1
summary(lm(formula = tot_port_ret ~ inflation, data = Q1_Q5_Returns))

reg <-lm(formula = tot_port_ret ~ inflation, data = Q1_Q5_Returns)
4*(nrow(Q1_Q5_Returns)/100)^(2/9)
library(sandwich)
library(lmtest)
coeftest(reg, NeweyWest(reg, lag = 6, prewhite = FALSE))

#get summmary statistics for the whole protfolio
summary(Q1_Q5_Returns) # min, max, median, mean
sapply(Q1_Q5_Returns, sd, na.rm=TRUE) #standard deviation
sapply(Q1_Q5_Returns, var, na.rm=TRUE) # variance

library(moments)

skewness(Q1_Q5_Returns) # skewness
kurtosis(Q1_Q5_Returns) # kurtosis

# success rate:

nrow(dplyr::filter(Q1_Q5_Returns, tot_port_ret > inflation)) /nrow(Q1_Q5_Returns)

### Calculate the Fama French Loadings of the Q1 Portfolio #####
#
# Import FF5 dataset:https://www.bf.uzh.ch/en/research/databases.html
## Equalized Breaking Point Portfolio is used

FF5 <- read_excel("~/Desktop/MA/Data/FF5_Japan.xlsx")


# Change date format of FF5 to numeric to be in alignment with the general class format
FF5$date <- as.numeric(as.character(FF5$date))

Q1_FF5 <- merge(FF5,Q1_Returns,by=c("date"))
Q1_FF5$inflation <- NULL

# Bring all numericals to the same percentage level
Q1_FF5$tot_port_ret <- Q1_FF5$tot_port_ret *100

Q1_FF5$excess_ret <- Q1_FF5$tot_port_ret - Q1_FF5$RF

summary(lm(formula = excess_ret ~ Mkt.RF + SMB + HML + RMW + CMA, data = Q1_FF5))
reg <-lm(formula = excess_ret ~ Mkt.RF + SMB + HML + RMW + CMA, data = Q1_FF5)
4*(nrow(Q1_FF5)/100)^(2/9)
library(sandwich)
library(lmtest)
coeftest(reg, NeweyWest(reg, lag = 5, prewhite = FALSE))

### Calculate the Fama French Loadings of the Q2 Portfolio #####

Q2_FF5 <- merge(FF5,Q2_Returns,by=c("date"))
Q2_FF5$inflation <- NULL

# Bring all numericals to the same percentage level
Q2_FF5$tot_port_ret <- Q2_FF5$tot_port_ret *100

Q2_FF5$excess_ret <- Q2_FF5$tot_port_ret - Q2_FF5$RF

summary(lm(formula = excess_ret ~ Mkt.RF + SMB + HML + RMW + CMA, data = Q2_FF5))

reg <-lm(formula = excess_ret ~ Mkt.RF + SMB + HML + RMW + CMA, data = Q2_FF5)
4*(nrow(Q2_FF5)/100)^(2/9)
library(sandwich)
library(lmtest)
coeftest(reg, NeweyWest(reg, lag = 5, prewhite = FALSE))

### Calculate the Fama French Loadings of the Q3 Portfolio #####

Q3_FF5 <- merge(FF5,Q3_Returns,by=c("date"))
Q3_FF5$inflation <- NULL

# Bring all numericals to the same percentage level
Q3_FF5$tot_port_ret <- Q3_FF5$tot_port_ret *100

Q3_FF5$excess_ret <- Q3_FF5$tot_port_ret - Q3_FF5$RF

summary(lm(formula = excess_ret ~ Mkt.RF + SMB + HML + RMW + CMA, data = Q3_FF5))

reg <-lm(formula = excess_ret ~ Mkt.RF + SMB + HML + RMW + CMA, data = Q3_FF5)
4*(nrow(Q3_FF5)/100)^(2/9)
library(sandwich)
library(lmtest)
coeftest(reg, NeweyWest(reg, lag = 5, prewhite = FALSE))

### Calculate the Fama French Loadings of the Q4 Portfolio #####

Q4_FF5 <- merge(FF5,Q4_Returns,by=c("date"))
Q4_FF5$inflation <- NULL

# Bring all numericals to the same percentage level
Q4_FF5$tot_port_ret <- Q4_FF5$tot_port_ret *100

Q4_FF5$excess_ret <- Q4_FF5$tot_port_ret - Q4_FF5$RF

summary(lm(formula = excess_ret ~ Mkt.RF + SMB + HML + RMW + CMA, data = Q4_FF5))

reg <-lm(formula = excess_ret ~ Mkt.RF + SMB + HML + RMW + CMA, data = Q4_FF5)
4*(nrow(Q4_FF5)/100)^(2/9)
library(sandwich)
library(lmtest)
coeftest(reg, NeweyWest(reg, lag = 5, prewhite = FALSE))

### Calculate the Fama French Loadings of the Q5 Portfolio #####

Q5_FF5 <- merge(FF5,Q5_Returns,by=c("date"))
Q5_FF5$inflation <- NULL

# Bring all numericals to the same percentage level
Q5_FF5$tot_port_ret <- Q5_FF5$tot_port_ret *100

Q5_FF5$excess_ret <- Q5_FF5$tot_port_ret - Q5_FF5$RF

summary(lm(formula = excess_ret ~ Mkt.RF + SMB + HML + RMW + CMA, data = Q5_FF5))
reg <-lm(formula = excess_ret ~ Mkt.RF + SMB + HML + RMW + CMA, data = Q5_FF5)
4*(nrow(Q5_FF5)/100)^(2/9)
library(sandwich)
library(lmtest)
coeftest(reg, NeweyWest(reg, lag = 5, prewhite = FALSE))



### Calculate the Fama French Loadings of the Q1-Q5 Long-Short Portfolio #####

Q1_Q5_FF5 <- merge(FF5,Q1_Q5_Returns,by=c("date"))
Q1_Q5_FF5$inflation <- NULL

# Bring all numericals to the same percentage level
Q1_Q5_FF5$tot_port_ret <- Q1_Q5_FF5$tot_port_ret *100

Q1_Q5_FF5$excess_ret <- Q1_Q5_FF5$tot_port_ret - Q1_Q5_FF5$RF

summary(lm(formula = excess_ret ~ Mkt.RF + SMB + HML + RMW + CMA, data = Q1_Q5_FF5))

reg <-lm(formula = excess_ret ~ Mkt.RF + SMB + HML + RMW + CMA, data = Q1_Q5_FF5)
4*(nrow(Q1_Q5_FF5)/100)^(2/9)
library(sandwich)
library(lmtest)
coeftest(reg, NeweyWest(reg, lag = 5, prewhite = FALSE))


### Calculate the Fama French Loadings of the Full Portfolio #####

Portfolio_FF5 <- merge(FF5,Portfolio_Returns,by=c("date"))
Portfolio_FF5$inflation <- NULL

# Bring all numericals to the same percentage level
Portfolio_FF5$tot_port_ret <- Portfolio_FF5$tot_port_ret *100

Portfolio_FF5$excess_ret <- Portfolio_FF5$tot_port_ret - Portfolio_FF5$RF

summary(lm(formula = excess_ret ~ Mkt.RF + SMB + HML + RMW + CMA, data = Portfolio_FF5))
reg <-lm(formula = excess_ret ~ Mkt.RF + SMB + HML + RMW + CMA, data = Portfolio_FF5)
4*(nrow(Portfolio_FF5)/100)^(2/9)
library(sandwich)
library(lmtest)
coeftest(reg, NeweyWest(reg, lag = 5, prewhite = FALSE))

rm(Q1_FF5)
rm(Q2_FF5)
rm(Q3_FF5)
rm(Q4_FF5)
rm(Q5_FF5)

rm(Q1_Returns)
rm(Q2_Returns)
rm(Q3_Returns)
rm(Q4_Returns)
rm(Q5_Returns)
rm(Q1_Q5_Returns)

### Calculate inflation betas for out of sample monthly rebalanced portfolio #####
#
#
Rebalanced_Raw <- select(InflaData, date, PERMNO, RET)

# Transpose dataframe by date with PERMNO´s as columns to be able to run the rollregres function
Rebalanced <- reshape(Rebalanced_Raw, idvar = "date", timevar = "PERMNO", direction = "wide") # https://stackoverflow.com/questions/5890584/how-to-reshape-data-from-long-to-wide-format
Rebalanced_Data <- Rebalanced 

Japan_Inflation$date <- as.numeric(as.character(Japan_Inflation$date))


# Add the inflation column
Rebalanced_Data <- merge(Rebalanced_Data,Japan_Inflation,by=c("date"))

#Flip the order of the dataset (newest to oldest dates)
Rebalanced_Data <- Rebalanced_Data %>% map_df(rev)

#Wichtig: Calculate Rolling Betas for the portfolio
nms <- startsWith(names(Rebalanced_Data), "RET")
slopes <- function(ix, x, Y) cov(x = x[ix], y = Y[ix,]) / var(x[ix])
betas <- rollapply(1:nrow(Rebalanced_Data), 60, slopes, align = "left", 
                   x = Rebalanced_Data$inflation, Y = Rebalanced_Data[nms])

# Add back dates to beta file
dates <- select(Rebalanced_Data, date)
dates <- dates[1:(nrow(Rebalanced_Data)-59),]
betas <- cbind(dates, betas)

#Idea: Plot distribution of rolling inflation betas over time in the MA
#Calculate quintiles for each PERMNO and year

storage <- list()
for(i in 1:(nrow(Rebalanced_Data)-59)){
  storage[[i]] <-  ntile(betas[i,2:ncol(betas)], 5) ## 
}

#Add date column and column names back to the list

quantiles <- as.data.frame(matrix(unlist(storage),nrow=length(storage),byrow=TRUE))
quantiles <- cbind(dates, quantiles)

#Flip order to be in alignment with initial classification
quantiles[quantiles == 1] <- 6
quantiles[quantiles == 2] <- 7
quantiles[quantiles == 3] <- 3
quantiles[quantiles == 4] <- 2
quantiles[quantiles == 5] <- 1
quantiles[quantiles == 6] <- 5
quantiles[quantiles == 7] <- 4

namesx <- names(Rebalanced)

namesy <- names(quantiles)
setnames(quantiles, old = namesy, new = namesx)


# Remove the RET. part from the headers
names(quantiles) = gsub(pattern = "RET.", replacement = "", x = names(quantiles))

#Transpose dataframe
library(reshape2)
long <- reshape2::melt(setDT(quantiles), id.vars = c("date"), variable.name = "PERMNO")
long$PERMNO <- as.numeric(as.character(long$PERMNO))
quantile_data <-merge(InflaData, long, by=c("date","PERMNO"))
colnames(quantile_data)[colnames(quantile_data) == "value"] <- "quintile"
quantile_data$RET <- quantile_data$RET + 1

rm(namesx)
rm(namesy)
rm(nms)
rm(long)
rm(quantiles)
rm(Rebalanced_Raw)
rm(Rebalanced)
rm(storage)
rm(slopes)
rm(i)
rm(dates)


###Calculate out of sample portfolio returns of Q1####
#
#

Q1_RollRet <- select(quantile_data, date, PERMNO, RET, mktcap, inflation, quintile)
Q1_RollRet <- Q1_RollRet[which(Q1_RollRet$quintile == 1),]


# Calculate the market cap of all stocks in the portfolio
dummy <- aggregate(mktcap ~ date, Q1_RollRet, sum)  
colnames(dummy)[colnames(dummy) == "mktcap"] <- "total_mktcap"
Q1_RollRet <- merge(dummy,Q1_RollRet,by=c("date"))
rm(dummy)


#Calculate the fraction of a stock in the overall portfolio
Q1_RollRet <- transform(Q1_RollRet, frct_port = mktcap / total_mktcap)

#Calculate the monthly portfolio returns
Q1_RollRet$frcXret <- Q1_RollRet$frct_port * Q1_RollRet$RET
dummy <- aggregate(frcXret ~ date, Q1_RollRet, sum)
colnames(dummy)[colnames(dummy) == "frcXret"] <- "tot_port_ret"
Q1_RollRet <- merge(dummy,Q1_RollRet,by=c("date"))
rm(dummy)
Q1_RollRet$inflation <- Q1_RollRet$inflation +1
Q1_RollRet$real_tot_port_ret <- Q1_RollRet$tot_port_ret / Q1_RollRet$inflation
Q1_RollRet$inflation <- Q1_RollRet$inflation -1


##Calculate nominal portfolio CAGR

dummy <- select(Q1_RollRet, date, tot_port_ret) # extract total portfolio returns for every year
dummy <- unique(dummy)
aggregate(tot_port_ret ~ tot_port_ret, dummy, prod)              # Calcluate Cumulative Return over total time horizon

#Calculate CAGR by taking the n root of the end value
aggregate(tot_port_ret ~ tot_port_ret, dummy, prod)  ^ (1/(nrow(dummy)/12)) # Yearly CAGR!
rm(dummy)


##Calculate real portfolio CAGR

dummy <- select(Q1_RollRet, date, real_tot_port_ret) # extract total portfolio returns for every year
dummy <- unique(dummy)
aggregate(real_tot_port_ret ~ real_tot_port_ret, dummy, prod)              # Calcluate Cumulative Return over total time horizon

#Calculate CAGR by taking the n root of the end value
aggregate(real_tot_port_ret ~ real_tot_port_ret, dummy, prod)  ^ (1/(nrow(dummy)/12)) # Yearly CAGR!
rm(dummy)

# Calculate the inflation beta for the whole portfolio

Q1_RollRet_Returns <- select(Q1_RollRet, date, tot_port_ret, inflation, real_tot_port_ret) # extract total portfolio returns for every year
Q1_RollRet_Returns <- unique(Q1_RollRet_Returns)
Q1_RollRet_Returns$tot_port_ret <- Q1_RollRet_Returns$tot_port_ret -1
summary(lm(formula = tot_port_ret ~ inflation, data = Q1_RollRet_Returns))

reg <-lm(formula = tot_port_ret ~ inflation, data = Q1_RollRet_Returns)
4*(nrow(Q1_RollRet_Returns)/100)^(2/9)
library(sandwich)
library(lmtest)
coeftest(reg, NeweyWest(reg, lag = 5, prewhite = FALSE))

#get summmary statistics for the whole protfolio
summary(Q1_RollRet_Returns) # min, max, median, mean
sapply(Q1_RollRet_Returns, sd, na.rm=TRUE) #standard deviation
sapply(Q1_RollRet_Returns, var, na.rm=TRUE) # variance

library(moments)

skewness(Q1_RollRet_Returns) # skewness
kurtosis(Q1_RollRet_Returns) # kurtosis

# success rate:

nrow(dplyr::filter(Q1_RollRet_Returns, tot_port_ret > inflation)) /nrow(Q1_RollRet_Returns)

# Year distribution of Q1_RollRet observations

Q1_RollRet_hist <- plyr::count(Q1_RollRet,c('date')) # disable plyr first, den enable it again
plot(Q1_RollRet_hist$date, Q1_RollRet_hist$freq)
rm(Q1_RollRet_hist)




###Calculate out of sample portfolio returns of Q2####
#

Q2_RollRet <- select(quantile_data, date, PERMNO, RET, mktcap, inflation, quintile)
Q2_RollRet <- Q2_RollRet[which(Q2_RollRet$quintile == 2),]

# Calculate the market cap of all stocks in the portfolio
dummy <- aggregate(mktcap ~ date, Q2_RollRet, sum)  
colnames(dummy)[colnames(dummy) == "mktcap"] <- "total_mktcap"
Q2_RollRet <- merge(dummy,Q2_RollRet,by=c("date"))
rm(dummy)


#Calculate the fraction of a stock in the overall portfolio
Q2_RollRet <- transform(Q2_RollRet, frct_port = mktcap / total_mktcap)

#Calculate the monthly portfolio returns
Q2_RollRet$frcXret <- Q2_RollRet$frct_port * Q2_RollRet$RET
dummy <- aggregate(frcXret ~ date, Q2_RollRet, sum)
colnames(dummy)[colnames(dummy) == "frcXret"] <- "tot_port_ret"
Q2_RollRet <- merge(dummy,Q2_RollRet,by=c("date"))
rm(dummy)
Q2_RollRet$inflation <- Q2_RollRet$inflation +1
Q2_RollRet$real_tot_port_ret <- Q2_RollRet$tot_port_ret / Q2_RollRet$inflation
Q2_RollRet$inflation <- Q2_RollRet$inflation -1


##Calculate nominal portfolio CAGR

dummy <- select(Q2_RollRet, date, tot_port_ret) # extract total portfolio returns for every year
dummy <- unique(dummy)
aggregate(tot_port_ret ~ tot_port_ret, dummy, prod)              # Calcluate Cumulative Return over total time horizon

#Calculate CAGR by taking the n root of the end value
aggregate(tot_port_ret ~ tot_port_ret, dummy, prod)  ^ (1/(nrow(dummy)/12)) # Yearly CAGR!
rm(dummy)


##Calculate real portfolio CAGR

dummy <- select(Q2_RollRet, date, real_tot_port_ret) # extract total portfolio returns for every year
dummy <- unique(dummy)
aggregate(real_tot_port_ret ~ real_tot_port_ret, dummy, prod)              # Calcluate Cumulative Return over total time horizon

#Calculate CAGR by taking the n root of the end value
aggregate(real_tot_port_ret ~ real_tot_port_ret, dummy, prod)  ^ (1/(nrow(dummy)/12)) # Yearly CAGR!
rm(dummy)

# Calculate the inflation beta for the whole portfolio

Q2_RollRet_Returns <- select(Q2_RollRet, date, tot_port_ret, inflation, real_tot_port_ret) # extract total portfolio returns for every year
Q2_RollRet_Returns <- unique(Q2_RollRet_Returns)
Q2_RollRet_Returns$tot_port_ret <- Q2_RollRet_Returns$tot_port_ret -1
summary(lm(formula = tot_port_ret ~ inflation, data = Q2_RollRet_Returns))

reg <-lm(formula = tot_port_ret ~ inflation, data = Q2_RollRet_Returns)
4*(nrow(Q2_RollRet_Returns)/100)^(2/9)
library(sandwich)
library(lmtest)
coeftest(reg, NeweyWest(reg, lag = 5, prewhite = FALSE))

#get summmary statistics for the whole protfolio
summary(Q2_RollRet_Returns) # min, max, median, mean
sapply(Q2_RollRet_Returns, sd, na.rm=TRUE) #standard deviation
sapply(Q2_RollRet_Returns, var, na.rm=TRUE) # variance

library(moments)

skewness(Q2_RollRet_Returns) # skewness
kurtosis(Q2_RollRet_Returns) # kurtosis

# success rate:

nrow(dplyr::filter(Q2_RollRet_Returns, tot_port_ret > inflation)) /nrow(Q2_RollRet_Returns)

# Year distribution of Q2_RollRet observations

Q2_RollRet_hist <- plyr::count(Q2_RollRet,c('date')) # disable plyr first, den enable it again
plot(Q2_RollRet_hist$date, Q2_RollRet_hist$freq)
rm(Q2_RollRet_hist)







###Calculate out of sample portfolio returns of Q3####
#
#

Q3_RollRet <- select(quantile_data, date, PERMNO, RET, mktcap, inflation, quintile)
Q3_RollRet <- Q3_RollRet[which(Q3_RollRet$quintile == 3),]


# Calculate the market cap of all stocks in the portfolio
dummy <- aggregate(mktcap ~ date, Q3_RollRet, sum)  
colnames(dummy)[colnames(dummy) == "mktcap"] <- "total_mktcap"
Q3_RollRet <- merge(dummy,Q3_RollRet,by=c("date"))
rm(dummy)


#Calculate the fraction of a stock in the overall portfolio
Q3_RollRet <- transform(Q3_RollRet, frct_port = mktcap / total_mktcap)

#Calculate the monthly portfolio returns
Q3_RollRet$frcXret <- Q3_RollRet$frct_port * Q3_RollRet$RET
dummy <- aggregate(frcXret ~ date, Q3_RollRet, sum)
colnames(dummy)[colnames(dummy) == "frcXret"] <- "tot_port_ret"
Q3_RollRet <- merge(dummy,Q3_RollRet,by=c("date"))
rm(dummy)
Q3_RollRet$inflation <- Q3_RollRet$inflation +1
Q3_RollRet$real_tot_port_ret <- Q3_RollRet$tot_port_ret / Q3_RollRet$inflation
Q3_RollRet$inflation <- Q3_RollRet$inflation -1


##Calculate nominal portfolio CAGR

dummy <- select(Q3_RollRet, date, tot_port_ret) # extract total portfolio returns for every year
dummy <- unique(dummy)
aggregate(tot_port_ret ~ tot_port_ret, dummy, prod)              # Calcluate Cumulative Return over total time horizon

#Calculate CAGR by taking the n root of the end value
aggregate(tot_port_ret ~ tot_port_ret, dummy, prod)  ^ (1/(nrow(dummy)/12)) # Yearly CAGR!
rm(dummy)


##Calculate real portfolio CAGR

dummy <- select(Q3_RollRet, date, real_tot_port_ret) # extract total portfolio returns for every year
dummy <- unique(dummy)
aggregate(real_tot_port_ret ~ real_tot_port_ret, dummy, prod)              # Calcluate Cumulative Return over total time horizon

#Calculate CAGR by taking the n root of the end value
aggregate(real_tot_port_ret ~ real_tot_port_ret, dummy, prod)  ^ (1/(nrow(dummy)/12)) # Yearly CAGR!
rm(dummy)

# Calculate the inflation beta for the whole portfolio

Q3_RollRet_Returns <- select(Q3_RollRet, date, tot_port_ret, inflation, real_tot_port_ret) # extract total portfolio returns for every year
Q3_RollRet_Returns <- unique(Q3_RollRet_Returns)
Q3_RollRet_Returns$tot_port_ret <- Q3_RollRet_Returns$tot_port_ret -1
summary(lm(formula = tot_port_ret ~ inflation, data = Q3_RollRet_Returns))

reg <-lm(formula = tot_port_ret ~ inflation, data = Q3_RollRet_Returns)
4*(nrow(Q3_RollRet_Returns)/100)^(2/9)
library(sandwich)
library(lmtest)
coeftest(reg, NeweyWest(reg, lag = 5, prewhite = FALSE))

#get summmary statistics for the whole protfolio
summary(Q3_RollRet_Returns) # min, max, median, mean
sapply(Q3_RollRet_Returns, sd, na.rm=TRUE) #standard deviation
sapply(Q3_RollRet_Returns, var, na.rm=TRUE) # variance

library(moments)

skewness(Q3_RollRet_Returns) # skewness
kurtosis(Q3_RollRet_Returns) # kurtosis

# success rate:

nrow(dplyr::filter(Q3_RollRet_Returns, tot_port_ret > inflation)) /nrow(Q3_RollRet_Returns)

# Year distribution of Q3_RollRet observations

Q3_RollRet_hist <- plyr::count(Q3_RollRet,c('date')) # disable plyr first, den enable it again
plot(Q3_RollRet_hist$date, Q3_RollRet_hist$freq)
rm(Q3_RollRet_hist)

###Calculate out of sample portfolio returns of Q4####
#
#

Q4_RollRet <- select(quantile_data, date, PERMNO, RET, mktcap, inflation, quintile)
Q4_RollRet <- Q4_RollRet[which(Q4_RollRet$quintile == 4),]


# Calculate the market cap of all stocks in the portfolio
dummy <- aggregate(mktcap ~ date, Q4_RollRet, sum)  
colnames(dummy)[colnames(dummy) == "mktcap"] <- "total_mktcap"
Q4_RollRet <- merge(dummy,Q4_RollRet,by=c("date"))
rm(dummy)


#Calculate the fraction of a stock in the overall portfolio
Q4_RollRet <- transform(Q4_RollRet, frct_port = mktcap / total_mktcap)

#Calculate the monthly portfolio returns
Q4_RollRet$frcXret <- Q4_RollRet$frct_port * Q4_RollRet$RET
dummy <- aggregate(frcXret ~ date, Q4_RollRet, sum)
colnames(dummy)[colnames(dummy) == "frcXret"] <- "tot_port_ret"
Q4_RollRet <- merge(dummy,Q4_RollRet,by=c("date"))
rm(dummy)
Q4_RollRet$inflation <- Q4_RollRet$inflation +1
Q4_RollRet$real_tot_port_ret <- Q4_RollRet$tot_port_ret / Q4_RollRet$inflation
Q4_RollRet$inflation <- Q4_RollRet$inflation -1


##Calculate nominal portfolio CAGR

dummy <- select(Q4_RollRet, date, tot_port_ret) # extract total portfolio returns for every year
dummy <- unique(dummy)
aggregate(tot_port_ret ~ tot_port_ret, dummy, prod)              # Calcluate Cumulative Return over total time horizon

#Calculate CAGR by taking the n root of the end value
aggregate(tot_port_ret ~ tot_port_ret, dummy, prod)  ^ (1/(nrow(dummy)/12)) # Yearly CAGR!
rm(dummy)


##Calculate real portfolio CAGR

dummy <- select(Q4_RollRet, date, real_tot_port_ret) # extract total portfolio returns for every year
dummy <- unique(dummy)
aggregate(real_tot_port_ret ~ real_tot_port_ret, dummy, prod)              # Calcluate Cumulative Return over total time horizon

#Calculate CAGR by taking the n root of the end value
aggregate(real_tot_port_ret ~ real_tot_port_ret, dummy, prod)  ^ (1/(nrow(dummy)/12)) # Yearly CAGR!
rm(dummy)

# Calculate the inflation beta for the whole portfolio

Q4_RollRet_Returns <- select(Q4_RollRet, date, tot_port_ret, inflation, real_tot_port_ret) # extract total portfolio returns for every year
Q4_RollRet_Returns <- unique(Q4_RollRet_Returns)
Q4_RollRet_Returns$tot_port_ret <- Q4_RollRet_Returns$tot_port_ret -1
summary(lm(formula = tot_port_ret ~ inflation, data = Q4_RollRet_Returns))

reg <-lm(formula = tot_port_ret ~ inflation, data = Q4_RollRet_Returns)
4*(nrow(Q4_RollRet_Returns)/100)^(2/9)
library(sandwich)
library(lmtest)
coeftest(reg, NeweyWest(reg, lag = 5, prewhite = FALSE))

#get summmary statistics for the whole protfolio
summary(Q4_RollRet_Returns) # min, max, median, mean
sapply(Q4_RollRet_Returns, sd, na.rm=TRUE) #standard deviation
sapply(Q4_RollRet_Returns, var, na.rm=TRUE) # variance

library(moments)

skewness(Q4_RollRet_Returns) # skewness
kurtosis(Q4_RollRet_Returns) # kurtosis

# success rate:

nrow(dplyr::filter(Q4_RollRet_Returns, tot_port_ret > inflation)) /nrow(Q4_RollRet_Returns)

# Year distribution of Q4_RollRet observations

Q4_RollRet_hist <- plyr::count(Q4_RollRet,c('date')) # disable plyr first, den enable it again
plot(Q4_RollRet_hist$date, Q4_RollRet_hist$freq)
rm(Q4_RollRet_hist)

###Calculate out of sample portfolio returns of Q5####
#
#

Q5_RollRet <- select(quantile_data, date, PERMNO, RET, mktcap, inflation, quintile)
Q5_RollRet <- Q5_RollRet[which(Q5_RollRet$quintile == 5),]


# Calculate the market cap of all stocks in the portfolio
dummy <- aggregate(mktcap ~ date, Q5_RollRet, sum)  
colnames(dummy)[colnames(dummy) == "mktcap"] <- "total_mktcap"
Q5_RollRet <- merge(dummy,Q5_RollRet,by=c("date"))
rm(dummy)


#Calculate the fraction of a stock in the overall portfolio
Q5_RollRet <- transform(Q5_RollRet, frct_port = mktcap / total_mktcap)

#Calculate the monthly portfolio returns
Q5_RollRet$frcXret <- Q5_RollRet$frct_port * Q5_RollRet$RET
dummy <- aggregate(frcXret ~ date, Q5_RollRet, sum)
colnames(dummy)[colnames(dummy) == "frcXret"] <- "tot_port_ret"
Q5_RollRet <- merge(dummy,Q5_RollRet,by=c("date"))
rm(dummy)
Q5_RollRet$inflation <- Q5_RollRet$inflation +1
Q5_RollRet$real_tot_port_ret <- Q5_RollRet$tot_port_ret / Q5_RollRet$inflation
Q5_RollRet$inflation <- Q5_RollRet$inflation -1


##Calculate nominal portfolio CAGR

dummy <- select(Q5_RollRet, date, tot_port_ret) # extract total portfolio returns for every year
dummy <- unique(dummy)
aggregate(tot_port_ret ~ tot_port_ret, dummy, prod)              # Calcluate Cumulative Return over total time horizon

#Calculate CAGR by taking the n root of the end value
aggregate(tot_port_ret ~ tot_port_ret, dummy, prod)  ^ (1/(nrow(dummy)/12)) # Yearly CAGR!
rm(dummy)


##Calculate real portfolio CAGR

dummy <- select(Q5_RollRet, date, real_tot_port_ret) # extract total portfolio returns for every year
dummy <- unique(dummy)
aggregate(real_tot_port_ret ~ real_tot_port_ret, dummy, prod)              # Calcluate Cumulative Return over total time horizon

#Calculate CAGR by taking the n root of the end value
aggregate(real_tot_port_ret ~ real_tot_port_ret, dummy, prod)  ^ (1/(nrow(dummy)/12)) # Yearly CAGR!
rm(dummy)

# Calculate the inflation beta for the whole portfolio

Q5_RollRet_Returns <- select(Q5_RollRet, date, tot_port_ret, inflation, real_tot_port_ret) # extract total portfolio returns for every year
Q5_RollRet_Returns <- unique(Q5_RollRet_Returns)
Q5_RollRet_Returns$tot_port_ret <- Q5_RollRet_Returns$tot_port_ret -1
summary(lm(formula = tot_port_ret ~ inflation, data = Q5_RollRet_Returns))

reg <-lm(formula = tot_port_ret ~ inflation, data = Q5_RollRet_Returns)
4*(nrow(Q5_RollRet_Returns)/100)^(2/9)
library(sandwich)
library(lmtest)
coeftest(reg, NeweyWest(reg, lag = 5, prewhite = FALSE))

#get summmary statistics for the whole protfolio
summary(Q5_RollRet_Returns) # min, max, median, mean
sapply(Q5_RollRet_Returns, sd, na.rm=TRUE) #standard deviation
sapply(Q5_RollRet_Returns, var, na.rm=TRUE) # variance

library(moments)

skewness(Q5_RollRet_Returns) # skewness
kurtosis(Q5_RollRet_Returns) # kurtosis

# success rate:

nrow(dplyr::filter(Q5_RollRet_Returns, tot_port_ret > inflation)) /nrow(Q5_RollRet_Returns)

# Year distribution of Q5_RollRet observations

Q5_RollRet_hist <- plyr::count(Q5_RollRet,c('date')) # disable plyr first, den enable it again
plot(Q5_RollRet_hist$date, Q5_RollRet_hist$freq)
rm(Q5_RollRet_hist)


###Calculate out of sample portfolio returns Long-Short Portfolio Q1-Q5 ########

# Calculate the nominal annual mean return

Q1_Q5_RollRet_Returns <- Q1_RollRet_Returns
Q1_Q5_RollRet_Returns$tot_port_ret <- Q1_Q5_RollRet_Returns$tot_port_ret - Q5_RollRet_Returns$tot_port_ret
Q1_Q5_RollRet_Returns$tot_port_ret <- Q1_Q5_RollRet_Returns$tot_port_ret +1
Q1_Q5_RollRet_Returns$real_tot_port_ret <- Q1_Q5_RollRet_Returns$tot_port_ret /(1+Q1_Q5_RollRet_Returns$inflation)

##Calculate nominal portfolio CAGR

aggregate(tot_port_ret ~ tot_port_ret, Q1_Q5_RollRet_Returns, prod)              # Calcluate Cumulative Return over total time horizon

#Calculate CAGR by taking the n root of the end value
aggregate(tot_port_ret ~ tot_port_ret, Q1_Q5_RollRet_Returns, prod)  ^ (1/(nrow(Q1_Q5_RollRet_Returns)/12)) # Yearly CAGR!

##Calculate real portfolio CAGR

aggregate(real_tot_port_ret ~ real_tot_port_ret, Q1_Q5_RollRet_Returns, prod)              # Calcluate Cumulative Return over total time horizon

#Calculate CAGR by taking the n root of the end value
aggregate(real_tot_port_ret ~ real_tot_port_ret, Q1_Q5_RollRet_Returns, prod)  ^ (1/(nrow(Q1_Q5_RollRet_Returns)/12)) # Yearly CAGR!

# Calculate the inflation beta for the whole portfolio


Q1_Q5_RollRet_Returns$tot_port_ret <- Q1_Q5_RollRet_Returns$tot_port_ret -1
summary(lm(formula = tot_port_ret ~ inflation, data = Q1_Q5_RollRet_Returns))

reg <-lm(formula = tot_port_ret ~ inflation, data = Q1_Q5_RollRet_Returns)
4*(nrow(Q1_Q5_RollRet_Returns)/100)^(2/9)
library(sandwich)
library(lmtest)
coeftest(reg, NeweyWest(reg, lag = 5, prewhite = FALSE))

#get summmary statistics for the whole protfolio
summary(Q1_Q5_RollRet_Returns) # min, max, median, mean
sapply(Q1_Q5_RollRet_Returns, sd, na.rm=TRUE) #standard deviation
sapply(Q1_Q5_RollRet_Returns, var, na.rm=TRUE) # variance

library(moments)

skewness(Q1_Q5_RollRet_Returns) # skewness
kurtosis(Q1_Q5_RollRet_Returns) # kurtosis

# success rate:

nrow(dplyr::filter(Q1_Q5_RollRet_Returns, tot_port_ret > inflation)) /nrow(Q1_Q5_RollRet_Returns)

# Year distribution of Q5_RollRet observations

Q1_Q5_RollRet_Returns_hist <- plyr::count(Q1_Q5_RollRet_Returns,c('date')) # disable plyr first, den enable it again
plot(Q1_Q5_RollRet_Returns_hist$date, Q1_Q5_RollRet_Returns_hist$freq)
rm(Q1_Q5_RollRet_Returns_hist)



###Calculate out of sample portfolio returns of Portfolio####
#
#

Portfolio_RollRet <- select(quantile_data, date, PERMNO, RET, mktcap, inflation, quintile)


# Calculate the market cap of all stocks in the portfolio
dummy <- aggregate(mktcap ~ date, Portfolio_RollRet, sum)  
colnames(dummy)[colnames(dummy) == "mktcap"] <- "total_mktcap"
Portfolio_RollRet <- merge(dummy,Portfolio_RollRet,by=c("date"))
rm(dummy)


#Calculate the fraction of a stock in the overall portfolio
Portfolio_RollRet <- transform(Portfolio_RollRet, frct_port = mktcap / total_mktcap)

#Calculate the monthly portfolio returns
Portfolio_RollRet$frcXret <- Portfolio_RollRet$frct_port * Portfolio_RollRet$RET
dummy <- aggregate(frcXret ~ date, Portfolio_RollRet, sum)
colnames(dummy)[colnames(dummy) == "frcXret"] <- "tot_port_ret"
Portfolio_RollRet <- merge(dummy,Portfolio_RollRet,by=c("date"))
rm(dummy)
Portfolio_RollRet$inflation <- Portfolio_RollRet$inflation +1
Portfolio_RollRet$real_tot_port_ret <- Portfolio_RollRet$tot_port_ret / Portfolio_RollRet$inflation
Portfolio_RollRet$inflation <- Portfolio_RollRet$inflation -1


##Calculate nominal portfolio CAGR

dummy <- select(Portfolio_RollRet, date, tot_port_ret) # extract total portfolio returns for every year
dummy <- unique(dummy)
aggregate(tot_port_ret ~ tot_port_ret, dummy, prod)              # Calcluate Cumulative Return over total time horizon

#Calculate CAGR by taking the n root of the end value
aggregate(tot_port_ret ~ tot_port_ret, dummy, prod)  ^ (1/(nrow(dummy)/12)) # Yearly CAGR!
rm(dummy)


##Calculate real portfolio CAGR

dummy <- select(Portfolio_RollRet, date, real_tot_port_ret) # extract total portfolio returns for every year
dummy <- unique(dummy)
aggregate(real_tot_port_ret ~ real_tot_port_ret, dummy, prod)              # Calcluate Cumulative Return over total time horizon

#Calculate CAGR by taking the n root of the end value
aggregate(real_tot_port_ret ~ real_tot_port_ret, dummy, prod)  ^ (1/(nrow(dummy)/12)) # Yearly CAGR!
rm(dummy)

# Calculate the inflation beta for the whole portfolio

Portfolio_RollRet_Returns <- select(Portfolio_RollRet, date, tot_port_ret, inflation, real_tot_port_ret) # extract total portfolio returns for every year
Portfolio_RollRet_Returns <- unique(Portfolio_RollRet_Returns)
Portfolio_RollRet_Returns$tot_port_ret <- Portfolio_RollRet_Returns$tot_port_ret -1
summary(lm(formula = tot_port_ret ~ inflation, data = Portfolio_RollRet_Returns))

reg <-lm(formula = tot_port_ret ~ inflation, data = Portfolio_RollRet_Returns)
4*(nrow(Portfolio_RollRet_Returns)/100)^(2/9)
library(sandwich)
library(lmtest)
coeftest(reg, NeweyWest(reg, lag = 5, prewhite = FALSE))

#get summmary statistics for the whole protfolio
summary(Portfolio_RollRet_Returns) # min, max, median, mean
sapply(Portfolio_RollRet_Returns, sd, na.rm=TRUE) #standard deviation
sapply(Portfolio_RollRet_Returns, var, na.rm=TRUE) # variance

library(moments)

skewness(Portfolio_RollRet_Returns) # skewness
kurtosis(Portfolio_RollRet_Returns) # kurtosis

# success rate:

nrow(dplyr::filter(Portfolio_RollRet_Returns, tot_port_ret > inflation)) /nrow(Portfolio_RollRet_Returns)

# Year distribution of Portfolio_RollRet observations

Portfolio_RollRet_hist <- plyr::count(Portfolio_RollRet,c('date')) # disable plyr first, den enable it again
plot(Portfolio_RollRet_hist$date, Portfolio_RollRet_hist$freq)
rm(Portfolio_RollRet_hist)



### Calculate the Fama French Loadings of the Q1_RollRet Portfolio #####
#
# Import FF5 dataset: https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/Data_Library/f-f_5developed.html
Q1_RollRet_FF5 <- merge(FF5,Q1_RollRet_Returns,by=c("date"))
Q1_RollRet_FF5$inflation <- NULL

# Bring all numericals to the same percentage level
Q1_RollRet_FF5$tot_port_ret <- Q1_RollRet_FF5$tot_port_ret *100

Q1_RollRet_FF5$excess_ret <- Q1_RollRet_FF5$tot_port_ret - Q1_RollRet_FF5$RF

summary(lm(formula = excess_ret ~ Mkt.RF + SMB + HML + RMW + CMA, data = Q1_RollRet_FF5))

reg <-lm(formula = excess_ret ~ Mkt.RF + SMB + HML + RMW + CMA, data = Q1_RollRet_FF5)
4*(nrow(Q1_RollRet_FF5)/100)^(2/9)
library(sandwich)
library(lmtest)
coeftest(reg, NeweyWest(reg, lag = 5, prewhite = FALSE))


### Calculate the Fama French Loadings of the Q2_RollRet Portfolio #####

Q2_RollRet_FF5 <- merge(FF5,Q2_RollRet_Returns,by=c("date"))
Q2_RollRet_FF5$inflation <- NULL

# Bring all numericals to the same percentage level
Q2_RollRet_FF5$tot_port_ret <- Q2_RollRet_FF5$tot_port_ret *100

Q2_RollRet_FF5$excess_ret <- Q2_RollRet_FF5$tot_port_ret - Q2_RollRet_FF5$RF

summary(lm(formula = excess_ret ~ Mkt.RF + SMB + HML + RMW + CMA, data = Q2_RollRet_FF5))
reg <-lm(formula = excess_ret ~ Mkt.RF + SMB + HML + RMW + CMA, data = Q2_RollRet_FF5)
4*(nrow(Q2_RollRet_FF5)/100)^(2/9)
library(sandwich)
library(lmtest)
coeftest(reg, NeweyWest(reg, lag = 5, prewhite = FALSE))

### Calculate the Fama French Loadings of the Q3_RollRet Portfolio #####

Q3_RollRet_FF5 <- merge(FF5,Q3_RollRet_Returns,by=c("date"))
Q3_RollRet_FF5$inflation <- NULL

# Bring all numericals to the same percentage level
Q3_RollRet_FF5$tot_port_ret <- Q3_RollRet_FF5$tot_port_ret *100

Q3_RollRet_FF5$excess_ret <- Q3_RollRet_FF5$tot_port_ret - Q3_RollRet_FF5$RF

summary(lm(formula = excess_ret ~ Mkt.RF + SMB + HML + RMW + CMA, data = Q3_RollRet_FF5))

reg <-lm(formula = excess_ret ~ Mkt.RF + SMB + HML + RMW + CMA, data = Q3_RollRet_FF5)
4*(nrow(Q3_RollRet_FF5)/100)^(2/9)
library(sandwich)
library(lmtest)
coeftest(reg, NeweyWest(reg, lag = 5, prewhite = FALSE))

### Calculate the Fama French Loadings of the Q4_RollRet Portfolio #####

Q4_RollRet_FF5 <- merge(FF5,Q4_RollRet_Returns,by=c("date"))
Q4_RollRet_FF5$inflation <- NULL

# Bring all numericals to the same percentage level
Q4_RollRet_FF5$tot_port_ret <- Q4_RollRet_FF5$tot_port_ret *100

Q4_RollRet_FF5$excess_ret <- Q4_RollRet_FF5$tot_port_ret - Q4_RollRet_FF5$RF

summary(lm(formula = excess_ret ~ Mkt.RF + SMB + HML + RMW + CMA, data = Q4_RollRet_FF5))

reg <-lm(formula = excess_ret ~ Mkt.RF + SMB + HML + RMW + CMA, data = Q4_RollRet_FF5)
4*(nrow(Q4_RollRet_FF5)/100)^(2/9)
library(sandwich)
library(lmtest)
coeftest(reg, NeweyWest(reg, lag = 5, prewhite = FALSE))

### Calculate the Fama French Loadings of the Q5_RollRet Portfolio #####

Q5_RollRet_FF5 <- merge(FF5,Q5_RollRet_Returns,by=c("date"))
Q5_RollRet_FF5$inflation <- NULL

# Bring all numericals to the same percentage level
Q5_RollRet_FF5$tot_port_ret <- Q5_RollRet_FF5$tot_port_ret *100

Q5_RollRet_FF5$excess_ret <- Q5_RollRet_FF5$tot_port_ret - Q5_RollRet_FF5$RF

summary(lm(formula = excess_ret ~ Mkt.RF + SMB + HML + RMW + CMA, data = Q5_RollRet_FF5))

reg <-lm(formula = excess_ret ~ Mkt.RF + SMB + HML + RMW + CMA, data = Q5_RollRet_FF5)
4*(nrow(Q5_RollRet_FF5)/100)^(2/9)
library(sandwich)
library(lmtest)
coeftest(reg, NeweyWest(reg, lag = 5, prewhite = FALSE))


### Calculate the Fama French Loadings of the Q1_Q5 Long Short Portfolio #####

Q1_Q5_RollRet_Returns_FF5 <- merge(FF5,Q1_Q5_RollRet_Returns,by=c("date"))
Q1_Q5_RollRet_Returns_FF5$inflation <- NULL

# Bring all numericals to the same percentage level
Q1_Q5_RollRet_Returns_FF5$tot_port_ret <- Q1_Q5_RollRet_Returns_FF5$tot_port_ret *100

Q1_Q5_RollRet_Returns_FF5$excess_ret <- Q1_Q5_RollRet_Returns_FF5$tot_port_ret - Q1_Q5_RollRet_Returns_FF5$RF

summary(lm(formula = excess_ret ~ Mkt.RF + SMB + HML + RMW + CMA, data = Q1_Q5_RollRet_Returns_FF5))

reg <-lm(formula = excess_ret ~ Mkt.RF + SMB + HML + RMW + CMA, data = Q1_Q5_RollRet_Returns_FF5)
4*(nrow(Q1_Q5_RollRet_Returns_FF5)/100)^(2/9)
library(sandwich)
library(lmtest)
coeftest(reg, NeweyWest(reg, lag = 5, prewhite = FALSE))



### Calculate the Fama French Loadings of the Full RollRet portfolio #####

Portfolio_RollRet_FF5 <- merge(FF5,Portfolio_RollRet_Returns,by=c("date"))
Portfolio_RollRet_FF5$inflation <- NULL

# Bring all numericals to the same percentage level
Portfolio_RollRet_FF5$tot_port_ret <- Portfolio_RollRet_FF5$tot_port_ret *100

Portfolio_RollRet_FF5$excess_ret <- Portfolio_RollRet_FF5$tot_port_ret - Portfolio_RollRet_FF5$RF

summary(lm(formula = excess_ret ~ Mkt.RF + SMB + HML + RMW + CMA, data = Portfolio_RollRet_FF5))

reg <-lm(formula = excess_ret ~ Mkt.RF + SMB + HML + RMW + CMA, data = Portfolio_RollRet_FF5)
4*(nrow(Portfolio_RollRet_FF5)/100)^(2/9)
library(sandwich)
library(lmtest)
coeftest(reg, NeweyWest(reg, lag = 5, prewhite = FALSE))

rm(Q1_RollRet_Returns)
rm(Q2_RollRet_Returns)
rm(Q3_RollRet_Returns)
rm(Q4_RollRet_Returns)
rm(Q5_RollRet_Returns)
rm(Portfolio_RollRet_Returns)

##### Figure 4: Average Inflation Beta and sd by year of the five year rolling beta####

#Use the "betas file"
average_rolling_betas <- betas[,1:2]
colnames(average_rolling_betas)[2] <- "average"
average_rolling_betas$average <- rowMeans(betas[,2:ncol(betas)], na.rm=T)
average_rolling_betas$sd <- apply(betas[,2:ncol(betas)],1,sd, na.rm=T)

##### Percentage of stocks with sign change YoY ####

beta_change <- data.frame(diff(as.matrix(sign(betas[,2:ncol(betas)])),lag=12))
beta_change$date <- betas[1:nrow(beta_change),1] # add data column
beta_change <- beta_change %>% select(date, everything()) # move data column to the front

beta_change$count_change_1 <- apply(beta_change, 1, function(x) length(which(x==-2)))
beta_change$count_change_2 <- apply(beta_change, 1, function(x) length(which(x==2)))
beta_change$count_change <- beta_change$count_change_1+beta_change$count_change_2

beta_change$count_stable <- apply(beta_change, 1, function(x) length(which(x==0)))
beta_change <- select(beta_change, date, count_change, count_stable)
beta_change$percentage_flip <- beta_change$count_change /(beta_change$count_change+ beta_change$count_stable)



#### Figure 5: Cross Sectional Distribution of the Inflation Betas ####

beta_distribution <- betas[betas$date == 201201, ]
beta_distribution <- beta_distribution[,2:ncol(beta_distribution) ]
beta_distribution <- as.numeric(beta_distribution)    # Convert character to numeric
summary(beta_distribution)
hist(beta_distribution)
rm(beta_distribution)


#####Calculate the TTM div yield (different issues are weighted by market cap) #### 
dividend$div[is.na(dividend$div)] <- 0
colnames(dividend)[colnames(dividend) == "gvkey"] <- "PERMNO"
colnames(dividend)[colnames(dividend) == "datadate"] <- "date"

dividend <- dplyr::filter(dividend, ajexdi > 0)

dividend <- dividend[!dividend$frct < 0.03,] # Dividend of side issues is often given the same as the main issues -> flawed

dividend$Month_Year <- substr(dividend$date, 1,6)

# account for stock splits
dividend$prccd <- dividend$prccd / dividend$ajexdi
dividend$div <- dividend$div / dividend$ajexdi

monthly_div_merged <- aggregate(div ~ Month_Year + merged, dividend, sum)
colnames(monthly_div_merged)[colnames(monthly_div_merged) == "Month_Year"] <- "date"

Transposed_div_per_merged <- reshape(monthly_div_merged, idvar = "date", timevar = "merged", direction = "wide") # https://stackoverflow.com/questions/5890584/how-to-reshape-data-from-long-to-wide-format
TTM_div_merged <- rollapply(data = Transposed_div_per_merged[,2:ncol(Transposed_div_per_merged)],width=12,FUN=sum, by.column=T, align = "right", partial=T, na.rm=T)


TTM_div_merged <- cbind(select(Transposed_div_per_merged, date), TTM_div_merged)
names(TTM_div_merged) = gsub(pattern = "div.", replacement = "", x = names(TTM_div_merged))

TTM_div_merged <- reshape2::melt(setDT(TTM_div_merged), id.vars = c("date"), variable.name = "merged")
colnames(TTM_div_merged)[colnames(TTM_div_merged) == "value"] <- "div"

#TTM_div_merged$merged <- as.numeric(as.character(TTM_div_merged$merged))

# Get prccd and frc of the last day of the month per "merged"
prccd_month_end <- dividend %>% arrange(merged, date) %>% group_by(merged, Month_Year) %>% summarise_all(last)

# Extract observations with non-overlapping time window
dummy <- aggregate(frct ~ Month_Year + PERMNO, prccd_month_end, sum)
dummy <- dummy[which(dummy$frct > 1),]
dummy$frct <- NULL


dummy_1 <- merge(dummy, prccd_month_end, by=c("Month_Year","PERMNO")) # NA's match

# Exclude the observations with frct >1 (non-overlapping time window) from the original dataset
dummy$merged_dummy <- paste(dummy$Month_Year, dummy$PERMNO)
prccd_month_end$merged_dummy <- paste(prccd_month_end$Month_Year, prccd_month_end$PERMNO)
prccd_month_end <- prccd_month_end[!(prccd_month_end$merged_dummy %in% dummy$merged_dummy),]
dummy$merged_dummy <- NULL
prccd_month_end$merged_dummy <- NULL
rm(dummy)

# Extract the rows witht the last overlapping rows and add it back to the original dataset 
dummy_1 <- dummy_1[which(dummy_1$frct < 1),]
dummy_1 <- select(dummy_1, date, PERMNO)
dummy_1 <- match_df(dividend, dummy_1)

prccd_month_end <- rbind (data.frame(prccd_month_end), data.frame(dummy_1))

rm(dummy_1)

prccd_month_end$div <- NULL
prccd_month_end$date <- NULL
colnames(prccd_month_end)[colnames(prccd_month_end) == "Month_Year"] <- "date"

dividend_fusioned <- merge(TTM_div_merged, prccd_month_end, by=c("date","merged")) # NA's match
dividend_fusioned$div_yield <- (dividend_fusioned$div / dividend_fusioned$prccd)

dividend_fusioned$frct_yield <- dividend_fusioned$div_yield * dividend_fusioned$frct

dividend_fusioned <- aggregate(frct_yield ~ date + PERMNO, dividend_fusioned, sum)
colnames(dividend_fusioned)[colnames(dividend_fusioned) == "frct_yield"] <- "div_yield"
dividend_fusioned$date <- as.numeric(as.character(dividend_fusioned$date))
dividend_fusioned$PERMNO=as.factor(dividend_fusioned$PERMNO)

rm(TTM_div_merged)
rm(Transposed_div_per_merged)
rm(monthly_div_merged)
rm(dividend)

##### Calculate Momentum ####

#Add 1 to every return
Momentum <- Rebalanced_Data[,1:(ncol(Rebalanced_Data)-1)] + 1
Momentum$date <- Momentum$date -1 

#Calculate 12 month return
Momentum <- rollapply(data = Momentum[,2:ncol(Momentum)],width=12,FUN=prod, by.column=T, align = "left")
Momentum <- cbind(Rebalanced_Data[1:nrow(Momentum),1], Momentum)
names(Momentum) = gsub(pattern = "RET.", replacement = "", x = names(Momentum))

Momentum <- reshape2::melt(setDT(Momentum), id.vars = c("date"), variable.name = "PERMNO")
colnames(Momentum)[colnames(Momentum) == "value"] <- "momentum"
Momentum <- Momentum[!is.na(Momentum$momentum),]
Momentum$momentum <- Momentum$momentum -1

### Create Reversal Column #### 

#Turn old dataset around (with oldes now on top)
Reversal <- Rebalanced_Data %>% map_df(rev)

# Shift all rows one down
dummy <- Reversal[,2:(ncol(Reversal)-1)] %>% mutate_all(.funs = funs(lag))

Reversal <- cbind(Reversal[,1], dummy)
Reversal <- Reversal %>% map_df(rev)
Reversal <- Reversal[-nrow(Reversal),]
rm(dummy)

### Create transposed reversal dataframe
names(Reversal) = gsub(pattern = "RET.", replacement = "", x = names(Reversal))

#Transpose dataframe
Reversal <- melt(setDT(Reversal), id.vars = c("date"), variable.name = "PERMNO")
colnames(Reversal)[colnames(Reversal) == "value"] <- "reversal"
Reversal <- Reversal %>% drop_na(reversal)

##### 12 month rolling standard deviation: WATCH OUT: Number of business days is hardcoded! #### 

colnames(volatility)[colnames(volatility) == "gvkey"] <- "PERMNO"
colnames(volatility)[colnames(volatility) == "datadate"] <- "date"
colnames(volatility)[colnames(volatility) == "return"] <- "RET"
volatility$RET <- volatility$RET -1

Rebalanced_volatility <- reshape(volatility, idvar = "date", timevar = "PERMNO", direction = "wide") # https://stackoverflow.com/questions/5890584/how-to-reshape-data-from-long-to-wide-format
date <- Rebalanced_volatility[1:nrow(Rebalanced_volatility),1]

rm(volatility)

#Calculate sd of last business days in the year
Rebalanced_volatility <- rollapply(data = Rebalanced_volatility[,2:ncol(Rebalanced_volatility)],width=245,FUN=sd, by.column=T, align = "right", na.rm=T) # US has 245 business days a year

x <- nrow(date)
Rebalanced_volatility <- cbind(date[255:x,1], Rebalanced_volatility)
rm(x)
names(Rebalanced_volatility) = gsub(pattern = "RET.", replacement = "", x = names(Rebalanced_volatility))
rm(date)
Rebalanced_volatility <- reshape2::melt(setDT(Rebalanced_volatility), id.vars = c("date"), variable.name = "PERMNO")
colnames(Rebalanced_volatility)[colnames(Rebalanced_volatility) == "value"] <- "sd"
Rebalanced_volatility <- Rebalanced_volatility[!is.na(Rebalanced_volatility$sd),]
Rebalanced_volatility$Month_Year <- substr(Rebalanced_volatility$date, 1,6)

Volatility <- Rebalanced_volatility %>% arrange(PERMNO, date) %>% group_by(PERMNO, Month_Year) %>% summarise_all(last)
Volatility$date <- NULL
colnames(Volatility)[colnames(Volatility) == "Month_Year"] <- "date"
Volatility$date <- as.numeric(as.character(Volatility$date))

######  Table 8 #####

# Bring betas file in similar form
names(betas) = gsub(pattern = "RET.", replacement = "", x = names(betas))
betas <- reshape2::melt(setDT(betas), id.vars = c("date"), variable.name = "PERMNO")
betas <- betas %>% drop_na(value)
colnames(betas)[colnames(betas) == "value"] <- "beta"

Table8 <- merge(betas, Reversal, by=c("date","PERMNO"))
Table8 <- merge(Table8, Momentum, by=c("date","PERMNO"))
Table8 <- merge(Table8, Volatility, by=c("date","PERMNO"))
Table8 <- merge(Table8, dividend_fusioned, by=c("date","PERMNO"))

## Coefficients for the whole portfolio
summary(lm(formula = beta ~ reversal + momentum + sd + div_yield, data = Table8))

reg <-lm(formula = beta ~ reversal + momentum + sd + div_yield, data = Table8)
4*(nrow(Table8)/100)^(2/9)
library(sandwich)
library(lmtest)
coeftest(reg, NeweyWest(reg, lag = 16, prewhite = FALSE))

## Coefficients of the Q1 Portfolio
dummy <- select(Q1_RollRet, date, PERMNO)
summary(lm(formula = beta ~ reversal + momentum + sd + div_yield, data = merge(dummy, Table8, by=c("date","PERMNO"))))

reg <-lm(formula = beta ~ reversal + momentum + sd + div_yield, data = merge(dummy, Table8, by=c("date","PERMNO")))

4*(nrow(Table8)/100)^(2/9)
library(sandwich)
library(lmtest)
coeftest(reg, NeweyWest(reg, lag = 16, prewhite = FALSE))

## Coefficients of the Q2 Portfolio
dummy <- select(Q2_RollRet, date, PERMNO)
summary(lm(formula = beta ~ reversal + momentum + sd + div_yield, data = merge(dummy, Table8, by=c("date","PERMNO"))))
reg <-lm(formula = beta ~ reversal + momentum + sd + div_yield, data = merge(dummy, Table8, by=c("date","PERMNO")))
coeftest(reg, NeweyWest(reg, lag = 16, prewhite = FALSE))

## Coefficients of the Q3 Portfolio
dummy <- select(Q3_RollRet, date, PERMNO)
summary(lm(formula = beta ~ reversal + momentum + sd + div_yield, data = merge(dummy, Table8, by=c("date","PERMNO"))))
reg <-lm(formula = beta ~ reversal + momentum + sd + div_yield, data = merge(dummy, Table8, by=c("date","PERMNO")))
coeftest(reg, NeweyWest(reg, lag = 16, prewhite = FALSE))

## Coefficients of the Q4 Portfolio
dummy <- select(Q4_RollRet, date, PERMNO)
summary(lm(formula = beta ~ reversal + momentum + sd + div_yield, data = merge(dummy, Table8, by=c("date","PERMNO"))))
coeftest(reg, NeweyWest(reg, lag = 16, prewhite = FALSE))

## Coefficients of the Q5 Portfolio
dummy <- select(Q5_RollRet, date, PERMNO)
summary(lm(formula = beta ~ reversal + momentum + sd + div_yield, data = merge(dummy, Table8, by=c("date","PERMNO"))))
rm(dummy)
reg <-lm(formula = beta ~ reversal + momentum + sd + div_yield, data = merge(dummy, Table8, by=c("date","PERMNO")))
coeftest(reg, NeweyWest(reg, lag = 16, prewhite = FALSE))

rm(Q1_RollRet)
rm(Q2_RollRet)
rm(Q3_RollRet)
rm(Q4_RollRet)
rm(Q5_RollRet)

#### Inflation hedgin by sector: Communication #### 

# Communication Services
Communication <- Table1_ID[which(Table1_ID$sector == "Communication Services"),]
Communication_Betas <- match_df(betas, Communication)
Communication <- match_df(InflaData, Communication)

# Inflation Beta for all Stocks
summary(lm(formula = RET ~ inflation, data = Communication))

reg <-lm(formula = RET ~ inflation, data = Communication)
4*(nrow(Communication)/100)^(2/9)
library(sandwich)
library(lmtest)
coeftest(reg, NeweyWest(reg, lag = 8, prewhite = FALSE))

# Calculate the market cap of all stocks in the portfolio
dummy <- aggregate(mktcap ~ date, Communication, sum)  
colnames(dummy)[colnames(dummy) == "mktcap"] <- "total_mktcap"
Communication <- merge(dummy,Communication,by=c("date"))
rm(dummy)

#Calculate the fraction of a stock in the overall portfolio
Communication <- transform(Communication, frct_port = mktcap / total_mktcap)

#Calculate the monthly portfolio returns
Communication$frcXret <- Communication$frct_port * Communication$RET
dummy <- aggregate(frcXret ~ date, Communication, sum)
colnames(dummy)[colnames(dummy) == "frcXret"] <- "tot_port_ret"
Communication <- merge(dummy,Communication,by=c("date"))
rm(dummy)

Communication <- select(Communication, date, tot_port_ret, inflation) # extract total portfolio returns for every year
Communication <- unique(Communication)

summary(lm(formula = tot_port_ret ~ inflation, data = Communication))

reg <-lm(formula = RET ~ inflation, data = Communication)
4*(nrow(Table8)/100)^(2/9)
library(sandwich)
library(lmtest)
coeftest(reg, NeweyWest(reg, lag = 8, prewhite = FALSE))

#### Inflation hedgin by sector: Consumer_Staples #### 

# Consumer_Staples Services
Consumer_Staples <- Table1_ID[which(Table1_ID$sector == "Consumer Staples"),]
Consumer_Staples_Betas <- match_df(betas, Consumer_Staples)
Consumer_Staples <- match_df(InflaData, Consumer_Staples)

# Inflation Beta for all Stocks
summary(lm(formula = RET ~ inflation, data = Consumer_Staples))

# Calculate the market cap of all stocks in the portfolio
dummy <- aggregate(mktcap ~ date, Consumer_Staples, sum)  
colnames(dummy)[colnames(dummy) == "mktcap"] <- "total_mktcap"
Consumer_Staples <- merge(dummy,Consumer_Staples,by=c("date"))
rm(dummy)


#Calculate the fraction of a stock in the overall portfolio
Consumer_Staples <- transform(Consumer_Staples, frct_port = mktcap / total_mktcap)

#Calculate the monthly portfolio returns
Consumer_Staples$frcXret <- Consumer_Staples$frct_port * Consumer_Staples$RET
dummy <- aggregate(frcXret ~ date, Consumer_Staples, sum)
colnames(dummy)[colnames(dummy) == "frcXret"] <- "tot_port_ret"
Consumer_Staples <- merge(dummy,Consumer_Staples,by=c("date"))
rm(dummy)

Consumer_Staples <- select(Consumer_Staples, date, tot_port_ret, inflation) # extract total portfolio returns for every year
Consumer_Staples <- unique(Consumer_Staples)

summary(lm(formula = tot_port_ret ~ inflation, data = Consumer_Staples))



#### Inflation hedgin by sector: Industrials #### 

# Industrials Services
Industrials <- Table1_ID[which(Table1_ID$sector == "Industrials"),]
Industrials_Betas <- match_df(betas, Industrials)
Industrials <- match_df(InflaData, Industrials)

# Inflation Beta for all Stocks
summary(lm(formula = RET ~ inflation, data = Industrials))

# Calculate the market cap of all stocks in the portfolio
dummy <- aggregate(mktcap ~ date, Industrials, sum)  
colnames(dummy)[colnames(dummy) == "mktcap"] <- "total_mktcap"
Industrials <- merge(dummy,Industrials,by=c("date"))
rm(dummy)


#Calculate the fraction of a stock in the overall portfolio
Industrials <- transform(Industrials, frct_port = mktcap / total_mktcap)

#Calculate the monthly portfolio returns
Industrials$frcXret <- Industrials$frct_port * Industrials$RET
dummy <- aggregate(frcXret ~ date, Industrials, sum)
colnames(dummy)[colnames(dummy) == "frcXret"] <- "tot_port_ret"
Industrials <- merge(dummy,Industrials,by=c("date"))
rm(dummy)

Industrials <- select(Industrials, date, tot_port_ret, inflation) # extract total portfolio returns for every year
Industrials <- unique(Industrials)

summary(lm(formula = tot_port_ret ~ inflation, data = Industrials))




#### Inflation hedgin by sector: Health_Care #### 

# Health_Care Services
Health_Care <- Table1_ID[which(Table1_ID$sector == "Health Care"),]
Health_Care_Betas <- match_df(betas, Health_Care)
Health_Care <- match_df(InflaData, Health_Care)

# Inflation Beta for all Stocks
summary(lm(formula = RET ~ inflation, data = Health_Care))


# Calculate the market cap of all stocks in the portfolio
dummy <- aggregate(mktcap ~ date, Health_Care, sum)  
colnames(dummy)[colnames(dummy) == "mktcap"] <- "total_mktcap"
Health_Care <- merge(dummy,Health_Care,by=c("date"))
rm(dummy)


#Calculate the fraction of a stock in the overall portfolio
Health_Care <- transform(Health_Care, frct_port = mktcap / total_mktcap)

#Calculate the monthly portfolio returns
Health_Care$frcXret <- Health_Care$frct_port * Health_Care$RET
dummy <- aggregate(frcXret ~ date, Health_Care, sum)
colnames(dummy)[colnames(dummy) == "frcXret"] <- "tot_port_ret"
Health_Care <- merge(dummy,Health_Care,by=c("date"))
rm(dummy)

Health_Care <- select(Health_Care, date, tot_port_ret, inflation) # extract total portfolio returns for every year
Health_Care <- unique(Health_Care)

summary(lm(formula = tot_port_ret ~ inflation, data = Health_Care))





#### Inflation hedgin by sector: Materials #### 

# Materials Services
Materials <- Table1_ID[which(Table1_ID$sector == "Materials"),]
Materials_Betas <- match_df(betas, Materials)
Materials <- match_df(InflaData, Materials)

# Inflation Beta for all Stocks
summary(lm(formula = RET ~ inflation, data = Materials))

# Calculate the market cap of all stocks in the portfolio
dummy <- aggregate(mktcap ~ date, Materials, sum)  
colnames(dummy)[colnames(dummy) == "mktcap"] <- "total_mktcap"
Materials <- merge(dummy,Materials,by=c("date"))
rm(dummy)


#Calculate the fraction of a stock in the overall portfolio
Materials <- transform(Materials, frct_port = mktcap / total_mktcap)

#Calculate the monthly portfolio returns
Materials$frcXret <- Materials$frct_port * Materials$RET
dummy <- aggregate(frcXret ~ date, Materials, sum)
colnames(dummy)[colnames(dummy) == "frcXret"] <- "tot_port_ret"
Materials <- merge(dummy,Materials,by=c("date"))
rm(dummy)

Materials <- select(Materials, date, tot_port_ret, inflation) # extract total portfolio returns for every year
Materials <- unique(Materials)

summary(lm(formula = tot_port_ret ~ inflation, data = Materials))





#### Inflation hedgin by sector: Energy #### 

# Energy Services
Energy <- Table1_ID[which(Table1_ID$sector == "Energy"),]
Energy_Betas <- match_df(betas, Energy)
Energy <- match_df(InflaData, Energy)

# Inflation Beta for all Stocks
summary(lm(formula = RET ~ inflation, data = Energy))


# Calculate the market cap of all stocks in the portfolio
dummy <- aggregate(mktcap ~ date, Energy, sum)  
colnames(dummy)[colnames(dummy) == "mktcap"] <- "total_mktcap"
Energy <- merge(dummy,Energy,by=c("date"))
rm(dummy)


#Calculate the fraction of a stock in the overall portfolio
Energy <- transform(Energy, frct_port = mktcap / total_mktcap)

#Calculate the monthly portfolio returns
Energy$frcXret <- Energy$frct_port * Energy$RET
dummy <- aggregate(frcXret ~ date, Energy, sum)
colnames(dummy)[colnames(dummy) == "frcXret"] <- "tot_port_ret"
Energy <- merge(dummy,Energy,by=c("date"))
rm(dummy)

Energy <- select(Energy, date, tot_port_ret, inflation) # extract total portfolio returns for every year
Energy <- unique(Energy)

summary(lm(formula = tot_port_ret ~ inflation, data = Energy))







#### Inflation hedgin by sector: Utilities #### 

# Utilities Services
Utilities <- Table1_ID[which(Table1_ID$sector == "Utilities"),]
Utilities_Betas <- match_df(betas, Utilities)
Utilities <- match_df(InflaData, Utilities)

# Inflation Beta for all Stocks
summary(lm(formula = RET ~ inflation, data = Utilities))

# Calculate the market cap of all stocks in the portfolio
dummy <- aggregate(mktcap ~ date, Utilities, sum)  
colnames(dummy)[colnames(dummy) == "mktcap"] <- "total_mktcap"
Utilities <- merge(dummy,Utilities,by=c("date"))
rm(dummy)


#Calculate the fraction of a stock in the overall portfolio
Utilities <- transform(Utilities, frct_port = mktcap / total_mktcap)

#Calculate the monthly portfolio returns
Utilities$frcXret <- Utilities$frct_port * Utilities$RET
dummy <- aggregate(frcXret ~ date, Utilities, sum)
colnames(dummy)[colnames(dummy) == "frcXret"] <- "tot_port_ret"
Utilities <- merge(dummy,Utilities,by=c("date"))
rm(dummy)

Utilities <- select(Utilities, date, tot_port_ret, inflation) # extract total portfolio returns for every year
Utilities <- unique(Utilities)

summary(lm(formula = tot_port_ret ~ inflation, data = Utilities))








#### Inflation hedgin by sector: Consumer_Discretionary #### 

# Consumer_Discretionary Services
Consumer_Discretionary <- Table1_ID[which(Table1_ID$sector == "Consumer Discretionary"),]
Consumer_Discretionary_Betas <- match_df(betas, Consumer_Discretionary)
Consumer_Discretionary <- match_df(InflaData, Consumer_Discretionary)

# Inflation Beta for all Stocks
summary(lm(formula = RET ~ inflation, data = Consumer_Discretionary))

# Calculate the market cap of all stocks in the portfolio
dummy <- aggregate(mktcap ~ date, Consumer_Discretionary, sum)  
colnames(dummy)[colnames(dummy) == "mktcap"] <- "total_mktcap"
Consumer_Discretionary <- merge(dummy,Consumer_Discretionary,by=c("date"))
rm(dummy)


#Calculate the fraction of a stock in the overall portfolio
Consumer_Discretionary <- transform(Consumer_Discretionary, frct_port = mktcap / total_mktcap)

#Calculate the monthly portfolio returns
Consumer_Discretionary$frcXret <- Consumer_Discretionary$frct_port * Consumer_Discretionary$RET
dummy <- aggregate(frcXret ~ date, Consumer_Discretionary, sum)
colnames(dummy)[colnames(dummy) == "frcXret"] <- "tot_port_ret"
Consumer_Discretionary <- merge(dummy,Consumer_Discretionary,by=c("date"))
rm(dummy)

Consumer_Discretionary <- select(Consumer_Discretionary, date, tot_port_ret, inflation) # extract total portfolio returns for every year
Consumer_Discretionary <- unique(Consumer_Discretionary)

summary(lm(formula = tot_port_ret ~ inflation, data = Consumer_Discretionary))


#### Inflation hedgin by sector: Real_Estate #### 

# Real_Estate Services
Real_Estate <- Table1_ID[which(Table1_ID$sector == "Real Estate"),]
Real_Estate_Betas <- match_df(betas, Real_Estate)
Real_Estate <- match_df(InflaData, Real_Estate)

# Inflation Beta for all Stocks
summary(lm(formula = RET ~ inflation, data = Real_Estate))

# Calculate the market cap of all stocks in the portfolio
dummy <- aggregate(mktcap ~ date, Real_Estate, sum)  
colnames(dummy)[colnames(dummy) == "mktcap"] <- "total_mktcap"
Real_Estate <- merge(dummy,Real_Estate,by=c("date"))
rm(dummy)


#Calculate the fraction of a stock in the overall portfolio
Real_Estate <- transform(Real_Estate, frct_port = mktcap / total_mktcap)

#Calculate the monthly portfolio returns
Real_Estate$frcXret <- Real_Estate$frct_port * Real_Estate$RET
dummy <- aggregate(frcXret ~ date, Real_Estate, sum)
colnames(dummy)[colnames(dummy) == "frcXret"] <- "tot_port_ret"
Real_Estate <- merge(dummy,Real_Estate,by=c("date"))
rm(dummy)

Real_Estate <- select(Real_Estate, date, tot_port_ret, inflation) # extract total portfolio returns for every year
Real_Estate <- unique(Real_Estate)

summary(lm(formula = tot_port_ret ~ inflation, data = Real_Estate))








#### Inflation hedgin by sector: IT #### 

# IT Services
IT <- Table1_ID[which(Table1_ID$sector == "Information Technology"),]
IT_Betas <- match_df(betas, IT)
IT <- match_df(InflaData, IT)

# Inflation Beta for all Stocks
summary(lm(formula = RET ~ inflation, data = IT))

# Calculate the market cap of all stocks in the portfolio
dummy <- aggregate(mktcap ~ date, IT, sum)  
colnames(dummy)[colnames(dummy) == "mktcap"] <- "total_mktcap"
IT <- merge(dummy,IT,by=c("date"))
rm(dummy)


#Calculate the fraction of a stock in the overall portfolio
IT <- transform(IT, frct_port = mktcap / total_mktcap)

#Calculate the monthly portfolio returns
IT$frcXret <- IT$frct_port * IT$RET
dummy <- aggregate(frcXret ~ date, IT, sum)
colnames(dummy)[colnames(dummy) == "frcXret"] <- "tot_port_ret"
IT <- merge(dummy,IT,by=c("date"))
rm(dummy)

IT <- select(IT, date, tot_port_ret, inflation) # extract total portfolio returns for every year
IT <- unique(IT)

summary(lm(formula = tot_port_ret ~ inflation, data = IT))








#### Inflation hedgin by sector: Financials #### 

# Financials Services
Financials <- Table1_ID[which(Table1_ID$sector == "Financials"),]
Financials_Betas <- match_df(betas, Financials)
Financials <- match_df(InflaData, Financials)

# Inflation Beta for all Stocks
summary(lm(formula = RET ~ inflation, data = Financials))

# Calculate the market cap of all stocks in the portfolio
dummy <- aggregate(mktcap ~ date, Financials, sum)  
colnames(dummy)[colnames(dummy) == "mktcap"] <- "total_mktcap"
Financials <- merge(dummy,Financials,by=c("date"))
rm(dummy)


#Calculate the fraction of a stock in the overall portfolio
Financials <- transform(Financials, frct_port = mktcap / total_mktcap)

#Calculate the monthly portfolio returns
Financials$frcXret <- Financials$frct_port * Financials$RET
dummy <- aggregate(frcXret ~ date, Financials, sum)
colnames(dummy)[colnames(dummy) == "frcXret"] <- "tot_port_ret"
Financials <- merge(dummy,Financials,by=c("date"))
rm(dummy)

Financials <- select(Financials, date, tot_port_ret, inflation) # extract total portfolio returns for every year
Financials <- unique(Financials)

summary(lm(formula = tot_port_ret ~ inflation, data = Financials))

# Results: Single stock betas all (!) positive and mostly significant. Portfolio level betas all insignificant and mixed coefficients


#### Plot Rolling Betas per sector:Communication ####
library(DescTools)
Communication_Betas$beta <- Winsorize(Communication_Betas$beta)
Average_Beta_Communication <- aggregate(Communication_Betas[, 3], list(Communication_Betas$date), mean)
dummy <- aggregate(Communication_Betas[, 3], list(Communication_Betas$date), sd)
colnames(dummy)[colnames(dummy) == "beta"] <- "sd"
Average_Beta_Communication <- merge(Average_Beta_Communication, dummy, by=c("Group.1"))
colnames(Average_Beta_Communication)[colnames(Average_Beta_Communication) == "Group.1"] <- "date"
rm(dummy)

#### Plot Rolling Betas per sector:Consumer_Staples ####
Consumer_Staples_Betas$beta <- Winsorize(Consumer_Staples_Betas$beta)

Average_Beta_Consumer_Staples <- aggregate(Consumer_Staples_Betas[, 3], list(Consumer_Staples_Betas$date), mean)
dummy <- aggregate(Consumer_Staples_Betas[, 3], list(Consumer_Staples_Betas$date), sd)
colnames(dummy)[colnames(dummy) == "beta"] <- "sd"
Average_Beta_Consumer_Staples <- merge(Average_Beta_Consumer_Staples, dummy, by=c("Group.1"))
colnames(Average_Beta_Consumer_Staples)[colnames(Average_Beta_Consumer_Staples) == "Group.1"] <- "date"
rm(dummy)


#### Plot Rolling Betas per sector:Industrials ####
Industrials_Betas$beta <- Winsorize(Industrials_Betas$beta)

Average_Beta_Industrials <- aggregate(Industrials_Betas[, 3], list(Industrials_Betas$date), mean)
dummy <- aggregate(Industrials_Betas[, 3], list(Industrials_Betas$date), sd)
colnames(dummy)[colnames(dummy) == "beta"] <- "sd"
Average_Beta_Industrials <- merge(Average_Beta_Industrials, dummy, by=c("Group.1"))
colnames(Average_Beta_Industrials)[colnames(Average_Beta_Industrials) == "Group.1"] <- "date"
rm(dummy)


#### Plot Rolling Betas per sector:Health_Care ####
Health_Care_Betas$beta <- Winsorize(Health_Care_Betas$beta)

Average_Beta_Health_Care <- aggregate(Health_Care_Betas[, 3], list(Health_Care_Betas$date), mean)
dummy <- aggregate(Health_Care_Betas[, 3], list(Health_Care_Betas$date), sd)
colnames(dummy)[colnames(dummy) == "beta"] <- "sd"
Average_Beta_Health_Care <- merge(Average_Beta_Health_Care, dummy, by=c("Group.1"))
colnames(Average_Beta_Health_Care)[colnames(Average_Beta_Health_Care) == "Group.1"] <- "date"
rm(dummy)


#### Plot Rolling Betas per sector:Materials ####
Materials_Betas$beta <- Winsorize(Materials_Betas$beta)

Average_Beta_Materials <- aggregate(Materials_Betas[, 3], list(Materials_Betas$date), mean)
dummy <- aggregate(Materials_Betas[, 3], list(Materials_Betas$date), sd)
colnames(dummy)[colnames(dummy) == "beta"] <- "sd"
Average_Beta_Materials <- merge(Average_Beta_Materials, dummy, by=c("Group.1"))
colnames(Average_Beta_Materials)[colnames(Average_Beta_Materials) == "Group.1"] <- "date"
rm(dummy)


#### Plot Rolling Betas per sector:Energy ####
Energy_Betas$beta <- Winsorize(Energy_Betas$beta)

Average_Beta_Energy <- aggregate(Energy_Betas[, 3], list(Energy_Betas$date), mean)
dummy <- aggregate(Energy_Betas[, 3], list(Energy_Betas$date), sd)
colnames(dummy)[colnames(dummy) == "beta"] <- "sd"
Average_Beta_Energy <- merge(Average_Beta_Energy, dummy, by=c("Group.1"))
colnames(Average_Beta_Energy)[colnames(Average_Beta_Energy) == "Group.1"] <- "date"
rm(dummy)


#### Plot Rolling Betas per sector:Utilities ####
Utilities_Betas$beta <- Winsorize(Utilities_Betas$beta)

Average_Beta_Utilities <- aggregate(Utilities_Betas[, 3], list(Utilities_Betas$date), mean)
dummy <- aggregate(Utilities_Betas[, 3], list(Utilities_Betas$date), sd)
colnames(dummy)[colnames(dummy) == "beta"] <- "sd"
Average_Beta_Utilities <- merge(Average_Beta_Utilities, dummy, by=c("Group.1"))
colnames(Average_Beta_Utilities)[colnames(Average_Beta_Utilities) == "Group.1"] <- "date"
rm(dummy)


#### Plot Rolling Betas per sector:Consumer_Discretionary ####
Consumer_Discretionary_Betas$beta <- Winsorize(Consumer_Discretionary_Betas$beta)

Average_Beta_Consumer_Discretionary <- aggregate(Consumer_Discretionary_Betas[, 3], list(Consumer_Discretionary_Betas$date), mean)
dummy <- aggregate(Consumer_Discretionary_Betas[, 3], list(Consumer_Discretionary_Betas$date), sd)
colnames(dummy)[colnames(dummy) == "beta"] <- "sd"
Average_Beta_Consumer_Discretionary <- merge(Average_Beta_Consumer_Discretionary, dummy, by=c("Group.1"))
colnames(Average_Beta_Consumer_Discretionary)[colnames(Average_Beta_Consumer_Discretionary) == "Group.1"] <- "date"
rm(dummy)


#### Plot Rolling Betas per sector:Real_Estate ####
Real_Estate_Betas$beta <- Winsorize(Real_Estate_Betas$beta)

Average_Beta_Real_Estate <- aggregate(Real_Estate_Betas[, 3], list(Real_Estate_Betas$date), mean)
dummy <- aggregate(Real_Estate_Betas[, 3], list(Real_Estate_Betas$date), sd)
colnames(dummy)[colnames(dummy) == "beta"] <- "sd"
Average_Beta_Real_Estate <- merge(Average_Beta_Real_Estate, dummy, by=c("Group.1"))
colnames(Average_Beta_Real_Estate)[colnames(Average_Beta_Real_Estate) == "Group.1"] <- "date"
rm(dummy)


#### Plot Rolling Betas per sector:IT ####
IT_Betas$beta <- Winsorize(IT_Betas$beta)

Average_Beta_IT <- aggregate(IT_Betas[, 3], list(IT_Betas$date), mean)
dummy <- aggregate(IT_Betas[, 3], list(IT_Betas$date), sd)
colnames(dummy)[colnames(dummy) == "beta"] <- "sd"
Average_Beta_IT <- merge(Average_Beta_IT, dummy, by=c("Group.1"))
colnames(Average_Beta_IT)[colnames(Average_Beta_IT) == "Group.1"] <- "date"
rm(dummy)


#### Plot Rolling Betas per sector:Financials ####
Financials_Betas$beta <- Winsorize(Financials_Betas$beta)

Average_Beta_Financials <- aggregate(Financials_Betas[, 3], list(Financials_Betas$date), mean)
dummy <- aggregate(Financials_Betas[, 3], list(Financials_Betas$date), sd)
colnames(dummy)[colnames(dummy) == "beta"] <- "sd"
Average_Beta_Financials <- merge(Average_Beta_Financials, dummy, by=c("Group.1"))
colnames(Average_Beta_Financials)[colnames(Average_Beta_Financials) == "Group.1"] <- "date"
rm(dummy)


#### Q1 Dividend ########

dividend_quantiles <- dividend_fusioned %>% group_by(date) %>% mutate(Quantile = ntile(div_yield, 5))

dividend_Q1 <- dividend_quantiles[which(dividend_quantiles$Quantile == 5),]
dividend_Q1$Quantile <- NULL
dividend_Q1 <- merge(dividend_Q1, select(InflaData, date, PERMNO, RET, mktcap, inflation), by=c("date", "PERMNO"))

summary(lm(formula = RET ~ inflation, data = dividend_Q1))

reg <-lm(formula = RET ~ inflation, data = dividend_Q1)
4*(nrow(dividend_Q1)/100)^(2/9)
library(sandwich)
library(lmtest)
coeftest(reg, NeweyWest(reg, lag = 2, prewhite = FALSE))

# Calculate the market cap of all stocks in the portfolio
dummy <- aggregate(mktcap ~ date, dividend_Q1, sum)  
colnames(dummy)[colnames(dummy) == "mktcap"] <- "total_mktcap"
dividend_Q1 <- merge(dummy,dividend_Q1,by=c("date"))
rm(dummy)

#Calculate the fraction of a stock in the overall portfolio
dividend_Q1 <- transform(dividend_Q1, frct_port = mktcap / total_mktcap)

#Calculate the monthly portfolio returns
dividend_Q1$frcXret <- dividend_Q1$frct_port * dividend_Q1$RET
dummy <- aggregate(frcXret ~ date, dividend_Q1, sum)
colnames(dummy)[colnames(dummy) == "frcXret"] <- "tot_port_ret"
dividend_Q1 <- merge(dummy,dividend_Q1,by=c("date"))
rm(dummy)

dividend_Q1 <- select(dividend_Q1, date, tot_port_ret, inflation) # extract total portfolio returns for every year
dividend_Q1 <- unique(dividend_Q1)

summary(lm(formula = tot_port_ret ~ inflation, data = dividend_Q1))

reg <-lm(formula = tot_port_ret ~ inflation, data = dividend_Q1)
4*(nrow(dividend_Q1)/100)^(2/9)
library(sandwich)
library(lmtest)
coeftest(reg, NeweyWest(reg, lag = 6, prewhite = FALSE))

#### Q2:Dividend ####

dividend_Q2 <- dividend_quantiles[which(dividend_quantiles$Quantile == 4),]
dividend_Q2$Quantile <- NULL
dividend_Q2 <- merge(dividend_Q2, select(InflaData, date, PERMNO, RET, mktcap, inflation), by=c("date", "PERMNO"))

summary(lm(formula = RET ~ inflation, data = dividend_Q2))

reg <-lm(formula = RET ~ inflation, data = dividend_Q2)
4*(nrow(dividend_Q2)/100)^(2/9)
library(sandwich)
library(lmtest)
coeftest(reg, NeweyWest(reg, lag = 12, prewhite = FALSE))

# Calculate the market cap of all stocks in the portfolio
dummy <- aggregate(mktcap ~ date, dividend_Q2, sum)  
colnames(dummy)[colnames(dummy) == "mktcap"] <- "total_mktcap"
dividend_Q2 <- merge(dummy,dividend_Q2,by=c("date"))
rm(dummy)

#Calculate the fraction of a stock in the overall portfolio
dividend_Q2 <- transform(dividend_Q2, frct_port = mktcap / total_mktcap)

#Calculate the monthly portfolio returns
dividend_Q2$frcXret <- dividend_Q2$frct_port * dividend_Q2$RET
dummy <- aggregate(frcXret ~ date, dividend_Q2, sum)
colnames(dummy)[colnames(dummy) == "frcXret"] <- "tot_port_ret"
dividend_Q2 <- merge(dummy,dividend_Q2,by=c("date"))
rm(dummy)

dividend_Q2 <- select(dividend_Q2, date, tot_port_ret, inflation) # extract total portfolio returns for every year
dividend_Q2 <- unique(dividend_Q2)

summary(lm(formula = tot_port_ret ~ inflation, data = dividend_Q2))

reg <-lm(formula = tot_port_ret ~ inflation, data = dividend_Q2)
4*(nrow(dividend_Q2)/100)^(2/9)
library(sandwich)
library(lmtest)
coeftest(reg, NeweyWest(reg, lag = 6, prewhite = FALSE))


#### Q3:Dividend ####

dividend_Q3 <- dividend_quantiles[which(dividend_quantiles$Quantile == 3),]
dividend_Q3$Quantile <- NULL
dividend_Q3 <- merge(dividend_Q3, select(InflaData, date, PERMNO, RET, mktcap, inflation), by=c("date", "PERMNO"))

summary(lm(formula = RET ~ inflation, data = dividend_Q3))
reg <-lm(formula = RET ~ inflation, data = dividend_Q3)
4*(nrow(dividend_Q3)/100)^(2/9)
library(sandwich)
library(lmtest)
coeftest(reg, NeweyWest(reg, lag = 12, prewhite = FALSE))

# Calculate the market cap of all stocks in the portfolio
dummy <- aggregate(mktcap ~ date, dividend_Q3, sum)  
colnames(dummy)[colnames(dummy) == "mktcap"] <- "total_mktcap"
dividend_Q3 <- merge(dummy,dividend_Q3,by=c("date"))
rm(dummy)

#Calculate the fraction of a stock in the overall portfolio
dividend_Q3 <- transform(dividend_Q3, frct_port = mktcap / total_mktcap)

#Calculate the monthly portfolio returns
dividend_Q3$frcXret <- dividend_Q3$frct_port * dividend_Q3$RET
dummy <- aggregate(frcXret ~ date, dividend_Q3, sum)
colnames(dummy)[colnames(dummy) == "frcXret"] <- "tot_port_ret"
dividend_Q3 <- merge(dummy,dividend_Q3,by=c("date"))
rm(dummy)

dividend_Q3 <- select(dividend_Q3, date, tot_port_ret, inflation) # extract total portfolio returns for every year
dividend_Q3 <- unique(dividend_Q3)

summary(lm(formula = tot_port_ret ~ inflation, data = dividend_Q3))

reg <-lm(formula = tot_port_ret ~ inflation, data = dividend_Q3)
4*(nrow(dividend_Q3)/100)^(2/9)
library(sandwich)
library(lmtest)
coeftest(reg, NeweyWest(reg, lag = 6, prewhite = FALSE))

#### Q4:Dividend ####

dividend_Q4 <- dividend_quantiles[which(dividend_quantiles$Quantile == 2),]
dividend_Q4$Quantile <- NULL
dividend_Q4 <- merge(dividend_Q4, select(InflaData, date, PERMNO, RET, mktcap, inflation), by=c("date", "PERMNO"))

summary(lm(formula = RET ~ inflation, data = dividend_Q4))
reg <-lm(formula = RET ~ inflation, data = dividend_Q4)
4*(nrow(dividend_Q4)/100)^(2/9)
library(sandwich)
library(lmtest)
coeftest(reg, NeweyWest(reg, lag = 12, prewhite = FALSE))

# Calculate the market cap of all stocks in the portfolio
dummy <- aggregate(mktcap ~ date, dividend_Q4, sum)  
colnames(dummy)[colnames(dummy) == "mktcap"] <- "total_mktcap"
dividend_Q4 <- merge(dummy,dividend_Q4,by=c("date"))
rm(dummy)

#Calculate the fraction of a stock in the overall portfolio
dividend_Q4 <- transform(dividend_Q4, frct_port = mktcap / total_mktcap)

#Calculate the monthly portfolio returns
dividend_Q4$frcXret <- dividend_Q4$frct_port * dividend_Q4$RET
dummy <- aggregate(frcXret ~ date, dividend_Q4, sum)
colnames(dummy)[colnames(dummy) == "frcXret"] <- "tot_port_ret"
dividend_Q4 <- merge(dummy,dividend_Q4,by=c("date"))
rm(dummy)

dividend_Q4 <- select(dividend_Q4, date, tot_port_ret, inflation) # extract total portfolio returns for every year
dividend_Q4 <- unique(dividend_Q4)

summary(lm(formula = tot_port_ret ~ inflation, data = dividend_Q4))

reg <-lm(formula = tot_port_ret ~ inflation, data = dividend_Q4)
4*(nrow(dividend_Q4)/100)^(2/9)
library(sandwich)
library(lmtest)
coeftest(reg, NeweyWest(reg, lag = 6, prewhite = FALSE))

#### Q5:Dividend ####

dividend_Q5 <- dividend_quantiles[which(dividend_quantiles$Quantile == 1),]
dividend_Q5$Quantile <- NULL
dividend_Q5 <- merge(dividend_Q5, select(InflaData, date, PERMNO, RET, mktcap, inflation), by=c("date", "PERMNO"))

summary(lm(formula = RET ~ inflation, data = dividend_Q5))

reg <-lm(formula = RET ~ inflation, data = dividend_Q5)
4*(nrow(dividend_Q5)/100)^(2/9)
library(sandwich)
library(lmtest)
coeftest(reg, NeweyWest(reg, lag = 12, prewhite = FALSE))

# Calculate the market cap of all stocks in the portfolio
dummy <- aggregate(mktcap ~ date, dividend_Q5, sum)  
colnames(dummy)[colnames(dummy) == "mktcap"] <- "total_mktcap"
dividend_Q5 <- merge(dummy,dividend_Q5,by=c("date"))
rm(dummy)

#Calculate the fraction of a stock in the overall portfolio
dividend_Q5 <- transform(dividend_Q5, frct_port = mktcap / total_mktcap)

#Calculate the monthly portfolio returns
dividend_Q5$frcXret <- dividend_Q5$frct_port * dividend_Q5$RET
dummy <- aggregate(frcXret ~ date, dividend_Q5, sum)
colnames(dummy)[colnames(dummy) == "frcXret"] <- "tot_port_ret"
dividend_Q5 <- merge(dummy,dividend_Q5,by=c("date"))
rm(dummy)

dividend_Q5 <- select(dividend_Q5, date, tot_port_ret, inflation) # extract total portfolio returns for every year
dividend_Q5 <- unique(dividend_Q5)

summary(lm(formula = tot_port_ret ~ inflation, data = dividend_Q5))

reg <-lm(formula = tot_port_ret ~ inflation, data = dividend_Q5)
4*(nrow(dividend_Q5)/100)^(2/9)
library(sandwich)
library(lmtest)
coeftest(reg, NeweyWest(reg, lag = 6, prewhite = FALSE))


###Q1-Q5 Dividend: Long-Short ########

# Calculate the nominal annual mean return

dividend_Q1_Q5 <- dividend_Q1
dividend_Q1_Q5$tot_port_ret <- dividend_Q1$tot_port_ret - dividend_Q5$tot_port_ret

summary(lm(formula = tot_port_ret ~ inflation, data = dividend_Q1_Q5))

reg <-lm(formula = tot_port_ret ~ inflation, data = dividend_Q1_Q5)
4*(nrow(dividend_Q1_Q5)/100)^(2/9)
library(sandwich)
library(lmtest)
coeftest(reg, NeweyWest(reg, lag = 6, prewhite = FALSE))




#### InflaBeta Regression for all Stocks ####
summary(lm(formula = RET ~ inflation, data = InflaData))


#### Q1-Q5 Cross Section #### ####
Q1_Q5_coef_cross_section <- Table8 %>% select(date, beta, reversal, sd, momentum, div_yield) %>% na.omit( )%>%
  as_tibble %>%                                                   # first set the data in long format
  nest(beta, reversal, sd, momentum, div_yield) %>%                                        # now nest the dependent and independent factors
  mutate(model = map(data, ~lm(beta ~ reversal + sd + momentum + div_yield, data = .))) %>%   # fit the model using purrr
  mutate(tidy_model = map(model, tidy)) %>%                       # clean the model output with broom
  select(-data, -model) %>%                                       # remove the "untidy" parts
  unnest()

# Q1-Q5 R2
Q1_Q5_R2_cross_section <- Table8 %>% select(date, beta, reversal, momentum, sd, div_yield) %>% na.omit( )%>%
  as_tibble %>%                                                   # first set the data in long format
  nest(beta, reversal, sd, momentum, div_yield) %>%                                        # now nest the dependent and independent factors
  mutate(model = map(data, ~lm(beta ~ reversal + sd + momentum + div_yield, data = .))) %>%   # fit the model using purrr
  mutate(glance_model = map(model, glance)) %>%                       # clean the model output with broom
  select(-data, -model) %>%                                       # remove the "untidy" parts
  unnest()


### Calculate Average Coefficients Q1-Q5

# reversal
summary(Q1_Q5_coef_cross_section[which(Q1_Q5_coef_cross_section$term == "reversal"),])

# sd
summary(Q1_Q5_coef_cross_section[which(Q1_Q5_coef_cross_section$term == "sd"),])

# momentum
summary(Q1_Q5_coef_cross_section[which(Q1_Q5_coef_cross_section$term == "momentum"),])

# div_yield
summary(Q1_Q5_coef_cross_section[which(Q1_Q5_coef_cross_section$term == "div_yield"),])

### Calculate Average R2
summary(Q1_Q5_R2_cross_section)







#### Q1-Q5 Cross Section #### ####
coeftest(lm(formula = tot_port_ret ~ inflation, data = dividend_Q2), NeweyWest(lm(formula = tot_port_ret ~ inflation, data = dividend_Q2), lag = 6, prewhite = FALSE))

x <- NeweyWest(lm(formula = tot_port_ret ~ inflation, data = dividend_Q2), lag = 6, prewhite = FALSE)

Q1_Q5_coef_cross_section <- Table8 %>% select(date, beta, reversal, sd, momentum, div_yield) %>% na.omit( )%>%
  as_tibble %>%                                                   # first set the data in long format
  nest(beta, reversal, sd, momentum, div_yield) %>%                                        # now nest the dependent and independent factors
  mutate(model = map(data, ~NeweyWest(lm(beta ~ reversal + sd + momentum + div_yield, data = .)))) %>%   # fit the model using purrr
  mutate(tidy_model = map(model, tidy)) %>%                       # clean the model output with broom
  select(-data, -model) %>%                                       # remove the "untidy" parts
  unnest()

# Q1-Q5 R2
Q1_Q5_R2_cross_section <- Table8 %>% select(date, beta, reversal, momentum, sd, div_yield) %>% na.omit( )%>%
  as_tibble %>%                                                   # first set the data in long format
  nest(beta, reversal, sd, momentum, div_yield) %>%                                        # now nest the dependent and independent factors
  mutate(model = map(data, ~lm(beta ~ reversal + sd + momentum + div_yield, data = .))) %>%   # fit the model using purrr
  mutate(glance_model = map(model, glance)) %>%                       # clean the model output with broom
  select(-data, -model) %>%                                       # remove the "untidy" parts
  unnest()


### Calculate Average Coefficients Q1-Q5

# reversal
summary(Q1_Q5_coef_cross_section[which(Q1_Q5_coef_cross_section$term == "reversal"),])

# sd
summary(Q1_Q5_coef_cross_section[which(Q1_Q5_coef_cross_section$term == "sd"),])

# momentum
summary(Q1_Q5_coef_cross_section[which(Q1_Q5_coef_cross_section$term == "momentum"),])

# div_yield
summary(Q1_Q5_coef_cross_section[which(Q1_Q5_coef_cross_section$term == "div_yield"),])

### Calculate Average R2
summary(Q1_Q5_R2_cross_section)






#### Q1 Cross Section ####

# Get back dataset with Q1 out of sample PERMNO´s and dates
Q1_Predict <- select(quantile_data, date, PERMNO, quintile)
Q1_Predict <- Q1_Predict[which(Q1_Predict$quintile == 1),]

Table8$PERMNO <- as.numeric(as.character(Table8$PERMNO))

# Merge dataset based on two conditions to add momentum, sd etc. variables to Q1 portfolio
Q1_Predict <- merge(Q1_Predict[,1:2], Table8, by=c("date","PERMNO")) # NA's match

Q1_coef_cross_section <- Q1_Predict %>% select(date, beta, reversal, sd, momentum, div_yield) %>% na.omit( )%>%
  as_tibble %>%                                                   # first set the data in long format
  nest(beta, reversal, sd, momentum, div_yield) %>%                                        # now nest the dependent and independent factors
  mutate(model = map(data, ~lm(beta ~ reversal + sd + momentum + div_yield, data = .))) %>%   # fit the model using purrr
  mutate(tidy_model = map(model, tidy)) %>%                       # clean the model output with broom
  select(-data, -model) %>%                                       # remove the "untidy" parts
  unnest()

# Q1-Q5 R2
Q1_R2_cross_section <- Q1_Predict %>% select(date, beta, reversal, sd, momentum, div_yield) %>% na.omit( )%>%
  as_tibble %>%                                                   # first set the data in long format
  nest(beta, reversal, sd, momentum, div_yield) %>%                                        # now nest the dependent and independent factors
  mutate(model = map(data, ~lm(beta ~ reversal + sd + momentum + div_yield, data = .))) %>%   # fit the model using purrr
  mutate(glance_model = map(model, glance)) %>%                       # clean the model output with broom
  select(-data, -model) %>%                                       # remove the "untidy" parts
  unnest()


### Calculate Average Coefficients Q1

# reversal
summary(Q1_coef_cross_section[which(Q1_coef_cross_section$term == "reversal"),])

# sd
summary(Q1_coef_cross_section[which(Q1_coef_cross_section$term == "sd"),])

# momentum
summary(Q1_coef_cross_section[which(Q1_coef_cross_section$term == "momentum"),])

# div_yield
summary(Q1_coef_cross_section[which(Q1_coef_cross_section$term == "div_yield"),])

### Calculate Average R2
summary(Q1_R2_cross_section)



#### Q2 Cross Section ####

# Get back dataset with Q1 out of sample PERMNO´s and dates
Q2_Predict <- select(quantile_data, date, PERMNO, quintile)
Q2_Predict <- Q2_Predict[which(Q2_Predict$quintile == 2),]

# Merge dataset based on two conditions to add momentum, sd etc. variables to Q1 portfolio
Q2_Predict <- merge(Q2_Predict[,1:2], Table8, by=c("date","PERMNO")) # NA's match

Q2_coef_cross_section <- Q2_Predict %>% select(date, beta, reversal, sd, momentum, div_yield) %>% na.omit( )%>%
  as_tibble %>%                                                   # first set the data in long format
  nest(beta, reversal, sd, momentum, div_yield) %>%                                        # now nest the dependent and independent factors
  mutate(model = map(data, ~lm(beta ~ reversal + sd + momentum + div_yield, data = .))) %>%   # fit the model using purrr
  mutate(tidy_model = map(model, tidy)) %>%                       # clean the model output with broom
  select(-data, -model) %>%                                       # remove the "untidy" parts
  unnest()

# Q1-Q5 R2
Q2_R2_cross_section <- Q2_Predict %>% select(date, beta, reversal, sd, momentum, div_yield) %>% na.omit( )%>%
  as_tibble %>%                                                   # first set the data in long format
  nest(beta, reversal, sd, momentum, div_yield) %>%                                        # now nest the dependent and independent factors
  mutate(model = map(data, ~lm(beta ~ reversal + sd + momentum + div_yield, data = .))) %>%   # fit the model using purrr
  mutate(glance_model = map(model, glance)) %>%                       # clean the model output with broom
  select(-data, -model) %>%                                       # remove the "untidy" parts
  unnest()


### Calculate Average Coefficients Q2

# reversal
summary(Q2_coef_cross_section[which(Q2_coef_cross_section$term == "reversal"),])

# sd
summary(Q2_coef_cross_section[which(Q2_coef_cross_section$term == "sd"),])

# momentum
summary(Q2_coef_cross_section[which(Q2_coef_cross_section$term == "momentum"),])

# div_yield
summary(Q2_coef_cross_section[which(Q2_coef_cross_section$term == "div_yield"),])

### Calculate Average R2
summary(Q2_R2_cross_section)





#### Q3 Cross Section ####

# Get back dataset with Q1 out of sample PERMNO´s and dates
Q3_Predict <- select(quantile_data, date, PERMNO, quintile)
Q3_Predict <- Q3_Predict[which(Q3_Predict$quintile == 3),]

# Merge dataset based on two conditions to add momentum, sd etc. variables to Q1 portfolio
Q3_Predict <- merge(Q3_Predict[,1:2], Table8, by=c("date","PERMNO")) # NA's match

Q3_coef_cross_section <- Q3_Predict %>% select(date, beta, reversal, sd, momentum, div_yield) %>% na.omit( )%>%
  as_tibble %>%                                                   # first set the data in long format
  nest(beta, reversal, sd, momentum, div_yield) %>%                                        # now nest the dependent and independent factors
  mutate(model = map(data, ~lm(beta ~ reversal + sd + momentum + div_yield, data = .))) %>%   # fit the model using purrr
  mutate(tidy_model = map(model, tidy)) %>%                       # clean the model output with broom
  select(-data, -model) %>%                                       # remove the "untidy" parts
  unnest()

# Q1-Q5 R2
Q3_R2_cross_section <- Q3_Predict %>% select(date, beta, reversal, sd, momentum, div_yield) %>% na.omit( )%>%
  as_tibble %>%                                                   # first set the data in long format
  nest(beta, reversal, sd, momentum, div_yield) %>%                                        # now nest the dependent and independent factors
  mutate(model = map(data, ~lm(beta ~ reversal + sd + momentum + div_yield, data = .))) %>%   # fit the model using purrr
  mutate(glance_model = map(model, glance)) %>%                       # clean the model output with broom
  select(-data, -model) %>%                                       # remove the "untidy" parts
  unnest()


### Calculate Average Coefficients Q3

# reversal
summary(Q3_coef_cross_section[which(Q3_coef_cross_section$term == "reversal"),])

# sd
summary(Q3_coef_cross_section[which(Q3_coef_cross_section$term == "sd"),])

# momentum
summary(Q3_coef_cross_section[which(Q3_coef_cross_section$term == "momentum"),])

# div_yield
summary(Q3_coef_cross_section[which(Q3_coef_cross_section$term == "div_yield"),])

### Calculate Average R2
summary(Q3_R2_cross_section)




#### Q4 Cross Section ####

# Get back dataset with Q1 out of sample PERMNO´s and dates
Q4_Predict <- select(quantile_data, date, PERMNO, quintile)
Q4_Predict <- Q4_Predict[which(Q4_Predict$quintile == 4),]

# Merge dataset based on two conditions to add momentum, sd etc. variables to Q1 portfolio
Q4_Predict <- merge(Q4_Predict[,1:2], Table8, by=c("date","PERMNO")) # NA's match

Q4_coef_cross_section <- Q4_Predict %>% select(date, beta, reversal, sd, momentum, div_yield) %>% na.omit( )%>%
  as_tibble %>%                                                   # first set the data in long format
  nest(beta, reversal, sd, momentum, div_yield) %>%                                        # now nest the dependent and independent factors
  mutate(model = map(data, ~lm(beta ~ reversal + sd + momentum + div_yield, data = .))) %>%   # fit the model using purrr
  mutate(tidy_model = map(model, tidy)) %>%                       # clean the model output with broom
  select(-data, -model) %>%                                       # remove the "untidy" parts
  unnest()

# Q4 R2
Q4_R2_cross_section <- Q4_Predict %>% select(date, beta, reversal, sd, momentum, div_yield) %>% na.omit( )%>%
  as_tibble %>%                                                   # first set the data in long format
  nest(beta, reversal, sd, momentum, div_yield) %>%                                        # now nest the dependent and independent factors
  mutate(model = map(data, ~lm(beta ~ reversal + sd + momentum + div_yield, data = .))) %>%   # fit the model using purrr
  mutate(glance_model = map(model, glance)) %>%                       # clean the model output with broom
  select(-data, -model) %>%                                       # remove the "untidy" parts
  unnest()


### Calculate Average Coefficients Q4

# reversal
summary(Q4_coef_cross_section[which(Q4_coef_cross_section$term == "reversal"),])

# sd
summary(Q4_coef_cross_section[which(Q4_coef_cross_section$term == "sd"),])

# momentum
summary(Q4_coef_cross_section[which(Q4_coef_cross_section$term == "momentum"),])

# div_yield
summary(Q4_coef_cross_section[which(Q4_coef_cross_section$term == "div_yield"),])

### Calculate Average R2
summary(Q4_R2_cross_section)


#### Q5 Cross Section ####

# Get back dataset with Q1 out of sample PERMNO´s and dates
Q5_Predict <- select(quantile_data, date, PERMNO, quintile)
Q5_Predict <- Q5_Predict[which(Q5_Predict$quintile == 5),]

# Merge dataset based on two conditions to add momentum, sd etc. variables to Q1 portfolio
Q5_Predict <- merge(Q5_Predict[,1:2], Table8, by=c("date","PERMNO")) # NA's match

Q5_coef_cross_section <- Q5_Predict %>% select(date, beta, reversal, sd, momentum, div_yield) %>% na.omit( )%>%
  as_tibble %>%                                                   # first set the data in long format
  nest(beta, reversal, sd, momentum, div_yield) %>%                                        # now nest the dependent and independent factors
  mutate(model = map(data, ~lm(beta ~ reversal + sd + momentum + div_yield, data = .))) %>%   # fit the model using purrr
  mutate(tidy_model = map(model, tidy)) %>%                       # clean the model output with broom
  select(-data, -model) %>%                                       # remove the "untidy" parts
  unnest()

# Q5 R2
Q5_R2_cross_section <- Q5_Predict %>% select(date, beta, reversal, sd, momentum, div_yield) %>% na.omit( )%>%
  as_tibble %>%                                                   # first set the data in long format
  nest(beta, reversal, sd, momentum, div_yield) %>%                                        # now nest the dependent and independent factors
  mutate(model = map(data, ~lm(beta ~ reversal + sd + momentum + div_yield, data = .))) %>%   # fit the model using purrr
  mutate(glance_model = map(model, glance)) %>%                       # clean the model output with broom
  select(-data, -model) %>%                                       # remove the "untidy" parts
  unnest()


### Calculate Average Coefficients Q5

# reversal
summary(Q5_coef_cross_section[which(Q5_coef_cross_section$term == "reversal"),])

# sd
summary(Q5_coef_cross_section[which(Q5_coef_cross_section$term == "sd"),])

# momentum
summary(Q5_coef_cross_section[which(Q5_coef_cross_section$term == "momentum"),])

# div_yield
summary(Q5_coef_cross_section[which(Q5_coef_cross_section$term == "div_yield"),])

### Calculate Average R2
summary(Q5_R2_cross_section)
