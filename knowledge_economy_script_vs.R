#' ---
#' title: Replication Package - "The Impact of Knowledge Worker on Wage Inequality" (a working paper) 
#' author: "Andrea Chu"
#' date: "Feb 9, 2022"
#' ---

#' For first-time user only
#' 
#install.packages("ipumsr")
#install.packages("labelled")
#install.packages("dplyr")   
#install.packages("reshape")
#install.packages("quantreg")

### ***Free up memory before starting this project
rm(list = ls())
#working_path <- "/knowledge_economy"
#setwd(working_path)

#' ## Step: Loading data from IPUMS
#'
library(ipumsr)
ddi <- read_ipums_ddi("cps_00012.xml")
data <- read_ipums_micro(ddi)
#names(data)
#head(data)

#' ## Step: Building the base dataset
#'
cpsdata <- subset(data, MONTH == 3   #sample, March survey data only
                  & !is.na(CPSIDP) & CPSIDP > 0 #valid individual record
                  & (SEX %in% c(1,2))  #valid gender
                  & !is.na(ASECWT)     #valid weight 
                  & YEAR <= 2019       #sample between 2009 and 2019
                  & EARNWEEK <= 2885    # topcode 2885 for years 1998-onward
                  & WKSTAT == 11         # Full-time hours (35+), usually full-time, other work status requires data processing before using the data (more later)
                 # & !(WKSTAT %in% 99)  #99=NIU, blank, or not in labor force
                  & !(OCC1990 %in% c(991, 999) ),  #991=Unemployed, 999=N/A
                  select=c(CPSIDP,YEAR, AGE, SEX,ASECWT,OCC1990,EDUC,WKSTAT, EARNWEEK, STATECENSUS, STATEFIP))

# recode SEX(1=male, 2=female) to Male(1=male, 0=female)
cpsdata$MALE <- ifelse(cpsdata$SEX==2,0,cpsdata$SEX)

# check if any duplicates in the dataset
cpsdata$key <- paste(cpsdata$CPSIDP, cpsdata$YEAR, cpsdata$AGE, cpsdata$SEX, sep="-")
if( length(cpsdata$key) == length(unique(cpsdata$key)) ){
  print(paste("Successfully left joined: total record count " ,length(cpsdata$key) ))
}else{
  stop("Require further analysis: duplicates found." )
}
# expected: 106397


### data summary
summary(cpsdata)

#' ## Step: EDA
#' EARNWEEK(Y), Gender/MALE(X)
#'
library(ggplot2)
library(labelled)

ggplot(cpsdata, aes(AGE, weight = ASECWT))  +
  geom_histogram(aes(y = ..density..),binwidth=1) +
  ylab("Percentage") +
  facet_grid(MALE ~ .)

ggplot(cpsdata, aes(EARNWEEK, weight = ASECWT))  +
  geom_histogram() +
  facet_grid(MALE ~ .)

#' ## Step: EDA 
#' EARNWEEK(Y), STATECENSUS(X)
#'
library("dplyr")                     

tab_earnweek_by_state  <- 
  cpsdata %>%                               # Summary by group using dplyr
  group_by(STATECENSUS) %>% 
  summarize( p10 = quantile(EARNWEEK, 0.1),
            p20 = quantile(EARNWEEK, 0.2),
            median = median(EARNWEEK),
            mean = mean(EARNWEEK),
            p80 = quantile(EARNWEEK, 0.8),
            p90 = quantile(EARNWEEK, 0.9)
           )

#View(tab_earnweek_by_state, "Weekly Earnings by State")

#' ## Step: EDA
#' EARNWEEK(Y), EDUC_SOME_COLLEGE(X)
#'
## view all possible values of education
#tab_educ_values <- ipums_val_labels(cpsdata$EDUC)
#View(tab_educ_values, "List of all Educ Values")
cpsdata$EDUC_SOME_COLLEGE <- ifelse(cpsdata$EDUC >=80, 1,0 )

#viz after separating population into with and without some college education
ggplot(cpsdata, aes(EARNWEEK, weight = ASECWT))  +
  geom_histogram() +
  facet_grid(EDUC_SOME_COLLEGE ~ .)

#' ## Step: Construct an attribute 
#' Create "knowledge" occupational attribute (based on wording in job title e.g. analyst, instructor, ...)
#' 
#output to excel for manual selection -- don't overwrite unless it's needed
#write.csv(tab_occ_values, "tab_occ_values.csv")

# step of marking knowledge occupation is subjective. will be reviewed.

#read occupation with knowledge workers attributes
tab_occ_values_updated <- read.csv(file= "tab_occ_values_updated.csv")

#keep essential columns
tab_occ_values <- tab_occ_values_updated[c("occ_val", "occ_label","occ_knowledge_worker")]

#rename field from occ_knowledge_worker to occ_knowledge because it's an attribute tied to occupation
names(tab_occ_values) <- c("occ_val", "occ_label","occ_knowledge")

#' ## Step: Merge two datasets
#' Append additional attribute to each individual based on his/her occupational title
#' 
cpsdata_v2 <- merge(x = cpsdata, y = tab_occ_values,
                    by.x ="OCC1990", by.y = "occ_val", 
                    all.x = TRUE, all.y = FALSE  )

## checking if it's left join
if( length(cpsdata_v2$key) == length(cpsdata$key) ){
  print(paste("Successfully left joined: total record count " ,length(cpsdata_v2$key) ))
} else {
  stop("Require further analysis. Stop here. ") 
}

## viz
cpsdata_v2 %>%
  ggplot( aes(x=EARNWEEK, fill=occ_knowledge, weight = ASECWT)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#c36059", "#69b3a2")) +
  labs(fill="")

#' ## Step: Construct a macro panel data (source: BEA)
#' Reshape BEA data to panel data (id=state, time=year)
#' Create one lag state-level GDP with one lag 
#' Merge state-level data with CPS micro data
#' 
#' 
macro_gdpdata <- read.csv(file="BEA_state_gdp.csv")
### all fip codes in BEA dataset have trailing 000. 
## Note: don't substr because 01 and 10 will be tranlates to 10
macro_gdpdata$statefip <- as.numeric(macro_gdpdata$GeoFips )/1000
macro_gdpdata$GeoName[1] <- "United States"
#keep essential columns
#note macro data requires one lagged year for econometric model
macro_gdpdata <- macro_gdpdata[c("statefip", "GeoName","X2008","X2009",
                                 "X2010","X2011","X2012","X2013","X2014","X2015","X2016","X2017","X2018","X2019")]
macro_gdpdata<-macro_gdpdata[with(macro_gdpdata, order(statefip)),]


## step-detail: construct state level
library(reshape)
macro_gdpdata_panel <- melt(macro_gdpdata, id=c("statefip","GeoName"))
names(macro_gdpdata_panel) <- c("statefip" ,"State" , "Year", "GDP" )
macro_gdpdata_panel$Year <- substr(macro_gdpdata_panel$Year , 2, 5)

# sort first, before lagging GDP values
macro_gdpdata_panel <- macro_gdpdata_panel[with(macro_gdpdata_panel, order(statefip, Year)), ]
# create lag values for cpsdata
library(dplyr)
macro_gdpdata_panel$GDP_PY <-ifelse(macro_gdpdata_panel$Year >= 2009,
                                    lag(macro_gdpdata_panel$GDP, n=1), 0)

# view cpsdata before merging state level gdp
#tab_states <- ipums_val_labels(cpsdata$STATEFIP)
#View(tab_states)

## step-detail: merge state level with micro data(cpsdata)
cpsdata_v3 <- merge(x = cpsdata_v2, y = macro_gdpdata_panel,
                    by.x = c("STATEFIP","YEAR"), by.y = c( "statefip","Year"), 
                    all.x = TRUE, all.y = FALSE  )

## checking if it's left join
if( length(cpsdata_v3$key) == length(cpsdata_v2$key) ){
  print(paste("Successfully left joined: total record count " ,length(cpsdata_v3$key) ))
}else{
  stop("Require further analysis. Stop here. ") 
}

#' ## Step: Construct a macro panel data
#' Subset country-level GDP from the previous step 
#' Merge country-level data with CPS micro data
#' 
# select country level gdp
macro_gdpdata_panel_country_level <- macro_gdpdata_panel[macro_gdpdata_panel$statefip==0,]
# keep essential columns, don't need fip because it's country level
macro_gdpdata_panel_country_level <- macro_gdpdata_panel_country_level[,c("Year", "GDP" ,"GDP_PY")]
# rename gdp and gdp_py to country level
names(macro_gdpdata_panel_country_level) = c("Year", "GDP_USA" ,"GDP_USA_PY")
## step-detail: merge country level with micro data(cpsdata)
cpsdata_v4 <- merge(x = cpsdata_v3, y = macro_gdpdata_panel_country_level,
                    by.x = c("YEAR"), by.y = c( "Year"), 
                    all.x = TRUE, all.y = FALSE  )
## checking if it's left join
if( length(cpsdata_v4$key) == length(cpsdata_v3$key) ){
  print(paste("Successfully left joined: total record count " ,length(cpsdata_v3$key) ))
}else{
  stop("Require further analysis. Stop here. ") 
}
cpsdata_v4$occ_knowledge <- ifelse(cpsdata_v4$occ_knowledge=="Y" , 1, 0)
cpsdata_v4$occ_knowledge_worker  <- ifelse(cpsdata_v4$EDUC_SOME_COLLEGE==1 & cpsdata_v4$occ_knowledge==1, 1, 0)

#' ## Step: Clean up 
#' Free memory before intensive calculation
#' 
#' 
rm(ddi)
rm(cpsdata)
rm(cpsdata_v2)
rm(cpsdata_v3)
rm(data)
rm(macro_gdpdata)
rm(macro_gdpdata_panel)
rm(macro_gdpdata_panel_country_level)


#' ## Step: Quantile Regression - Main Model
#' Return to occupational choice : occ_knowledge 
#' Return to college education: EDUC_SOME_COLLEGE 
#' Marginal return to knowledge worker: occ_knowledge_worker -- captures the additional gain for one with college education and decided to work in a knowledge-intensive occupation
#' 
library(quantreg)
attach(cpsdata_v4)
X <- cbind(AGE, SEX, EDUC_SOME_COLLEGE, occ_knowledge, occ_knowledge_worker, GDP_PY, GDP, GDP_USA, GDP_USA_PY)
Y <- cbind(EARNWEEK)

### linear regression as a benchmark to compare against quantile regression
#reg <- lm( Y ~ X, data=cpsdata_v4)
#summary(reg)

### summary in graph
sqreg <- rq( Y ~ X , tau = seq(.1,  .9, by = .1),weights=ASECWT , data=cpsdata_v4)
sqreg.plot <- summary(sqreg)
plot(sqreg.plot)

### summary in table format
qreg_p10_p90 <- rq( Y ~ X , tau = c( .1, .9),weights=ASECWT , data=cpsdata_v4)
summary(qreg_p10_p90)

#' ## Step: Quantile Regression - Alternative Model (Control)
#' Return to occupational choice : occ_knowledge 
#' Return to college education: EDUC_SOME_COLLEGE 
#' 
X2 <- cbind(AGE, SEX, EDUC_SOME_COLLEGE, occ_knowledge,  GDP_PY, GDP, GDP_USA, GDP_USA_PY)
qreg_p10_p90_alt <- rq( Y ~ X2 , tau = c( .1, .9), weights=ASECWT ,data=cpsdata_v4)
summary(qreg_p10_p90_alt)

### ***step: Wrapping up
detach("cpsdata_v4")
