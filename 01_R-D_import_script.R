library(readr)
library(rstudioapi)
library(RMySQL)
library(dplyr)
library(stringr)
library(stringdist)
library(tidyr)
library(psych)

#**********************************************************************************************************************************************************************************************
# Code for: (1) importing data from IP Australia (IPGOD); Capital IQ; DatAnalysis.  Pre-select variables based on availability and perform basic pre-processing
#           (2) Integrate data
#           (3) Perform pre-processing and data wrangling (before exploratory data analysis)
#**********************************************************************************************************************************************************************************************

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to source file location

#===STEP 1 (import, pre-select and basic pre-processing) ===========================================================================================================================================================
#---IPAustralia data:
psswd <- .rs.askForPassword("Database password:") #activate a pop up asking for the password, which is then stored in this variable
mydb <- RMySQL::dbConnect(MySQL(), user = 'root', password = psswd, dbname = 'IP_Australia') # connect with MySQL database
query <- function(...) dbGetQuery(mydb, ...) #---Function to make queries faster and easier
ip_raw <- query("SELECT * FROM IP_Australia.applications_party_activity WHERE party_type = 'organisation' AND (party_role_category = 'applicant' OR party_role_category = 'designer' OR party_role_category = 'licensee');")
lapply(dbListConnections(dbDriver(drv = "MySQL")), dbDisconnect) # close all opened connections to db
ip_raw <- ip_raw[,c(1,4,7,8,12,13)] #remove columns not needed
ip_raw[ip_raw == "NA"] <- NA #replace 'NA' strings by actual NA
#ip_raw <- ip_raw[!is.na(ip_raw$abn),] #filter out ip from organisation without abn
#---End



#---CIQ data:
CIQ_rawdata <- read_csv("data/Capital_IQ_2020_10_09.csv")
CIQ_rawdata <- data.frame(CIQ_rawdata) #make it a df
CIQ_rawdata[CIQ_rawdata == "-"] <- NA
CIQ_rawdata <- CIQ_rawdata[,-c(7,16)] #remove columns that, after review, provide very few non-missing values, and that are already collected in CIQ_2_rawdata

# Obtain the ASX ticker in CIQ data (it's not explicit in the data)
CIQ_rawdata$ticker <- NA
for (i in 1:nrow(CIQ_rawdata)) {
  if(is.na(CIQ_rawdata$Exchange.Ticker[i]) | substr(CIQ_rawdata$Exchange.Ticker[i], 1, 4) != "ASX:") { #if the primary ticker is not from the ASX, or is NA, grab it from the Exchanges Secondary Listings
    end <- regexpr("Company:", CIQ_rawdata$Exchanges..Secondary.Listings.[i])[1] - 2 #get where 'Company:' substring starts, then substract 2 to get where the ticker ends
    CIQ_rawdata$ticker[i] <- substr(CIQ_rawdata$Exchanges..Secondary.Listings.[i], 5, end)
  } else { #if the primary ticker is from the ASX, grab the part after 'ASX:'
    CIQ_rawdata$ticker[i] <- substring(CIQ_rawdata$Exchange.Ticker[i], 5) 
  }
}
CIQ_rawdata <- CIQ_rawdata[,c(1,ncol(CIQ_rawdata),2:(ncol(CIQ_rawdata) - 1))] # move the new ticker column
CIQ_rawdata <- CIQ_rawdata[,!(names(CIQ_rawdata) %in% c("Exchange.Ticker", "Exchanges..Primary.Listing.", "Exchanges..Secondary.Listings."))] #remove old exchange listings info columns
#---End



#---CIQ data - 2nd batch of variables:
CIQ_2_rawdata <- read_csv("data/Capital_IQ_2_2020_10_28.csv")
CIQ_2_rawdata <- data.frame(CIQ_2_rawdata) #make it a df
CIQ_2_rawdata[CIQ_2_rawdata == "-"] <- NA
CIQ_2_rawdata <- CIQ_2_rawdata[,-c(1,3,6:8,10:13,16,18:21,31,34,36,37)] #remove columns that, after review, provide very few non-missing values, and that are already collected in CIQ_rawdata

# Obtain the ASX ticker in CIQ data (it's not explicit in the data)
CIQ_2_rawdata$ticker <- NA
for (i in 1:nrow(CIQ_2_rawdata)) {
  if(is.na(CIQ_2_rawdata$Exchange.Ticker[i]) | substr(CIQ_2_rawdata$Exchange.Ticker[i], 1, 4) != "ASX:") { #if the primary ticker is not from the ASX, or is NA, grab it from the Exchanges Secondary Listings
    end <- regexpr("Company:", CIQ_2_rawdata$Exchanges..Secondary.Listings.[i])[1] - 2 #get where 'Company:' substring starts, then substract 2 to get where the ticker ends
    CIQ_2_rawdata$ticker[i] <- substr(CIQ_2_rawdata$Exchanges..Secondary.Listings.[i], 5, end)
  } else { #if the primary ticker is from the ASX, grab the part after 'ASX:'
    CIQ_2_rawdata$ticker[i] <- substring(CIQ_2_rawdata$Exchange.Ticker[i], 5) 
  }
}
CIQ_2_rawdata <- CIQ_2_rawdata[,c(ncol(CIQ_2_rawdata),1:(ncol(CIQ_2_rawdata) - 1))] # move the new ticker column
CIQ_2_rawdata <- CIQ_2_rawdata[,!(names(CIQ_2_rawdata) %in% c("Exchange.Ticker", "Exchanges..Primary.Listing.", "Exchanges..Secondary.Listings."))] #remove old exchange listings info columns
#---End



#---DatAnalysis data:
DA_rawdata <- read_csv("data/DatAnalysis_2020_10_27.csv")
DA_rawdata <- data.frame(DA_rawdata) #make it a df
DA_rawdata[DA_rawdata == "--"] <- NA
DA_rawdata$ABN <- gsub(" ", "", DA_rawdata$ABN) #remove empty spaces in ABN
#---End



#---DatAnalysis data - 2nd batch of variables:
DA_2_rawdata <- read_csv("data/DatAnalysis_2_2020_10_28.csv")
DA_2_rawdata <- data.frame(DA_2_rawdata) #make it a df
DA_2_rawdata[DA_2_rawdata == "--"] <- NA
DA_2_rawdata <- DA_2_rawdata[,-c(2,3)] #remove columns that are already collected in CIQ_rawdata


#---DatAnalysis data - 3rd batch: R&D expenditures values sent by Morningstar:
DA_3_rawdata <- read_csv("data/DatAnalysis_RD_2020_10_30.csv")
DA_3_rawdata <- data.frame(DA_3_rawdata[,c(2,3,5)]) # make it df
DA_3_rawdata$Value <- DA_3_rawdata$Value * -1 #make it positive, for consistency with CIQ data
colnames(DA_3_rawdata)[1] <- "ASX.Code"

#The dates are in 'DD/MM/YY 0:00' format. Some firms report in Dec, others in Jun. Collapse this field to the year, retrieving the YY by substracting 6 spaces from right to left:
DA_3_rawdata$Date <- paste0("RD_20", substr(DA_3_rawdata$Date, nchar(DA_3_rawdata$Date)-6, nchar(DA_3_rawdata$Date)-5)) 

#Remove duplicates and keep first:
DA_3_rawdata <- DA_3_rawdata %>%
  group_by(ASX.Code, Date) %>%
  distinct(Date, .keep_all = T)

DA_3_rawdata <- DA_3_rawdata %>% spread(Date, Value) #transform from long to wide
#---End
#===End STEP 1 ===============================================================================================================================================================================



#===STEP 2 (Data integration) ===========================================================================================================================================================
#---CIQ <-> DA
CIQ_rawdata_full <- merge(CIQ_rawdata, CIQ_2_rawdata, by = "ticker") #merge the two CIQ datasets
DA_rawdata_full <- merge(DA_rawdata, DA_2_rawdata, by ="ASX.Code") #merge the two DA datasets
DA_rawdata_full <- merge(DA_rawdata_full, DA_3_rawdata, by = "ASX.Code", all.x = T) #merge the 3rd DA dataset with the rest
names(DA_rawdata_full)[names(DA_rawdata_full) == "ASX.Code"] <- "ticker"
tmp <- merge(CIQ_rawdata_full, DA_rawdata_full, by = "ticker") #merge CIQ with DA

#check duplicates:
tmp_dup <- tmp %>%
  group_by(ticker) %>%
  filter(n() > 1)
  #result: 10 obs, i.e. 5 duplicated firms (duplicated twice). After review, they are the same. Keep first:
tmp <- tmp %>%
  group_by(ticker) %>%
  distinct(ticker, .keep_all = T)

#check they are the same companies via company name:
tmp$Company.Name.x <- str_replace(tmp$Company.Name.x," \\(.*", "") #To drop the '(ASX:..)' stuff after the company name from CIQ, this replace whatever it's after " (" by an empty string
#fuzzy matching:
tmp$company_name_match <- stringdist(tmp$Company.Name.x, tmp$Company.Name.y, method = "lv")
  #result: 17 companies with Levenshtein distance > 10. After desktop research, it's confirmed that all of them belong to the corresponding company (some name changes, some variations in names, etc.)
tmp <- tmp[,!(names(tmp) %in% c("Company.Name.x", "company_name_match"))] #remove name match column, and names from CIQ (DA seems more up-to-date)
#--End


#---(CIQ+DA) <-> IPAustralia
#First, build the no. patents, trademarks and designs variables:
ASX_ABNs <- unique(tmp$ABN) #vector with all ABNs
ASX_ABNs <- ASX_ABNs[!is.na(ASX_ABNs)] #remove NA as it's not an actual ABN

colnames(ip_raw)[3] <- "company_name_ip"
ip_raw_agg <- ip_raw[ip_raw$abn %in% ASX_ABNs,] %>%
  group_by(abn, company_name_ip, ip_right_type) %>%
  summarise(n = n()) %>% 
  spread(ip_right_type, n)

unique_company_name_ip <- ip_raw_agg[!duplicated(ip_raw_agg$abn),c(1,2)]

ip_raw_agg <- ip_raw[ip_raw$abn %in% ASX_ABNs,] %>%
  group_by(abn, ip_right_type) %>%
  summarise(n = n()) %>% 
  spread(ip_right_type, n)

tmp2 <- merge(ip_raw_agg, unique_company_name_ip)
colnames(tmp2)[c(2, 3, 4)] <- c("no_designs", "no_patents", "no_trademarks")
colnames(tmp2)[1] <- "ABN"

#Merge:
data <- merge(tmp, tmp2, by = "ABN", all.x = T) #leaves unmatched records from CIQ+DA, as missing values for IP is actually data

#check duplicates:
data_dup <- data %>%
  group_by(ticker) %>%
  filter(n() > 1)
  #result: no duplicates

#check they are the same companies via company name:
#fuzzy matching:
data$company_name_match <- stringdist(data$Company.Name.y, data$company_name_ip, method = "lv")
  #result: 83 companies with Levenshtein distance > 10. After desktop research, all but 5 present problems. Is DA, rather than IPAustralia, the one with problems.
  #resolution: manually change the ABNs in tmp (DA) data for the correct one, then re-merge:
tmp[tmp$ticker == "BRU","ABN"] <- "71130651437" #from https://abr.business.gov.au
tmp[tmp$ticker == "AUP","ABN"] <- "39778584426" #from https://abr.business.gov.au
tmp[tmp$ticker == "ICQ","ABN"] <- "91157710846" #from https://abr.business.gov.au
tmp[tmp$ticker == "TWR","ABN"] <- NA #could not find anything in https://abr.business.gov.au
tmp[tmp$ticker == "SDL","ABN"] <- "19055719394" #from https://abr.business.gov.au
tmp[tmp$ticker == "CLH","ABN"] <- "74010230716" #from https://abr.business.gov.au

#Re-Merge:
data <- merge(tmp, tmp2, by = "ABN", all.x = T) #leaves unmatched records from CIQ+DA, as missing values for IP is actually data
data$company_name_match <- stringdist(data$Company.Name.y, data$company_name_ip, method = "lv") #inspection passed: troublesome records not present anymore
data <- data[,!(names(data) %in% c("company_name_ip", "company_name_match"))] #remove name match column, and names from IPAustralia (retain DA to keep consistency)
data <- data[!is.na(data$ABN), ] # do not accept companies without ABN, as they are unable to be linked with ip data (creating bias)
data[c("no_designs", "no_patents", "no_trademarks")][is.na(data[c("no_designs", "no_patents", "no_trademarks")])] <- 0 #replace NAs in design, patents and trademarks with 0, as the absence of data actually indicate 0 design, patents and trademarks found in ip data
#--End
#===End STEP 2 ===============================================================================================================================================================================




#===STEP 3 (Pre-processing and data wrangling) ===========================================================================================================================================================
#eliminate redundant columns:
data <- data[,!(names(data) %in% c("Primary.Address", "X..of.Directors.that.are.Internal", "Former.Name", "Country.Region.of.Incorporation", "Registered.Office.State"))]

#re-order columns:
#data <- data[,c(2,31,1,13,32,12,23,36,17,18,19,28,4,5,33,34,35,3,41,25,24,29,20,21,22,26,11,6,7,40,27,8,39,9,37,10,38,30,42,46,47,48,14,15,16,43,44,45)]

#remove commas in columns representing numbers, and convert to numbers:
data$X..of.Directors.that.are.External <- as.numeric(data$X..of.Directors.that.are.External)
data$Number.of.Geographic.Segments..Annual. <- as.numeric(data$Number.of.Geographic.Segments..Annual.)
data$Year.Founded <- as.numeric(data$Year.Founded)
data$ASX.Listing.Date <- as.numeric(substring(data$ASX.Listing.Date, 7))
data$No..of.Employees <- as.numeric(gsub(",", "", data$No..of.Employees))
data$Number.of.Employees...Global..Latest. <- as.numeric(gsub(",", "", data$Number.of.Employees...Global..Latest.))
data$Total.Employees..Latest.Annual. <- as.numeric(gsub(",", "", data$Total.Employees..Latest.Annual.))
data$Market.Capitalisation <- as.numeric(gsub(",", "", data$Market.Capitalisation)) / 1000000 #transform to million like CIQ
data$Market.Capitalization..My.Setting...Latest...AUDmm..Historical.rate. <- as.numeric(gsub(",", "", data$Market.Capitalization..My.Setting...Latest...AUDmm..Historical.rate.))
data$Total.Revenue <- as.numeric(gsub(",", "", data$Total.Revenue)) / 1000000 #transform to million like CIQ
data$Total.Revenue..Latest.Annual...AUDmm..Historical.rate. <- as.numeric(gsub(",", "", data$Total.Revenue..Latest.Annual...AUDmm..Historical.rate.))
data$Total.Assets <- as.numeric(gsub(",", "", data$Total.Assets)) / 1000000 #transform to million like CIQ
data$Total.Assets..Latest.Annual...AUDmm..Historical.rate. <- as.numeric(gsub(",", "", data$Total.Assets..Latest.Annual...AUDmm..Historical.rate.))
data$NCA.Intangibles.ExGW. <- as.numeric(gsub(",", "", data$NCA.Intangibles.ExGW.)) / 1000000 #transform to million like CIQ
data$Total.Intangibles..Latest.Annual...AUD..Historical.rate. <- as.numeric(gsub(",", "", data$Total.Intangibles..Latest.Annual...AUD..Historical.rate.))
data$R.D.Expense..LTM...2...AUDmm..Historical.rate. <- as.numeric(gsub(",", "", data$R.D.Expense..LTM...2...AUDmm..Historical.rate.))
data$R.D.Expense..LTM...1...AUDmm..Historical.rate. <- as.numeric(gsub(",", "", data$R.D.Expense..LTM...1...AUDmm..Historical.rate.))
data$R.D.Expense..LTM...AUDmm..Historical.rate. <- as.numeric(gsub(",", "", data$R.D.Expense..LTM...AUDmm..Historical.rate.))
data$RD_2018 <- as.numeric(gsub(",", "", data$RD_2018)) / 1000000 #transform to million like CIQ
data$RD_2019 <- as.numeric(gsub(",", "", data$RD_2019)) / 1000000 #transform to million like CIQ
data$RD_2020 <- as.numeric(gsub(",", "", data$RD_2020)) / 1000000 #transform to million like CIQ

#coalesce no employees variable:
data <- data %>% 
  mutate(no_employees = dplyr::coalesce(No..of.Employees, Number.of.Employees...Global..Latest., Total.Employees..Latest.Annual.)) #preference by lower number of NAs (DA first, then CIQs preserving order)
data <- data[,!(names(data) %in% c("No..of.Employees", "Number.of.Employees...Global..Latest.", "Total.Employees..Latest.Annual."))] #remove old employees variables

#coalesce market capitalisation variable:
data <- data %>%
  mutate(market_capitalisation = dplyr::coalesce(Market.Capitalisation, Market.Capitalization..My.Setting...Latest...AUDmm..Historical.rate.)) #preference by lower number of NAs (DA first)
data <- data[,!(names(data) %in% c("Market.Capitalisation", "Market.Capitalization..My.Setting...Latest...AUDmm..Historical.rate."))] #remove old market capitalisation variables

#coalesce revenue variable:
data <- data %>%
  mutate(total_revenue = dplyr::coalesce(Total.Revenue, Total.Revenue..Latest.Annual...AUDmm..Historical.rate.)) #preference by lower number of NAs (DA first)
data <- data[,!(names(data) %in% c("Total.Revenue", "Total.Revenue..Latest.Annual...AUDmm..Historical.rate."))] #remove old revenue variables


#coalesce assets variable:
data <- data %>%
  mutate(total_assets = dplyr::coalesce(Total.Assets, Total.Assets..Latest.Annual...AUDmm..Historical.rate.)) #preference by lower number of NAs (DA first)
data <- data[,!(names(data) %in% c("Total.Assets", "Total.Assets..Latest.Annual...AUDmm..Historical.rate."))] #remove old assets variables


#coalesce intangibles variable:
data <- data %>%
  mutate(intangible_assets = dplyr::coalesce(NCA.Intangibles.ExGW., Total.Intangibles..Latest.Annual...AUD..Historical.rate.)) #preference by lower number of NAs (DA first)
data <- data[,!(names(data) %in% c("NCA.Intangibles.ExGW.", "Total.Intangibles..Latest.Annual...AUD..Historical.rate."))] #remove old intangibles variables

#re-order columns:
data <- data[,c(2,25,1,8,26,7,18,30,12,13,14,23,4,5,27,28,29,3,31,20,19,24,15,16,17,21,6,22,38,39,40,41,42,35,36,37,9,10,11,32,33,34)]

#set proper column names:
colnames(data) <- c("ticker","company_name","ABN","business_description","trading_status","company_status","ownership","country_of_incorporation","headquarters_city","headquarters_state",
                    "headquarters_country","no_geographic_segments","primary_industry","industry_classifications","GICS_sector","GICS_industry_group","GICS_industry","year_founded",
                    "ASX_listing_year","product_names","product_descriptions","no_business_segments","business_relationships","competitors","customers","suppliers","strategic_alliances", 
                    "pct_external_directors","no_employees","market_capitalisation","total_revenue","total_assets","intangible_assets","no_designs","no_patents","no_trademarks",
                    "RD_LTM2","RD_LTM1","RD_LTM", "RD_2018_DA", "RD_2019_DA", "RD_2020_DA")
#===End STEP 3 ===============================================================================================================================================================================

write.csv(data, "data_step1.csv", row.names = F) #save all data into a file for further analysis

