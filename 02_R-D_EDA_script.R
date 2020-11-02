library(readr)
library(psych)
library(stringr)
library(dplyr)
library(naniar)
library(ggplot2)
library(GGally)

#**********************************************************************************************************************************************************************************************
# Code for: (1) Inspection and variable construction
#           (2) Descriptive statistics, exploratory analysis
#           (3) Visual exploration through visual analytics of key variables
#**********************************************************************************************************************************************************************************************


#===STEP 1 (Inspection and variable construction) ===========================================================================================================================================================
data <- read_csv("data_step1.csv")
data$ABN <- as.character(data$ABN)

#---Check number of levels for categorical variables, collapse if needed, and turn into factors:
#trading status:
length(unique(data$trading_status)) #3, good
data$trading_status <- as.factor(data$trading_status)

#company status:
length(unique(data$company_status)) #4, good
data$company_status <- as.factor(data$company_status)

#country incorporation:
length(unique(data$country_of_incorporation)) #14, excessive considering no interest in profiling R&D by country. Make it Aus, Overseas
data$country_of_incorporation <- ifelse(data$country_of_incorporation == "Australia", "Australia", "Overseas")
data$country_of_incorporation <- as.factor(data$country_of_incorporation)

#hq city:
length(unique(data$headquarters_city)) #303, excessive, but difficult to collapse. Will leave as is as meatadata (unlikely to be used in models)
data$headquarters_city <- as.factor(data$headquarters_city)

#hq  state:
length(unique(data$headquarters_state)) #31, collapse to 8 states/terr. and Overseas
#Note, this turns NAs to Overseas, but after reviewing they are all from overseas, so, no issue:
data$headquarters_state <- ifelse(!(data$headquarters_state %in% c("Victoria", "Western Australia", "Queensland", "New South Wales",
                                                                   "Tasmania", "South Australia", "Northern Territories", 
                                                                   "Australian Capital Territory")), "Overseas", data$headquarters_state)
data$headquarters_state <- as.factor(data$headquarters_state)

#hq country:
length(unique(data$headquarters_country)) #15, collapse to Australia, Overseas
data$headquarters_country <- ifelse(data$headquarters_country == "Australia", "Australia", "Overseas")
data$headquarters_country <- as.factor(data$headquarters_country)

#primary industry:
length(unique(data$primary_industry)) #138, difficult to callapse. Leave as is as metadata (not to be used in models)

#GICS sector:
length(unique(data$GICS_sector)) #11, better to keep intact
data$GICS_sector <- as.factor(data$GICS_sector)

#GICS ind group:
length(unique(data$GICS_industry_group)) #24, better to keep intact
data$GICS_industry_group <- as.factor(data$GICS_industry_group)

#GICS ind:
length(unique(data$GICS_industry)) #67, if collapsed will be equal to the other GICS variables. Leave as is as metadata (not to be used in models)
#---End



#---Construct continuous variables that need construction:
#ownership:
data$no_corporate_investments <- stringr::str_count(data$ownership, "Corporate Investment") # no companies in which the firm is/has invested
data$no_sponsors <- stringr::str_count(data$ownership, "Sponsor") #no organisations investing/invested in the firm
data <- data[,!(names(data) == "ownership")] # remove old ownership variable


#industry classifications:
data$industry_classifications2 <- paste0(" ", data$industry_classifications) #needed to properly compare first listed items with the rest
data$industry_classifications2 <- sapply(strsplit(data$industry_classifications2, ";"), function(x) paste(unique(x), collapse = ";")) #removed duplicated items
data$no_industries_associated <- stringr::str_count(data$industry_classifications2, ";") + 1 #no. industries listed in industry classifications field (they may be from different classif. levels)
data <- data[,!(names(data) %in% c("industry_classifications", "industry_classifications2"))] # remove old variables

#product names and descriptions:
data$no_products <- stringr::str_count(data$product_descriptions, ":") #count no. products (all of them have the name followed by ':' followed by description)
data <- data[,!(names(data) %in% c("product_names", "product_descriptions"))] # remove old variables (product_names was actually redundant with product_description, the latter had more products)

#business relationships:
data$no_business_relationships <- stringr::str_count(data$business_relationships, ";") + 1 #no. businesses for which the firm has relationships with
data <- data[,!(names(data) == "business_relationships")] #remove old variable

#competitors:
data$no_competitors <- stringr::str_count(data$competitors, ";") + 1 #no. competitors reported
data <- data[,!(names(data) == "competitors")] #remove old variable

#customers:
data$no_customers <- stringr::str_count(data$customers, ";") + 1 #no. customers reported
data <- data[,!(names(data) == "customers")] #remove old variable

#suppliers:
data$no_suppliers <- stringr::str_count(data$suppliers, ";") + 1 #no. suppliers reported
data <- data[,!(names(data) == "suppliers")] #remove old variable

#strategic alliances:
data$no_strategic_alliances <- stringr::str_count(data$strategic_alliances, ";") + 1 #no. organisations with which the firm has strat. alliances with
data <- data[,!(names(data) == "strategic_alliances")] #remove old variable

#R&D expenditure (DV):
#Values from CIQ:
data <- data %>%
  mutate(mean_3yr_RD_exp_CIQ = rowMeans(.[,c("RD_LTM2", "RD_LTM1", "RD_LTM")], na.rm = T))
data$mean_3yr_RD_exp_CIQ <- gsub("NaN", NA, data$mean_3yr_RD_exp_CIQ) #replace NaN (from calculating means with no values) by NA
data$mean_3yr_RD_exp_CIQ <- as.numeric(data$mean_3yr_RD_exp_CIQ)

#Values from DA:
data <- data %>%
  mutate(mean_3yr_RD_exp_DA = rowMeans(.[,c("RD_2018_DA", "RD_2019_DA", "RD_2020_DA")], na.rm = T))
data$mean_3yr_RD_exp_DA <- gsub("NaN", NA, data$mean_3yr_RD_exp_DA) #replace NaN (from calculating means with no values) by NA
data$mean_3yr_RD_exp_DA <- as.numeric(data$mean_3yr_RD_exp_DA)

#coalesce both (keeping CIQ as priority given amount of non-missing values):
data <- data %>%
  mutate(mean_3yr_RD_exp = dplyr::coalesce(mean_3yr_RD_exp_CIQ, mean_3yr_RD_exp_DA))

data$has_RD_exp <- ifelse(!is.na(data$mean_3yr_RD_exp), "yes", "no") #create a binary variable indicating if the firm has any R&D expenditure for the last 3 years
data <- data[,!(names(data) %in% c("mean_3yr_RD_exp", "mean_3yr_RD_exp_CIQ", "mean_3yr_RD_exp_DA", "RD_LTM2", "RD_LTM1", "RD_LTM", "RD_2018_DA", "RD_2019_DA", "RD_2020_DA"))] #remove old R&D variables
data$has_RD_exp <- as.factor(data$has_RD_exp)
#---
#===End STEP 1 ===============================================================================================================================================================================




#===STEP 2 (descriptive statistics and exploratory analysis) =================================================================================================================================
describe(data)

#assess missing values:
naniar::gg_miss_var(data[,-c(1:4)], show_pct = TRUE) #naniar::vis_miss(data)
  #Result: 13 continuous variables with missing values. Add dummies to incorporate the missingness mechanism into the models to see if they play a role, and impute with median:

data$no_competitors_na <- ifelse(is.na(data$no_competitors), 1, 0) #create dummy variable
data$no_competitors <- ifelse(is.na(data$no_competitors), median(data$no_competitors, na.rm = T), data$no_competitors) #impute with median if NA

data$no_strategic_alliances_na <- ifelse(is.na(data$no_strategic_alliances), 1, 0) #create dummy variable
data$no_strategic_alliances <- ifelse(is.na(data$no_strategic_alliances), median(data$no_strategic_alliances, na.rm = T), data$no_strategic_alliances) #impute with median if NA

data$no_employees_na <- ifelse(is.na(data$no_employees), 1, 0) #create dummy variable
data$no_employees <- ifelse(is.na(data$no_employees), median(data$no_employees, na.rm = T), data$no_employees) #impute with median if NA

data$no_customers_na <- ifelse(is.na(data$no_customers), 1, 0) #create dummy variable
data$no_customers <- ifelse(is.na(data$no_customers), median(data$no_customers, na.rm = T), data$no_customers) #impute with median if NA

data$year_founded_na <- ifelse(is.na(data$year_founded), 1, 0) #create dummy variable
data$year_founded <- ifelse(is.na(data$year_founded), median(data$year_founded, na.rm = T), data$year_founded) #impute with median if NA

data$no_suppliers_na <- ifelse(is.na(data$no_suppliers), 1, 0) #create dummy variable
data$no_suppliers <- ifelse(is.na(data$no_suppliers), median(data$no_suppliers, na.rm = T), data$no_suppliers) #impute with median if NA

data$no_geographic_segments_na <- ifelse(is.na(data$no_geographic_segments), 1, 0) #create dummy variable
data$no_geographic_segments <- ifelse(is.na(data$no_geographic_segments), median(data$no_geographic_segments, na.rm = T), data$no_geographic_segments) #impute with median if NA

data$intangible_assets_na <- ifelse(is.na(data$intangible_assets), 1, 0) #create dummy variable
data$intangible_assets <- ifelse(is.na(data$intangible_assets), median(data$intangible_assets, na.rm = T), data$intangible_assets) #impute with median if NA

data$no_business_relationships_na <- ifelse(is.na(data$no_business_relationships), 1, 0) #create dummy variable
data$no_business_relationships <- ifelse(is.na(data$no_business_relationships), median(data$no_business_relationships, na.rm = T), data$no_business_relationships) #impute with median if NA

data$no_products_na <- ifelse(is.na(data$no_products), 1, 0) #create dummy variable
data$no_products <- ifelse(is.na(data$no_products), median(data$no_products, na.rm = T), data$no_products) #impute with median if NA

data$no_business_segments_na <- ifelse(is.na(data$no_business_segments), 1, 0) #create dummy variable
data$no_business_segments <- ifelse(is.na(data$no_business_segments), median(data$no_business_segments, na.rm = T), data$no_business_segments) #impute with median if NA

data$total_revenue_na <- ifelse(is.na(data$total_revenue), 1, 0) #create dummy variable
data$total_revenue <- ifelse(is.na(data$total_revenue), median(data$total_revenue, na.rm = T), data$total_revenue) #impute with median if NA

data$pct_external_directors_na <- ifelse(is.na(data$pct_external_directors), 1, 0) #create dummy variable
data$pct_external_directors <- ifelse(is.na(data$pct_external_directors), median(data$pct_external_directors, na.rm = T), data$pct_external_directors) #impute with median if NA

#drop the 1 firm without hq city and 1 firm without primary industry, and 1 firm without GICS_industry:
data <- data[complete.cases(data),]
  #Now, the data has no missing values -> ready to be visually analysed and modelled
#===End STEP 2 ===============================================================================================================================================================================



#===STEP 3 (Visual exploration through visual analytics of key variables) ====================================================================================================================
#Individual variables vs R&D variable:
#gridExtra::grid.arrange(ggplot(data, aes(year_founded)) + geom_density() + geom_rug(),
#                        ggplot(data, aes(has_RD_exp)) + geom_bar() + coord_flip(),
#                        nrow = 4, top = "Distribution of firm features (predictors) and R&D expenditure (outcome)")

#Bivariate relationship with RD vars:
gridExtra::grid.arrange(ggplot(data, aes(x = trading_status, fill = has_RD_exp)) + geom_bar(position = "fill") + coord_flip(),
                        ggplot(data, aes(x = company_status, fill = has_RD_exp)) + geom_bar(position = "fill") + coord_flip(),
                        ggplot(data, aes(x = country_of_incorporation, fill = has_RD_exp)) + geom_bar(position = "fill") + coord_flip(),
                        ggplot(data, aes(x = headquarters_country, fill = has_RD_exp)) + geom_bar(position = "fill") + coord_flip(),
                        ggplot(data, aes(x = headquarters_state, fill = has_RD_exp)) + geom_bar(position = "fill") + coord_flip(),
                        ggplot(data, aes(x = no_geographic_segments, color = has_RD_exp, fill = has_RD_exp)) + geom_density(alpha = .15) + scale_x_continuous(trans = 'log10'),
                        ggplot(data, aes(x = GICS_sector, fill = has_RD_exp)) + geom_bar(position = "fill") + coord_flip(),
                        ggplot(data, aes(x = GICS_industry_group, fill = has_RD_exp)) + geom_bar(position = "fill") + coord_flip() + theme(axis.text.y = element_text(size = 6)),
                        ggplot(data, aes(x = year_founded, color = has_RD_exp, fill = has_RD_exp)) + geom_density(alpha = .15) + scale_x_continuous(trans = 'log10'),
                        ggplot(data, aes(x = ASX_listing_year, color = has_RD_exp, fill = has_RD_exp)) + geom_density(alpha = .15) + scale_x_continuous(trans = 'log10'),
                        ggplot(data, aes(x = no_business_segments, color = has_RD_exp, fill = has_RD_exp)) + geom_density(alpha = .15) + scale_x_continuous(trans = 'log10'),
                        ggplot(data, aes(x = pct_external_directors, color = has_RD_exp, fill = has_RD_exp)) + geom_density(alpha = .15) + scale_x_continuous(trans = 'log10'),
                        ggplot(data, aes(x = no_employees, color = has_RD_exp, fill = has_RD_exp)) + geom_density(alpha = .15) + scale_x_continuous(trans = 'log10'),
                        ggplot(data, aes(x = market_capitalisation, color = has_RD_exp, fill = has_RD_exp)) + geom_density(alpha = .15) + scale_x_continuous(trans = 'log10'),
                        ggplot(data, aes(x = total_revenue, color = has_RD_exp, fill = has_RD_exp)) + geom_density(alpha = .15) + scale_x_continuous(trans = 'log10'),
                        ggplot(data, aes(x = total_assets, color = has_RD_exp, fill = has_RD_exp)) + geom_density(alpha = .15) + scale_x_continuous(trans = 'log10'),
                        ggplot(data, aes(x = intangible_assets, color = has_RD_exp, fill = has_RD_exp)) + geom_density(alpha = .15) + scale_x_continuous(trans = 'log10'),
                        ggplot(data, aes(x = no_designs, color = has_RD_exp, fill = has_RD_exp)) + geom_density(alpha = .15) + scale_x_continuous(trans = 'log10'),
                        ggplot(data, aes(x = no_patents, color = has_RD_exp, fill = has_RD_exp)) + geom_density(alpha = .15) + scale_x_continuous(trans = 'log10'),
                        ggplot(data, aes(x = no_trademarks, color = has_RD_exp, fill = has_RD_exp)) + geom_density(alpha = .15) + scale_x_continuous(trans = 'log10'),
                        ggplot(data, aes(x = no_corporate_investments, color = has_RD_exp, fill = has_RD_exp)) + geom_density(alpha = .15) + scale_x_continuous(trans = 'log10'),
                        ggplot(data, aes(x = no_sponsors, color = has_RD_exp, fill = has_RD_exp)) + geom_density(alpha = .15) + scale_x_continuous(trans = 'log10'),
                        ggplot(data, aes(x = no_industries_associated, color = has_RD_exp, fill = has_RD_exp)) + geom_density(alpha = .15) + scale_x_continuous(trans = 'log10'),
                        ggplot(data, aes(x = no_products, color = has_RD_exp, fill = has_RD_exp)) + geom_density(alpha = .15) + scale_x_continuous(trans = 'log10'),
                        ggplot(data, aes(x = no_business_relationships, color = has_RD_exp, fill = has_RD_exp)) + geom_density(alpha = .15) + scale_x_continuous(trans = 'log10'),
                        ggplot(data, aes(x = no_competitors, color = has_RD_exp, fill = has_RD_exp)) + geom_density(alpha = .15) + scale_x_continuous(trans = 'log10'),
                        ggplot(data, aes(x = no_customers, color = has_RD_exp, fill = has_RD_exp)) + geom_density(alpha = .15) + scale_x_continuous(trans = 'log10'),
                        ggplot(data, aes(x = no_suppliers, color = has_RD_exp, fill = has_RD_exp)) + geom_density(alpha = .15) + scale_x_continuous(trans = 'log10'),
                        ggplot(data, aes(x = no_strategic_alliances, color = has_RD_exp, fill = has_RD_exp)) + geom_density(alpha = .15) + scale_x_continuous(trans = 'log10'),
                        nrow = 8, top = "Bivariate relationship between firm features (predictors) and existence of R&D expenditure (outcome)")
#===End STEP 3 ===============================================================================================================================================================================

write.csv(data, "data_step2.csv", row.names = F) #save all data into a file for further analysis

