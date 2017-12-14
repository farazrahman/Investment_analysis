# Project: Sparks Funds Investment Case Study
# Authors: Faraz Rahman

# Install necessary R packages

library(tidyr)
library(dplyr)
library(stringr)
library(sqldf)

# Source Datasets

# companies.txt - Data with unique records at company level
# rounds2.csv - Data with unique records at funding level. A company may have multiple records
# mapping.csv - Maps primary sector to main sector

# Checkpoint 1: Data Preparation

# Read the source files 

companies <- read.delim("companies.txt", header = T, sep= '\t', stringsAsFactors = F, fill = TRUE, quote = "\"", comment.char = "", na.strings = c("","NA"))
rounds2 <- read.csv("rounds2.csv",header = TRUE, stringsAsFactors = FALSE, na.strings = c("","NA"))

sector_mapping <- read.csv("mapping.csv",stringsAsFactors = FALSE, check.names= TRUE, na.strings = c("","NA"))

# Prepare companies and rounds2 data to be merged
rounds2$company_permalink <- sapply(rounds2$company_permalink, tolower)
companies$permalink <- sapply(companies$permalink, tolower)

# Get unique list of companies from rounds2 dataframe
funding_distinct_list <- count(distinct(rounds2, company_permalink))

# Get unique list of companies from companies dataframe
company_distinct_list <- count(distinct(companies, permalink))

# Merge companies and rounds2 dataframes by their unique column permalink
master_frame <- merge(rounds2, companies, by.x = "company_permalink", by.y = "permalink", all = T)

# Checkpoint 2: Analysis by Funding Type

# Filter master dataframe for only four funding types

master_subset <- subset(master_frame, funding_round_type %in% c('angel','venture','seed','private_equity'))

# Find average funding amount by funding type for the funding categories - angel, venture, seed and private_equity

avg_funding_by_type <- setNames(summarise(group_by(master_subset, funding_round_type), mean(raised_amount_usd, na.rm = TRUE)), c('Funding_Round_Type', 'Average_Funding_Amount'))
selected_funding_type <-as.character(subset(avg_funding_by_type, between(Average_Funding_Amount, 5000000, 15000000))[1,1])

# Venture funding type is ideal for the company among the four venture types chosen above as Spark Funds wants to invest between 5 million - 15 million 

# Checkpoint 3: Analysis by Country 

# Identify top nine countries which have received the highest total funding 
# (across ALL sectors for the chosen investment type which is venture type)

# Filter the master frame based on venture funding type
venture_funding <- filter(master_frame, funding_round_type == "venture" & !(country_code == ""))

# Group the countries by country code and summarise the total raised amount
funding_by_ctry <- arrange(summarise(group_by(venture_funding, country_code), total_funding_amt=sum(raised_amount_usd, na.rm = TRUE)), desc(total_funding_amt))
top9 <- head(funding_by_ctry[complete.cases(funding_by_ctry),],9)

# Checkpoing 4: Analysis by Sector

# Prepare the master_frame and mapping data to be merged
master_frame$category_list <- sapply(master_frame$category_list, tolower)
sector_mapping$category_list <- sapply(sector_mapping$category_list, tolower)
master_frame <- mutate(master_frame, primary_sector = word(master_frame$category_list,1,sep = "\\|"))
names(sector_mapping) <- c("category_list", "Automotive_and_Sports", "Blanks", "Cleantech_and_Semiconductors", "Entertainment", "Health","Manufacturing","News_Search_and_Messaging", "Others", "Social_Finance_Analytics_Advertising")

# Gather the mapping data from wide to long format and remove the rows with 0 value
sector_mapping <- gather(sector_mapping, key = "Main_Sector", value = "value", Automotive_and_Sports:Social_Finance_Analytics_Advertising)
sector_mapping <- subset(sector_mapping, value==1, select=(-value))

# Correct the spellings of categories where na is replaced by 0
sector_mapping$category_list <- str_replace_all(sector_mapping$category_list, "([0])", "na")

# Merge master data frame with sector mapping in order to map all the primary sectors with their 8 main categories
sector_merge <- merge(master_frame, sector_mapping, by.x = "primary_sector", by.y = "category_list", all.x = TRUE)

# Checkpoint 5: Analysis by Sector - Part 2

# Create separate dataframes for Top 3 English Speaking Countries for venture funding type and funding range between 5 - 15 million

master_subset2 <- na.omit(subset(sector_merge, funding_round_type == selected_funding_type & between(raised_amount_usd, 5000000, 15000000), select = c(country_code, name, Main_Sector, funding_round_type, raised_amount_usd)))

D1_country <- subset(master_subset2, country_code =="USA")
D2_country <- subset(master_subset2, country_code =="GBR")
D3_country <- subset(master_subset2, country_code =="IND")

# Group the 8 main categories and summarise to find the total number of investements and total amount of investments in each category

US_agg_by_sector <- sqldf("select Main_Sector, count(*) `num_of_investments`, sum(raised_amount_usd) `Amount_of_investments` from D1_country group by Main_Sector")
UK_agg_by_sector <- sqldf("select Main_Sector, count(*) `num_of_investments`, sum(raised_amount_usd) `Amount_of_investments` from D2_country group by Main_Sector")
IND_agg_by_sector <- sqldf("select Main_Sector, count(*) `num_of_investments`, sum(raised_amount_usd) `Amount_of_investments` from D3_country group by Main_Sector")

# Remove the blank sectors

US_agg_by_sector <- subset(US_agg_by_sector, Main_Sector != 'Blanks')
UK_agg_by_sector <- subset(UK_agg_by_sector, Main_Sector != 'Blanks')
IND_agg_by_sector <- subset(IND_agg_by_sector, Main_Sector != 'Blanks')

# Calculate total number of investments for Top 3 countries

total_num_inv_US <- sum(US_agg_by_sector$num_of_investments)
total_num_inv_UK <- sum(UK_agg_by_sector$num_of_investments)
total_num_inv_IND <- sum(IND_agg_by_sector$num_of_investments)

# Calculate total amount of investments for Top 3 countries

total_amt_inv_US <- sum(US_agg_by_sector$Amount_of_investments)
total_amt_inv_UK <- sum(UK_agg_by_sector$Amount_of_investments)
total_amt_inv_IND <- sum(IND_agg_by_sector$Amount_of_investments)

# Top 3 sectors in Top 3 countries

top1_sector_US <- as.character(arrange(US_agg_by_sector, desc(num_of_investments))[1,1])
top2_sector_US <- as.character(arrange(US_agg_by_sector, desc(num_of_investments))[2,1])
top3_sector_US <- as.character(arrange(US_agg_by_sector, desc(num_of_investments))[3,1])

top1_sector_UK <- as.character(arrange(UK_agg_by_sector, desc(num_of_investments))[1,1])
top2_sector_UK <- as.character(arrange(UK_agg_by_sector, desc(num_of_investments))[2,1])
top3_sector_UK <- as.character(arrange(UK_agg_by_sector, desc(num_of_investments))[3,1])

top1_sector_IND <- as.character(arrange(IND_agg_by_sector, desc(num_of_investments))[1,1])
top2_sector_IND <- as.character(arrange(IND_agg_by_sector, desc(num_of_investments))[2,1])
top3_sector_IND <- as.character(arrange(IND_agg_by_sector, desc(num_of_investments))[3,1])

# Number of investments in Top 3 sectors for Top 3 countries

top1_sector_US_inv_cnt <-  as.character(arrange(US_agg_by_sector, desc(num_of_investments))[1,2])
top2_sector_US_inv_cnt <-  as.character(arrange(US_agg_by_sector, desc(num_of_investments))[2,2])
top3_sector_US_inv_cnt <-  as.character(arrange(US_agg_by_sector, desc(num_of_investments))[3,2])

top1_sector_UK_inv_cnt <-  as.character(arrange(UK_agg_by_sector, desc(num_of_investments))[1,2])
top2_sector_UK_inv_cnt <-  as.character(arrange(UK_agg_by_sector, desc(num_of_investments))[2,2])
top3_sector_UK_inv_cnt <-  as.character(arrange(UK_agg_by_sector, desc(num_of_investments))[3,2])

top1_sector_IND_inv_cnt <-  as.character(arrange(IND_agg_by_sector, desc(num_of_investments))[1,2])
top2_sector_IND_inv_cnt <-  as.character(arrange(IND_agg_by_sector, desc(num_of_investments))[2,2])
top3_sector_IND_inv_cnt <-  as.character(arrange(IND_agg_by_sector, desc(num_of_investments))[3,2])

# Top companies receiving the highest investment in the Top 2 sectors for Top 3 countries

top1_sector_us_company <- subset(D1_country, Main_Sector==top1_sector_US, select=c(name, raised_amount_usd))
top_company_us_sec1 <- arrange(aggregate(raised_amount_usd ~ name, data=top1_sector_us_company, sum), desc(raised_amount_usd))[1,1]

top2_sector_us_company <- subset(D1_country, Main_Sector==top2_sector_US, select=c(name, raised_amount_usd))
top_company_us_sec2 <- arrange(aggregate(raised_amount_usd ~ name, data=top2_sector_us_company, sum), desc(raised_amount_usd))[1,1]

top1_sector_uk_company <- subset(D2_country, Main_Sector==top1_sector_UK, select=c(name, raised_amount_usd))
top_company_uk_sec1 <- arrange(aggregate(raised_amount_usd ~ name, data=top1_sector_uk_company, sum), desc(raised_amount_usd))[1,1]

top2_sector_uk_company <- subset(D2_country, Main_Sector==top2_sector_UK, select=c(name, raised_amount_usd))
top_company_uk_sec2 <- arrange(aggregate(raised_amount_usd ~ name, data=top2_sector_uk_company, sum), desc(raised_amount_usd))[1,1]

top1_sector_ind_company <- subset(D3_country, Main_Sector==top1_sector_IND, select=c(name, raised_amount_usd))
top_company_ind_sec1 <- arrange(aggregate(raised_amount_usd ~ name, data=top1_sector_ind_company, sum), desc(raised_amount_usd))[1,1]

top2_sector_ind_company <- subset(D3_country, Main_Sector==top2_sector_IND, select=c(name, raised_amount_usd))
top_company_ind_sec2 <- arrange(aggregate(raised_amount_usd ~ name, data=top2_sector_ind_company, sum), desc(raised_amount_usd))[1,1]

# Checkpoint 6: Plots in Tableeau

# Convert R dataframes to csv files for creating plots in Tableau

write.csv(avg_funding_by_type, "funding_amount_types.csv", row.names = F)

write.csv(top9, "top9_Counties_for_funding.csv", row.names = F)

temp1 <- mutate(US_agg_by_sector, country_code='USA')
temp2 <- mutate(UK_agg_by_sector, country_code='GBR')
temp3 <- mutate(IND_agg_by_sector, country_code='IND')

funding_by_ctry_sector <- rbind(temp1, temp2, temp3)

write.csv(funding_by_ctry_sector, "funding_by_ctry_sector.csv", row.names = F)




