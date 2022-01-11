#-------------------------------------------------
# Set working directory
#-------------------------------------------------

setwd("/Users/witold/GitHub/complaints")

#-------------------------------------------------
# Load libraries
#-------------------------------------------------

library(tidyverse)
library(tidyquant)
library(lubridate)

library(stringr)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)

#-------------------------------------------------
# Load data
#-------------------------------------------------

# Load data
df <- read_csv("data/complaints.csv")
				
# List selected from those companies with most complaints
bank_list <- c(
				"EQUIFAX, INC.", # NYSE: EFX
				"TRANSUNION INTERMEDIATE HOLDINGS, INC.", # TRU
				"Experian Information Solutions Inc.", # EXPGY
				"BANK OF AMERICA, NATIONAL ASSOCIATION", # NYSE: BAC
				"WELLS FARGO & COMPANY", # NYSE: WFC
				"JPMORGAN CHASE & CO.", # NYSE: JPM
				"CITIBANK, N.A.", # NYSE: C
				"CAPITAL ONE FINANCIAL CORPORATION", # NYSE: COF
				"Navient Solutions, LLC.", # NASDAQ: NAVI
				"SYNCHRONY FINANCIAL", # NYSE: SYF
				"U.S. BANCORP", # NYSE: USB
				"AMERICAN EXPRESS COMPANY", # AXP
				"PORTFOLIO RECOVERY ASSOCIATES INC", # PRAA
				"Ocwen Financial Corporation", # NYSE: OCN
				"NATIONSTAR MORTGAGE", # NSM
				"DISCOVER BANK", # DFS
				"Alliance Data Card Services", # ADS
				"PNC Bank N.A.", # NYSE: PNC
				"ENCORE CAPITAL GROUP INC.", # ECPG
				"TD BANK US HOLDING COMPANY", # NYSE: TD
				"Paypal Holdings, Inc", # PYPL
				"AES/PHEAA", # AES
				"Santander Consumer USA Holdings Inc.", # SC
				"ALLY FINANCIAL INC.", #ALLY
				"TRANSWORLD SYSTEMS INC" # TSI
				#"Ditech Financial LLC", # delisted, bankrupt in 2021
				)

# Subset df by bank list
df <- df %>% filter(Company %in% bank_list)

# Rename using tickers
df <- df %>% mutate(Company=recode(Company,
				"EQUIFAX, INC."="EFX",
				"TRANSUNION INTERMEDIATE HOLDINGS, INC."="TRU",
				"Experian Information Solutions Inc."="EXPGY",
				"BANK OF AMERICA, NATIONAL ASSOCIATION"="BAC",
				"WELLS FARGO & COMPANY"="WFC",
				"JPMORGAN CHASE & CO."="JPM",
				"CITIBANK, N.A."="C",
				"CAPITAL ONE FINANCIAL CORPORATION"="COF",
				"Navient Solutions, LLC."="NAVI",
				"SYNCHRONY FINANCIAL"="SYF",
				"U.S. BANCORP"="USB",
				"AMERICAN EXPRESS COMPANY"="AXP",
				"PORTFOLIO RECOVERY ASSOCIATES INC"="PRAA",
				"Ocwen Financial Corporation"="OCN",
				"NATIONSTAR MORTGAGE"="NSM",
				"DISCOVER BANK"="DFS",
				"Alliance Data Card Services"="ADS",
				"PNC Bank N.A."="PNC",
				"ENCORE CAPITAL GROUP INC."="ECPG",
				"TD BANK US HOLDING COMPANY"="TD",
				"Paypal Holdings, Inc"="PYPL",
				"AES/PHEAA"="AES",
				"Santander Consumer USA Holdings Inc."="SC",
				"ALLY FINANCIAL INC."="ALLY",
				"TRANSWORLD SYSTEMS INC"="TSI"
				))


#-------------------------------------------------
# Textual analysis
#-------------------------------------------------

# Aggregate complaint text by yearmon
df2 <- df %>%
	filter(`Date received`>=as.Date("2015-01-01")) %>%
    select(`Date received`,Company,`Consumer complaint narrative`,Product,`Sub-product`,Issue,`Sub-issue`,`Complaint ID`) %>%
    na.omit() %>%
    mutate(year=year(`Date received`)) %>%
    group_by(Company,year) %>%
    summarize(complaint = str_c(`Consumer complaint narrative`, collapse = ". "))

# Convert df to corpus
corp_complaints <- corpus(df2, text_field = "complaint")

# Rename doc names in corpus
docid <- paste(df2$Company,df2$year, sep = " - ")
docnames(corp_complaints) <- docid

# Make tokens
toks_complaints <- tokens(corp_complaints, remove_punct = TRUE)

# Remove stop words
toks_complaints <- tokens_select(toks_complaints, pattern = stopwords("en"), selection = "remove")

# Convert to DFM format
dfmat_complaints <- dfm(toks_complaints)

# Stem words
dfmat_complaints <- dfm_wordstem(dfmat_complaints)

# Run tf-idf weightings
dfmat_complaints_tfidf <- dfm_tfidf(dfmat_complaints)

# Calculate cosine similarity
tstat_simil <- textstat_simil(dfmat_complaints, method = "cosine")

# Convert to tibble
tstat_simil_df <- as_tibble(tstat_simil)

# Add descriptive tags
tstat_simil_df <- tstat_simil_df %>%
    mutate(
        year1=as.numeric(str_sub(document1,-4,-1)),
        year2=as.numeric(str_sub(document2,-4,-1)),
        company1=sub(" .*", "", document1),
        company2=sub(" .*", "", document2)
    )

# Save to file
write_csv(tstat_simil_df,"data/tstat_simil.csv")

#-------------------------------------------------
# Create variables from textual analysis
#-------------------------------------------------

# Create var1 (time-series of changes since 2015)
simil_var1 <- tstat_simil_df %>% 
	filter(year1==2015,company1==company2)
	
# Plot for sanity check
simil_var1 %>% 
	ggplot(aes(x=year2,y=cosine)) + 
		geom_line() + theme_classic() + 
		facet_wrap(~company1)

# Create var2 (YoY changes)
simil_var2 <- tstat_simil_df %>% 
	filter(year1==(year2-1),company1==company2) %>%
	group_by(year1,year2) %>% 
	mutate(
		rank = dense_rank(desc(cosine)),
		quantile = ntile(desc(cosine),5),
		date=make_date(year1+2,1,1)
		) 

# Plot for sanity check
simil_var2 %>% 
    ggplot(aes(x=year2,y=cosine)) + 
    geom_point() + 
    theme_classic() + 
    facet_wrap(~company1) +
    theme(axis.text.x = element_text(angle = 35, hjust=1)) +
    labs(title="Cosine Similarity of Complaints YoY",x="")
		
# Plot for sanity check
simil_var2 %>% 
	ggplot(aes(x=year2,y=rank)) + 
		geom_point() + 
		theme_classic() + 
		facet_wrap(~company1)

# Convert dataframes to xts for processing in portfolio weights
interim <- simil_var2 %>%
    ungroup() %>%
    select(date,company1,quantile) %>%
    filter(date<=as.Date("2021-12-31"))
   
q1 <- interim %>% filter(quantile==1) %>% group_by(date) %>% mutate(quantile = quantile/sum(quantile)) %>% spread(company1,quantile) %>% replace(is.na(.), 0)
q1_xts <- xts(as.data.frame(q1)[,-1], order.by=as.data.frame(q1)[,1])

q2 <- interim %>% filter(quantile==2) %>% group_by(date) %>% mutate(quantile = quantile/sum(quantile)) %>% spread(company1,quantile) %>% replace(is.na(.), 0)
q2_xts <- xts(as.data.frame(q2)[,-1], order.by=as.data.frame(q2)[,1])

q3 <- interim %>% filter(quantile==3) %>% group_by(date) %>% mutate(quantile = quantile/sum(quantile)) %>% spread(company1,quantile) %>% replace(is.na(.), 0)
q3_xts <- xts(as.data.frame(q3)[,-1], order.by=as.data.frame(q3)[,1])

q4 <- interim %>% filter(quantile==4) %>% group_by(date) %>% mutate(quantile = quantile/sum(quantile)) %>% spread(company1,quantile) %>% replace(is.na(.), 0)
q4_xts <- xts(as.data.frame(q4)[,-1], order.by=as.data.frame(q4)[,1])

q5 <- interim %>% filter(quantile==5) %>% group_by(date) %>% mutate(quantile = quantile/sum(quantile)) %>% spread(company1,quantile) %>% replace(is.na(.), 0)
q5_xts <- xts(as.data.frame(q5)[,-1], order.by=as.data.frame(q5)[,1])

q5_q1 <- q1 %>% 
    gather(symbol,weight,-date) %>% 
    filter(weight!=0) %>% 
    mutate(weight=weight*-1*1/3) %>% 
    rbind(q5 %>% gather(symbol,weight,-date) %>% filter(weight!=0) %>% mutate(weight=weight*1.33)) %>%
    group_by(date) %>% 
    spread(symbol,weight) %>% 
    replace(is.na(.), 0)
q5_q1_xts <- xts(as.data.frame(q5_q1)[,-1], order.by=as.data.frame(q5_q1)[,1])   

    
q1_q5 <- q5 %>% 
    gather(symbol,weight,-date) %>% 
    filter(weight!=0) %>% 
    mutate(weight=weight*-1*1/3) %>% 
    rbind(q1 %>% gather(symbol,weight,-date) %>% filter(weight!=0) %>% mutate(weight=weight*1.33)) %>%
    group_by(date) %>% 
    spread(symbol,weight) %>% 
    replace(is.na(.), 0)
q1_q5_xts <- xts(as.data.frame(q1_q5)[,-1], order.by=as.data.frame(q1_q5)[,1])  