#-------------------------------------------------
# Set working directory
#-------------------------------------------------

setwd("/Users/witold/GitHub/complaints")

#-------------------------------------------------
# Load libraries
#-------------------------------------------------

library(FFdownload)
library(timetk)

library(tidyverse)
library(tidyquant)

#-------------------------------------------------
# Construct portfolio
#-------------------------------------------------

# Stock selection
stock_list <- c(
				"EFX",
				"TRU",
				"EXPGY",
				"BAC",
				"WFC",
				"JPM",
				"C",
				"COF",
				"NAVI",
				"SYF",
				"USB",
				"AXP",
				"PRAA",
				"OCN",
				"NSM",
				"DFS",
				"ADS",
				"PNC",
				"ECPG",
				"TD",
				"PYPL",
				"AES",
				"SC",
				"ALLY",
				"TSI"
) 

# Create equal weighted portfolio & calculate returns
stock_returns_monthly <- stock_list %>%
    tq_get(get  = "stock.prices",
           from = "2017-01-01",
           to   = "2021-12-31") %>%
    group_by(symbol) %>%
    tq_transmute(select     = adjusted, 
                 mutate_fun = periodReturn, 
                 period     = "monthly", 
                 col_rename = "Ra")

# Create weights mapping in xts (bring in from complaints.R)
weights_xts <- q1_xts

# Reformat to xts for processing and reconvert back to tibble for tidyquant
interim <- spread(stock_returns_monthly,symbol,Ra)
interim <- xts(as.data.frame(interim)[,-1], order.by=as.data.frame(interim)[,1])
portfolio_returns_monthly_xts <- Return.portfolio(interim, weights=weights_xts)
portfolio_returns_monthly <- data_frame(fortify(portfolio_returns_monthly_xts))
names(portfolio_returns_monthly) <- c("date","Ra")
portfolio_returns_monthly$symbol <- "portfolio"
portfolio_returns_monthly$date <- as.yearmon(portfolio_returns_monthly$date)

#-------------------------------------------------
# Plot portfolio for sanity check
#-------------------------------------------------                 
                 
# Convert returns to equity line          
portfolio_growth_monthly <- portfolio_returns_monthly %>%
    tq_portfolio(assets_col   = symbol, 
                 returns_col  = Ra, 
                 col_rename   = "investment.growth",
                 wealth.index = TRUE) #%>% mutate(investment.growth = investment.growth * 1e9)
    
# Plot equity graph
portfolio_growth_monthly %>%
    ggplot(aes(x = date, y = investment.growth)) +
    geom_line(size = 2, color = palette_light()[[1]]) +
    labs(title = "Portfolio Growth",
    	 subtitle = "Quintile: Q1",
         caption = "Rebalancing yearly on first trading day of year",
         x = "", y = "Portfolio Value") +
    geom_smooth(method = "loess") +
    theme_tq() +
    scale_color_tq() # + scale_y_continuous(labels = scales::dollar)
    
# Plot weights
gather(fortify(weights_xts),key=symbol,weight,-Index) %>%
    filter(weight!=0) %>%
    ggplot(aes(x=Index,y=weight,fill=symbol)) + 
    geom_bar(stat="identity",col="white") + 
    theme_classic() +
    geom_text(aes(label = symbol),position = position_stack(vjust = 0.5), size = 2) +
    labs(
        title="Weights over time",
        subtitle="Quintile: Q1",
        caption="Rebalancing yearly on first trading day of year"
    ) +
    theme(legend.position="none")
    
    
#-------------------------------------------------
# Load Fama French factor models
#-------------------------------------------------   
    
tempd <- tempdir()
inputlist <- c("F-F_Research_Data_Factors","F-F_Momentum_Factor","F-F_ST_Reversal_Factor","F-F_LT_Reversal_Factor")
FFdownload(exclude_daily=TRUE,tempd=tempd,download=TRUE,download_only=TRUE,inputlist=inputlist)

tempf <- paste0(tempd,"\\FFdata.RData")
FFdownload(output_file = tempf, exclude_daily=TRUE,tempd=tempd,download=FALSE,
           download_only=FALSE,inputlist = inputlist)
    
load(file = tempf)
FFdata$`x_F-F_Research_Data_Factors`$monthly$Temp2 %>% timetk::tk_tbl(rename_index = "ym") %>%
  left_join(FFdata$`x_F-F_Momentum_Factor`$monthly$Temp2 %>% timetk::tk_tbl(rename_index = "ym"),by="ym") %>%
  left_join(FFdata$`x_F-F_LT_Reversal_Factor`$monthly$Temp2 %>% timetk::tk_tbl(rename_index = "ym"),by="ym") %>%
  left_join(FFdata$`x_F-F_ST_Reversal_Factor`$monthly$Temp2 %>% timetk::tk_tbl(rename_index = "ym"),by="ym") %>% head()
    
FF_3factor <- fortify(FFdata$`x_F-F_Research_Data_Factors`$monthly$Temp2) %>% dplyr::rename(date=Index)
FF_3factor <- as_tibble(FF_3factor)

#-------------------------------------------------
# Run 3 Factor Model
#-------------------------------------------------   

# Merge porfolio & FFdata
rtns <- portfolio_returns_monthly %>% 
					left_join(FF_3factor) %>% 
					select(-symbol) %>% 
					dplyr::rename(portfolio=Ra) %>%
					mutate(
						#portfolio=portfolio*100
						Mkt.RF = Mkt.RF/100,
						SMB = SMB/100,
						HML = HML/100,
						RF = RF/100
						)

# Calculate excess rtns
ffregression <- lm((portfolio-RF) ~ Mkt.RF + SMB + HML, data=rtns)

# Print results
print(summary(ffregression))