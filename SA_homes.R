library(tidyverse)
library(forecastML)
setwd("G:/My Drive/Folder Share/_Projects/__R/01_Personal R Projects/01_SA homes/p2-housing")

######## User Defined Function #1 ###########
# getPayment() function to calculate payment
# requires salePrice (in dollars)
# requires irate (interest rate between 0 and 100)
# optional term_y for term in years

getPayment <- function(salePrice, irate, term_y = 30){
  mrate = irate/1200
  n = term_y*12
  answer <- salePrice*mrate*((1+mrate)^n)/(((1+mrate)^(n))-1)
  answer
}


# Original median home sale price at 100% is $143,000
mhsp_0 <- 143

#create df from txt file
readfile <- read.table("tabData.txt", header=TRUE, sep="\t")

# Remove $ from the Median home prices
readfile$Median.Sale.Price <- gsub("\\$","",as.character(readfile$Median.Sale.Price))

# Remove K from the Median home prices
readfile$Median.Sale.Price <- as.integer(gsub("K","",as.character(readfile$Median.Sale.Price)))

# Added an index to have a numeric x-axis. Need to change it to a date later
#Probably not needed anymore
#readfile$index <- c(1:nrow(readfile))

#ggplot(data = readfile, mapping = aes(x = index, y = Median.Sale.Price)) + geom_line()

# Load 2nd dataframe
readmortgage  <- read.csv("MORTGAGE30US.csv")

#Make a DATE Column on df#1
readfile$DATE <- as.Date(readfile$Month.of.Period.End, format="%m/%d/%Y")

#Make a backup.date column on df#2
#May not be needed anymore
##readmortgage$backup.date <- readmortgage$DATE
  
#Convert string into date datatype for the date column on df #2
readmortgage$DATE <- as.Date(readmortgage$DATE)


#New approach:
# http://www.perceptualedge.com/articles/visual_business_intelligence/dual-scaled_axes.pdf
# Use a relative amount from the start

#### We need to fill the gaps, and interpolate
#Small df#1
readfile_small <- readfile[,c('DATE','Median.Sale.Price')]

# Make gaps, one day apart instead of monthly.
readfile_small <- fill_gaps(readfile_small, date_col = 1, frequency = '1 day')

# NEXT is to fill the gaps with actual interpolation with 
# Fill in the data gaps with linear interpolation
small_noGaps <- as.data.frame(approx(readfile_small$Median.Sale.Price, n = length(readfile_small[,2])))

# Replace the index column with DATE from df#3 (which is smaller than df#1)
small_noGaps$x <- readfile_small$DATE

#Change some column names
colnames(small_noGaps) <- c('DATE','Median.Home.Price')

####### Process 2nd dataframe #######

#Lengthen df#2 to include all days, filled with NA
mortgage_NA<-fill_gaps(readmortgage, date_col = 1, frequency = '1 day')

#Replace NAs with interpolated values
mortgage_interp <- as.data.frame(approx(mortgage_NA$MORTGAGE30US, n = length(mortgage_NA[,2])))

#Give the new df the same column names
colnames(mortgage_interp) <- colnames(mortgage_NA)

#Recover the date values that were lost during the interpolation
mortgage_interp$DATE <- mortgage_NA$DATE

# Merge the two dataframes
df = merge(small_noGaps, mortgage_interp, by="DATE", all.x=TRUE, all.y=TRUE)

# fill back the mortgage rate column for 'df' for the missing dates, "backward fill"
## NOTE THAT MORTGAGE for rows 1:85 is BACKWARD FILL
df[1:85,'MORTGAGE30US'] <- df[86,'MORTGAGE30US']

# fill forward the relative median sale price for 'df' for missing dates, "forward fill"
## NOTE THAT relative median sale price for rows 3655:3733 is FORWARD FILL
df[3655:3733,'Median.Home.Price'] <- df[3654,'Median.Home.Price']

#custom formula here
#add a column for monthly pmt
df$pmt <- getPayment(df$Median.Home.Price*1000,df$MORTGAGE30US)

#add a column for relative monthly pmt
df$Rpmt <- df$pmt / df$pmt[1]

#add a column for relative median home price
df$RelSalePrice <- df$Median.Home.Price / df$Median.Home.Price[1]

#Relative 30 yr interest rate
df$Rel30Rate <- df$MORTGAGE30US / df$MORTGAGE30US[1]


# Values used below
title0 <- "House Sale Price, Interest Rate, Mortgage payment - San Antonio, TX"
lab_0<- 'Initial: $143k; 3.88%; $672/mo'
lab_f <- 'Final: $272k; 5.11%; $1,478/mo'

#Graph 3 lines
ggplot(data = df, aes(x = DATE)) + 
  geom_line(data = df, aes(y = RelSalePrice, color = "Sale Price"), size=1.05) + 
  geom_line(data = df, aes(y = Rpmt, color = "Payment"), size=1.05) + 
  geom_line(data = df, aes(y = Rel30Rate, color = "Interest Rate"), size=1.05) +
  
  # Label #1
  geom_text(data=subset(df, (Rpmt==1) & (RelSalePrice==1)),
            aes(DATE,Rpmt,label=as.character(lab_0)
                ),
            hjust=0,
            vjust=6.3
            ) +
  # Label #2
  geom_text(data=subset(df, (Median.Home.Price==272) & (MORTGAGE30US==5.11)),
            aes(DATE,Rpmt,label=as.character(lab_f)
            ),
            hjust=1.05,
            vjust=0.5
  ) +
  # Some labels
  labs(title=title0,
       x="Date",
       y="Relative Magnitude",
       color="Values\n") +
  # Legend colors
  scale_color_manual(values=c("Interest Rate"="black", 
                              "Payment"="blue", 
                              "Sale Price"="red")) +
  theme(
    legend.position="bottom"
  )

#Clean up variables
rm(mortgage_interp)
rm(mortgage_NA)
rm(readfile)
rm(readfile_small)
rm(readmortgage)
rm(small_noGaps)
#detach("package:tidyverse", unload=TRUE)
#detach("package:ggplot2", unload=TRUE)
#detach("package:tibble", unload=TRUE)
#detach("package:tidyr", unload=TRUE)
#detach("package:readr", unload=TRUE)
#detach("package:purrr", unload=TRUE)
#detach("package:dplyr", unload=TRUE)
#detach("package:stringr", unload=TRUE)
#detach("package:forcats", unload=TRUE)
