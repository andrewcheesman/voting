# Looking at count voting history 
# Voting data from https://github.com/tonmcg/County_Level_Election_Results_12-16
# Population data from US Census https://www.census.gov/support/USACdataDownloads.html

library(ggplot2)

vot <- read.csv("cty_data.csv", stringsAsFactors = F)

colnames(vot) <- c("fips_code", "county", 
                   "t2008", "d2008", "r2008", "o2008",
                   "t2012", "d2012", "r2012", "o2012",
                   "t2016", "d2016", "r2016", "o2016")

pop <- read.csv("cty_pop.csv", stringsAsFactors = F)

pop1 <- pop[,c("Area_name", "STCOU", "POP010130D", "POP010140D", "POP010150D", "POP010160D", "POP010170D", "POP010180D", "POP010190D", "POP010200D", "POP010210D")]
colnames(pop1) <- c("county", "fips_code", "p1930",  "p1940",  "p1950",  "p1960",  "p1970",  "p1980",  "p1990",  "p2000",  "p2010")

pop2 <- pop1[!(pop1$fips_code %in% c(0, 1000, 2000, 4000, 5000, 6000, 8000, 9000, 
                                     10000, 11000, 12000, 13000, 15000, 16000, 17000, 18000, 19000, 
                                     20000, 21000, 22000, 23000, 24000, 25000, 26000, 27000, 28000, 29000, 
                                     30000, 31000, 32000, 33000, 34000, 35000, 36000, 37000, 38000, 39000, 
                                     40000, 41000, 42000, 44000, 45000, 46000, 47000, 48000, 49000, 
                                     50000, 51000, 53000, 54000, 55000, 56000, 60000, 66000, 72000, 78000)),]

vot2 <- vot[!(vot$fips_code %in% c(0, 1000, 2000, 4000, 5000, 6000, 8000, 9000, 
                                   10000, 11000, 12000, 13000, 15000, 16000, 17000, 18000, 19000, 
                                   20000, 21000, 22000, 23000, 24000, 25000, 26000, 27000, 28000, 29000, 
                                   30000, 31000, 32000, 33000, 34000, 35000, 36000, 37000, 38000, 39000, 
                                   40000, 41000, 42000, 44000, 45000, 46000, 47000, 48000, 49000, 
                                   50000, 51000, 53000, 54000, 55000, 56000, 60000, 66000, 72000, 78000)),]

bas <- merge(pop2, vot2[,c(-2)], by = "fips_code")
rm(pop, pop1, pop2, vot, vot2)

bas$pd08 <- bas$d2008 / bas$p2010
bas$pr08 <- bas$r2008 / bas$p2010
bas$po08 <- bas$o2008 / bas$p2010
bas$pt08 <- bas$t2008 / bas$p2010

bas$pd12 <- bas$d2012 / bas$p2010
bas$pr12 <- bas$r2012 / bas$p2010
bas$po12 <- bas$o2012 / bas$p2010
bas$pt12 <- bas$t2012 / bas$p2010

bas$pd16 <- bas$d2016 / bas$p2010
bas$pr16 <- bas$r2016 / bas$p2010
bas$po16 <- bas$o2016 / bas$p2010
bas$pt16 <- bas$t2016 / bas$p2010

bas$mg08 <- bas$pd08 - bas$pr08
bas$mg12 <- bas$pd12 - bas$pr12
bas$mg16 <- bas$pd16 - bas$pr16

bas$tn12 <- bas$pt12 - bas$pt08
bas$tn16 <- bas$pt16 - bas$pt12






























































