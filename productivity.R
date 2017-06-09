
library(reshape2)
library(readxl)
library(readr)
library(dplyr)
library(ggplot2)

employment_rate <- read_csv("~/GitHub/productivity-trap/employment_rate.csv", 
                            col_types = cols(TIME = col_character()))

employment_rate$`Flag Codes` <- NULL
employment_rate$FREQUENCY <- as.factor(employment_rate$FREQUENCY)


employment_rate$MEASURE <- as.factor(employment_rate$MEASURE)



comp <- employment_rate[employment_rate$FREQUENCY=="A" & 
                          employment_rate$SUBJECT=="TOT" & 
                          employment_rate$MEASURE=="PC_WKGPOP",]

names(comp)[names(comp)=="LOCATION"] <- "Code"
names(comp)[names(comp)=="TIME"] <- "Year"
names(comp)[names(comp)=="Value"] <- "emp_rate"


productivity <- read_excel("~/GitHub/productivity-trap/productivity.xlsx")


prod_melt <- melt(productivity)

names(prod_melt)[names(prod_melt)=="variable"] <- "Year"
names(prod_melt)[names(prod_melt)=="value"] <- "prod_rate"


prod_emp <- left_join(prod_melt, comp)

prod_emp$FREQUENCY <- NULL
prod_emp$MEASURE <- NULL
prod_emp$INDICATOR <- NULL
prod_emp$SUBJECT <- NULL

extras <- list("USSR-LTU", "EU28", "G-7", "EA19", "OECD")

prod_emp2 <- prod_emp[ !(prod_emp$Code %in% extras) & 
                         is.na(prod_emp$emp_rate)==FALSE &
                         is.na(prod_emp$prod_rate)==FALSE,]

prod_emp2$Country <- as.factor(prod_emp2$Country)

prod_emp2$Code <- as.factor(prod_emp2$Code)

cor.test(prod_emp2$emp_rate, prod_emp2$prod_rate)

lm_prod_emp <- lm(emp_rate ~ prod_rate, data=prod_emp2)

#qplot(emp_rate, prod_rate, data=prod_emp2)

p1 <- ggplot(prod_emp2, aes(x=emp_rate, y=prod_rate, group = Year)) + geom_point(aes(colour = factor(Country)))

p1

summary(prod_emp2$Code)

brit <- prod_emp2[prod_emp2$Code=="GBR",]

brit2 <- melt(brit)

pbrit <- ggplot(brit2, aes(y=value, x=Year, group = variable)) + geom_line(aes(col=variable))

pbrit


prod_emp3 <- melt(prod_emp2)

prod_emp3$Year <- as.numeric(prod_emp3$Year)

prod_emp3 <- prod_emp3[prod_emp3$Year>=2000,]



grid <- ggplot(prod_emp3, aes(y=value, x=Year, group = variable)) + geom_line(aes(col=variable)) + facet_wrap(~ Country, nrow = 4)

grid
