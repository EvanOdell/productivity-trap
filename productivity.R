
pacman::p_load(reshape2)
pacman::p_load(readxl)
pacman::p_load(readr)
pacman::p_load(dplyr)
pacman::p_load(ggplot2)
pacman::p_load(plm)

employment_rate <- read_csv("employment_rate.csv", col_types = cols(TIME = col_character()))

employment_rate$`Flag Codes` <- NULL
employment_rate$FREQUENCY <- as.factor(employment_rate$FREQUENCY)

employment_rate$MEASURE <- as.factor(employment_rate$MEASURE)

comp <- employment_rate[employment_rate$FREQUENCY=="A" & 
                          employment_rate$SUBJECT=="TOT" & 
                          employment_rate$MEASURE=="PC_WKGPOP",]

names(comp)[names(comp)=="LOCATION"] <- "Code"
names(comp)[names(comp)=="TIME"] <- "Year"
names(comp)[names(comp)=="Value"] <- "emp_rate"

productivity <- read_excel("productivity.xlsx")

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

prod_emp_sub <- prod_emp2[as.numeric(as.character(prod_emp2$Year)) >= 2005,]

prod_emp_sub$Year <- as.numeric(as.character(prod_emp_sub$Year))

m <- lm(prod_rate ~ emp_rate, prod_emp_sub)

u <- lm(prod_rate ~ emp_rate, prod_emp_sub[prod_emp_sub$Country=="United Kingdom",])

format(summary(m)$r.squared, digits = 3)

format(summary(u)$r.squared, digits = 3)

p1 <- ggplot(prod_emp_sub, aes(x=emp_rate, y=prod_rate, group = Year)) +
  geom_smooth(aes(x=emp_rate, y=prod_rate, group = NULL), method = "lm", alpha=0.7, size=1, se = FALSE) +
  geom_line(stat="smooth", data=prod_emp_sub[prod_emp_sub$Country=="United Kingdom",],
              aes(x=emp_rate, y=prod_rate, group = NULL), method = "lm", colour="red", alpha=0.7, size=1, se=FALSE) +
  geom_point(data=prod_emp_sub[prod_emp_sub$Country=="United Kingdom",],
             aes(x=emp_rate, y=prod_rate), colour="black", size=3) +
  geom_point(aes(colour = factor(Country)), alpha=0.75) +
  ylab("Productivity in USD\n (constant prices, 2010 PPPs)") + 
  xlab("Employment Rate (Percent)") +
  labs(colour="Country") +
  ggtitle("Figure 1: Employment Rate vs Productivity in the OECD, 2005 – 2016")

p1

ggsave(p1, filename="prod_trend.png", type = "cairo-png", width = 30, height = 18, units = "cm")

prod_emp3 <- melt(prod_emp2)

prod_emp3$Year <- as.numeric(prod_emp3$Year)

prod_emp3 <- prod_emp3[prod_emp3$Year>=2005,]

prod_emp3$variable <- factor(prod_emp3$variable, levels=c("prod_rate", "emp_rate"), labels=c("Productivity (USD, constant prices, 2010 PPPs)", "Employment Rate (Percent)"))

prod_grid <- ggplot(prod_emp3, aes(y=value, x=Year, group = variable)) +
  geom_line(aes(col=variable)) +
  facet_wrap(~ Country, nrow = 6) +
  scale_x_continuous(breaks=c(2005,2010,2015)) + 
  theme(legend.position = "bottom") + 
  ylab("Productivity (USD)/Employment (Percent)") + 
  xlab("Year") +
  labs(colour="Measurements") +
  ggtitle("Figure 2: Employment Rate & Productivity Trends in the OECD, 2005 – 2016")

prod_grid

ggsave(plot=prod_grid, filename="prod_grid.png", type = "cairo-png", width = 30, height = 18, units = "cm")


plm_fixed <- plm(prod_rate ~ emp_rate, data = prod_emp_sub, index= "Country", effect = "twoways", model="within")

plm_pooled <- plm(prod_rate ~ emp_rate, data = prod_emp_sub, index= "Country", effect = "twoways", model="pooling")

summary(plm_fixed)

summary(plm_pooled)

plm_table <- stargazer(plm_fixed, plm_pooled, type="text", star.char = c("*", "**", "***"), star.cutoffs = c(0.05, 0.01, 0.001))



