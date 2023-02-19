library(readr)
library(readxl)
library(tidyverse)
library(lubridate)
library(ggplot2)

#############################
#Real Estate data analysis
#############################
#Real Estate Data from 2001 to 2020 with twon data
Real_Estate_Sales_2001_2020_GL <- read_csv("Real_Estate_Sales_2001-2020_GL.csv", 
                                           col_types = cols(`Date Recorded` = col_date(format = "%m/%d/%Y")))
#Real_Estate_Sales_2001_2020_GL$Town <- as.factor(Real_Estate_Sales_2001_2020_GL$Town)

Real_Estate_Sales_new <- Real_Estate_Sales_2001_2020_GL %>% drop_na(Town)

CT_County_Town <- read_excel("CT_County_Town.xlsx")

Tolland <- CT_County_Town %>% filter(County == "Tolland") %>% select("Town name")
New_Haven <- CT_County_Town %>% filter(County == "New Haven") %>% select("Town name")
Windham <- CT_County_Town %>% filter(County == "Windham") %>% select("Town name")
Fairfield <- CT_County_Town %>% filter(County == "Fairfield") %>% select("Town name")
Hartford <- CT_County_Town %>% filter(County == "Hartford") %>% select("Town name")
Litchfield <- CT_County_Town %>% filter(County == "Litchfield") %>% select("Town name")
Middlesex <- CT_County_Town %>% filter(County == "Middlesex") %>% select("Town name")
New_London <- CT_County_Town %>% filter(County == "New London") %>% select("Town name")


Real.Estate.Tolland <- Real_Estate_Sales_new %>% filter(Town %in% Tolland$`Town name`) %>% mutate(County = "Tolland")
Real.Estate.New.Haven <- Real_Estate_Sales_new %>% filter(Town %in% New_Haven$`Town name`) %>% mutate(County = "New Haven")
Real.Estate.Windham <- Real_Estate_Sales_new %>% filter(Town %in% Windham$`Town name`) %>% mutate(County = "Windham")
Real.Estate.Fairfield <- Real_Estate_Sales_new %>% filter(Town %in% Fairfield$`Town name`) %>% mutate(County = "Fairfield")
Real.Estate.Hartford <- Real_Estate_Sales_new %>% filter(Town %in% Hartford$`Town name`) %>% mutate(County = "Hartford")
Real.Estate.Litchfield <- Real_Estate_Sales_new %>% filter(Town %in% Litchfield$`Town name`) %>% mutate(County = "Litchfield")
Real.Estate.Middlesex <- Real_Estate_Sales_new %>% filter(Town %in% Middlesex$`Town name`) %>% mutate(County = "Middlesex")
Real.Estate.New.London <- Real_Estate_Sales_new %>% filter(Town %in% New_London$`Town name`) %>% mutate(County = "New London")

Real.Estate.County <- rbind(Real.Estate.Tolland, 
                            Real.Estate.New.Haven, 
                            Real.Estate.Windham, 
                            Real.Estate.Fairfield,
                            Real.Estate.Hartford,
                            Real.Estate.Litchfield,
                            Real.Estate.Middlesex,
                            Real.Estate.New.London)

Covid_start_date <- as.Date("2020-01-01")

Real.Estate.County <- Real.Estate.County %>% 
  filter(`Date Recorded` > Covid_start_date) %>% 
  mutate(Month = months(`Date Recorded`),
         Year = year(`Date Recorded`))
Real.Estate.County$Month <- factor(Real.Estate.County$Month, 
                                   levels=c("January",
                                            "February",
                                            "March",
                                            "April",
                                            "May",
                                            "June",
                                            "July",
                                            "August",
                                            "September",
                                            "October",
                                            "November",
                                            "December"), 
                                   ordered=TRUE)

Real.Estate.County$Year <- as.factor(Real.Estate.County$Year)
Real.Estate.County <- Real.Estate.County %>% filter(`Sale Amount` <= 4e+09)
View(Real.Estate.County)
ggplot(Real.Estate.County, aes(x=`Date Recorded`, y=`Sale Amount`, group=County, color=County)) +
  geom_line()
ggplot(Real.Estate.County, aes(x=Month)) + 
  geom_bar() +
  facet_grid(County~.)

ggplot(Real.Estate.County, aes(x=Month, y=`Sale Amount`)) + 
  geom_boxplot() +
  facet_grid(Year~.)

#plot 1: median sale value for time and county
Real.Estate.County.Median.Sale <- Real.Estate.County %>%
  group_by(County, `Date Recorded`) %>%
  summarise(Sale_median=(median(`Sale Amount`)))

Real.Estate.County.Median.Sale$County <- as.factor(Real.Estate.County.Median.Sale$County)
View(Real.Estate.County.Median.Sale)
ggplot(Real.Estate.County.Median.Sale, aes(x=`Date Recorded`, y=Sale_median, group=County, color=County)) +
  geom_line()

ggplot(Real.Estate.County.Median.Sale, aes(x=`Date Recorded`, y=Sale_median)) +
  geom_line() +
  facet_grid(County~.)
#plot 2: Same plot as plot 1 but using Residential Type or Property Type to split them to individual plots
Real.Estate.County.Median.Sale.Residential <- Real.Estate.County %>%
  group_by(County, `Date Recorded`, `Residential Type`) %>%
  summarise(Sale_median=(median(`Sale Amount`)))

Real.Estate.County.Median.Sale.Residential <- na.omit(Real.Estate.County.Median.Sale.Residential)
Real.Estate.County.Median.Sale.Residential$County <- as.factor(Real.Estate.County.Median.Sale.Residential$County)
Real.Estate.County.Median.Sale.Residential$`Residential Type` <- as.factor(Real.Estate.County.Median.Sale.Residential$`Residential Type`)
View(Real.Estate.County.Median.Sale.Residential)

ggplot(Real.Estate.County.Median.Sale.Residential, aes(x=`Date Recorded`, y=Sale_median,group=County, color=County)) +
  geom_line() +
  facet_grid(`Residential Type`~.)
#plot 3: Residuals from regression model for each County (Sale Amount = Type (Property or residential)) over time

#Compare with Covide time series plot to see if there is anything impact in housing price

#Think about other factors that can be affect the housing price

#############################
#COVID 19 data analysis
#############################
COVID.19.County <- read_csv("COVID-19_Cases__Hospitalizations__and_Deaths__By_County__-_ARCHIVE.csv", 
                                                                         col_types = cols(`Date updated` = col_date(format = "%m/%d/%Y")))
COVID.19.County <- COVID.19.County %>% 
  mutate(Month = months(`Date updated`),
         Year = year(`Date updated`))
COVID.19.County$County <- as.factor(COVID.19.County$County)
COVID.19.County$Month <- factor(COVID.19.County$Month, 
                                   levels=c("January",
                                            "February",
                                            "March",
                                            "April",
                                            "May",
                                            "June",
                                            "July",
                                            "August",
                                            "September",
                                            "October",
                                            "November",
                                            "December"), 
                                   ordered=TRUE)

COVID.19.County$mon_yr = format(COVID.19.County$`Date updated`, "%Y-%m") 

begin_month <- COVID.19.County %>% group_by(mon_yr) %>% filter(`Date updated` == min(`Date updated`))

end_month <- COVID.19.County %>% group_by(mon_yr) %>% filter(`Date updated` == max(`Date updated`))

merge_data <- merge(begin_month, end_month, by=c("mon_yr", "County","County code","Month","Year"))

merge.data.case.per.month <- merge_data %>% select(c("County","Month","Year","Total cases.x","Total cases.y","mon_yr"))
merge.data.case.per.month <- merge.data.case.per.month %>% mutate(`Total Case` = `Total cases.y`-`Total cases.x` )

merge.data.case.per.month$mon_yr <- as.Date(paste(merge.data.case.per.month$mon_yr,"-01",sep=""))
ggplot(merge.data.case.per.month, aes(x=mon_yr, y = `Total Case`, group=County, color=County)) +
  geom_line()

Tolland.COVID.19 <- merge.data.case.per.month %>% filter(County == "Tolland")
Tolland.COVID.19$mon_yr <- as.Date(paste(Tolland.COVID.19$mon_yr,"-01",sep=""))
ggplot(Tolland.COVID.19, aes(x=mon_yr, y=`Total Case`)) + 
  geom_line()

ggplot(data=Tolland.COVID.19, aes(x=Month, y=`Total Case`)) + 
  geom_line() +
  facet_grid(Year~.)


ggplot(data=COVID.19.County, aes(x=`Date updated`, y=`Total cases`,group=County, color=County)) + geom_line()


