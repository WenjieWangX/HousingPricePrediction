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

Real_Estate_Sales_new <- Real_Estate_Sales_2001_2020_GL %>% drop_na(Town)

CT_County_Town <- read_excel("CT_County_Town.xlsx")

Tolland <- CT_County_Town %>% filter(County == "Tolland") %>% dplyr::select("Town name")
New_Haven <- CT_County_Town %>% filter(County == "New Haven") %>% dplyr::select("Town name")
Windham <- CT_County_Town %>% filter(County == "Windham") %>% dplyr::select("Town name")
Fairfield <- CT_County_Town %>% filter(County == "Fairfield") %>% dplyr::select("Town name")
Hartford <- CT_County_Town %>% filter(County == "Hartford") %>% dplyr::select("Town name")
Litchfield <- CT_County_Town %>% filter(County == "Litchfield") %>% dplyr::select("Town name")
Middlesex <- CT_County_Town %>% filter(County == "Middlesex") %>% dplyr::select("Town name")
New_London <- CT_County_Town %>% filter(County == "New London") %>% dplyr::select("Town name")


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

# Real.Estate.Tolland <- Real.Estate.County %>% filter(Town %in% Tolland$`Town name`) %>% mutate(County = "Tolland")
# nrow(Real.Estate.Tolland)
# View(Real.Estate.Tolland)

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


Real.Estate.County$mon_yr = format(Real.Estate.County$`Date Recorded`, "%Y-%m") 
Real.Estate.County.Median <- Real.Estate.County %>%
  group_by(County, mon_yr, `Property Type`, `Residential Type`) %>%
  summarise(Sale_median=(median(`Sale Amount`)))
Real.Estate.County.Median <- na.omit(Real.Estate.County.Median)
View(Real.Estate.County.Median)

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

merge.data.case.per.month <- merge_data %>% dplyr::select(c("County","Month","Year","Total cases.x","Total cases.y","mon_yr"))
merge.data.case.per.month <- merge.data.case.per.month %>% mutate(`Total Case` = `Total cases.y`-`Total cases.x` )
Covid.19.Month.Cases <- merge.data.case.per.month %>% dplyr::select(c("County","Month","Year","mon_yr", "Total Case"))
View(Covid.19.Month.Cases)
#merge.data.case.per.month$mon_yr <- as.Date(paste(merge.data.case.per.month$mon_yr,"-01",sep=""))

House.Covid <- merge(Real.Estate.County.Median, Covid.19.Month.Cases, by=c("mon_yr","County"))
View(House.Covid)
