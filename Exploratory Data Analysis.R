install.packages('fastDummies')
library('fastDummies')
library(ggplot2)
library(dplyr)
library(tidyr)

#Importing the datasets
housing = read.csv("/Users/Desktop/housing.csv")
arrest = read.csv("/Users/Desktop/arrest data.csv")

#Dropping irrelevant columns in each dataset
housing=subset(housing, select = -c(Building.ID, Number, BBL, BIN, Census.Tract, NTA...Neighborhood.Tabulation.Area,Latitude, Longitude, Longitude..Internal.,coordinate, Latitude..Internal.))
arrest=subset(arrest, select = -c(PD_CD, PD_DESC,KY_CD, ARREST_PRECINCT,LAW_CODE,JURISDICTION_CODE, X_COORD_CD, Y_COORD_CD, Latitude, Longitude, Latitude.Longitude,City.Council.Districts, Police.Precincts, Community.Districts, Borough.Boundaries))
View(housing)
View(arrest)
names(housing)                 
names(arrest)

#Dropping missing values
sum(is.na(arrest))
arrest <- na.omit(arrest)
sum(is.na(housing))
housing<- na.omit(housing)

#Mutating arrest dataset borough name to full name 

arrest$ARREST_BORO <- str_replace_all(arrest$ARREST_BORO, 'Q', 'Queens')
arrest$ARREST_BORO <- str_replace_all(arrest$ARREST_BORO, 'M', 'Manhattan')
arrest$ARREST_BORO <- str_replace_all(arrest$ARREST_BORO, 'B', 'Bronx')
arrest$ARREST_BORO <- str_replace_all(arrest$ARREST_BORO, 'S', 'State Island')
arrest$ARREST_BORO <- str_replace_all(arrest$ARREST_BORO, 'K', 'Brooklyn')
View(arrest)

#Dropping irrelevant crime types
arrest <- arrest[!(arrest$OFNS_DESC == "ABORTION" | arrest$OFNS_DESC == "ADMINISTRATIVE CODE" | 
                     arrest$OFNS_DESC == "ADMINISTRATIVE CODES"| arrest$OFNS_DESC == "AGRICULTURE & MRKTS LAW-UNCLASSIFIED" | 
                     arrest$OFNS_DESC == "OTHER STATE LAWS" | arrest$OFNS_DESC == "OTHER STATE LAWS (NON PENAL LA" |
                     arrest$OFNS_DESC == "F.C.A. P.I.N.O.S." | arrest$OFNS_DESC == "NEW YORK CITY HEALTH CODE" | 
                     arrest$OFNS_DESC == "VEHICLE AND TRAFFIC LAWS"), ]
table(arrest$OFNS_DESC)

#Dummy coding the categorical variables
arrest <- dummy_cols(arrest, select_column = c("OFNS_DESC", "LAW_CAT_CD", "ARREST_BORO", "AGE_GROUP", "PERP_SEX", "PERP_RACE"))
housing <- dummy_cols(housing, select_columns = c("Borough", "Reporting.Construction.Type",
                                                  "Extended.Affordability.Only", "Prevailing.Wage.Status"))
View(arrest)
View(housing)

#Create visualization bar charts
ggplot(housing, aes(x=Borough))+ geom_bar(width=0.7, fill="steelblue")+ geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white") + 
  print(labs(y = "Count of building project", x = "Borough")) +
  print(ggtitle("Building project visualization by Borough"))

ggplot(arrest, aes(x=ARREST_BORO))+ geom_bar(width=0.7, fill="steelblue")+ geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white") + 
  print(labs(y = "Count of arrest", x = "Borough")) +
  print(ggtitle("NYPD Arrest visualization by borough"))


#Export the cleaned datasets
write.table(arrest, file = "arrest_cleaned.csv", sep = ",")
write.table(housing, file = "housing_cleaned.csv", sep = ",")


