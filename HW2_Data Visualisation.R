# Part 1

setwd("C:/Users/Bharathi/OneDrive/Documents/Data Visualisation")

#The below code creates a data frame containing the sales done by various people from respective states
df1=data.frame(Name=c('James','Paul','Richards','Marico','Samantha','Ravi','Raghu',
                      'Richards','George','Ema','Samantha','Catherine'),
               State=c('Alaska','California','Texas','North Carolina','California','Texas',
                       'Alaska','Texas','North Carolina','Alaska','California','Texas'),
               Sales=c(14,24,31,12,13,7,9,31,18,16,18,14))

#The below code groups the data by the state and calculates the sum of sales for each state using the sum function in aggregate
aggregate(df1$Sales, by=list(df1$State), FUN=sum)

#Load the library
library(dplyr)
#The below code uses the pipe operator from the dplyr library.
#The code pipes the df1 into group_by function to subset the data by their states.
#and pipe line to summarise the sum of sales for each state and create a new column called sum_sales.
df1 %>% group_by(State) %>% summarise(sum_sales = sum(Sales))

#Part 2

#Load the dataset
worldcup <- read.csv("WorldCupMatches.csv")

#(a)Find the size of the data frame.
head(worldcup)

#The number of columns is 20
ncol(worldcup)

#The number of rows is 852
nrow(worldcup)

#(b)Use summary function to report the statistical summary of your data.
summary(worldcup)

#The dataset contains data from 1930 WorldCup to 2014 WorldCup.
# 50% of the home teams goal were at least 2 and the max number of goals made by the home team is 10
# 50% of the away teams goal were at least 1 and the max number of goals made by the away team is 7
# In the attendance column, there are 2 missing values and there were at least 2000 people attending the worldcup matches.
# During Half-Time,25% of the home teams goal were at least 1, the max number of goals made by the home team was 6 and 
# 25% of the away teams goal was at least 1 and the max number of goals made by the away team was 5.

#(c)The number of unique locations olympics were held is 151.
length(unique(worldcup$City))

#(d)The average attendance is 45165
#(e)For each Home Team, what is the total number of goals scored?

worldcup %>% group_by(Home.Team.Name) %>% summarise(sum_goals =sum(Home.Team.Goals))

#(f)The average number of attendees for each year? 

average_attendees = worldcup %>% group_by(Year) %>% summarise(average_attendees = mean(Attendance))

library(ggplot2)

ggplot(average_attendees, aes(x = Year, y = average_attendees)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Trend of World Cup Attendees Over the Years",
    x = "Year",
    y = "Number of Attendees"
  ) +
  theme_minimal()

#The average number of attendees was the highest during 1994 world cup match.
#The average number of attendees has been generally rising from 1938 till 1994.
#The least number of attendees was during the 1938 world cup match

#Part 3

metabolite = read.csv('metabolite.csv', header=T)
head(metabolite)

#(a)There are 35 Alzheimers patients in the data set.

alzheimers_count = metabolite %>% filter(Label == "Alzheimer") %>% count()
alzheimers_count

#(b)Determine the number of missing values for each column

missing_values = metabolite %>% summarise_all(~sum(is.na(.)))
missing_values

#(c)Remove the rows which has missing value for the Dopamine column and assign the result to a new data frame.
sum(is.na(metabolite$Dopamine))
metabolites_new = metabolite %>% filter(!is.na(Dopamine))

#(d)In the new data frame, replace the missing values in the c4-OH-Pro column with the median value of the same column.
median_c4_OH_pro = median(metabolites_new$c4.OH.Pro, na.rm = TRUE)
metabolites_new$c4.OH.Pro[is.na(metabolites_new$c4.OH.Pro)] = median_c4_OH_pro

#(e)Drop the columns which have more than 25% missing values

colSums(is.na(metabolites_new))

#Calculate the percentage of missing values for each column
missing_percentage = colSums(is.na(metabolites_new)) / nrow(metabolites_new) * 100
missing_percentage

#Identify the columns to be removed
columns_remove = names(missing_percentage[missing_percentage > 25])
columns_remove
#Remove the columns
Metabolite_clean = metabolites_new[, !(names(metabolites_new) %in% columns_remove)]
