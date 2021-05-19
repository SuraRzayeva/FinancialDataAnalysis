library(ggplot2)
summary(rawData)
head(rawData)
tail(rawData)
str(rawData)
summary(rawData)

# Changing from non-factor to factor
rawData <- read.csv(stringsAsFactors = T, na.strings = c(""), 'P3-Future-500-The-Dataset.csv')
rawData$ID <- factor(rawData$ID)
rawData$Inception <- factor(rawData$Inception)



#converting chr to numeric
?as.numeric()

#converting num/chr to factor
?factor()

#converting factor to num - first turn them into chr, then to num
?as.numeric(as.character())

#sub() and gsub() 

rawData$Expenses <- gsub(' Dollars', '', rawData$Expenses)
rawData$Expenses <- gsub(',', '', rawData$Expenses)

rawData$Revenue <- gsub('\\$', '', rawData$Revenue)
rawData$Revenue <- gsub(',', '', rawData$Revenue)

rawData$Growth <- gsub('%', '', rawData$Growth)

rawData$Expenses <- as.numeric(rawData$Expenses)
rawData$Revenue <- as.numeric(rawData$Revenue)
rawData$Growth <- as.numeric(rawData$Growth)

# Dealing with missing DATA
head(rawData, 20)
median(rawData$Employees, na.rm = T)

rawData[!complete.cases(rawData), ]

#filtering using which() for non-missing data - selects without considering na
rawData$Revenue == 9254614
rawData[which(rawData$Revenue == 9254614), ]
rawData[which(rawData$Employees == 45), ]

#na.rm 
rawData[is.na(rawData$Employees), ]

# Remove records with missing data
rawData_backup <- rawData
rawData[!complete.cases(rawData), ]
rawData[is.na(rawData$Industry), ]

rawData[!is.na(rawData$Industry), ]
rawData <- rawData[!is.na(rawData$Industry), ]


rownames(rawData) <- 1:nrow(rawData)

tail(rawData)

# Correcting DATA through factual analysis

rawData[!complete.cases(rawData), ]
rawData[is.na(rawData$State) & rawData$City == 'New York', ]
rawData[is.na(rawData$State) & rawData$City == 'New York', 'State'] <- 'NY'
rawData[is.na(rawData$State)  & rawData$City == 'San Francisco', 'State'] <- 'CA'


#Replacing missing data through Median Imputation Method 

median(rawData[rawData$Industry == 'Retail', ]$Employees, na.rm = T)
median(rawData[rawData$Industry == 'Retail', 'Employees'], na.rm = T)
median(rawData[rawData$Industry == 'Financial Services', ]$Employees, na.rm = T)
median(rawData[rawData$Industry == 'Financial Services', 'Employees'], na.rm = T)
rawData[is.na(rawData$Employees) & rawData$Industry == 'Retail', 'Employees'] <- median(rawData[rawData$Industry == 'Retail', ]$Employees, na.rm = T)
rawData[is.na(rawData$Employees) & rawData$Industry == 'Financial Services', 'Employees'] <- median(rawData[rawData$Industry == 'Financial Services', ]$Employees, na.rm = T)
rawData[c(3, 330), ]

rawData[is.na(rawData$Growth), ]
median(rawData[rawData$Industry == 'Construction', "Growth"], na.rm = T)
rawData[is.na(rawData$Growth), 'Growth'] <- median(rawData[rawData$Industry == 'Construction', "Growth"], na.rm = T)

median(rawData[is.na(rawData$Revenue), ])
median(rawData[rawData$Industry == 'Construction', 'Revenue'], na.rm = T)
rawData[is.na(rawData$Revenue), 'Revenue'] <- median(rawData[rawData$Industry == 'Construction', 'Revenue'], na.rm = T)

median(rawData[rawData$Industry == 'Construction', "Expenses"], na.rm = T)
rawData[is.na(rawData$Expenses) & rawData$Industry == 'Construction', "Expenses"] <- median(rawData[rawData$Industry == 'Construction', "Expenses"], na.rm = T)
median(rawData[rawData$Industry == 'IT Services', 'Expenses'], na.rm = T)
rawData[is.na(rawData$Expenses), 'Expenses'] <- median(rawData[rawData$Industry == 'IT Services', 'Expenses'], na.rm = T)

rawData[rawData$Name == 'Ganzlax', "Expenses"] <- NA

rawData[is.na(rawData$Profit), ]

rawData[is.na(rawData$Profit), 'Profit'] <- rawData[is.na(rawData$Profit), "Revenue"] - rawData[is.na(rawData$Profit), "Expenses"]
rawData[is.na(rawData$Expenses), "Expenses"] <- rawData[is.na(rawData$Expenses), "Revenue"] - rawData[is.na(rawData$Expenses), "Profit"]

plotBase <- ggplot(data = rawData)
plotRevenueExpenses <- plotBase + geom_point(aes(x = Revenue, y = Expenses, size = Profit, color = Industry), alpha = .5) +
  labs(title = 'Revenue, Expenses and Profit based on Industries', x = 'Revenue in $USD', y = 'Expenses in $USD') +
  theme_minimal()
plotRevenueExpenses

plotIndustryTrends <- plotBase + geom_point(aes(x = Revenue, y = Expenses, color = Industry), alpha = .5) +
  labs(title = 'Industry Trends for Expenses ~ Revenue', x = 'Revenue in $USD', y = 'Expenses in $USD') +
  geom_smooth(aes(x = Revenue, y = Expenses, color = Industry), fill = NA, size = 1.2) +
  theme_minimal()
plotIndustryTrends

boxplotRevenueGrowth <- plotBase + geom_jitter(aes(x = Revenue, y = Growth, color = Industry), size = 0.4, width = .3) +
  geom_boxplot(aes(x = Revenue, y = Growth, color = Industry), size = 0.5, alpha = .6, outlier.color = NA) +
  labs(title = 'Growth in % by Industry', x = 'Revenue in $USD', y = 'Growth in %') +
  theme_minimal()
boxplotRevenueGrowth


