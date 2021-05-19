rawData <- read.csv(stringsAsFactors = T, na.strings = c(""), 'P3-Future-500-The-Dataset.csv')
str(rawData)
summary(rawData)


# ---------- Changing ID and Inception Year to factor ---------- #
rawData$ID <- factor(rawData$ID)
rawData$Inception <- factor(rawData$Inception)


# ---------- Cleaning up the columns and changing them to num ---------- #
rawData$Expenses <- gsub(' Dollars', '', rawData$Expenses)
rawData$Expenses <- gsub(',', '', rawData$Expenses)

rawData$Revenue <- gsub('\\$', '', rawData$Revenue)
rawData$Revenue <- gsub(',', '', rawData$Revenue)

rawData$Growth <- gsub('%', '', rawData$Growth)

rawData$Expenses <- as.numeric(rawData$Expenses)
rawData$Revenue <- as.numeric(rawData$Revenue)
rawData$Growth <- as.numeric(rawData$Growth)


# ---------- Checking missing data ---------- #
rawData_backup_with_na <- rawData
rawData[!complete.cases(rawData), ]


# ---------- Removing records where Industry == na ---------- #
rawData[is.na(rawData$Industry), ]
rawData <- rawData[!is.na(rawData$Industry), ]
rownames(rawData) <- NULL


# ---------- Replacing missing data through Factual Analysis Method  ---------- #
rawData[!complete.cases(rawData), ]
rawData[is.na(rawData$State) & rawData$City == 'New York', 'State'] <- 'NY'
rawData[is.na(rawData$State)  & rawData$City == 'San Francisco', 'State'] <- 'CA'


# ---------- Replacing missing data through Median Imputation Method  ---------- #
median(rawData[rawData$Industry == 'Retail', 'Employees'], na.rm = T)
rawData[is.na(rawData$Employees) & rawData$Industry == 'Retail', 'Employees'] <- median(rawData[rawData$Industry == 'Retail', 'Employees'], na.rm = T)
median(rawData[rawData$Industry == 'Financial Services', 'Employees'], na.rm = T)
rawData[is.na(rawData$Employees) & rawData$Industry == 'Financial Services', 'Employees'] <- median(rawData[rawData$Industry == 'Financial Services', 'Employees'], na.rm = T)

median(rawData[rawData$Industry == 'Construction', "Growth"], na.rm = T)
rawData[is.na(rawData$Growth), 'Growth'] <- median(rawData[rawData$Industry == 'Construction', "Growth"], na.rm = T)

median(rawData[is.na(rawData$Revenue), ])
median(rawData[rawData$Industry == 'Construction', 'Revenue'], na.rm = T)
rawData[is.na(rawData$Revenue), 'Revenue'] <- median(rawData[rawData$Industry == 'Construction', 'Revenue'], na.rm = T)

rawData[is.na(rawData$Expenses), "Expenses"] <- rawData[is.na(rawData$Expenses), "Revenue"] - rawData[is.na(rawData$Expenses), "Profit"]
rawData[is.na(rawData$Profit), 'Profit'] <- rawData[is.na(rawData$Profit), "Revenue"] - rawData[is.na(rawData$Profit), "Expenses"]

# ---------- Plotting the data and visualizing results  ---------- #
library(ggplot2)

plotBase <- ggplot(data = rawData)
plotRevenueExpenses <- plotBase + geom_point(aes(x = Revenue, y = Expenses, size = Profit, color = Industry), alpha = .5) +
  labs(title = 'Revenue, Expenses and Profit based on Industries', x = 'Revenue in $USD', y = 'Expenses in $USD') +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = 'bold', margin = margin(30, 0, 30, 0)),
    axis.title.x = element_text(family = 'sans', size = 10, margin = margin(30, 0, 10, 0)),
    axis.title.y = element_text(family = 'sans', size = 10, margin = margin(0, 30, 0, 10))
  )
plotRevenueExpenses

plotIndustryTrends <- plotBase + geom_point(aes(x = Revenue, y = Expenses, color = Industry, size = Profit), alpha = .35) +
  labs(title = 'Industry Trends for Expenses ~ Revenue', x = 'Revenue in $USD', y = 'Expenses in $USD') +
  geom_smooth(aes(x = Revenue, y = Expenses, color = Industry), fill = NA, size = 0.8) +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = 'bold', margin = margin(30, 0, 30, 0)),
    axis.title.x = element_text(family = 'sans', size = 10, margin = margin(30, 0, 10, 0)),
    axis.title.y = element_text(family = 'sans', size = 10, margin = margin(0, 30, 0, 10))
  )
plotIndustryTrends

boxplotRevenueGrowth <- plotBase + 
  geom_jitter(aes(x = Industry, y = Growth, color = Industry), alpha= 0.6 ) +
  geom_boxplot(aes(x = Industry, y = Growth, color = Industry), fill = 'white', size = 0.5, alpha = .6, outlier.color = NA) +
  labs(title = 'Growth in % by Industry', x = 'Industry', y = 'Growth in %') +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = 'bold', margin = margin(30, 0, 30, 0)),
    axis.title.x = element_text(family = 'sans', size = 10, margin = margin(30, 0, 10, 0)),
    axis.title.y = element_text(family = 'sans', size = 10, margin = margin(0, 30, 0, 10))
  )

boxplotRevenueGrowth

# ---------- The End  ---------- #



