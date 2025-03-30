library("dplyr")
library("readxl")
library("naniar")
library("mice")
library("ggplot2")
library("caret")


givenDataFrame <- read_excel("D:\\AIUB\\Fall 24-25\\Data Science\\Mid Project 1\\Given Dataset\\dataset.xlsx")

givenDataFrame <- as.data.frame(apply(givenDataFrame,2, tolower))


totalAttribute <- ncol(givenDataFrame)
i <- 1

while(i <= totalAttribute){
  givenDataFrame[,i] <- trimws(givenDataFrame[,i])
  i <- i+1
}

givenDataFrame <- distinct(givenDataFrame)

colSums(is.na(givenDataFrame))   


gg_miss_upset(givenDataFrame)


       

givenDataFrame <- subset(givenDataFrame,!is.na(Gender))
givenDataFrame <- subset(givenDataFrame,!is.na(Depression))


mostFreqVal <- sort(table(givenDataFrame$`Sleep Duration`), decreasing = TRUE)
givenDataFrame$`Sleep Duration`[is.na(givenDataFrame$`Sleep Duration`)] <- names(mostFreqVal[1]) 


average <- mean(as.numeric(givenDataFrame$Age[!is.na(givenDataFrame$Age)]))
givenDataFrame$Age[is.na(givenDataFrame$Age)] <- floor(average)





all_possible_values <- tolower(c("5-6 Hours","7-8 Hours","More than 8 hours","Less than 5 hours"))
givenDataFrame <- subset(givenDataFrame,`Sleep Duration` %in% all_possible_values)

all_possible_values <- tolower(c("yes","no"))
givenDataFrame <- subset(givenDataFrame,`Have you ever had suicidal thoughts ?` %in% all_possible_values)

all_possible_values <- tolower(c("yes","no"))
givenDataFrame <- subset(givenDataFrame,`Family History of Mental Illness` %in% all_possible_values)

all_possible_values <- tolower(c("yes","no"))
givenDataFrame <- subset(givenDataFrame,`Depression` %in% all_possible_values)

all_possible_values <- tolower(c("male","female"))
givenDataFrame <- subset(givenDataFrame,`Gender` %in% all_possible_values)

all_possible_values <- tolower(c("unhealthy","moderate","healthy"))
givenDataFrame <- subset(givenDataFrame,`Dietary Habits` %in% all_possible_values)



handle_outliers <- function(dataFrame, column){
  totalData <- nrow(dataFrame)
  d_stats <- summary(as.numeric(dataFrame[,column]))
  
  
  IQR <- 1.5 * (as.numeric(d_stats[5]) - as.numeric(d_stats[2]))
  
  
  q1 <- as.numeric(d_stats[2]) - IQR
  q3 <- as.numeric(d_stats[5]) + IQR
  
  
  i <- 1
  while(i <= totalData){
    if(!between(as.numeric(dataFrame[i,column]),q1,q3)){
      dataFrame <- dataFrame[-i,]
      i <- i-1
      totalData <- totalData - 1
    }
    i <- i+1
  }
  return(dataFrame)
}


givenDataFrame <- handle_outliers(givenDataFrame, 3)
givenDataFrame <- handle_outliers(givenDataFrame, 4)
givenDataFrame <- handle_outliers(givenDataFrame, 8)
givenDataFrame <- handle_outliers(givenDataFrame, 9)
givenDataFrame <- handle_outliers(givenDataFrame, 2)



normalize_data <- function(dataFrame, column){
  i <- 1
  totalData <- nrow(dataFrame)

  maxVal <- as.numeric(max(dataFrame[,column]))
  minVal <- as.numeric(min(dataFrame[,column]))

  while(i <= totalData){
    dataFrame[i,column] <- (as.numeric(dataFrame[i,column]) - minVal) / (maxVal-minVal)
    i <- i+1
  }
  return(dataFrame)
}


normalized_attribute <- normalize_data(givenDataFrame, 2)


value_count <- sort(table(givenDataFrame$Depression))
minor_value <- names(value_count)
totalData <- nrow(givenDataFrame)
oversampled <- 0
i <- 1
while(i <= totalData && oversampled <= (as.numeric(value_count[length(value_count)]) - as.numeric(value_count[1]))-1){
  if(givenDataFrame[i,11] == minor_value[1]){
    givenDataFrame <- rbind(givenDataFrame, givenDataFrame[i,])
    oversampled <- oversampled + 1
    totalData <- totalData + 1
  }
  i <- i+1
}

print(table(givenDataFrame$Depression))



value_count <- sort(table(givenDataFrame$Depression), decreasing = TRUE)
major_value <- names(value_count)
totalData <- nrow(givenDataFrame)
undersampled <- 0
i <- 1
while(i <= totalData && undersampled <= (as.numeric(value_count[1]) - as.numeric(value_count[length(value_count)]))-1){
  if(givenDataFrame[i,11] == major_value[1]){
    givenDataFrame <- givenDataFrame[-i,]
    undersampled <- undersampled + 1
    totalData <- totalData + 1
  }
  i <- i+1
}

print(table(givenDataFrame$Depression))





givenDataFrame$`Sleep Duration` <- as.numeric(factor(givenDataFrame$`Sleep Duration`, levels = c("less than 5 hours", "5-6 hours", "7-8 hours","more than 8 hours")))
givenDataFrame$Depression <- as.numeric(factor(givenDataFrame$Depression, levels = c("no","yes")))
givenDataFrame$Gender <- as.numeric(factor(givenDataFrame$Gender, levels = c("female","male")))
givenDataFrame$`Dietary Habits` <- as.numeric(factor(givenDataFrame$`Dietary Habits`, levels = c("unhealthy","moderate","healthy")))
givenDataFrame$`Have you ever had suicidal thoughts ?` <- as.numeric(factor(givenDataFrame$`Have you ever had suicidal thoughts ?`, levels = c("no","yes")))
givenDataFrame$`Family History of Mental Illness` <- as.numeric(factor(givenDataFrame$`Family History of Mental Illness`, levels = c("no","yes")))



givenDataFrame$`Academic Pressure` <- factor(givenDataFrame$`Academic Pressure`, levels = c(1,2,3,4,5), labels=c("Low","Below Average", "Average", "High", "Extremely High"))




average <- mean(as.numeric(givenDataFrame$Age))
print(average)

med <- median(as.numeric(givenDataFrame$`Study Hours`))
print(med)

mod <- names(sort(table(givenDataFrame$Gender),decreasing = TRUE))[1]
print(mod)

range <- max(as.numeric(givenDataFrame$Age)) - min(as.numeric(givenDataFrame$Age))
print(range)

variance <- var(givenDataFrame$Age)
print(variance)

std_dev <- sd(givenDataFrame$Age)
print(std_dev)





i <- 1
while(i <= ncol(givenDataFrame)){
  givenDataFrame[,i] <- as.numeric(givenDataFrame[,i])
  i <- i+1
}


correlation_matrix <- cor(givenDataFrame)
cor_target <- correlation_matrix[,11]
selected_features <- names(cor_target[abs(cor_target) > 0.4])
feature_selected_df <- select(givenDataFrame, selected_features)


sample_data <- givenDataFrame[sample(1:nrow(givenDataFrame),50),]


temp_df <- subset(givenDataFrame,`Sleep Duration` == "2")[5:10,]
temp_df <- filter(givenDataFrame,`Study Hours` >= 6)[1:10,]
temp_df <- arrange(givenDataFrame,Age)
temp_df <- mutate(givenDataFrame, avg=as.numeric(`Study Hours`)/4)
temp_df <- transmute(givenDataFrame, avg=as.numeric(`Study Hours`)/4)


ggplot(givenDataFrame, aes(x = Age)) + geom_histogram(binwidth = 1, fill = "skyblue", color = "black") + labs(title = "Histogram of Age", x="Age", y = "Count")
ggplot(givenDataFrame, aes(x = `Study Hours`)) + geom_histogram(binwidth = 1, fill = "skyblue", color = "black") + labs(title = "Histogram of Study Hours", x="Hours", y = "Count")
