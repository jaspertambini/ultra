# Package installation

options(warn = -1, scipen = 999)
suppressPackageStartupMessages(library(car))
suppressPackageStartupMessages(library(MASS))
suppressPackageStartupMessages(library(leaps))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(moments))
suppressPackageStartupMessages(library(PerformanceAnalytics))
suppressPackageStartupMessages(library(caTools))
suppressPackageStartupMessages(library(ggcorrplot))

# Cleaning data

ultra1 <- read.csv("C:\\git\\github_test\\ultra\\linear_regression.csv", row.names=1, header=TRUE, na.strings=c("","NA"))
ultra1$Time <- lubridate::hms(ultra1$Time)
ultra1$Time <- period_to_seconds(ultra1$Time)
ultra1$Split_time <- lubridate::hms(ultra1$Split_time)
ultra1$Split_time <- period_to_seconds(ultra1$Split_time)
ultra1$Year<- as.factor(ultra1$Year)
ultra1$Type<- as.factor(ultra1$Type)
ultra1$Sleep<- as.factor(ultra1$Sleep)
ultra1$Date<- as.Date(ultra1$Date, "%d/%m/%Y")
ultra1$Feet <- gsub('1 Good', 'Good',
                    gsub('2 Fair', 'Fair',
                         gsub('3 Poor', 'Poor', ultra1$Feet)))
ultra1 <- ultra1 %>% rownames_to_column(var = "rowid") %>% mutate(Main.reason.for.retiring = ifelse(is.na(Main.reason.for.retiring), 'None', Main.reason.for.retiring),
                           Other.problems = ifelse(is.na(Other.problems), 'None', Other.problems),
                           Feet = ifelse(is.na(Feet), 'None', Feet)) %>% column_to_rownames(var = "rowid")
cats<- c('Main.reason.for.retiring', 'Other.problems','Feet','Weather..Personal.Temp')
ints<- c('Distance.Covered','Climb','Position')
for (cat in cats) {
    ultra1[,cat]<- as.factor(ultra1[,cat])
}
for (int in ints) {
    ultra1[,int]<- as.numeric(ultra1[,int])
}

# Missing values imputation

ultratme<- ultra1 %>% filter(Rest.Stop==1) %>% group_by(Rest.Stop) %>%
    dplyr::summarize(Mean = mean(Time, na.rm=TRUE)) %>% dplyr::select(-Rest.Stop)
Meantme<- ultratme$Mean
ultrapos<- ultra1 %>% filter(Rest.Stop==1) %>% group_by(Rest.Stop) %>%
    dplyr::summarize(Mean = mean(Position, na.rm=TRUE)) %>% dplyr::select(-Rest.Stop)
Meanpos<- ultrapos$Mean
ultraspd<- ultra1 %>% filter(Rest.Stop==1) %>% group_by(Rest.Stop) %>%
    dplyr::summarize(Mean = mean(Avg.Speed, na.rm=TRUE)) %>% dplyr::select(-Rest.Stop)
Meanspd<- ultraspd$Mean
ultra2<- ultra1 %>% rownames_to_column(var = "rowid") %>% mutate(Time = replace(Time,
           is.na(Time) & Rest.Stop==1,
          Meantme),
                           Split_time = replace(Split_time,
           is.na(Split_time) & Rest.Stop==1,
          Meantme),
                         Position = replace(Position,
           is.na(Position) & Rest.Stop==1,
          Meanpos),
                              Avg.Speed = replace(Avg.Speed,
           is.na(Avg.Speed) & Rest.Stop==1,
          Meanspd)) %>% column_to_rownames(var = "rowid")
ultra2<- ultra2 %>%
  na.omit
target<- ultra2$Split_time
ultra3<- ultra2

# Feature transformation

ultra3$Rest.Stop <- sin(0.785*ultra3$Rest.Stop)
ultra4<- ultra3 %>%
  na.omit
ultra3stats <- ultra3[, c(4,7,8,9)]
chart.Correlation(ultra3stats, histogram=TRUE, pch=19)
nums<- c("Position", "Split_time")
skewness(ultra3$Split_time)
skewness(ultra3$Position)
skewness(ultra3$Avg.Speed)
skewness(ultra3$Climb)
ultra3[nums] <- lapply(ultra3[nums], log1p)
skewness(ultra3$Split_time)
skewness(ultra3$Position)
skewness(ultra3$Avg.Speed)
skewness(ultra3$Climb)
ultrastats2 <- ultra3[, c(4,7,8,9)]
chart.Correlation(ultrastats2, histogram=TRUE, pch=19)

# Feature engineering

ultra4<- ultra3 %>% rownames_to_column(var = "rowid") %>% mutate(Month= month(as.POSIXlt(Date, format="%d/%m/%Y")))%>% arrange(Date) %>% column_to_rownames(var = "rowid")
ultra4$Month <- cos((-0.52*ultra4$Month)+0.5)
ultracorr<- select_if(ultra4, is.numeric)
ultracorr <- cor(ultracorr, use = "complete.obs", method = 'spearman')
ggcorrplot(ultracorr,
           hc.order = TRUE,
           type = "lower",
           lab = TRUE)
names(ultra4) <- tolower(names(ultra4))
ultra4<- ultra4 %>% dplyr::select(-event)
ultra4$type <- droplevels(ultra4$type)
ultra4$weather..personal.temp <- droplevels(ultra4$weather..personal.temp)
ultra4$sleep <- droplevels(ultra4$sleep)
ultra4$feet <- droplevels(ultra4$feet)
ultra4$main.reason.for.retiring <- droplevels(ultra4$main.reason.for.retiring)
ultra4$other.problems <- droplevels(ultra4$other.problems)
ultra5<- ultra4 #%>% select(c(position, avg.speed, month, year, climb, distance))
str(ultra4)
# Encoding

dummy <- dummyVars(" ~ .", data=ultra5)
ultra6 <- data.frame(predict(dummy, newdata = ultra5))
ultra6 <- ultra5#[ - as.numeric(which(apply(ultra5, 2, var) == 0))]
position<- ultra5$position

# Scaling
ultra6<- subset(ultra6, select=-position)
ultra6<- ultra6 %>% rownames_to_column(var = "rowid") %>%  mutate_if(is.numeric, scale) %>% column_to_rownames(var = "rowid")
ultra6 <- cbind(ultra6, position)
split <- sample.split(ultra6$position, SplitRatio = 0.9)
training_set <- subset(ultra6, split == TRUE)
test_set <- subset(ultra6, split == FALSE)

classifier <- lm(formula = position ~ .,
                      data = training_set)

summary(classifier)
#avPlots(classifier)

train.control <- trainControl(method = "cv", number = 10)

step.model <- train(position ~., data = training_set,
                    method = "leapBackward",
                    tuneGrid = data.frame(nvmax = 1:17),
                    trControl = train.control
                    )


# step.model$results
#
# step.model$bestTune
#
# coef(step.model$finalModel, 15)

y_pred <- predict(step.model, type = 'raw', newdata = test_set)
predictions<- as.data.frame(y_pred)
actuals<- as.data.frame(test_set[17])
results<- cbind(predictions,actuals)
results<- exp(results)
results