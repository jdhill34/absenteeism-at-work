library(ROCR)
library(pROC)
library(ggplot2)

library(quantreg)
pre_absent = read.table(file.choose(), header = TRUE, sep=';')
absent=pre_absent
#nrow(pre_absent[(pre_absent$Absenteeism.time.in.hours <= 8),])
#absent = pre_absent[(pre_absent$Absenteeism.time.in.hours <= 8),]
yuh = pre_absent[(pre_absent$Absenteeism.time.in.hours <= 8),]
yuh2 = pre_absent[(pre_absent$Reason.for.absence!=0),]

677/697
absent = absent[(absent$Reason.for.absence!=0),]
#absent = absent[(absent$Reason.for.absence!=2),]
#absent = absent[(absent$Reason.for.absence!=3),]
#absent = absent[(absent$Reason.for.absence!=4),]
#absent = absent[(absent$Reason.for.absence!=5),]




split = .5
absent["randomDecider"] = runif(nrow(absent)) #gives a random value to each row in absent, for us to randomize on


ggplot(data=absent, aes(x=Reason.for.absence, y=Absenteeism.time.in.hours)) + geom_point()

train <- absent[(absent$randomDecider <= split), ]
test <- absent[(absent$randomDecider > split), ]


quant = rq(Absenteeism.time.in.hours~Reason.for.absence+Age+Day.of.the.week+Weight+Height+Body.mass.index, data = train)
summary(quant)


plot(Absenteeism.time.in.hours~Reason.for.absence, data=train)
abline(lm(Absenteeism.time.in.hours~Reason.for.absence, data=train), col="red")
abline(rq(Absenteeism.time.in.hours~Reason.for.absence, data=train, tau=.25), col="purple")
abline(rq(Absenteeism.time.in.hours~Reason.for.absence, data=train, tau=.5), col="blue")


rqP = predict(quant, test)

summary(absent$Education)



par(adj = .5)
plot(x = test$Reason.for.absence, y = rqP, col="blue", xlab = "Reason for Absence", ylab = "Hours Missing", main = "Hours Missing ~ Reason for Absence")

par(adj =1)
title(sub="    Predicted Values in Blue \n Actual Values in Red")
par(adj = .5)
points(x = test$Reason.for.absence, y = test$Absenteeism.time.in.hours, col="red")


ggplot(data=train, aes(quant$residuals)) +
  geom_histogram(binwidth = 1, color = "black", fill = "purple4") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Histogram for Quantile Regression Residuals")

sum(rqP <= 8)


plottingData = data.frame(test)
plottingData$pred = rqP
plottingData$TPraw = ifelse(plottingData$pred<=8 & plottingData$Absenteeism.time.in.hours<=8, 1, 0) #TEST LESS THAN 8 RESULT LESS THAN 8
plottingData$FPraw = ifelse(plottingData$pred<=8 & plottingData$Absenteeism.time.in.hours>8, 1, 0) #TEST LESS THAN 8 GREATER THAN 8
plottingData$FNraw = ifelse(plottingData$pred>8 & plottingData$Absenteeism.time.in.hours<=8, 1, 0) #TEST GREATER THAN 8 RESULT LESS THAN 8
plottingData$TNraw = ifelse(plottingData$pred>8 & plottingData$Absenteeism.time.in.hours>8, 1, 0) #TEST GREATER THAN 8 RESULT GREATER THAN 8
#plottingData = plottingData[c(1:21)]
TP = sum(plottingData$TPraw)
FP = sum(plottingData$FPraw)
FN = sum(plottingData$FNraw)
TN = sum(plottingData$TNraw)

a = data.frame(c(TP, FP), c(TN, FN))
a

ctable <- as.table(matrix(c(TP, FN, TN, FP), nrow = 2, byrow = TRUE))
fourfoldplot(ctable, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main = "Confusion Matrix")


#.971 .916 .978
sensitivity = TP/(TP+FN)

accuracy = (TP+TN)/(TP+FN+FP+TN)

#.05 .27 .08
specificty = TN/(TN+FP)


# ################################ OLD CODE BELOW HERE################################################
# #plottingData$Pred.hours[4]
# #plottingData$Absenteeism.time.in.hours[4]

# ##medList is the idea model based on medical reasons - are the workers killing themselves with work? are they healthy? 
# #medList = c("Absenteeism.time.in.hours~Reason.for.absence+Month.of.absence+Distance.from.Residence.to.Work+Work.load.Average.day+Day.of.the.week+Age+Height+Weight")
# #interestingly enough, there is almost no strong coorelation between the "social aspects" of the data and unresponsive absences
# #socialPredicter = glm(socialList, data=unjust)

# #socialList = c("Absenteeism.time.in.hours~Distance.from.Residence.to.Work+Service.time+Day.of.the.week+Son+Social.drinker+Pet")
# f = as.formula("Absenteeism.time.in.hours~Reason.for.absence + Disciplinary.failure + Day.of.the.week + Body.mass.index") #these give the most consistent Pr values, disciplinary.failure has a high std.er
# number3 = lm(f, data=train)
# summary(number3)

# #PLOTTING TRAINING DATA COMPARED TO MODEL RESIDUALS
# ggplot(data=train, aes(number3$residuals)) +
#   geom_histogram(binwidth = 1, color = "black", fill = "purple4") +
#   theme(panel.background = element_rect(fill = "white"),
#         axis.line.x=element_line(),
#         axis.line.y=element_line()) +
#   ggtitle("Histogram for Model Residuals")
# #

# #renames the variables in two_var_test
# two_var_test = data.frame(test$Reason.for.absence, test$Disciplinary.failure, test$Day.of.the.week, test$Age, test$Body.mass.index)
# names(two_var_test)[names(two_var_test)=="test.Reason.for.absence"] = "Reason.for.absence"
# names(two_var_test)[names(two_var_test)=="test.Disciplinary.failure"] = "Disciplinary.failure"
# names(two_var_test)[names(two_var_test)=="test.Day.of.the.week"] = "Day.of.the.week"
# names(two_var_test)[names(two_var_test)=="test.Age"] = "Age"
# names(two_var_test)[names(two_var_test)=="test.Body.mass.index"] = "Body.mass.index"

# two_var_test$predicted = predict(number3, two_var_test)

# ggplot(two_var_test, aes (Reason.for.absence, predicted)) + geom_point()

# plot(x = two_var_test$Reason.for.absence, y = two_var_test$predicted, col="blue")
# points(x = two_var_test$Reason.for.absence, y = test$Absenteeism.time.in.hours, col="red")
# #############################################################

# #polyTest2 = as.formula("Absenteeism.time.in.hours~Reason.for.absence + Month.of.absence + Day.of.the.week + Seasons + Transportation.expense + Distance.from.Residence.to.Work + Service.time + Age+Disciplinary.failure + Education + Son+Weight + Height + Body.mass.index +I(Reason.for.absence^2)") #these give the most consistent Pr values, disciplinary.failure has a high std.er

# polyTest = lm(Absenteeism.time.in.hours~Reason.for.absence + log(Reason.for.absence), data=train )
# polyPredict = predict(polyTest, test)

# ggplot(data=train, aes(polyTest$residuals)) +
#   geom_histogram(binwidth = 1, color = "black", fill = "purple4") +
#   theme(panel.background = element_rect(fill = "white"),
#         axis.line.x=element_line(),
#         axis.line.y=element_line()) +
#   ggtitle("Histogram for Model Residuals For NM2")

# plot(x = two_var_test$Reason.for.absence, y = polyPredict, col="blue", xlab = "Reason for Absence", ylab = "Hours Missing")
# points(x = two_var_test$Reason.for.absence, y = test$Absenteeism.time.in.hours, col="red")

# #################################################

# fullTest = as.formula("Absenteeism.time.in.hours~Reason.for.absence + Month.of.absence + Day.of.the.week + Seasons + Transportation.expense + Distance.from.Residence.to.Work + Service.time + Age + Work.load.Average.day + Hit.target + Disciplinary.failure + Education + Son + Social.drinker + Social.smoker + Pet + Weight + Height + Body.mass.index") #these give the most consistent Pr values, disciplinary.failure has a high std.er
# fullModel = lm(fullTest, data=train)

# fullPredict = predict(fullModel, test)

# plot(x = two_var_test$Reason.for.absence, y = fullPredict, col="blue", xlab = "Reason for Absence", ylab = "Hours Missing")
# points(x = two_var_test$Reason.for.absence, y = test$Absenteeism.time.in.hours, col="red")


# ggplot(data=train, aes(fullModel$residuals)) +
#   geom_histogram(binwidth = 1, color = "black", fill = "purple4") +
#   theme(panel.background = element_rect(fill = "white"),
#         axis.line.x=element_line(),
#         axis.line.y=element_line()) +
#   ggtitle("Histogram for Model Residuals For NM2")
# #

# nrow(pre_absent[(pre_absent$Reason.for.absence == 29),])

# #ggplot() + geom_bar(aes(ceiling(fullPredict), fill="red")) + geom_bar(aes(absent$Absenteeism.time.in.hours, fill="blue"))

# #saving the predicted values and plotting them against reason for absence


# test_set = data.frame(test)
# #test_set$inDistance ifelse(fullPredict <= 8 )

# test_set$Pred.hours = ifelse(fullPredict < 0, 0, fullPredict)
# test_set$closeness = ifelse(test_set$Pred.hours > test_set$Absenteeism.time.in.hours, 
#                             test_set$Absenteeism.time.in.hours / test_set$Pred.hours,
#                             test_set$Pred.hours / test_set$Absenteeism.time.in.hours)

# ########################################################
# d23 = (absent[(absent$Reason.for.absence == 23),])
# summary(d23$Absenteeism.time.in.hours)
# ggplot(data = d23, aes(x=Absenteeism.time.in.hours)) + geom_bar()

# #PREDICTED LINEAR PLOT OF DATA WITHOUT ZEROS
# ggplot(data = two_var_test, aes(x = Reason.for.absence, y = predicted)) 
# geom_point() +
#   stat_smooth(method = "lm", col = "red") +
#   theme(panel.background = element_rect(fill = "white"),
#         axis.line.x=element_line(),
#         axis.line.y=element_line()) +
#   ggtitle("Linear Model Fitted to Data")+ stat_smooth()

# ##### TEST SET LINEAR MODEL FITTED TO DATA
# ggplot(data = test, aes(x = Reason.for.absence, y = Absenteeism.time.in.hours)) +
#   geom_point() +
#   stat_smooth(method = "lm", col = "red") +
#   theme(panel.background = element_rect(fill = "white"),
#         axis.line.x=element_line(),
#         axis.line.y=element_line()) +
#   ggtitle("Linear Model Fitted to Data") + stat_smooth()


# ################# GENERAL PLOTS ###########################
# plot(train$pred~train$Reason.for.absence)
# ggplot(train, aes(x=pred, color=Absenteeism.time.in.hours)) + geom_density()
# ggplot(test, aes(x=pred, color=Absenteeism.time.in.hours)) + geom_density()

# absence_table = table(reason=test$Reason.for.absence, absent_hours=test$Absenteeism.time.in.hours)
# absence_table

# boxplot(test$Absenteeism.time.in.hours~test$Reason.for.absence, main = "TEST" )
# boxplot(absent$Absenteeism.time.in.hours~absent$Reason.for.absence, main = "REAL")


# #names(absent)
# # generic plots
# #plot(absent$Reason.for.absence, absent$Absenteeism.time.in.hours, xlab="reason for absence", ylab="how long absent")
# #plot(absent$Distance.from.Residence.to.Work, absent$Absenteeism.time.in.hours, xlab="distance from work", ylab="how long absent")
# #plot(absent$Day.of.the.week, absent$Absenteeism.time.in.hours, xlab="day of the week", ylab="how long absent")



# #how are the absences split?
# #doty = c(2,3,4,5,6)

# #161 absences on monday
# #154 on Tues
# #156 on Wed
# #125 on Thurs
# #144 on Friday
# #for(day in doty){
# # peepee = absent[(absent$Day.of.the.week == day),]
# #print(nrow(peepee))
# #}
