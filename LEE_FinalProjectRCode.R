#Lee, Gahyoung (Kylie)

install.packages("colorspace")
install.packages("gridExtra")
install.packages("ggcorrplot")
install.packages("tidyverse")
install.packages("corrplot")
install.packages("VIM")

library(dplyr)
library(plyr)
library(colorspace)
library(ggplot2)
library(gridExtra)
library(ggcorrplot)
library(tidyverse)
library(corrplot)
library(VIM)

#Load the data
getwd()
setwd("C:/Users/User/Desktop/ALY6000/WK6/archive (1)")
BP <- read.csv("framingham.csv", header = TRUE)

#Check the dataframe
dim(BP)
head(BP)

#Check the dataframe
summary(BP)
str(BP)

#BP_CR <- select(BP, c(age, sysBP, diaBP))
#corrplot(BP_CR)

#Check for missing value
table(is.na(BP)) #Total number of NA
colSums(is.na(BP)) #check NA by variables

aggr_plot <- aggr(BP, numbers=TRUE, sortVars=TRUE, cex.axis=0.6, gap=3) #proportion of NA 

#Fill out missing value#
#continuous value <- fill with mean, categorical value <- fill with mode
BP$cigsPerDay[is.na(BP$cigsPerDay)] <- mean(BP$cigsPerDay, na.rm=TRUE)
BP$glucose[is.na(BP$glucose)] <- mean(BP$glucose, na.rm=TRUE)
BP$BMI[is.na(BP$BMI)] <- mean(BP$BMI, na.rm=TRUE)
BP$heartRate[is.na(BP$heartRate)] <- mean(BP$heartRate, na.rm=TRUE)
BP$totChol[is.na(BP$totChol)] <- mean(BP$totChol, na.rm=TRUE)
#built-in mode fuction
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]}
BP$BPMeds[is.na(BP$BPMeds)] <- Mode(BP$BPMeds)
BP$education[is.na(BP$education)] <- Mode(BP$education)

colSums(is.na(BP))
sum(is.na(BP))

#Add column: HighBP = 1 when sysBP>=140 or diaBP>90 
BP$HighBP <- ifelse(BP$sysBP >= 140 | BP$diaBP > 90, 1, 0)

#correlation among variables #data type cannot be categorical
glm(HighBP~., data = BP)
CR <- cor(BP)
#corrplot(CR, method="number", insig = "blank",type="lower")
ggcorrplot(CR,hc.order = TRUE,type="lower",lab = TRUE,
           lab_size=3,method = "circle",
           title = "Correlogram of Variables",ggtheme = theme_light)

#create data frame
BP_N <- data.frame(BP)

#change data type;check categorical value
BP_N$male <- as.factor(BP_N$male)
BP_N$education <- as.factor(BP_N$education)
BP_N$currentSmoker <- as.factor(BP_N$currentSmoker)
BP_N$BPMeds <- as.factor(BP_N$BPMeds)
BP_N$prevalentStroke <- as.factor(BP_N$prevalentStroke)
BP_N$prevalentHyp <- as.factor(BP_N$prevalentHyp)
BP_N$diabetes <- as.factor(BP_N$diabetes)
BP_N$HighBP <- as.factor(BP_N$HighBP)

str(BP_N)

####Before removing outliner####
#Scatter Plot: between sysBP and diaBP
#Plot: distribution of sysBP and diaBP 
BP_N %>%
  ggplot(aes(sysBP, diaBP)) + geom_jitter(color="pink") + theme_minimal() + 
  labs(title="Distribution of Systolic & Diastolic Blood Pressure",x="sysBP(mmHg)",y="diaBP(mmHg)") + 
  stat_smooth(method = lm, se=FALSE, color="Red") +
  annotate(geom="text", x=250, y=75, label="r = 0.78", color="red") +
  geom_vline(aes(xintercept=120), linetype = "dashed", color='dark grey', size = 0) +
  geom_hline(aes(yintercept=80), linetype = "dashed", color='dark grey', size = 0)
#correlation
cor(BP_N$sysBP, BP_N$diaBP)
cor.test(BP_N$sysBP, BP_N$diaBP)

#Frequency check
summary(BP_N$prevalentHyp)
table(BP_N$prevalentHyp)/nrow(BP_N) #a frequency table

summary(BP_N$HighBP)
table(BP_N$HighBP)/nrow(BP_N)

#Bar plot: distribution of prevalence of hypertension
ggplot(BP_N, aes(x=prevalentHyp, fill=HighBP)) + geom_bar(width=0.4) + theme_minimal() +
  labs(title="Distribution of Prevalence and Current Status of Hypertension ",x="The number of Prevalence Hypertension",
       y="Current Status of High Blood Pressure") +
  geom_text(stat="count", aes(label=..count..), position = position_stack(vjust=0.5)) +
  scale_fill_discrete(name="", labels=c("False","True")) +
  scale_x_discrete(expand = c(1,0), labels = c("False","True"))
#add label in the bar: geom_text(stat="count", aes(label=..count..), position = position_stack(vjust=0.5))

#Scatter plot: sysBP and diaBP by Gender
ggplot(BP_N, aes(sysBP, diaBP, colour=male)) +
  geom_point() + stat_smooth(method=lm, se=FALSE) + scale_color_discrete(labels=c("female", "male")) +
  ggtitle("Systolic & Diastolic Blood Pressure by Gender")
#scatter plot: scale_color_discrete(labels, box plot: scale_fill_discrete
#when I want to divide a graph; facet_grid(. ~ male) 

#Scatter plot: sysBP and diaBP by smoking status
ggplot(data = BP_N) + geom_point(mapping = aes(sysBP, diaBP, color = currentSmoker)) + theme_light() +
  ggtitle("Systolic & Diastolic Blood Pressure by Smoking") + labs(x="Systolic BP", y="Diastolic BP")
#labs= change the name of axis

#Scatter plot: sysBP, diaBP and prevalence of HBP
ggplot(data = BP_N) + geom_point(mapping = aes(sysBP, diaBP, color = prevalentHyp)) + 
  labs(title="Distribution of Blood Pressure by Previous Hypertension Status", 
       x="sysBP(mmHg)",y="diaBP(mmHg)") + 
  scale_color_manual(values = c("pink", "turquoise3"), name = "status",
                     labels = c("0(no)","1(yes)")) +
  geom_vline(aes(xintercept=120), linetype = "dashed", color='dark grey', size = 0) +
  geom_hline(aes(yintercept=80), linetype = "dashed", color='dark grey', size = 0) + theme_minimal()

#ggplot theme: theme_classic(), theme_bw(), theme_minimal(), theme_light()

#add column: 4 levels of Blood pressure to BP_N
BP_N$BP_Level <- ifelse(BP_N$sysBP < 120 & BP_N$diaBP < 80, "Normal", 
                        ifelse(BP_N$sysBP >= 120 & BP_N$sysBP <= 129 & BP_N$diaBP < 80, "Elevated BP", 
                               ifelse((BP_N$sysBP >= 130 & BP_N$sysBP < 140) | (BP_N$diaBP >= 80 & BP_N$diaBP < 90), "Stage1 HP",
                                      ifelse(BP_N$sysBP >= 140 | BP_N$diaBP >= 90, "Stage2 HP",0))))
#Plot by level of BP
ggplot(data = BP_N) + geom_point(mapping = aes(sysBP, diaBP, color = BP_Level)) + 
  labs(title="Distribution of Blood Pressure by Level", 
       x="sysBP(mmHg)",y="diaBP(mmHg)") + 
  scale_color_manual(values = c("pink", "hotpink2", "red", "red4"), name = "",
                     breaks = c("Normal","Elevated BP","Stage1 HP","Stage2 HP")) +
  geom_vline(aes(xintercept=120), linetype = "dashed", color='dark grey', size = 0) +
  geom_hline(aes(yintercept=80), linetype = "dashed", color='dark grey', size = 0) + theme_minimal()


####check outlier####
#outlier is defined as a data point that is located outside the fences of the boxplot
BP_OL <- data.frame(BP_N)
boxplot(BP_OL$sysBP)
boxplot(BP_OL$sysBP)$stats 
table(is.na(BP_OL$sysBP))  

#assign NA in outlier
BP_OL$sysBP <- ifelse(BP_OL$sysBP < 83 | BP_OL$sysBP >  185, NA, BP_OL$sysBP)
table(is.na(BP_OL$sysBP)) # the number of NA
BP_OL %>% filter(is.na(sysBP)) #print only nA
BP_OL2 <- BP_OL %>% filter(!is.na(sysBP)) #remove NA from sysBP
boxplot(BP_OL2$sysBP)
boxplot(BP_OL2$sysBP)$stats 

boxplot(BP_OL$diaBP)
boxplot(BP_OL$diaBP)$stats 
table(is.na(BP_OL$diaBP))

#assign NA in outlier
BP_OL2$diaBP <- ifelse(BP_OL2$diaBP < 5 | BP_OL2$diaBP > 113, NA, BP_OL2$diaBP)
table(is.na(BP_OL2$diaBP)) # the number of NA
BP_OL2 %>% filter(is.na(diaBP)) #print only nA
BP_OL3 <- BP_OL2 %>% filter(!is.na(diaBP)) #remove NA from iaBP
boxplot(BP_OL3$diaBP)
boxplot(BP_OL3$diaBP)$stats 
#check difference with boxplot
BP_N %>% ggplot(aes(prevalentHyp, sysBP)) + geom_boxplot()
BP_OL3 %>% ggplot(aes(prevalentHyp, sysBP)) + geom_boxplot()

#Scatter plot: correlation between sysBP and diaBP 
BP_OL3 %>%
  ggplot(aes(sysBP, diaBP)) + geom_jitter(color="pink") + theme_minimal() + 
  labs(title="Distribution of Systolic & Diastolic Blood Pressure",x="sysBP(mmHg)",y="diaBP(mmHg)") + 
  stat_smooth(method = lm, se=FALSE, color="Red") +
  annotate(geom="text", x=180, y=75, label="r = 0.75", color="red") +
  geom_vline(aes(xintercept=120), linetype = "dashed", color='dark grey', size = 0) +
  geom_hline(aes(yintercept=80), linetype = "dashed", color='dark grey', size = 0)
#correlation
cor(BP_OL3$sysBP, BP_OL3$diaBP)
cor.test(BP_OL3$sysBP, BP_OL3$diaBP)

#Scatter plot: sysBP, diaBP and prevalence of HBP
ggplot(data = BP_OL3) + geom_point(mapping = aes(sysBP, diaBP, color = prevalentHyp)) + 
  labs(title="Distribution of Blood Pressure by Previous Hypertension Status", 
       x="sysBP(mmHg)",y="diaBP(mmHg)") + 
  scale_color_manual(values = c("pink", "turquoise3"), name = "status",
                     labels = c("0(no)","1(yes)")) +
  geom_vline(aes(xintercept=120), linetype = "dashed", color='dark grey', size = 0) +
  geom_hline(aes(yintercept=80), linetype = "dashed", color='dark grey', size = 0) + theme_minimal()

#Compute mean and median from BP_OL3
groupA <- filter(BP_OL3, BP_Level == "Normal")
mean(groupA$sysBP) 
mean(groupA$diaBP)
median(groupA$sysBP)
median(groupA$diaBP)
groupB <- filter(BP_OL3, BP_Level == "Elevated BP")
mean(groupB$sysBP) 
mean(groupB$diaBP)
median(groupB$sysBP)
median(groupB$diaBP)
groupC <- filter(BP_OL3, BP_Level == "Stage1 HP")
mean(groupC$sysBP) 
mean(groupC$diaBP)
median(groupC$sysBP)
median(groupC$diaBP)
groupD <- filter(BP_OL3, BP_Level == "Stage2 HP")
mean(groupD$sysBP) 
mean(groupD$diaBP)
median(groupD$sysBP)
median(groupD$diaBP)

#Scatter plot: sysBP, diaBP and the level of BP
ggplot(data = BP_OL3) + geom_point(mapping = aes(sysBP, diaBP, color = BP_Level)) + 
  labs(title="Distribution of Blood Pressure by Level", 
       x="sysBP(mmHg)",y="diaBP(mmHg)") + 
  scale_color_manual(values = c("pink", "hotpink2", "red", "red4"), name = "",
                     breaks = c("Normal","Elevated BP","Stage1 HP","Stage2 HP")) +
  geom_vline(aes(xintercept=120), linetype = "dashed", color='dark grey', size = 0) +
  geom_hline(aes(yintercept=80), linetype = "dashed", color='dark grey', size = 0) + theme_minimal()

#Scatter plot: sysBP and diaBP by Gender
ggplot(BP_OL3, aes(sysBP, diaBP, color=male)) +
  geom_point() + stat_smooth(method=lm, se=FALSE) + scale_color_discrete(labels=c("female", "male")) +
  ggtitle("Systolic & Diastolic Blood Pressure by Gender") + theme_minimal()

#Bar plot: Education and Level of BP
ggplot(BP_OL3, aes(x=education, fill=BP_Level)) + geom_bar(position="fill",width=0.5) +
  labs(title="Level of Blood Pressure by Education",x="Education",
       y="Level of Blood Pressure") +
  scale_x_discrete(expand = c(0.2,0), labels = c("X High school","High Shcool", "Vocational school","College")) + 
  scale_fill_discrete(expand = c(0.2,0), labels = c("Normal","Elevated BP","Stage1 HP","Stage2 HP")) + theme_minimal()

#Bar Plot: Current Smoking status and Level of BP
ggplot(BP_OL3, aes(x=currentSmoker, fill=BP_Level)) + geom_bar(position="fill",width=0.5) +
  labs(title="Level of Blood Pressure by Smoking Status",x="Current Smoker") +
  scale_x_discrete(expand = c(1,0), labels = c("False","True")) +
  scale_fill_discrete(expand = c(0.2,0), 
                      labels = c("Normal","Elevated BP","Stage1 HP","Stage2 HP")) + theme_minimal()
