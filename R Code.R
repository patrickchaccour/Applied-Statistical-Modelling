# Installing Libraries
install.packages(c( "ggplot2","corrplot", "car"))

# Loading libraries
library(ggplot2)
library(corrplot)
library(car)

# Loading the DataSet
student_data <- read.csv("C:\\Users\\patri\\Downloads\\archive (1)\\student-mat.csv")
print(head(student_data))

# Data Pre-processing Cleaning:
print(sum(is.na(student_data)))

# Exploratory Data Analysis:
print(summary(student_data[,c('G1', 'G2', 'G3', 'freetime', 
                              'address', 'absences', 'age', 'school')]))

# Histograms:
ggplot(student_data, aes(x=G1)) +
  geom_histogram(binwidth=1, fill="blue", alpha=0.7)
ggplot(student_data, aes(x=G2)) + 
  geom_histogram(binwidth=1, fill="blue", alpha=0.7)
ggplot(student_data, aes(x=G3)) + 
  geom_histogram(binwidth=1, fill="blue", alpha=0.7)

ggplot(student_data, aes(x=freetime)) + 
  geom_histogram(binwidth=1, fill="blue", alpha=0.7)

ggplot(student_data, aes(x=sex, y=G3)) + 
  geom_boxplot(fill="blue", alpha=0.7) + 
  theme_minimal()

ggplot(student_data, aes(x=address, y=G3)) + 
  geom_boxplot(fill="blue", alpha=0.7) + 
  theme_minimal()

ggplot(student_data, aes(x=school, y=G3)) + 
  geom_boxplot(fill="blue", alpha=0.7) + 
  theme_minimal()


#Statistical testing

### Hypothesis 1:

#1) Shapiro-Wilk test 

GP <- student_data$G3[student_data$school == "GP"]
MS <- student_data$G3[student_data$school == "MS"]

test_gp <- shapiro.test(GP)
print(paste("Shapiro-Wilk Test for GP school: W =", 
            test_gp$statistic, ", p-value =", test_gp$p.value))

test_ms <- shapiro.test(MS)
print(paste("Shapiro-Wilk Test for MS school: W =",
            test_ms$statistic, ", p-value =", test_ms$p.value))

#2) Bartlett Test
results <- bartlett.test(G3 ~ school, data=student_data)
print(results)

#3) ANOVA Test
anova_result <- aov(G3 ~ school, data=student_data)
summary(anova_result)

# Plotting the distributions for visualization:
boxplot(G3 ~ school, data=student_data, 
        xlab="school", ylab="G3")

### Hypothesis 2:

#1) Shapiro-Wilk test 

Male <- student_data$G3[student_data$sex == "M"]
Female <- student_data$G3[student_data$sex == "F"]

test_M <- shapiro.test(Male)
print(paste("Shapiro-Wilk Test for Male Students: W =", 
            test_M$statistic, ", p-value =", test_M$p.value))

test_F <- shapiro.test(Female)
print(paste("Shapiro-Wilk Test for MS school: W =",
            test_F$statistic, ", p-value =", test_F$p.value))


#2) Bartlett Test
results2 <- bartlett.test(G3 ~ sex, data=student_data)
print(results2)

#3) ANOVA Test
anova_result2 <- aov(G3 ~ sex, data=student_data)
summary(anova_result2)

# Visualization
qqnorm(Male, main="Male Students")
qqline(Male, col="blue")

qqnorm(Female, main="Female Students")
qqline(Female, col="red")


### Hypothesis 3:

#1) Shapiro-Wilk test 

Urban <- student_data$G3[student_data$address == "U"]
Rural <- student_data$G3[student_data$address == "R"]

test_U <- shapiro.test(Urban)
print(paste("Shapiro-Wilk Test for Urban address: W =", 
            test_U$statistic, ", p-value =", test_U$p.value))

test_R <- shapiro.test(Rural)
print(paste("Shapiro-Wilk Test for Rural address: W =",
            test_R$statistic, ", p-value =", test_R$p.value))


#2) Bartlett Test
results3 <- bartlett.test(G3 ~ address, data=student_data)
print(results3)

#3) ANOVA Test
anova_result2 <- aov(G3 ~ address, data=student_data)
summary(anova_result2)

# Plotting the distributions for visualization
hist(Urban, main="Urban Address",
     xlab="G3 Grades", col="lightblue")

hist(Rural, main="Rural Address",
     xlab="G3 Grades", col="lightgreen")


### Hypothesis 4:

#1) Shapiro-Wilk test 

unique(student_data$freetime)
freetime_levels <- unique(student_data$freetime)

for (ft in c(1, 2, 3, 4, 5)) {
  subset <- student_data$G3[student_data$freetime == ft]
  results3 <- shapiro.test(subset)
  cat("Shapiro-Wilk Test for freetime level",
      ft, ": W =", results3$statistic, ", p-value =", results3$p.value, "\n")
}

#2) Bartlett Test
results4 <- bartlett.test(G3 ~ freetime, data=student_data)
print(results4)

#3) ANOVA Test
anova_result3 <- aov(G3 ~ freetime, data=student_data)
summary(anova_result3)

# Visualization
boxplot(G3 ~ freetime, data=student_data, 
        xlab="freetime", ylab="G3")

plot(anova_result3, 1)




