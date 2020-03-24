#Read csv file and save it in a variable
mechCar_mpg_data <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)

#Replace spaces in the column names with underscore
names(mechCar_mpg_data) <- str_replace_all(names(mechCar_mpg_data), c(" " = "_"))

#scatter plot to see the relationship between vehicle_weight and mpg
ggplot(data = mechCar_mpg_data, aes_string(x = "mpg", y = "vehicle_weight")) + geom_point()

#scatter plot to see the relationship between vehicle_length and mpg
ggplot(data = mechCar_mpg_data, aes_string(x = "mpg", y = "vehicle_length")) + geom_point()

#scatter plot to see the relationship between ground_clearance and mpg
ggplot(data = mechCar_mpg_data, aes_string(x = "mpg", y = "ground_clearance")) + geom_point()

#scatter plot to see the relationship between spoiler angle and mpg
ggplot(data = mechCar_mpg_data, aes_string(x = "mpg", y = "spoiler_angle")) + geom_point()

#Plot density for the data to visualize the distribution
ggplot(data = mechCar_mpg_data, aes_string(x = "mpg")) + geom_density()

#Shaprio test to get the p-value of the distribution
shapiro.test(mechCar_mpg_data$mpg)

#Shapiro-Wilk normality test
#data:  mechCar_mpg_data$mpg
#W = 0.98536, p-value = 0.7869

#Correlation between the columns and mpg of the dataset
col_matrix <- as.matrix(mechCar_mpg_data[,c("vehicle_length","ground_clearance","vehicle_weight", "spoiler_angle", "AWD", "mpg")]) #convert data frame into numeric matrix
cor(col_matrix)

#Linear model between mpg and vehicle_length
lm(mpg ~ vehicle_length, mechCar_mpg_data)

# Call:
#   lm(formula = mpg ~ vehicle_length, data = mechCar_mpg_data)
# 
# Coefficients:
#   (Intercept)  vehicle_length  
# -25.062           4.673   

summary(lm(mpg ~ vehicle_length, mechCar_mpg_data))

# Call:
#   lm(formula = mpg ~ vehicle_length, data = mechCar_mpg_data)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -26.303  -7.160  -1.231   9.374  26.670 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    -25.0622    13.2960  -1.885   0.0655 .  
# vehicle_length   4.6733     0.8774   5.326 2.63e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 12.47 on 48 degrees of freedom
# Multiple R-squared:  0.3715,	Adjusted R-squared:  0.3584 
# F-statistic: 28.37 on 1 and 48 DF,  p-value: 2.632e-06

#Visualize the fitted line against the dataset
model <- lm(mpg ~ vehicle_length, mechCar_mpg_data)
yvals <- model$coefficients['vehicle_length']*mechCar_mpg_data$vehicle_length + model$coefficients['(Intercept)']
plt <- ggplot(mechCar_mpg_data,aes(x=vehicle_length,y=mpg)) #import dataset into ggplot2
plt + geom_point() + geom_line(aes(y=yvals), color = "red") #plot scatter and linear model

#Single Linear model between mpg and vehicle_weight
summary(lm(mpg ~ vehicle_weight, mechCar_mpg_data))

model <- lm(mpg ~ vehicle_weight, mechCar_mpg_data)
yvals <- model$coefficients['vehicle_weight']*mechCar_mpg_data$vehicle_weight + model$coefficients['(Intercept)']
plt <- ggplot(mechCar_mpg_data,aes(x=vehicle_weight,y=mpg)) #import dataset into ggplot2
plt + geom_point() + geom_line(aes(y=yvals), color = "red") 

#Single Linear model between mpg and spoiler_angle
summary(lm(mpg ~ spoiler_angle, mechCar_mpg_data))

model <- lm(mpg ~ spoiler_angle, mechCar_mpg_data)
yvals <- model$coefficients['spoiler_angle']*mechCar_mpg_data$spoiler_angle + model$coefficients['(Intercept)']
plt <- ggplot(mechCar_mpg_data,aes(x=spoiler_angle,y=mpg)) #import dataset into ggplot2
plt + geom_point() + geom_line(aes(y=yvals), color = "red") 


#Single Linear model between mpg and ground_clearance
summary(lm(mpg ~ ground_clearance, mechCar_mpg_data))

model <- lm(mpg ~ spoiler_angle, mechCar_mpg_data)
yvals <- model$coefficients['ground_clearance']*mechCar_mpg_data$ground_clearance + model$coefficients['(Intercept)']
plt <- ggplot(mechCar_mpg_data,aes(x=ground_clearance,y=mpg)) #import dataset into ggplot2
plt + geom_point() + geom_line(aes(y=yvals), color = "red") 


#Single Linear model between mpg and AWD
summary(lm(mpg ~ AWD, mechCar_mpg_data))

model <- lm(mpg ~ AWD, mechCar_mpg_data)
yvals <- model$coefficients['AWD']*mechCar_mpg_data$AWD + model$coefficients['(Intercept)']
plt <- ggplot(mechCar_mpg_data,aes(x=AWD,y=mpg)) #import dataset into ggplot2
plt + geom_point() + geom_line(aes(y=yvals), color = "red") 


#Multiple linear regression
summary(lm(mpg ~ vehicle_length + ground_clearance + vehicle_weight + spoiler_angle + AWD,data=mechCar_mpg_data))

# Call:
#   lm(formula = mpg ~ vehicle_length + ground_clearance + vehicle_weight + 
#        spoiler_angle + AWD, data = mechCar_mpg_data)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -19.4701  -4.4994  -0.0692   5.4433  18.5849 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      -1.040e+02  1.585e+01  -6.559 5.08e-08 ***
#   vehicle_length    6.267e+00  6.553e-01   9.563 2.60e-12 ***
#   ground_clearance  3.546e+00  5.412e-01   6.551 5.21e-08 ***
#   vehicle_weight    1.245e-03  6.890e-04   1.807   0.0776 .  
# spoiler_angle     6.877e-02  6.653e-02   1.034   0.3069    
# AWD              -3.411e+00  2.535e+00  -1.346   0.1852    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 8.774 on 44 degrees of freedom
# Multiple R-squared:  0.7149,	Adjusted R-squared:  0.6825 
# F-statistic: 22.07 on 5 and 44 DF,  p-value: 5.35e-11

#################################################

#Suspension Coil Statistics:

suspension_data <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)

#Summary
suspension_summary <- suspension_data %>% group_by(Manufacturing_Lot) %>% summarize(Mean_PSI=mean(PSI), Median_PSI=median(PSI), SD_PSI=sd(PSI), var_PSI = var(PSI)) #create summary table

#Sample population data with PSI 1500
suspension_1500_mean <- suspension_data %>% filter(PSI==1500)

# One sample t-test with the dataset and the standard theoritical mean 1500.
t.test(suspension_data$PSI, mu=1500)
