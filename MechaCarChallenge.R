library(dplyr)
## Deliverable 1
Mecha_car <- read.csv('MechaCar_mpg.csv',stringsAsFactors = F) #read in dataset
# Create Linear Model
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=Mecha_car)
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=Mecha_car)) #generate summary statistics
# R-squared:  0.6825
# p-value: 5.35e-11

## Deliverable 2
Suspension_coil <- read.csv('Suspension_Coil.csv',stringsAsFactors = F) #read in dataset
total_summary <- Suspension_coil %>% summarize(Mean_PSI=mean(PSI),Median_PSI = median(PSI),Variance = var(PSI),SD = sd(PSI), .groups = 'keep') #create summary table with multiple columns
lot_summary <- Suspension_coil %>%  group_by(Manufacturing_Lot) %>% summarize(Mean_PSI=mean(PSI),Median_PSI = median(PSI),Variance = var(PSI),SD = sd(PSI), .groups = 'keep') #create summary table with multiple columns

## Deliverable 3
t.test(Suspension_coil$PSI, mu = 1500)

Lot1 = subset(Suspension_coil, Manufacturing_Lot == 'Lot1')
Lot2 = subset(Suspension_coil, Manufacturing_Lot == 'Lot2')
Lot3 = subset(Suspension_coil, Manufacturing_Lot == 'Lot3')

t.test(Lot1$PSI, mu = 1500) 
# p-value = 1
t.test(Lot2$PSI, mu = 1500)
# p-value = 0.6072
t.test(Lot3$PSI, mu = 1500)
# p-value = 0.04168