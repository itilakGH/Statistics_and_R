# Reading the MechaCar data csv file
MechaCars <- read.csv(file='MechaCar_mpg.csv', check.names = T, stringsAsFactors = F)
head(MechaCars)

# Running the linear regression test and creating its summary
lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD, data=MechaCars)
summary(lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD, data=MechaCars))

# to support our analysis and visualize it we are creating plots for a representative of each groups from the test
library(ggplot2)

length_mpg <- cor(MechaCars$vehicle.length, MechaCars$mpg)
plt <- ggplot(MechaCars, aes(x=vehicle.length, y=mpg))
plt + geom_point()

cor(MechaCars$vehicle.weight, MechaCars$mpg)
plt <- ggplot(MechaCars, aes(x=vehicle.weight, y=mpg))
plt + geom_point()

cor(MechaCars$AWD, MechaCars$mpg)
plt <- ggplot(MechaCars, aes(x=AWD, y=mpg))
plt + geom_point()

# Reading the Suspension Coil csv file
susp_coil <- read.csv(file='Suspension_Coil.csv', check.names = T, stringsAsFactors = F)
head(susp_coil)

# importing the tidyverse library
library(tidyverse)

# Reshaping the table to make it wide with columns for each Lot
wide_table <- susp_coil %>% spread(key="Manufacturing_Lot", value="PSI")
View(wide_table)
# Creating summary but this doesn't have st.dev and variance so we need something else
summary(wide_table)  

# installing packages for creating a summary which contains variance and standard deviation
install.packages("fBasics")
library(fBasics)
basicStats(wide_table)

# Dropping the Vehicle ID column
wide_table_clean <- wide_table[c(2,3,4)]
head(wide_table_clean)
# Creating the desired summary
basicStats(wide_table_clean)

# Running the one-sample T-Test (we assume that entire given data set is a sample
# and mu=1500 is the target)
t.test(susp_coil$PSI, mu=1500) 

# Running the one-sample T-Test for each Lot (each Lot given data set is a sample
# and mu=1500 is the target)
t.test(wide_table$Lot1, mu=1500)
t.test(wide_table$Lot2, mu=1500)
t.test(wide_table$Lot3, mu=1500)