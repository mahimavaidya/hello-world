# Setting the working directory
setwd("O:/R practice/My Work/Inttelipaat")

# Calling the necessary library
#install.packages("dplyr")
library(dplyr)     # Data manipulation
library(rlang)     # Dependency
library(tidyr)
library(MASS)      # Boston data set
library(corrplot)  # Correlation plotting

# Reading the data file
pd <- read.csv("pfizer.csv", header = T)
str(pd)
names(pd)
length(levels(pd$state))
unique(pd$state)
pd$org_indiv <- as.character(pd$org_indiv)
str(pd$org_indiv)

# Doctors in Florida who were paid $10,000 or more by Pfizer to run 'Expert-Led Forums'
FL_doc <- filter(pd , pd$state == "FL" & pd$total >= 10000 & pd$category == "Expert-Led Forums")
FL_doc$city
class(FL_doc)
# Extracting individual entry from subsetted data
subset(pd , pd$state == "FL" & pd$total >= 10000 & pd$category == "Expert-Led Forums")$first_name
filter(pd , pd$state == "FL" & pd$total >= 10000 & pd$category == "Expert-Led Forums")$first_name

# Sorting the list of the doctors from FL_doc in descending order by the payments received
FL_sort_total <- arrange(FL_doc, desc(total))

# Rename function: it can be used to rename the variables.
# Renaming 'first_plus' to 'first_plus1'
rd1 = rename(pd,first_plus1 = first_plus)
names(rd1)
# Changing the variable name using base functions
colnames(pd)[7] <- "category of expt"
names(pd)

# Arrange function: it can be used to sort the variables. The default sorting order of arrange() function is ascending and to sort a variable in descending order, use desc()
# sort data in ascending by multiple variables
sort_data1 <- arrange(pd, cash, total)
head(sort_data1,10)

# Sort one variable by descending order and other variable by ascending oder
sort_data2 <- arrange(pd, desc(total), other)
head(sort_data2,10)

# Doctors in California or New York who were paid $10,000 or more by Pfizer to run 'Expert-Led Forums'
ca_ny_doc <- pd %>% filter((state == "CA" | state == "NY") & total >= 10000 & category == "Expert-Led Forums")
head(ca_ny_doc)
# Sorting the above code in descending order
ca_ny_doc1 <- pd %>% filter((state == "CA" | state == "NY") & total >= 10000 & category == "Expert-Led Forums") %>%  arrange(desc(total))
head(ca_ny_doc1)
nrow(ca_ny_doc1)
# Doctors in states other than California who were paid $10,000 or more by Pfizer to run 'Expert-Led Forums'
not_ca_doc <- pd %>% filter(state != "CA" & total >= 10000 & category == "Expert-Led Forums")
head(not_ca_doc)
#write.csv(not_ca_doc, "Doctors not from CA.csv")

# 20 highest paid doctors across the four largest states (CA, TX, FL, NY)
ca_ny_tx_fl_doc <- pd %>% filter((state == "CA" | state == "NY" | state == "TX" | state == "FL") & category == "Professional Advising") %>% arrange(desc(total)) %>% head(20)
head(ca_ny_tx_fl_doc,3)

# Filter the data for all payments for running Expert-Led Forums or for Professional Advising,and arrange alphabetically by doctor (last name, then first name)
ea_payment <- pd %>% filter(category == "Expert-Led Forums" | category == "Professional Advising") %>% arrange(last_name, first_name)
head(ea_payment)

# Use pattern matching to filter text for Expert and Professional
expert_advice <- pd %>% filter(grepl("Expert|Professional", category))
head(expert_advice)

# Use pattern matching to filter text for other than Expert and Professional
not_expert_advice <- pd %>% filter(!grepl("Expert|Professional", category))
head(not_expert_advice)

# Append data frames
pd_combine <- bind_rows(expert_advice, not_expert_advice)
# To select a range of columns by name
#sd <- select(pd, org_indiv : state)
#select(pd, pd$org_indiv:pd$state)
example("select")
head(sd)

# Group and summarize data- Calculate the total payments, by state
payment_state <- pd %>% group_by(state) %>% summarize(sum = sum(total))
payment_state

# Group and summarize for multiple categories- Calculate total payments by state and category
summary_sc <- pd %>% group_by(state, category) %>% summarize(sum = sum(total), median = median(total), count = n())
head(summary_sc)

# Understanding the usage of mutate function
new_var <- mutate(pd, cash_percent = (cash/total)*100)

# IF ELSE AND NESTED LOOPS USAGE
# Create a new variable with IF_ELSE
summary(pd$total)
par(mfrow = c(2,2))
hist(pd$total, col = "green")
hist(sqrt(log(pd$total)), col = "red")
boxplot(pd$total, col = "blue")
boxplot(sqrt(log(pd$total)), col = "yellow")

pd$new_var <- if_else(pd$total < 1000, "low_value", "high_value", missing = "missing value")
head(pd)

# Nested IF ELSE
nd <- pd %>% mutate(newvar1 = if_else(is.na(pd$cash),"value is missing",
                                      if_else(pd$cash == 0,"cash is zero",
                                              if_else(pd$cash < 1000,"cash is low",
                                                      if_else(pd$cash >= 1000,"cash is high", "Others")))))
head(nd)
# check condition, "xyz"- if condition met, "abc"- if condition is not met


# Checking identicality of two variables
a <- 6
b <- 12/2
identical(3,3)
identical(a,b)
c <- data.frame(c(1,2,3,4,5,6,7,8,9,10,1,2))
colnames(c) <- "Randnum"
print(c)
length(c$Randnum)
length(unique(c$Randnum))

# Building the correlation matrix for the data set
attach(Boston)
head(Boston)
corelation <- cor(Boston)
cor(Boston$crim, Boston$zn)
'Interpretation of correlation magnitude
=> 0.75 - Strong correlation
Between 0.5 & 0.75 - Good correlation
=< 0.5 - Weak correlation'
# Using the correlation matrix as an input for corrplot
par(mfrow = c(2,2))    # Dividing plot window into many frames
corrplot(corelation)
corrplot(corelation, type = "lower", method = "square")
corrplot(corelation, type = "upper", method = "square")
corrplot(corelation, type = "lower", method = "circle")
corrplot(corelation, type = "lower", method = "number")

# Generate a randon data set
clinical.trial <- data.frame(patient = 1:100,
                             age = rnorm(100, mean = 60, sd = 6),
                             treatment = gl(2, 50, labels = c("Treatment", "Control")),
                             center = sample(paste("Center", LETTERS[1:5]), 100, replace = TRUE))
head(clinical.trial)
sort(names(clinical.trial))

library("pryr")
library()

# Data structures in R
# Vectors
# Preference given by R in a mixture- character, complex, numeric, logical
ad <- c(TRUE, 3, 4.5, 6i, "Mohan")
class(ad)
# Logical vector
lg <- c(TRUE, FALSE)
class(lg)
char <- c("1","0")
class(char)

# Matrix
# Creating a matrix
mat <- matrix(sample(1:10), nrow = 5, ncol = 10)
print(mat) ; class(mat)
mat1 <- matrix(c(1:50), nrow = 5, ncol = 10)
mat2 <- matrix(c(1:50), nrow = 5, ncol = 10, byrow = T)
# Transpose of matrix
mat3 <- t(mat1)
# Creating a diagonal matrix
diag(5,10)
# Checking the class of the matrix
class(mat)
# Converitng the matrix into a data frame
dat_mat <- as.data.frame(mat)
dat_mat
# Checking whether the conversion has been done or not
class(dat_mat)

# Building a data frame, assigning column names and row names and changing the column names
# Adding column names to the data frame
names(dat_mat) <- c("a","b","c","d","e","f","g","h","i","j")
names(dat_mat) <- letters[1:10]

# Array
arr <- 1:24
class(arr)
is.matrix(arr)
# Building a multi dimensional array haaving two rows, four columns and three matrices
arr1 <- array(1:24,dim = c(2,4,3))
arr1[1,4,2]

# Assigning row names to the data frame
rownames(dat_mat) <- c("A","B","C","D","E")
rownames(dat_mat) <- LETTERS[1:5]
# Changing the column name
names(dat_mat)[2] <- "Edureka"

# List
ls <- list(TRUE, 3, 4.5, 6i, "Mohan")
ls[[5]]
ls1 <- list(a = "Karthik", b = 1:5, data = head(iris))
print(ls1)
