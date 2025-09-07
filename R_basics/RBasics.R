## Code to compute solution to quadratic equation of the form ax^2 + bx + c
## define the variables
a <- 2 
b <- -1
c <- -4

## now compute the solution
(-b + sqrt(b^2 - 4*a*c)) / (2*a)
(-b - sqrt(b^2 - 4*a*c)) / (2*a)

N <- 100
SUM <- (N *(N+1))/2
print(SUM)

x <- seq(1, N)
sum(x)

ans <- log(x=sqrt(100),base=10)
print(ans)

x <- 5 
print(log(exp(x)))

# loading the dslabs package and the murders dataset
library(dslabs)
data(murders)

# determining that the murders dataset is of the "data frame" class
class(murders)
# finding out more about the structure of the object
str(murders)
# showing the first 6 lines of the dataset
head(murders)

# using the accessor operator to obtain the population column
murders$population
# displaying the variable names in the murders dataset
names(murders)
# determining how many entries are in a vector
pop <- murders$population
length(pop)
# vectors can be of class numeric and character
class(pop)
class(murders$state)

# logical vectors are either TRUE or FALSE
z <- 3 == 2
z
class(z)

# factors are another type of class
class(murders$region)
# obtaining the levels of a factor
levels(murders$region)

# We may create vectors of class numeric or character with the concatenate function
codes <- c(380, 124, 818)
country <- c("italy", "canada", "egypt")

# We can also name the elements of a numeric vector
# Note that the two lines of code below have the same result
codes <- c(italy = 380, canada = 124, egypt = 818)
codes <- c("italy" = 380, "canada" = 124, "egypt" = 818)

# We can also name the elements of a numeric vector using the names() function
codes <- c(380, 124, 818)
country <- c("italy","canada","egypt")
names(codes) <- country

# Using square brackets is useful for subsetting to access specific elements of a vector
codes[2]
codes[c(1,3)]
codes[1:2]

# If the entries of a vector are named, they may be accessed by referring to their name
codes["canada"]
codes[c("egypt","italy")]



x <- c(31, 4, 15, 92, 65)
x
sort(x)    # puts elements in order

index <- order(x)    # returns index that will put x in order
x[index]    # rearranging by this index puts elements in order
order(x)

murders$state[1:10]
murders$abb[1:10]

index <- order(murders$total)
murders$abb[index]    # order abbreviations by total murders

max(murders$total)    # highest number of total murders
i_max <- which.max(murders$total)    # index with highest number of murders
murders$state[i_max]    # state name with highest number of total murders

x <- c(31, 4, 15, 92, 65)
x
rank(x)    # returns ranks (smallest to largest)

# The name of the state with the maximum population is found by doing the following
murders$state[which.max(murders$population)]

# how to obtain the murder rate
murder_rate <- murders$total / murders$population * 100000

# ordering the states by murder rate, in decreasing order
murders$state[order(murder_rate, decreasing=TRUE)]

# defining murder rate as before
murder_rate <- murders$total / murders$population * 100000
# creating a logical vector that specifies if the murder rate in that state is less than or equal to 0.71
index <- murder_rate <= 0.71
# determining which states have murder rates less than or equal to 0.71
murders$state[index]
# calculating how many states have a murder rate less than or equal to 0.71
sum(index)

# creating the two logical vectors representing our conditions
west <- murders$region == "West"
safe <- murder_rate <= 1
# defining an index and identifying states with both conditions true
index <- safe & west
murders$state[index]

x <- c(FALSE, TRUE, FALSE, TRUE, TRUE, FALSE)
which(x)    # returns indices that are TRUE

# to determine the murder rate in Massachusetts we may do the following
index <- which(murders$state == "Massachusetts")
index
murder_rate[index]

# to obtain the indices and subsequent murder rates of New York, Florida, Texas, we do:
index <- match(c("New York", "Florida", "Texas"), murders$state)
index
murders$state[index]
murder_rate[index]

x <- c("a", "b", "c", "d", "e")
y <- c("a", "d", "f")
y %in% x

# to see if Boston, Dakota, and Washington are states
c("Boston", "Dakota", "Washington") %in% murders$state


library(dplyr)
library(dslabs)
data("murders")

# a simple scatterplot of total murders versus population
x <- murders$population /10^6
y <- murders$total
plot(x, y)

# a histogram of murder rates
murders <- mutate(murders, rate = total / population * 100000)
hist(murders$rate)

# boxplots of murder rates by region
boxplot(rate~region, data = murders)
## Exercise2
library(dslabs)
data(murders)
str(murders)
head(murders)

names(murders)

a <- murders$abb
class(a)

b <- murders[,2]
b

identical(a,b)

region <- murders$region
length(levels(region))

args(table)

c <- table(murders$state, murders$region)
c

d <- table(murders$region, murders$state)
d

murders$population

region <- murders$region
value <- murders$total
region <- reorder(region, value, FUN = sum)
levels(region)

record <- list(name = "John Doe",student_id = 1234,
grades = c(95, 82, 91, 97, 93),final_grade = "A")

record
class(record)

record[["student_id"]]

## VECTORS
codes <- c(380, 124, 818)
codes

country <- c("italy", "canada", "egypt")
country

codes <- c("italy" = 380, "canada" = 124, "egypt" = 818)
codes
class(codes)
names(codes)

codes <- c(380, 124, 818)
country <- c("italy","canada","egypt")
names(codes) <- country
codes

## SEQUENCES
seq(1, 10)
seq(1, 10, 2)

1:10
class(1:10)

class(seq(1, 10, 0.5))

codes[2]
codes[c(1,3)]
codes[1:2]
codes["canada"]
codes[c("egypt","italy")]

##Exercise 3 
temp <- c(35,88,42,84,81,30)
temp

city <- c("Beijing","Lagos","Paris","Rio de Janeiro","San Juan","Toronto")
city

names(temp) <- city

temp[1:3]
temp[c(3,5)]

?seq
s <- seq(from=12,to=73)
s

s <- seq(from=1,to=100, by =2)
s

s <- seq(from=6,to=55,by=4/7)
s
length(s)

a <- seq(1, 10)
class(a)

class(a<-1L)

x <- c("1", "3", "5")
y <- as.numeric(x)
y
class(y)

##Exercise 4 
pop <- murders$population
pop_sorted <- sort(pop)
pop_sorted[1]

index_pop <- order(pop)
index_pop[1]

which.min(pop)

states <- murders$state
states[index_pop[1]]

state_pops <- data.frame(name = states, population = pop)
ranks=rank(pop)
my_df <- data.frame(name=states,pop_rank=ranks )
my_df

state_pops <- data.frame(name = states, population = pop)
rev=order(pop, decreasing=TRUE)

rank_rev=rank(-pop)
rank_rev

my_df_rev <- data.frame(name=states[rev],pop_rank=pop[rev])
my_df_rev

my_df_rev_rank <- data.frame(name=states,pop_rank=rank_rev )
my_df_rev_rank

data("na_example")  
str(na_example)

ind <- is.na(na_example)
sums <- sum(na_example, na.rm = TRUE)  
avg = sums/ length(na_example)
avg

murder_rate <- murders$total / murders$population * 100000
murder_rate_avg = mean(murder_rate)
murders$abb[order(murder_rate)]

temp <- c(35, 88, 42, 84, 81, 30)
temp <- 5/9 * (temp - 32) 
city <- c("Beijing", "Lagos", "Paris", "Rio de Janeiro", 
          "San Juan", "Toronto")
city_temps <- data.frame(name = city, temperature = temp)

city_temps

num <- rep(1,100)
denum <- c(1:100)
denum <- denum * denum

num
denum

fractions <- num/denum
sum(fractions)

#FILTERS
murder_rate <- murders$total / murders$population * 100000 
ind <- murder_rate <= 0.71
sum(ind)
murders$state[ind]

west <- murders$region == "West"
safe <- murder_rate <= 1
ind <- safe & west
murders$state[ind]

ind <- which(murders$state == "California")
murder_rate[ind]

ind <- match(c("New York", "Florida", "Texas"), murders$state)
murder_rate[ind]

match(c("New York", "Florida", "Texas"), murders$state)
which(murders$state%in%c("New York", "Florida", "Texas"))

#EXERCISE 5
ind <- murder_rate < 1

low <- murders$state[murder_rate < 1]
low

low2 <- murders$state[which(murder_rate < 1)]
low2

low_ne <- murders$state[which(murder_rate < 1 & region=='Northeast')]
low_ne

state_avg <- murders$state[which(murder_rate<murder_rate_avg)]
state_avg

ind_abb <- match(c("AK", "MI", "IA"), murders$abb)
abb_list = murders$state[ind_abb]
abb_list

abbs <- c("MA", "ME", "MI", "MO", "MU") 
abbs%in%murders$abb

!abbs%in%murders$abb

#PLOTTING
x <- murders$population / 10^6
y <- murders$total
plot(x, y)

with(murders, plot(population, total))

x <- with(murders, total / population * 100000)
hist(x)

murders$rate <- with(murders, total / population * 100000)
boxplot(rate~region, data = murders)

#exercise
library(dslabs)
data(murders)
population_in_millions <- log10(murders$population/10^6)
total_gun_murders <- murders$total
plot(population_in_millions, total_gun_murders)

hist_var <- data.frame(name=murders$state,pop=population_in_millions)
hist_var

x <- with(murders,log10(pop))
hist(x)

murders$rate <- with(murders, total / population * 100000)
boxplot(population_in_millions~region, data = murders)

#Section 1 QUIZ
a <- 2 
b <- -1
c <- -4

## now compute the solution
(-b + sqrt(b^2 - 4*a*c)) / (2*a)
(-b - sqrt(b^2 - 4*a*c)) / (2*a)

log(x=1024,base=4)

library(dslabs)
data(movielens)
tail(movielens)

class(movielens$title)
class(movielens$genres)

nlevels(movielens$genres)

name <- c("Mandi", "Amy", "Nicole", "Olivia")
distance <- c(0.8, 3.1, 2.8, 4.0)
time <- c(10, 30, 40, 50)
time_hrs <- time/60
speed <- distance/time_hrs
speed
time_hrs

x = c(1,5,6)
x <- 3,"b",8
x <- c(4,"seven",9)
x

library(dslabs)
data(olive)
head(olive)

plot(olive$palmitic/100 , olive$palmitoleic )

x <- with(olive, olive$eicosenoic)
hist(x)

boxplot(olive$palmitic ~region, data = olive)

ind <- which.min(murder_rate)
if(murder_rate[ind] < 0.5){
  print(murders$state[ind]) 
} else{
  print("No state has murder rate that low")
}
if(murder_rate[ind] < 0.25){
  print(murders$state[ind]) 
} else{
  print("No state has a murder rate that low.")
}

a <- 0
ifelse(a > 0, 1/a, NA)

a <- c(0, 1, 2, -4, 5)
result <- ifelse(a > 0, 1/a, NA)
result

data(na_example)
no_nas <- ifelse(is.na(na_example), 0, na_example) 
sum(is.na(no_nas))

z <- c(TRUE, TRUE, FALSE)
any(z)
all(z)
