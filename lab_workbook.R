x <- 1.1
a <- 2.2
b <- 3.3
z <- ((x^a)^b)
print(z)
z <- x^(a^b)
print(z)
z <-3*(x^3)+2*(x^2)+1
print(z)


vector_b1 <- 1:5
vector_b2 <- rep(x=1:5,times=1:5)
print(vector_b2)

my_vec <- c(1,2,3,4,5)
rep(x=my_vec,times=my_vec)

vector_c1 <- 5:1
vector_c2 <- 1:5
vector_c3 <- rep(x=vector_c1,times=vector_c2)
print(vector_c3)

vector_c3 <- rep(x=5:1,times=1:5)
print(vector_c3)

vectorr <- runif(2)
names(vectorr) <- c("x","y")
print(vectorr)
r <- ((x^2) + (y^2))^(1/2)

print(r)

theta <- atan(y/x)
print(theta)

queue <- c("sheep", "fox", "owl", "ant")

queue <- c(queue,"snake")
print(queue)

queue <- queue[queue!="sheep"]
print(queue)

queue <- c("donkey", queue)
print(queue)

queue <- queue[queue!="snake"]
print(queue)

queue <- queue[queue!="owl"]
print(queue)

queue <- c(queue[queue!="ant"],"aphid","ant")
print(queue)

which(queue=="aphid")

vec_1 <- 1:100
vec_2 <- vec_1[(vec_1%%2)!=0&(vec_1%%3)!=0&(vec_1%%7)!=0]
print(vec_2)

vectorr <- runif(2)
names(vectorr) <- c("x","y")
print(vectorr)
r <- ((vectorr[["x"]]^2) + (vectorr[["y"]]^2))^(1/2)
print(r)
vectorr["x"]
vectorr[["x"]]
theta <- atan(vectorr[["y"]]/vectorr[["x"]])
print(theta)


#### lab notes ####
# matrices: ekements in a matrix are arranged in a 2-dimensional rectangular layout
# data frames: lists are vectors but can hold any type of data within each element, uses list() function, unlist() spits everything back out as a single atomic variable, but coerces everything
# you can interact with lists using element referincing somelist[x,y], to interact with individual elements within leach list, somelist[[2]][x,y]
# to transpose a matrix is to flip the rows and columns of the matrix
# spectral decomp (eigan): creation of your eigenvalue and eigenvectors
# remember to use typeof() to check the structure of your data

# make sure when you are creating your r markdown files to actually include code chunks


n_dims <- sample(3:10,1)
print(n_dims)

vec_a <- seq(from=1, to=(n_dims^2))
print(vec_a)

vec_a2 <- sample(x=vec_a)
print(vec_a2)

matrix_a <- matrix(data=vec_a2,nrow=n_dims)
print(matrix_a)

transposed_matrix_a <- t(matrix_a)
print(transposed_matrix_a)

sum(transposed_matrix_a[1, ])
sum(transposed_matrix_a[n_dims, ])

decomp <- eigen(transposed_matrix_a)
typeof(decomp$values)
typeof(decomp$vectors)

my_matrix <- matrix(runif(16),nrow=4)
my_logical_pre <- runif(100)
my_logical <- my_logical_pre<0.5
my_letters <- sample(x=letters, size=26)
my_list <- list(my_matrix, my_logical, my_letters)

my_list <- list(my_matrix <- matrix(runif(16),nrow=4),
                 my_logical <- runif(100)<0.5,
                 my_letters <- sample(x=letters, size=26))
new_list <- list(my_matrix[[2,2]], my_logical[2], my_letters[2])
typeof(new_list[[1]])
typeof(new_list[[2]])
typeof(new_list[[3]])

new_vec <- c(new_list[[1]],new_list[[2]],new_list[[3]])
print(new_vec)
typeof(new_vec)

d_frame <- data.frame(my_unis=runif(n=26,min=0, max=10),
                      my_letters=sample(x=LETTERS, size=26))

d_frame[sample(x=1:26,4),1] <- NA

which(!complete.cases(d_frame))

d_frame[order(d_frame$my_letters), ]

d_frame <- d_frame[order(d_frame$my_letters), ]

mean(d_frame$my_unis, na.rm = TRUE)



## Lab notes ##


# step 1: generate a fake dataset using your chosen dataset as a model
# step 2: generate relevant summary statistics (exploratory histogram, mean values, etc)
# step 3: conduct a statistical test- regression, ANOVA, etc
# step 4: Plot your data
# step 5: Use for loops to re-run your analysis, but with changed parameters or sample sizes
# step 6: store results from each loop and include a write-up


# make data
no_mito <- rnorm(6, mean=130, sd=17)
plus_mito <- rnorm(6, mean=230, sd=54)

# put data into a dataframe
Number_of_mitochondria <- c(no_mito,plus_mito)
Treatment_condition <- c(rep("no mito", length(no_mito)),rep("plus mito",length(plus_mito)))
mito_frame <- data.frame(Number_of_mitochondria, Treatment_condition)
print(mito_frame)


# find mean and standard deviation of no_mito
mean(mito_frame[1:length(no_mito),1])
sd(mito_frame[1:length(no_mito),1])
# find mean and standard deviation of plus_mito
mean(mito_frame[(length(no_mito)+1):(length(no_mito)+length(plus_mito)),1])
sd(mito_frame[(length(no_mito)+1):(length(no_mito)+length(plus_mito)),1])

# Perform a T-test
stat <- t.test(Number_of_mitochondria ~ Treatment_condition, data = mito_frame)
print(stat$p.value)

#Make a boxplot
boxplot(Number_of_mitochondria~Treatment_condition,data=mito_frame, main="Mito Transfer Data",
        xlab="Treatment", ylab="Number of Mitochondria")



replicates <- 3:6

for (i in replicates){
  no_mito <- rnorm(i, mean=130, sd=17)
  plus_mito <- rnorm(i, mean=230, sd=54)
  Number_of_mitochondria <- c(no_mito,plus_mito)
  Treatment_condition <- c(rep("no mito", length(no_mito)),rep("plus mito",length(plus_mito)))
  mito_frame <- data.frame(Number_of_mitochondria, Treatment_condition)
  stat <- t.test(Number_of_mitochondria ~ Treatment_condition, data = mito_frame)
  cat("Replicates=",i,"P-value=")
  print(stat$p.value)
}


mean_dif <- seq(from=10, to=100, by=10)
for (i in mean_dif){
  no_mito <- rnorm(6, mean=130, sd=17)
  plus_mito <- rnorm(6, mean=(130+i), sd=54)
  Number_of_mitochondria <- c(no_mito,plus_mito)
  Treatment_condition <- c(rep("no mito", length(no_mito)),rep("plus mito",length(plus_mito)))
  mito_frame <- data.frame(Number_of_mitochondria, Treatment_condition)
  stat <- t.test(Number_of_mitochondria ~ Treatment_condition, data = mito_frame)
  cat("Mean Difference=",i,"P-value=")
  print(stat$p.value)
}














library(tidyverse)

data(iris)
str(iris)

iris1 <- filter(iris, Species %in% c("virginica", "versicolor") & Sepal.Length > 6 & Sepal.Width>2.5)
str(iris1)




iris2 <- select(iris1, Species, Sepal.Length, Sepal.Width)
str(iris2)


iris3 <- arrange(iris2, by=desc(Sepal.Length))
head(iris3)

iris4 <- mutate(iris3, Sepal.Area=Sepal.Length*Sepal.Width)
str(iris4)


iris5 <- summarize(iris4, Av.Sepal.Length= mean(Sepal.Length), Av.Sepal.Width= mean(Sepal.Width), Sample.Size= n())
iris5

iris4%>%
  group_by(Species)%>%
  summarize(Av.Sepal.Length= mean(Sepal.Length), Av.Sepal.Width= mean(Sepal.Width), Sample.Size= n())


irisFinal <- iris%>%
  filter(Species %in% c("virginica", "versicolor") & Sepal.Length > 6 & Sepal.Width>2.5)%>%
  select(Species, Sepal.Length, Sepal.Width)%>%
  arrange(by=desc(Sepal.Length))%>%
  mutate(Sepal.Area=Sepal.Length*Sepal.Width)%>%
  group_by(Species)%>%
  summarize(Av.Sepal.Length= mean(Sepal.Length), Av.Sepal.Width= mean(Sepal.Width), Sample.Size= n())
irisFinal

iris%>%
  pivot_longer(cols = Sepal.Length:Petal.Width, names_to="Measure", values_to="Value", values_drop_na=T)


library(ggplot2) # for graphics
library(MASS) # for maximum likelihood estimation




# quick and dirty, a truncated normal distribution to work on the solution set

# z <- rnorm(n=3000,mean=0.2)
# z <- data.frame(1:36,z)
# names(z) <- list("ID","myVar")
# z <- z[z$myVar>0,]
#str(z)
#summary(z$myVar)

z <- read.table("Mito_Branches.csv",header=TRUE,sep=",")
names(z) <- list("Branches")
str(z)
summary(z)

p1 <- ggplot(data=z, aes(x=Branches, y=..density..)) +
  geom_histogram(color="grey60",fill="cornsilk",size=0.2)
print(p1)

p1 <-  p1 +  geom_density(linetype="dotted",size=0.75)
print(p1)

normPars <- fitdistr(z$Branches,"normal")
print(normPars)
str(normPars)
normPars$estimate["mean"] # note structure of getting a named attribute

fmeanML <- normPars$estimate["mean"]
sdML <- normPars$estimate["sd"]

xval <- seq(0,max(z$Branches),len=length(z$myVar))

stat <- stat_function(aes(x = xval, y = ..y..), fun = dnorm, colour="red", n = length(z$myVar), args = list(mean = meanML, sd = sdML))
p1 + stat
expoPars <- fitdistr(z$myVar,"exponential")
rateML <- expoPars$estimate["rate"]

stat2 <- stat_function(aes(x = xval, y = ..y..), fun = dexp, colour="blue", n = length(z$myVar), args = list(rate=rateML))
p1 + stat + stat2

stat3 <- stat_function(aes(x = xval, y = ..y..), fun = dunif, colour="darkgreen", n = length(z$myVar), args = list(min=min(z$myVar), max=max(z$myVar)))
p1 + stat + stat2 + stat3

gammaPars <- fitdistr(z$myVar,"gamma")
shapeML <- gammaPars$estimate["shape"]
rateML <- gammaPars$estimate["rate"]

stat4 <- stat_function(aes(x = xval, y = ..y..), fun = dgamma, colour="brown", n = length(z$myVar), args = list(shape=shapeML, rate=rateML))
p1 + stat + stat2 + stat3 + stat4

pSpecial <- ggplot(data=z, aes(x=myVar/(max(myVar + 0.1)), y=..density..)) +
  geom_histogram(color="grey60",fill="cornsilk",size=0.2) +
  xlim(c(0,1)) +
  geom_density(size=0.75,linetype="dotted")

betaPars <- fitdistr(x=z$myVar/max(z$myVar + 0.1),start=list(shape1=1,shape2=2),"beta")
shape1ML <- betaPars$estimate["shape1"]
shape2ML <- betaPars$estimate["shape2"]

statSpecial <- stat_function(aes(x = xval, y = ..y..), fun = dbeta, colour="orchid", n = length(z$myVar), args = list(shape1=shape1ML,shape2=shape2ML))
pSpecial + statSpecial

my_sim_data <- rgamma(n=36,shape=shapeML,rate=rateML)
my_sim_data <- data.frame(1:36,my_sim_data)
names(my_sim_data) <- list("ID","myVar")

p1 <- ggplot(data=my_sim_data, aes(x=myVar, y=..density..)) +
  geom_histogram(color="grey60",fill="palegreen2",size=0.2)
print(p1)

# simulate new data
my_sim_data <- rgamma(n=36,shape=shapeML,rate=rateML)
my_sim_data <- data.frame(1:36,my_sim_data)
names(my_sim_data) <- list("ID","myVar")

# make a histogram of the new data
p2 <- ggplot(data=my_sim_data, aes(x=myVar, y=..density..)) +
  geom_histogram(color="grey60",fill="palegreen2",size=0.2)

# add gamma probability density to the graph
gstat <- stat_function(aes(x = xval, y = ..y..), fun = dgamma, colour="magenta1", n = length(my_sim_data$myVar), args = list(shape=shapeML, rate=rateML))
p2 + gstat

library(rmdformats)

