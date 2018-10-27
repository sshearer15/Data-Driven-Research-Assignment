require(dplyr)
require(ggplot2)
require(tidyr)
library(MASS)
library(tree)
library("arm")

install.packages("graphics") 

data <- read.csv("Howe.csv")[-2]
head(data)

#Just look at state-level 
state.data <- data[which (data$GeoType == "State"),]

#Univerate regression between proportion who believe that humans are the cause and proportion who 
##support for funding renewables
##Intuitively, those who believe that humans are cause believe climate change is happening, so will exclude from model
human.and.fundrenewab <- lm(fundrenewables ~ human, state.data)

#Data viz showing the above, and by % of population that is worried, and population size
Dv1 <- ggplot(state.data, aes(x = human, y = fundrenewables)) +
  geom_point(aes(colour = -worried, size=TotalPop))+
  geom_smooth(method = "lm", colour = "red", show.legend = FALSE) +
  xlab("Believe humans are the cause (% of pop.)") +
  ylab("Support Funding Renewables (% of pop.)") +
  guides(colour=guide_legend(title = "Worried (% of pop.)")) +
  guides(size=guide_legend(title = "Pop.size")) +
  ggtitle("Relationship between public belief that humans are cause and their support for funding renewables")


###Label the highest point (state with greatest % who believe humans are cause, AND support funding renewables)
Dv1 + geom_text(data = state.data[state.data$fundrenewables == max(state.data$fundrenewables),], 
                aes(x = human-2, y = fundrenewables + 0.5, label=GeoName))
                

#Add a new binomial column (True/False) denoting whether or not the majority of the state supports funding renewables
state.data <- mutate(state.data, maj.regulate = ifelse(regulate > 75, 1,0))


##Create a logistic regression model to predict the likelihood that a state will "vote" to regulate based on
## the proportion of the population that: believes humans are the cause, are worried, timing, futuregen, discuss, trust climate scientists,
## and dev harm

#First tried to look at if any variables covaried before adding individually to model...
mycorr <- cor(data.matrix(state.data[,2:38]))
corrplot(mycorr,type='upper')

##Create a logistic regression model using all the aforementioned variables. Tried adding others at first, but the 
full.model <- glm(maj.regulate ~ human + worried + timing + futuregen + 
                    discuss + trustclimsciSST + devharm ,
                    data = state.data, family=binomial())

#Determine which variables are best predictors/statistically significant:
backward <- step(full.model, direction = 'backward')

#Turns out only human, futuregen, and discuss are
summary(backward)

##Run a simulation to determine prediction intervals (i.e. policy makers may be able to use polling on these questions
###to predict the likelihood of a bill/measure passing before even drafting legislation)
##Run a reasonably large simulation to get to as close to true values as possible (true for chosen model, that is)
rounds <- 1000
sim.glm <- sim(backward, rounds)


#Use the coefficients from the model to determine the probability that policy makers could get support from majority of 
##population based on varying levels of discussion; chose to hold the other variables in model (futuregen, human) at their medians.
##using another test-statistic would have yeilded different results.

get_reg_support_prob <- function(coefs, state.data) {
  logit <- coefs[1] + median(state.data$human)*coefs[2] +
    median(state.data$futuregen)*coefs[3] +
    state.data$discuss*coefs[4] 
  #probability below; we multiply coefficients then add
  return(exp(logit) / (1 + exp(logit)))
}

discuss_probs <-  matrix(0,nrow = rounds, ncol = nrow(state.data))

for (i in 1:rounds) {
  discuss_probs[i,] <- get_reg_support_prob(sim.glm@coef[i,], state.data)
}

#Calculate the confidence intervals for the probabilty of support for regulation at each value of discussion
discuss_probs_intervals <- apply(discuss_probs, 2, quantile, c(0.025, 0.975))

#Create a data frame showing the confidnce interval values for each value of "discuss"
discuss_probs_results<- data.frame(discuss=state.data$discuss, human=median(state.data$human),  
                                   futuregen=median(state.data$futuregen),
                                      qlower = discuss_probs_intervals[1,], 
                                      qupper = discuss_probs_intervals[2,])

#Reshape data for plotting
melted <- melt(discuss_probs_results[,c('discuss', 'qlower', 'qupper')], id.vars = 'discuss')

##Plot the confidence intervals 
plot1<- ggplot(melted, aes(x=discuss, y=value, color=variable)) + geom_point() + xlab('Discuss climate change regularly (% of pop.)') + 
  ylab('Prob. that majority support regulation') +
  theme_minimal() 


#Label axis and title/subtitle to clarify what is shown in graph 
plot1 + guides(color=guide_legend(title="Quantile")) + 
  ggtitle("Predicting Confidence Intervals for Public Support for Regulation") +
  labs(subtitle = "All variables except Discuss regularly held at their medians")










