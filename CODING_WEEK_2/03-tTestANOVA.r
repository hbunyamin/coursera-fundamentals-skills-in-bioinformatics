library(tidyverse, quietly = TRUE)
library(testthat, quietly = TRUE)

# We start with loading dataset 3 ("./data/DATA_SET_REFERENCE_3.csv") and performing QC
ds3 <- read.csv("./data/DATA_SET_REFERENCE_3.csv", row.names = 1, stringsAsFactors = TRUE)

glimpse(ds3)
# NO NAs...

# Compare variable "Exercise" between Planets (variable "Planet"), first by descriptive statistics 
# mean and standard deviation
ds3 %>%
    group_by(Planet) %>%
    summarise(
    count_planet = n(),
    mean_Exercise = mean(Exercise, na.rm = TRUE),
    sd_Exercise = sd(Exercise, na.rm = TRUE)
    )


# Now generate a boxplot showing the distribution of the variable "Exercise" between Planets (variable "Planet")
plt <- ggplot(ds3, aes(x = Planet, y = Exercise, fill = Planet)) +
geom_boxplot() +
theme_classic()
plt

# Since we hypothesize that there is a difference between the planets, we have to distinguish them
planet <- 'Venus'

# ggplot2 allows to easily fit a normal distribution on our data for a visual assessment
plt <- ds3 %>% 
filter(Planet == planet) %>% 
    ggplot() + 
    geom_histogram(aes(x = Exercise, y = ..density.., fill = Planet), binwidth = 5) +
    xlab("Exercrise") +
    stat_function(fun = dnorm, 
                  args = list(mean = mean(ds3$Exercise[ds3$Planet == planet]), 
                              sd = sd(ds3$Exercise[ds3$Planet == planet])))
plt

planet <- 'Earth'
plt <- ds3 %>% 
filter(Planet == planet) %>% 
    ggplot() + 
    geom_histogram(aes(x = Exercise, y = ..density.., fill = Planet), binwidth = 5) +
    xlab("Exercrise") +
    stat_function(fun = dnorm, 
                  args = list(mean = mean(ds3$Exercise[ds3$Planet == planet]), 
                              sd = sd(ds3$Exercise[ds3$Planet == planet])))
plt

options(repr.plot.width = 7, repr.plot.height = 4)
par(mfrow = c(1,2), bg = 'white')
# For Earth
qqnorm(ds3$Exercise[ds3$Planet == 'Earth'], pch = 1, frame = FALSE, main = 'Earth')
qqline(ds3$Exercise[ds3$Planet == 'Earth'], col = "steelblue", lwd = 2)

# For Venus
qqnorm(ds3$Exercise[ds3$Planet == 'Venus'], pch = 1, frame = FALSE, main = 'Venus')
qqline(ds3$Exercise[ds3$Planet == 'Venus'], col = "steelblue", lwd = 2)

# Perform a Bartlett test on the variable "Exercise" wrt. to the variable "Planet", and return the p-value

#ex_vs_planet.varTest.p <- 

# your code here
ex_vs_planet.varTest.p <- bartlett.test( Exercise ~ Planet, data=ds3 )$p.value

print(ex_vs_planet.varTest.p)



# Perform t-Test without assuming equal variance and store the p-value

#ex.vs.planet.ttest.pval <- 

# your code here
ex.vs.planet.ttest.pval <- t.test(Exercise ~ Planet, data=ds3)$p.value

print(ex.vs.planet.ttest.pval)



# Now perform a t-test assuming equal variance between both planets, again storing the p-Value

# ex.vs.planet.ttest.2.pval <- 

# your code here
ex.vs.planet.ttest.2.pval <- t.test( Exercise ~ Planet, data = ds3, var.equal = TRUE )$p.value


print(ex.vs.planet.ttest.2.pval)



# First look at the means of variable "Exercise" per planet (variable "Planet2").

ds3 %>%
group_by(Planet2) %>%
summarise(
    count_planets = n(),
    mean_time = mean(Exercise, na.rm = TRUE),
    sd_time = sd(Exercise, na.rm = TRUE)
)

# And generate the boxplots to get an intuition 
plt <- ggplot(ds3, aes(x = Planet2, y = Exercise, fill = Planet2)) +
geom_boxplot() +
theme_classic() +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plt

# Now perform an ANOVA for the variables Exercise and Planet2 and inspect the result

# anova_one_way <- 
# summary(anova_one_way)

# your code here
anova_one_way <- aov( Exercise ~ Planet2, data=ds3 )

summary(anova_one_way)



# Calculate and plot Tukey's Honest Significant Differences for our ANOVA of Exercise and Planet2

# ex.vs.planet.hsd <- 
# plot(ex.vs.planet.hsd)

# your code here
ex.vs.planet.hsd <- TukeyHSD(anova_one_way, ordered=TRUE)


print(ex.vs.planet.hsd)


