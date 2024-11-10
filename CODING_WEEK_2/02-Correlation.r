require(testthat, quietly = TRUE)

# Load dataset 2 ("./data/DATA_SET_REFERENCE_2.csv")
# and make sure the quality control is performed.

ds2 <- read.csv("./data/DATA_SET_REFERENCE_2.csv", row.names = 1)
summary(ds2)

ds2 <- ds2[complete.cases(ds2),]

# Calculate and return the covariance between variable "Weight" and "LDL". Round the obtained value
# to three decimal places after the comma and assign it to the requested variable.

# Weight_LDL_cov <- 

# your code here
Weight_LDL_cov <- round(sum((ds2$Weight - mean(ds2$Weight)) * (ds2$LDL - mean(ds2$LDL))) / (nrow(ds2) -1),3)

# check the obtained value:
print(Weight_LDL_cov)

test_that("The covariance variable type needs to be 'numeric'", {
    expect_equal(class(Weight_LDL_cov), 'numeric')
})


# Now calculate and return the Pearson correlation to see for ourselves if correlation is 
# independent of the data scaling. Again, round the correlation value to three decimal places:

# Weight_LDL_cor <- 

# your code here

Weight_LDL_cor <- round( Weight_LDL_cov/ ( sd(ds2$Weight) * sd( ds2$LDL ) ), 3  ) 

# First let's make sure the variable has the correct type:
print(class(Weight_LDL_cor))

# and then check the actual value
print(Weight_LDL_cor)

test_that("The correlation variable type needs to be 'numeric'", {
    expect_equal(class(Weight_LDL_cor), 'numeric')
})

test_that("The correlation 'Weight_LDL_cor' must be in the range [-1,1]", {
    expect_true(Weight_LDL_cor >= -1 & Weight_LDL_cor <= 1)
})

# Calculate the p-value for the correlation between Weight and LDL, allowing either 
# positive or negative correlation as alternative hypothesis:

# weight.ldl.test.pval <- 

# your code here
temp  <- cor.test( ds2$Weight, ds2$LDL, alternative = "two.sided" ) 
weight.ldl.test.pval <- temp$p.value

# Let's check the type of the obtained value, remember we are looking for the p-value of the correlation value:
print(class(weight.ldl.test.pval))

# And now let's check the value itself:
print(weight.ldl.test.pval)

test_that("The correlation p-value needs to be 'numeric'", {
    expect_equal(class(weight.ldl.test.pval), 'numeric')
})

test_that("The correlation p-value 'weight.ldl.test.pval' must be in the range [0,1]", {
    expect_true(weight.ldl.test.pval >= 0 & weight.ldl.test.pval <= 1)
})


# Now calculate and return the p-value for the positive correlation between Weight and LDL

# weight.ldl.pos.cor.pval <- 


# your code here
temp  <- cor.test( ds2$Weight, ds2$LDL, alternative = 'greater' ) 
weight.ldl.pos.cor.pval <- temp$p.value

print(class(weight.ldl.pos.cor.pval))
print(weight.ldl.pos.cor.pval)

test_that("The correlation p-value 'weight.ldl.pos.cor.pval' needs to be 'numeric'", {
    expect_equal(class(weight.ldl.pos.cor.pval), 'numeric')
})

test_that("The correlation p-value 'weight.ldl.pos.cor.pval' must be in the range [0,1]", {
    expect_true(weight.ldl.pos.cor.pval >= 0 & weight.ldl.pos.cor.pval <= 1)
})


# And finally, calculate the p-value for the negative correlation between Weight and LDL

# weight.ldl.neg.cor.pval <- 


# your code here
temp  <- cor.test( ds2$Weight, ds2$LDL, alternative = 'less' ) 
weight.ldl.neg.cor.pval <- temp$p.value

print(class(weight.ldl.neg.cor.pval))
print(weight.ldl.neg.cor.pval)

test_that("The correlation p-value 'weight.ldl.neg.cor.pval' needs to be 'numeric'", {
    expect_equal(class(weight.ldl.neg.cor.pval), 'numeric')
})

test_that("The correlation p-value 'weight.ldl.neg.cor.pval' must be in the range [0,1]", {
    expect_true(weight.ldl.neg.cor.pval >= 0 & weight.ldl.neg.cor.pval <= 1)
})



