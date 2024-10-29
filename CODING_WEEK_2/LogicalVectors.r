# require(testthat, quietly = TRUE)

# Load dataset 1 "./data/DATA_SET_REFERENCE_1.csv". Make sure that 
# the row and column names are assigned correctly using the parameters of the 
# loading function.

# ds1 <- 

# your code here
ds1  <- read.csv("./data/DATA_SET_REFERENCE_1.csv", row.names = 1)

head(ds1)

# test_that("Make sure the row and column names are assigned as such.", {
#     expect_equal(dim(ds1), c(100,8))
# })



ds1[10:15,]

colnames(ds1)

# Extract and return rows 10 to 15 from the dataset.

# ds1.subset <- 

# your code here
ds1.subset  <- ds1[10:15,]


dim(ds1)
dim(ds1.subset)

test_that("Did you include all colmns of the table?", {
    expect_equal(ncol(ds1.subset), 8)
})

test_that("Are you sure you applied the index to the rows of the table?", {
    expect_equal(nrow(ds1.subset), 6)
})



summary(ds1["Color_house"])

# Make a table of the number of patients distinguishing by the color of their house.

# col.house.tab <- 

# your code here
col.house.tab <- table(ds1["Color_house"])

print(col.house.tab)

test_that("Did you include all patients in the table?", {
    expect_equal(dim(col.house.tab), 3)
})



# Create a logical index of patients who weigh more than 100 kgs and have a blood sugar level higher than 120.

# idx <- 

# your code here
idx  <- ds1$Weigth > 100 & ds1$Sugar_blood > 120

# check how many patients fulfill the criteria
table(idx)

ds1[idx,]

test_that("Are you sure you selected only patients with Weight higher than 100 kg?", {
    expect_false(any(ds1$Weigth[idx] <= 100))
})

test_that("Are you sure you selected only patients with blood sugar level higher than 120?", {
    expect_false(any(ds1$Sugar_blood[idx] <= 120))
})


# Create a table showing how many of the above selected patients have been at least 4 times in the hospital?

# frequent.hospitalized.table <- 

# your code here
frequent.hospitalized.table  <- table(ds1[idx,]$Hospital_times >= 4)

print(frequent.hospitalized.table)

test_that("Are you sure you have selected patients using the idx created above, before applying the table function?", {
    expect_equal(sum(frequent.hospitalized.table), 8)
})

test_that("Are you sure you have selected only patients with more than 4 hospitalizations?", {
    expect_true(frequent.hospitalized.table['FALSE'] == 3)
})



head(ds1)

row.names(ds1[ds1$Weigth > 100 & ds1$Sugar_blood > 120 & ds1$Hospital_times >= 4,])

# Extract and return the rownames of patients who weigh more than 100 kgs, have a blood sugar level 
# higher than 120, and have been to the hospital at least 4 times?

# patient.names <- 

# your code here
patient.names <- row.names(ds1[ds1$Weigth > 100 & ds1$Sugar_blood > 120 & ds1$Hospital_times >= 4,])

print(patient.names)

test_that("Are you sure you have selected the patient names?", {
    expect_true(grep('Patient', patient.names[1]) == 1)
})

