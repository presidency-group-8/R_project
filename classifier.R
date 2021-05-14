# Import libraries
library(rpart)
library(rpart.plot)
library(caTools)
library(ggplot2)

# Load the dataset
stroke_data_set = read.csv("stroke-dataset.csv")


# Data pre-processing
preprocessing = function(){
  # Remove ID column
  dataset = stroke_data_set[, 2:12]
  print(paste("Number of rows before cleaning --> ", nrow(dataset)))
  
  # Rename columns for simplicity
  colnames(dataset) = c("gender", "age", "hyper", "heart", "marital", "worktype", "residence", "glucose", "bmi", "smoking", "stroke")
  
  # Data cleaning
  dataset = subset(dataset, gender != "N/A")
  dataset = subset(dataset, gender != "Other")
  dataset = subset(dataset, age != "N/A")
  dataset = subset(dataset, hyper != "N/A")
  dataset = subset(dataset, heart != "N/A")
  dataset = subset(dataset, marital != "N/A")
  dataset = subset(dataset, worktype != "N/A")
  dataset = subset(dataset, worktype != "Never_worked")
  dataset = subset(dataset, worktype != "children")
  dataset = subset(dataset, residence != "N/A", )
  dataset = subset(dataset, glucose != "N/A")
  dataset = subset(dataset, bmi != "N/A")
  dataset = subset(dataset, smoking != "N/A")
  dataset = subset(dataset, smoking != "Unknown")
  print(paste("Number of rows after cleaning --> ", nrow(dataset)))

  # Take first 800 rows to prevent over-fitting
  dataset = dataset[1:400, ]
  
  # Convert BMI to Numeric
  dataset$bmi = as.numeric(dataset$bmi)
  
  # Display the data set
  print(head(dataset))
  
  # Return back the pre-processed data
  return(dataset)
  
}


# Training the model
training = function(train_set){
  # model = rpart(formula = stroke~., data = train_set)
  model = glm(formula = stroke ~ ., data = train_set)
  print("TRAINING DONE")
  return(model)
}


# Testing the model
testing = function(test_set, model){
  result = predict(model, test_set)
  result = (table(ActualValue=test_set$stroke, PreditedValue=result>=0.7))
  print("TESTING DONE")
  print("Confusion Matrix")
  
  # id = seq(1, nrow(test_set))
  # test_set$id = id
  # test_set$predictions = result
  # ggplot(data=test_set, aes(x=id, y=result)) + geom_point() + ggtitle("Prediction - No Stroke")
  
  return(result)
}


# Train and Test data
train_and_test = function(model_name, data_set){
  # Shuffle the data set
  set.seed(101)
  rows = sample(nrow(data_set))
  data_set = data_set[rows, ]
  
  # Split the data into train and test (75:25)
  sample_set = sample.split(data_set, SplitRatio  = 0.75)
  train_set = subset(data_set, sample_set == TRUE)
  test_set = subset(data_set, sample_set == FALSE)
  
  # Train and Test the data
  print(model_name)
  model = training(train_set)
  testing(test_set, model)
}


# Predict the output fir the given inout
predictor = function(age, gender, ever_married, work_type, residence_type, hypertension, heart_disease, smoking_type, glucose, bmi){
  # Fetch all data
  gender = gtkComboBoxGetActiveText(gender)
  marital = gtkComboBoxGetActiveText(ever_married)
  residence_type = gtkComboBoxGetActiveText(residence_type)
  age = gtkEntryGetText(age)
  iage = as.numeric(age)
  work_type = gtkComboBoxGetActiveText(work_type)
  if(work_type == "Government") iwork_type = "Govt_job"
  else iwork_type = work_type
  hypertension = gtkComboBoxGetActiveText(hypertension)
  if(hypertension == "Yes") ihypertension = 1
  if(hypertension == "No") ihypertension = 0
  heart_disease = gtkComboBoxGetActiveText(heart_disease)
  if(heart_disease == "Yes") iheart_disease = 1
  if(heart_disease == "No") iheart_disease = 0
  smoking_type = gtkComboBoxGetActiveText(smoking_type)
  if(smoking_type == "Currently Smokes") ismoking_type = "smokes"
  if(smoking_type == "Formerly Smoked") ismoking_type = "formerly smoked"
  if(smoking_type == "Never Smoked") ismoking_type = "never smoked"
  glucose = gtkEntryGetText(glucose)
  iglucose = as.numeric(glucose)
  bmi = gtkEntryGetText(bmi)
  ibmi = as.numeric(bmi)
    
  # Generate a dataframe using input data
  inp = data.frame('gender'=gender, 'age'=iage, 'hyper'=ihypertension, 'heart'=iheart_disease, 
                   'marital'=marital, 'worktype'=iwork_type, 'residence'=residence_type, 
                   'glucose'=iglucose, 'bmi'=ibmi, 'smoking'= ismoking_type)
  
  # Predict the output
  result = predict(model, inp)
  return(as.numeric(result[1][1]))
}


# Perform pre processing
data_set = preprocessing()

# load the model

# The DT model
# model = rpart(formula = stroke ~ ., data = data_set)
# rpart.plot(model)

# The Logistic Regresson
model = glm(formula = stroke ~ ., data = data_set)
