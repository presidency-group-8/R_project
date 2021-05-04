library(ggplot2)
library(plotrix)

stroke_data_set = read.csv("stroke-dataset.csv")

preprocessing = function(){
  # Remove ID column
  dataset = stroke_data_set[, 2:12]
  print(paste("Number of rows before cleaning --> ", nrow(dataset)))
  
  # Take first 800 rows to prevent over-fitting
  dataset = dataset[1:600, ]
  
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
  
  # Convert BMI to Numeric
  dataset$bmi = as.numeric(dataset$bmi)
  
  # Shuffle the data set
  set.seed(101)
  rows = sample(nrow(dataset))
  dataset = dataset[rows, ]
  
  return(dataset)
  
}

dataset = preprocessing()


# Pie to analyze Class labels
# stroke_1 = nrow(dataset[dataset$stroke == 1,])
# stroke_1_p = paste(round((stroke_1 / nrow(dataset)) * 100, 2), '%')
# stroke_0 = nrow(dataset[dataset$stroke == 0,])
# stroke_0_p = paste(round((stroke_0 / nrow(dataset)) * 100, 2), '%')
# labels = c("% of No Stroke", "% of Stroke")
# pie3D(x = c(stroke_0, stroke_1), labels = c(stroke_0_p, stroke_1_p),
#       height=0.2,explode = 0.1, col=c(5, 4), main = "Stroke Dataset Labels' Analysis")
# legend("topright", labels, fill=c(5, 4), cex=1)

# Scatter plot to analyze Age vs stroke
# ggplot(data=dataset, aes(x=stroke, y=age, color="green")) + geom_point()

# Bar to analyze Gender
# qplot(data=dataset, x=gender, main="Gender", bins=3)

# Bar to analyze Hypertension
# qplot(data=dataset, x=hyper, main="Hypertension", bins=3)

# Bar to analyze Heart Disease
# qplot(data=dataset, x=heart, main="Heart Disease", bins=3)

# Bar to analyze Marital
# qplot(data=dataset, x=marital, main="Ever Married", bins=4)

# Bar to analyze Work type
# qplot(data=dataset, x=worktype, main="Work Type", bins=4)

# Bar to analyze Residence type
# qplot(data=dataset, x=residence, main="Residence Type", bins=4)

# Bar to analyze Residence type
# qplot(data=dataset, x=smoking, main="Smoking Status", bins=5)
 
# Hist to analyze Glucose
# hist(dataset$glucose, xlab="Glucose Levels", ylab="Frequency", col="yellow", border="black", main="Histogram of Glucose levels")

# Hist to analyze BMI
# hist(dataset$bmi, xlab="BMI", ylab="Frequency", col="blue", border="black", main="Histogram of BMI")
