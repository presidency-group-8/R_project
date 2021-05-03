dataset = read.csv("stroke-dataset.csv")
View(dataset)

preprocessing = function(){
  data = dataset[,c(2:11)]
  View(data)
}


training = function(){
  print("TRAINING")
}


testing = function(){
  print("TESTING")
}


predictor = function(name, age, gender, marital, work_type, residence_type, hypertension, heart_disease, smoking_type, glucose, bmi){
  name = gtkEntryGetText(name)
  age = gtkEntryGetText(age)
  gender = gtkComboBoxGetActiveText(gender)
  marital = gtkComboBoxGetActiveText(ever_married)
  work_type = gtkComboBoxGetActiveText(work_type)
  residence_type = gtkComboBoxGetActiveText(residence_type)
  hypertension = gtkComboBoxGetActiveText(hypertension)
  heart_disease = gtkComboBoxGetActiveText(heart_disease)
  smoking_type = gtkComboBoxGetActiveText(smoking_type)
  glucose = gtkEntryGetText(glucose)
  bmi = gtkEntryGetText(bmi)
  
  print(name)
  print(age)
  print(gender)
  print(marital)
  print(work_type)
  print(residence_type)
  print(hypertension)
  print(heart_disease)
  print(smoking_type)
  print(glucose)
  print(bmi)
}


preprocessing()

