# Import libraries
library(RGtk2)
source("./classifier.R")


# Validate the input values
evaluate = function(){
  # Access all the fields
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
  
  # EvaluateName
  if(name == ""){
    ok = gtkMessageDialog(window, "destroy-with-parent", "error",  "ok", "Please enter your name properly")
    if(ok$run() == GtkResponseType["ok"]) ok$destroy()
    return("error")
  }
  
  # Evaluate Age
  if(age == ""){
    ok = gtkMessageDialog(window, "destroy-with-parent", "error",  "ok", "Please enter your age properly")
    if(ok$run() == GtkResponseType["ok"]) ok$destroy()
    return("error")
  }
  age = as.integer(age)
  if (is.na(age) || age < 0 || age > 110){
    ok = gtkMessageDialog(window, "destroy-with-parent", "error",  "ok", "Please enter your age properly")
    if(ok$run() == GtkResponseType["ok"]) ok$destroy()
    return("error")
  }
  else{
    age = trunc(age)
  }
  
  # Evaluate Gender
  if(gender == "--Select--"){
    ok = gtkMessageDialog(window, "destroy-with-parent", "error",  "ok", "Please select your gender")
    if(ok$run() == GtkResponseType["ok"]) ok$destroy()
    return("error")
  }
  
  # Evaluate Marital status
  if(marital == "--Select--"){
    ok = gtkMessageDialog(window, "destroy-with-parent", "error",  "ok", "Please select your marital info")
    if(ok$run() == GtkResponseType["ok"]) ok$destroy()
    return("error")
  }
  
  # Evaluate Work type
  if(work_type == "--Select--"){
    ok = gtkMessageDialog(window, "destroy-with-parent", "error",  "ok", "Please select your work type")
    if(ok$run() == GtkResponseType["ok"]) ok$destroy()
    return("error")
  }
  
  # Evaluate Residency type
  if(residence_type == "--Select--"){
    ok = gtkMessageDialog(window, "destroy-with-parent", "error",  "ok", "Please select your name residency type")
    if(ok$run() == GtkResponseType["ok"]) ok$destroy()
    return("error")
  }
  
  # Evaluate Hypertension
  if(hypertension == "--Select--"){
    ok = gtkMessageDialog(window, "destroy-with-parent", "error",  "ok", "Please mention if you have hypertension or not")
    if(ok$run() == GtkResponseType["ok"]) ok$destroy()
    return("error")
  }
  
  # Evaluate Heart disease
  if(heart_disease == "--Select--"){
    ok = gtkMessageDialog(window, "destroy-with-parent", "error",  "ok", "Please mention if you have any heart disease or not")
    if(ok$run() == GtkResponseType["ok"]) ok$destroy()
    return("error")
  }
  
  # Evaluate Smoking status
  if(smoking_type == "--Select--"){
    ok = gtkMessageDialog(window, "destroy-with-parent", "error",  "ok", "Please mention your smoking status")
    if(ok$run() == GtkResponseType["ok"]) ok$destroy()
    return("error")
  }
  
  # Evaluate Glucose
  if(glucose == ""){
    ok = gtkMessageDialog(window, "destroy-with-parent", "error",  "ok", "Please enter your average glucose level properly")
    if(ok$run() == GtkResponseType["ok"]) ok$destroy()
    return("error")
  }
  glucose = as.numeric(glucose)
  if (is.na(glucose) || glucose < 0){
    ok = gtkMessageDialog(window, "destroy-with-parent", "error",  "ok", "Please enter a valid glucose level")
    if(ok$run() == GtkResponseType["ok"]) ok$destroy()
    return("error")
  }
  
  # Evaluate BMI
  if(bmi == ""){
    ok = gtkMessageDialog(window, "destroy-with-parent", "error",  "ok", "Please enter your BMI properly")
    if(ok$run() == GtkResponseType["ok"]) ok$destroy()
    return("error")
  }
  bmi = as.numeric(bmi)
  if (is.na(bmi) || bmi < 0){
    ok = gtkMessageDialog(window, "destroy-with-parent", "error",  "ok", "Please enter a valid BMI")
    if(ok$run() == GtkResponseType["ok"]) ok$destroy()
    return("error")
  }
  
  # Return no error if validation was successful
  return("no error")
}


# The RESET function
reset_function = function(){
  # Dialog to ask if the user is sure
  sure = gtkMessageDialog(window, "destroy-with-parent", "question",  "yes-no", "Are you sure you want to reset?")
  
  # If Sure then set default values to all input fields
  if (sure$run() == GtkResponseType["yes"]){
    sure$destroy()
    gtkEntrySetText(name, "")
    gtkEntrySetText(age, "")
    gender$setActive(0)
    ever_married$setActive(0)
    work_type$setActive(0)
    residence_type$setActive(0)
    hypertension$setActive(0)
    heart_disease$setActive(0)
    smoking_type$setActive(0)
    gtkEntrySetText(glucose, "")
    gtkEntrySetText(bmi, "")
  }
  
  # If not sure then close the dialog box
  else{
    sure$destroy()
  }
}


# The submit function
submit_function = function(){
  # Validate the inputs
  if(evaluate() == "error")
  {
    return(0)
  }
  
  # Display LOADING message to the user
  loading = gtkMessageDialog(window, "destroy-with-parent", "info",  "ok-cancel", "Loading Please Wait !")
  
  # Load the model
  if (loading$run() == GtkResponseType["ok"]){
    # Close the dialog box
    loading$destroy()  
    # Get prediction
    result = predictor(age, gender, ever_married, work_type, residence_type, hypertension, heart_disease, smoking_type, glucose, bmi)
    # Evaluate the result
    if(result<0.0) risk = "extremely low"
    if(result>0.0 && result<=0.13) risk = "low"
    if(result>0.13 && result<=0.38) risk = "moderate"
    if(result>0.38 && result<=0.75) risk = "high"
    if(result>0.75) risk = "extremely high"
    # Display the result
    result = paste("Dear ", gtkEntryGetText(name), " The risk of you having a stroke in the future is ", risk)
    show = gtkMessageDialog(window, "destroy-with-parent", "info",  "ok", result)
    if (show$run() == GtkResponseType["ok"])  show$destroy()
  }
  
  # If user clicks on cancel then stop loading the model
  else{
    loading$destroy()
    cancelled = gtkMessageDialog(window, "destroy-with-parent", "warning",  "ok", "Processing has been cancelled !")
    if (cancelled$run() == GtkResponseType["ok"])  cancelled$destroy()
  }
}


# The main window
window = gtkWindow("toplevel", show = FALSE)
# The title of the window
gtkWindowSetTitle(window, "Stroke Predictor")
# Size of the window
gtkWindowSetDefaultSize(window, 530, 500)

# A vertical box to enclose all widgets
box <- gtkVBox(TRUE, 4)

# Name and Age
name_and_age = gtkHBox(show = TRUE)
# Name
name_frame <- gtkFrame("Enter Your Name")
name = gtkEntryNew(show = TRUE)
name_frame$add(name)
name_and_age$packStart(name_frame, fill = TRUE)
# Age
age_frame <- gtkFrame("Enter Your Age")
age = gtkEntryNew(show = TRUE)
age_frame$add(age)
name_and_age$packStart(age_frame, fill = TRUE)
# Pack NAme and Age
box$packStart(name_and_age, fill = TRUE)

# Gender and Marital Status
gender_and_marital = gtkHBox(show = TRUE)
# Gender
gender_frame <- gtkFrame("Gender")
choiceList = c("--Select--", "Male", "Female")
gender <- gtkComboBoxNewText()
for (i in choiceList) gender$appendText(i)
gender$setActive(0)
gender_frame$add(gender)
gender_and_marital$packStart(gender_frame, fill = TRUE)
# Marital Status
ever_married_frame <- gtkFrame("Ever Married")
choiceList = c("--Select--", "Yes", "No")
  ever_married <- gtkComboBoxNewText()
for (i in choiceList) ever_married$appendText(i)
ever_married$setActive(0)
ever_married_frame$add(ever_married)
gender_and_marital$packStart(ever_married_frame, fill = TRUE)
# Pack Gender and Marital Status
box$packStart(gender_and_marital, fill = FALSE)

# Work Type
work_type_frame <- gtkFrame("Work Type")
choiceList = c("--Select--", "Private", "Government", "Self-employed")
work_type <- gtkComboBoxNewText()
for (i in choiceList) work_type$appendText(i)
work_type$setActive(0)
work_type_frame$add(work_type)
box$packStart(work_type_frame, fill = FALSE)

# Residence Type
residence_type_frame <- gtkFrame("Residence Type")
choiceList = c("--Select--", "Urban", "Rural")
residence_type <- gtkComboBoxNewText()
for (i in choiceList) residence_type$appendText(i)
residence_type$setActive(0)
residence_type_frame$add(residence_type)
box$packStart(residence_type_frame, fill = FALSE)

# Hypertension and Heart Disease
hypertension_and_heart = gtkHBox(show = TRUE)
# Hypertension
hypertension_frame <- gtkFrame("Hypertension")
choiceList = c("--Select--", "Yes", "No")
hypertension <- gtkComboBoxNewText()
for (i in choiceList) hypertension$appendText(i)
hypertension$setActive(0)
hypertension_frame$add(hypertension)
hypertension_and_heart$packStart(hypertension_frame, fill = TRUE)
# Heart Disease
heart_disease_frame <- gtkFrame("Heart Disease")
choiceList = c("--Select--", "Yes", "No")
heart_disease <- gtkComboBoxNewText()
for (i in choiceList) heart_disease$appendText(i)
heart_disease$setActive(0)
heart_disease_frame$add(heart_disease)
hypertension_and_heart$packStart(heart_disease_frame, fill = TRUE)
# Pack Hypertension and Heart Disease
box$packStart(hypertension_and_heart, fill = FALSE)

# Smoking Status
smoking_type_frame <- gtkFrame("Smoking Status")
choiceList = c("--Select--", "Currently Smokes", "Formerly Smoked", "Never Smoked")
smoking_type <- gtkComboBoxNewText()
for (i in choiceList) smoking_type$appendText(i)
smoking_type$setActive(0)
smoking_type_frame$add(smoking_type)
box$packStart(smoking_type_frame, fill = FALSE)

# Glucose and BMI
glucose_and_bmi = gtkHBox(show = TRUE)
# Glucose Level
glucose_frame <- gtkFrame("Enter Average Glucose Level")
glucose = gtkEntryNew(show = TRUE)
glucose_frame$add(glucose)
glucose_and_bmi$packStart(glucose_frame, fill = TRUE)
# BMI
bmi_frame <- gtkFrame("Enter BMI")
bmi = gtkEntryNew(show = TRUE)
bmi_frame$add(bmi)
glucose_and_bmi$packStart(bmi_frame, fill = TRUE)
# Pack Glucose and BMI
box$packStart(glucose_and_bmi, fill = TRUE)

# Buttons
buttons = gtkHBox(show = TRUE)
# Submit Button
submit = gtkButton("Submit")
gSignalConnect(submit, "clicked", function(e){
  submit_function()
})
buttons$packStart(submit, fill = TRUE)
# Submit Button
reset = gtkButton("Reset")
gSignalConnect(reset, "clicked", function(e){
    reset_function()
  }
)
buttons$packStart(reset, fill = TRUE)
# Pack Submit and Reset
box$packStart(buttons, fill = FALSE)

# Add all contents to the window
window$add(box)

# Show the window
window$show()
  

