#different types of inputs in UI
ui <- fluidPage(
  titlePanel("Input Types Example"),
  
  textInput("name", "Enter your name:"),
  numericInput("age", "Your age:", value = 25, min = 0, max = 120),
  sliderInput("score", "Select your score:", min = 0, max = 100, value = 50),
  selectInput("city", "Choose city:", choices = c("New York", "Paris", "Tokyo")),
  checkboxInput("subscribe", "Subscribe to newsletter?", value = TRUE),
  checkboxGroupInput("colors", "Favorite Colors:", 
                     choices = c("Red", "Green", "Blue")),
  radioButtons("gender", "Gender:", choices = c("Male", "Female", "Other")),
  dateInput("dob", "Date of Birth:"),
  dateRangeInput("vacation", "Select vacation range:"),
  fileInput("resume", "Upload your resume:"),
  actionButton("submit", "Submit Form")
)

