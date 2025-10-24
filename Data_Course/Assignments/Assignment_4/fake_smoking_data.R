# Create your fake dataset
fake_data <- data.frame(
  Year = 2008:2018,
  Smoking_Prevalence = c(20.6, 20.1, 19.5, 19.0, 18.0, 17.5, 16.8, 15.7, 15.5, 14.0, 13.7),
  Campaign = c("No", "No", "No", "No", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
)


write.csv(fake_data, "fake_smoking_data.csv", row.names = FALSE)


read.csv("fake_smoking_data.csv")
 