#Week 4: dplyr package
installed.packages("tidyverse")
#Task: Write the function to get a dataset from Base R: Titanic
#and give the dataframe a new name of your choice
#(hint: you will want your data to be a dataframe. Use the function: as.data.frame(Titanic))
get_Titanic_data <- function(){
  Titanic_df <- as.data.frame(Titanic)
  return(Titanic_df)
}

Titanic_data <- get_Titanic_data()
#See the top rows of the data
#TASK: Write the function to see the top rows of the data
view_top_rows <- function(dataframe,n=6){
  top_rows <- head(dataframe,n)
  return(top_rows)
}

top_rows_Titanic_data <- view_top_rows(Titanic_data)

print(top_rows_Titanic_data)
#Install and call the package dplyr
#TASK: Write the functions to install and call dplyr
install_dplyr <- function() {
  install.packages("dplyr")
}
install_dplyr()

load_dplyr <- function() {
  library("dplyr")
}
load_dplyr()
#Let's just see the Survived and Sex columns
#Task: Write the function to 'select' the Survived and Sex columns 
#(hint: use the 'select' function)
select_survived_sex <- function(dataframe) {  
  selected_columns <- dataframe %>%
  select(Survived, Sex)

return(selected_columns)
}
survived_sex_data <- select_survived_sex(Titanic_data)
print(survived_sex_data)

#Let's name the dataset with just the two columns, Survived and Sex
#TASK: Write the function to save the two columns as one new dataset
#and give it a name
save_survived_sex_dataset <- function(dataframe, new_dataset_name) {
  selected_columns <- dataframe %>%
    select(Survived, Sex)
  assign(new_dataset_name, selected_columns, envir = .GlobalEnv)
}


save_survived_sex_dataset(Titanic_data, "survived_sex_dataset")

print(survived_sex_dataset)

#Let's get rid of the Sex column in the new dataset created above
#TASK: Write the function that deselects the sex column
#(hint: use the 'select' function to not select a -column)
deselect_sex_column <- function(dataframe) {
  dataframe_no_sex <- dataframe %>%
    select(-Sex)
  
  return(dataframe_no_sex)
}

survived_dataset <- deselect_sex_column(survived_sex_dataset)

print(survived_dataset)
#Let's rename a column name
#TASK: Write the function that renames 'Sex' to 'Gender'
rename_sex_to_gender <- function(dataframe) {
  renamed_dataframe <- dataframe %>%
    rename(Gender = Sex)
  
  return(renamed_dataframe)
}

gender_survived_dataset <- rename_sex_to_gender(survived_sex_dataset)

print(gender_survived_dataset)
#Let's make a new dataframe with the new column name
#TASK: Write the function that names a new dataset that includes the 'gender' column
save_gender_survived_dataset <- function(dataframe, new_dataset_name) {
  renamed_dataframe <- dataframe %>%
    rename(Gender = Sex)
  
  assign(new_dataset_name, renamed_dataframe, envir = .GlobalEnv)
}

save_gender_survived_dataset(survived_sex_dataset, "gender_survived_dataset")

print(gender_survived_dataset)
#Let's 'filter' just the males from our dataset
#TASK: Write the function that includes only rows that are 'male'
filter_males <- function(dataframe) {
  male_dataframe <- dataframe %>%
    filter(Gender == "male")
  
  return(male_dataframe)
}

male_gender_survived_dataset <- filter_males(gender_survived_dataset)

print(male_gender_survived_dataset)
#Let's 'arrange' our data by gender (not the data you just filtered)
#TASK: Write the function to group the data by gender (hint: arrange())
arrange_by_gender <- function(dataframe) {
  arranged_dataframe <- dataframe %>%
    arrange(Gender)
  
  return(arranged_dataframe)
}

arranged_gender_survived_dataset <- arrange_by_gender(gender_survived_dataset)

print(arranged_gender_survived_dataset)
#Let's see how many people were examined in the dataset (total the frequency in the original dataframe)
#TASK: Sum the Freq column
#TASK: After you run it, write the total here:_2201___
calculate_total_frequency <- function(dataframe) {
  total_frequency <- sum(dataframe$Freq)
  
  return(total_frequency)
}

total_frequency <- calculate_total_frequency(Titanic_data)

print(total_frequency)
#Since we have a males dataset, let's make a females dataset
#TASK: Write the function that includes only rows that are 'female'
filter_females <- function(dataframe) {
  female_dataframe <- dataframe %>%
    filter(Gender == "female")
  
  return(female_dataframe)
}

female_gender_survived_dataset <- filter_females(gender_survived_dataset)

print(female_gender_survived_dataset)
#And now let's join the males and females
#TASK: Write the function that joins the male and female rows 
#(hint: try using 'union' or 'bind_rows')
# Function to join the male and female rows
join_male_female <- function(male_dataframe, female_dataframe) {
  joined_dataframe <- bind_rows(male_dataframe, female_dataframe)
  
  return(joined_dataframe)
}

joined_gender_survived_dataset <- join_male_female(male_gender_survived_dataset, female_gender_survived_dataset)

print(joined_gender_survived_dataset)

#Optional Task: add any of the other functions 
# Function to filter, arrange, and select specific columns from a dataset
process_dataset <- function(dataframe, gender) {
  # Filter rows based on the specified gender
  filtered_dataframe <- dataframe %>%
    filter(Gender == gender)
  
  # Arrange rows by the Survived column
  arranged_dataframe <- filtered_dataframe %>%
    arrange(Survived)
  
  # Select the Survived, Gender, and Freq columns
  selected_dataframe <- arranged_dataframe %>%
    select(Survived, Gender, Freq)
  
  return(selected_dataframe)
}

# Call the function to process the gender_survived_dataset dataframe for males
processed_male_dataset <- process_dataset(gender_survived_dataset, "male")

# Print the first few rows of the processed male dataframe
head(processed_male_dataset)

# Call the function to process the gender_survived_dataset dataframe for females
processed_female_dataset <- process_dataset(gender_survived_dataset, "female")

# Print the processed female dataframe
print(processed_female_dataset)

#you learned about from the dplyr package

