# Import Excel files into R

library(here)

# Building function to import files:

Import_files <- function(tab_name = NULL,choose_directory = NULL){
  

# 1. First part of the function, chooses directory to look for Excel files
  
  if(choose_directory == "data_folder") {
    
  data_folder_path = file.path(here("data"))  
    
  if (dir.exists(data_folder_path)) {
    
    return(data_folder_path)  
  }
  
  } else if (choose_directory == "data_cleansed") {
  
  data_cleansed_path = file.path(here("data_cleansed"))
    
  if (dir.exists(data_cleansed_path))  
    
  return(data_cleansed_path)
    
  } else { stop ("please provide your own directory")}
  
}

# Use function
Import_files(choose_directory = "data_folder")
Import_files(choose_directory = "data_cleansed")
Import_files(choose_directory = "my directory") # This will trigger error message
