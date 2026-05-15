# Import Excel files into R

library(here)

# Building function to import files:

Import_files <- function(file_name,tab_name = NULL,choose_directory = NULL){
  
  data_folder = here("data")
  
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
  
