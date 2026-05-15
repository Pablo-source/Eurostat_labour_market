# Import Excel files into R

Import_files <- function(file_name,tab_name = NULL,choose_directory = NULL){
  
  data_folder = here("data")
  
  if(choose_directory == "data_folder") {
    
  data_folder_path = file.path(here("data"))  
    
  if (dir.exists(data_folder_path)) {
    
    return(data_folder_path)  
    
  }
    
    

    
  } else if (choose_directory == "data_cleansed") {
    
  return()
    
  }
  
  data_folder_path = 
  
  data_cleansed_path = 
  
  if(dir.exists(data_folder_path){
    return(data_folder_path)
  } else if (dir.exists(data_cleances)){
    return(data_cleansed)
  } else {
    stop("Provide your own directory")
  }
    
  }
  
  pa
}