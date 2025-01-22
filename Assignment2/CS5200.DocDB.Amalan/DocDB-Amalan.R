# -----------------------------------------------------------
# Author: Vicky Daniel Amalan
# Date: 2025-01-21
# -----------------------------------------------------------


#Global variables
rootDir <- "docDB"
intakeDir <- "docTemp"

# Function to setup the database environment
#' Set up the document database directories
#' Creates intake and root directories if they don't exist
#' @return None
setupDB <- function() {
  #Create root directory if it does not exist
  if (!dir.exists(rootDir))
  {
    dir.create(rootDir)
  }
  #Create intake directory if it does not exist
  if (!dir.exists(intakeDir))
  {
    dir.create(intakeDir)
  }
  
}

# Function to check the validity of a file based on its name
#' Check if a file name follows the required format
#' Format: ClientName.FirstDay.LastDay.ext where ext is xml, csv, or json
#' @param fileName The name of the file to check
#' @return TRUE if file follows format, FALSE otherwise
checkFile <- function(fileName) {
  # Split the file name by dots
  parts <- strsplit(fileName, "\\.")[[1]]
  
  # Check if we have exactly 4 parts (3 dots)
  if (length(parts) != 4) {
    cat(
      sprintf(
        "Error: The file name '%s' must contain exactly three dots separating four parts.\n",
        fileName
      )
    )
    return(FALSE)
  }
  
  # Check if the extension is valid
  validExts <- c("xml", "csv", "json")
  if (!parts[4] %in% validExts) {
    cat(
      sprintf(
        "Error: The file '%s' has an invalid extension. Supported extensions are: xml, csv, json.\n",
        fileName
      )
    )
    return(FALSE)
  }
  
  # Check dates format (DDMMYY) and order
  firstDay <- parts[2]
  lastDay <- parts[3]
  
  if (nchar(firstDay) != 6 || nchar(lastDay) != 6) {
    cat(
      sprintf(
        "Error: The file '%s' has dates that must be in the format DDMMYY with exactly 6 characters.\n",
        fileName
      )
    )
    return(FALSE)
  }
  
  # Convert dates to Date objects for comparison
  tryCatch({
    firstDate <- as.Date(firstDay, format = "%d%m%y")
    lastDate <- as.Date(lastDay, format = "%d%m%y")
    
    # Check whether dates are valid and first date is less than or equal to last date
    if (is.na(firstDate) || is.na(lastDate)) {
      cat(sprintf("Error: The file '%s' contains invalid dates.\n", fileName))
      return(FALSE)
    }
    
    if (firstDate > lastDate) {
      cat(
        sprintf(
          "Error: The file '%s' has a start date that is later than the end date.\n",
          fileName
        )
      )
      return(FALSE)
    }
    
    return(TRUE)
  }, error = function(e) {
    cat(
      sprintf(
        "Error: An exception occurred while processing the dates in the file '%s'. Please check the format.\n",
        fileName
      )
    )
    return(FALSE)
  })
}

#' Gets the customer name from the file name
#' @param fileName The name of the file to check
#' @return customer name extracted from the file name
getCustomerName <- function(fileName) {
  #extract the first part from the file name
  customerName <- strsplit(fileName, "\\.")[[1]][1]
  return (customerName)
}

# Function to get first day from file path
#' @param fileName The name of the file
#' @return First day (characters between first and second dot)
getFirstDay <- function(fileName) {
  strsplit(fileName, "\\.")[[1]][2]
}

# Function to get file extension from file path
#' Extract file extension from filename
#' @param fileName The name of the file
#' @return File extension
getExtension <- function(fileName) {
  strsplit(fileName, "\\.")[[1]][4]
}

# Function to generate document path for storage
#' Generate document path based on parameters
#' @param root Root directory
#' @param firstDay First day from file name
#' @param ext File extension
#' @return Full path for document storage
genDocPath <- function(root, firstDay, ext) {
  file.path(root, firstDay, ext)
}

# Function to store a single document
#' Store a single document in the appropriate location
#' @param intakeFolder Source folder for the file
#' @param file File name to process
#' @param docFolder Root folder for document storage
#' @return TRUE if copy was successful, FALSE otherwise
storeDoc <- function(intakeFolder, file, docFolder = "docDB") {
  if (!checkFile(file)) {
    return(FALSE)
  }
  
  # Get file components
  customerName <- getCustomerName(file)
  firstDay <- getFirstDay(file)
  ext <- getExtension(file)
  
  # Generate target path
  targetPath <- genDocPath(docFolder, firstDay, ext)
  
  # Create directories if they don't exist
  if (!dir.exists(targetPath)) {
    dir.create(targetPath, recursive = TRUE)
  }
  
  # Source and destination file paths
  sourcePath <- file.path(intakeFolder, file)
  if (!file.exists(sourcePath)) {
    cat(sprintf("\nThe intake file '%s' does not exist", sourcePath))
    return(FALSE)
  }
  destPath <- file.path(targetPath, customerName)
  
  # Copy file
  success <- tryCatch({
    file.copy(sourcePath, destPath, overwrite = TRUE)
    TRUE
  }, error = function(e) {
    cat(sprintf("Error copying file '%s': %s\n", file, e$message))
    FALSE
  })
  
  
  # Verify copy was successful
  if (success && file.exists(destPath) &&
      file.size(sourcePath) == file.size(destPath)) {
    cat(sprintf("\nThe storage of file '%s' is successful\n", file))
    return(TRUE)
  }
  cat(sprintf("The storage of file '%s' failed", file))
  return(FALSE)
}

#' Process all documents in the intake folder
#' @param intakeFolder Source folder for files
#' @param rootFolder Root folder for document storage
storeAllDocs <- function(intakeFolder, rootFolder) {
  # Get list of files in intake folder
  files <- list.files(intakeFolder)
  successCount <- 0
  failedFiles <- character()
  
  for (file in files) {
    #call individual doc store
    if (storeDoc(intakeFolder, file, rootFolder)) {
      # Remove original file after successful copy
      file.remove(file.path(intakeFolder, file))
      successCount <- successCount + 1
    } else {
      failedFiles <- c(failedFiles, file)
    }
  }
  
  cat(sprintf("\nSuccessfully processed %d files\n", successCount))
  if (length(failedFiles) > 0) {
    cat("These files were not processed:\n")
    cat(paste(failedFiles, collapse = "\n"))
    cat("\n")
  }
}

# Function to reset the db
#' Reset the document database to empty state
#' @param root Root directory to reset
resetDB <- function(root) {
  if (dir.exists(root)) {
    unlink(file.path(root, "*"), recursive = TRUE)
    cat(sprintf("Reset completed for %s\n", root))
  } else {
    cat(sprintf("Directory '%s' does not exist.\n", root))
  }
}


# Main function for running tests
main <- function() {
  cat("Trying to store a document without an existing directory")
  #Storing a document without setting up the db will be causing missing directory error
  storeDoc(intakeDir, "Client1.010124.020124.xml")  #Missing Directory
  
  setupDB()
  
  # Test file name checking
  cat("\nTesting file name checking:\n")
  testFiles <- c(
    "Client1.010124.020124.xml",
    # Valid file
    "Bad.File.Name.txt",
    # Invalid extension
    "Client2.010124.010124.json",
    # Valid file
    "Client3.320124.010124.csv",
    # Invalid date
    "Client4.020224.010324.xml",
    # Valid file with different dates
    "Client5.010125.010126.csv",
    # Valid file with different dates
    "Client6.150115.150115.xml",
    # Valid file with same start and end date
    "Client7.290115.290115.csv",
    # Valid file with same start and end date
    "Client8.310114.310114.xml"    # Valid file with earlier date
  )
  
  for (file in testFiles) {
    checkFile(file)
  }
  
  # Generate document path test
  cat(
    sprintf(
      "\nThe generated Doc Path for the given root, first day and format is: '%s'\n",
      genDocPath(rootDir, "261124", "xml")
    )
  )
  
  #test getCustomerName method
  customerName <- getCustomerName("Client6.150115.150115.xml")
  
  if (customerName == "Client6") {
    cat("The customer name is correctly extracted\n")
  } else{
    cat("The customer name is incorrect\n")
  }
  
  #test getFirstDay method
  firstDay <- getFirstDay("Client6.150115.150115.xml")
  
  if (firstDay == "150115") {
    cat("The First Day is correctly extracted\n")
  } else{
    cat("The First Day is incorrect\n")
  }
  
  #test getExtension method
  extension <- getExtension("Client6.150115.150115.xml")
  
  if (extension == "xml") {
    cat("The Extension is correctly extracted\n")
  } else{
    cat("The Extension is incorrect\n")
  }
  
  # Create test files in intake directory
  cat("\nCreating test files in intake directory:\n")
  writeLines("test", file.path(intakeDir, "Client1.010124.020124.xml"))
  writeLines("test", file.path(intakeDir, "Client2.010124.010124.json"))
  writeLines("test", file.path(intakeDir, "Bad.File.Name.txt"))
  writeLines("test",
             file.path(intakeDir, "Client1.010124.020124.invalid"))
  writeLines("test", file.path(intakeDir, "Client4.020224.010324.xml"))
  writeLines("test", file.path(intakeDir, "Client5.010125.010126.csv"))
  writeLines("test", file.path(intakeDir, "Client6.150115.150115.xml"))
  writeLines("test", file.path(intakeDir, "Client7.290115.290115.csv"))
  writeLines("test", file.path(intakeDir, "Client8.310114.310114.xml"))
  
  # Test storing single document
  cat("\nTesting single document storage:\n")
  storeDoc(intakeDir, "Client1.010124.020124.xml")  # Valid
  storeDoc(intakeDir, "Client2.010124.010124.json")  # Valid
  storeDoc(intakeDir, "Bad.File.Name.txt")           # Invalid extension
  storeDoc(intakeDir, "Client3.320124.010124.csv")   # Invalid date
  storeDoc(intakeDir, "Client4.020224.010324.xml")   # Valid
  storeDoc(intakeDir, "Client5.010125.010126.csv")   # Valid
  storeDoc(intakeDir, "Client6.150115.150115.xml")   # Valid with same start and end date
  storeDoc(intakeDir, "Client7.290115.290115.csv")   # Valid with same start and end date
  storeDoc(intakeDir, "Client8.310114.310114.xml")   # Valid with earlier date
  
  cat("Trying to store a document without an existing file")
  storeDoc(intakeDir, "Client20.310114.310114.xml")  #missing file
  
  # Test storing all documents
  cat("\nTesting document storage for all files:\n")
  storeAllDocs(intakeDir, rootDir)
  
  # Reset database
  cat("\nResetting database:\n")
  resetDB(rootDir)
  
  # Test if reset works by checking if directories are empty
  cat("\nChecking root directory after reset:\n")
  if (length(list.files(rootDir)) == 0) {
    cat("The database has been successfully reset.\n")
  } else {
    cat("Reset failed, root directory still contains files.\n")
  }
  
  # Test if intake directory is empty after processing
  cat("\nChecking intake directory after processing:\n")
  if (length(list.files(intakeDir)) == 0) {
    cat("The intake directory is empty after processing.\n")
  } else {
    cat("Intake directory still contains files.\n")
  }
}

main()