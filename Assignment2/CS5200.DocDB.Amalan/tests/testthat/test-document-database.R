# TEST SCRIPT: test-document-database.R

library(testthat)

# Setup and tear down functions for tests
setup <- function() {
  setupDB() # Create directories
  # Dummy file creation
  writeLines("test", file.path(intakeDir, "Client1.010124.020124.xml"))
  writeLines("test", file.path(intakeDir, "Client2.010124.010124.json"))
  writeLines("test", file.path(intakeDir, "Bad.File.Name.txt"))  # Invalid
}

teardown <- function() {
  resetDB(rootDir) # Clean up after tests
  unlink(intakeDir, recursive = TRUE) # Remove intake directory
}


# Tests for checkFile()
test_that("checkFile - valid file name", {
  expect_true(checkFile("Client.110222.120222.xml"))
  expect_true(checkFile("Client.110222.110222.json"))
  expect_true(checkFile("Client.110222.110222.csv"))
})

test_that("checkFile - invalid file name - extension", {
  expect_false(checkFile("Client.110222.110222.bin"))
  expect_false(checkFile("Client.110222.110222")) # Missing extension
})


test_that("checkFile - invalid file name - dots", {
  expect_false(checkFile("Client.110222.110222"))
  expect_false(checkFile("Client110222.110222.xml"))
  expect_false(checkFile("Client.110222110222.xml"))
})

test_that("checkFile - invalid file name - date format", {
  expect_false(checkFile("Client.11022.110222.xml"))
  expect_false(checkFile("Client.110222.11022.xml"))
  expect_false(checkFile("Client.11222.110222.xml"))
})

test_that("checkFile - invalid file name - date order", {
  expect_false(checkFile("Client.120222.110222.xml"))
})

test_that("checkFile - invalid file name - invalid date", {
  expect_false(checkFile("Client.320124.010124.csv"))
})



# Tests for getCustomerName()
test_that("getCustomerName - extracts correctly", {
  expect_equal(getCustomerName("ClientA.110222.120222.xml"), "ClientA")
  expect_equal(getCustomerName("AnotherClient.110222.110222.json"),
               "AnotherClient")
})

# Tests for getFirstDay()
test_that("getFirstDay - extracts correctly", {
  expect_equal(getFirstDay("Client.110222.120222.xml"), "110222")
  expect_equal(getFirstDay("Client.010124.020224.xml"), "010124")
})


# Tests for getExtension()
test_that("getExtension - extracts correctly", {
  expect_equal(getExtension("Client.110222.120222.xml"), "xml")
  expect_equal(getExtension("Client.110222.120222.csv"), "csv")
})

# Tests for genDocPath()
test_that("genDocPath - generates correctly", {
  expect_equal(genDocPath("testRoot", "110222", "xml"),
               "testRoot/110222/xml")
  expect_equal(genDocPath("/path/to/root", "010124", "json"),
               "/path/to/root/010124/json")
})

# Tests for storeDoc()
test_that("storeDoc - successful storage", {
  setup()
  expect_true(storeDoc(intakeDir, "Client1.010124.020124.xml", rootDir))
  expect_true(file.exists(file.path(rootDir, "010124", "xml", "Client1")))
  teardown()
})

test_that("storeDoc - fails with invalid file", {
  setup()
  expect_false(storeDoc(intakeDir, "Bad.File.Name.txt", rootDir))
  teardown()
})

# Tests for storeAllDocs()
test_that("storeAllDocs - processes multiple files", {
  setup()
  storeAllDocs(intakeDir, rootDir)
  
  expect_true(file.exists(file.path(rootDir, "010124", "xml", "Client1")))
  expect_true(file.exists(file.path(rootDir, "010124", "json", "Client2")))
  expect_false(file.exists(file.path(
    intakeDir, "Client1.010124.020124.xml"
  ))) # Check if moved
  expect_false(file.exists(file.path(
    intakeDir, "Client2.010124.010124.json"
  ))) # Check if moved
  expect_true(file.exists(file.path(intakeDir, "Bad.File.Name.txt")))
  
  teardown()
})



# Tests for resetDB()
test_that("resetDB - clears the database", {
  setup()
  storeDoc(intakeDir, "Client1.010124.020124.xml", rootDir)
  resetDB(rootDir)
  expect_equal(length(list.files(rootDir)), 0)  # Check if empty
  teardown()
})