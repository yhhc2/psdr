library(psdr)

test_that("FindHomogeneousWindows works", {
  
  col.one <- c(1, 2, 3, 4, 5)
  col.two <- c("a", "a", "a", "a", "a")
  col.three <- c(1, 1, 1, 1, 1)
  
  single.window.data <- data.frame(col.one, col.two, col.three)
  
  result1 <- FindHomogeneousWindows(single.window.data , c("col.one", "col.two"))
  
  result2 <- FindHomogeneousWindows(single.window.data , c("col.two", "col.three"))
  
  expect_equal(result1, FALSE)
  expect_equal(result2, TRUE)
})


test_that("GetHomogeneousWindows works", {
  
  window.name.column <- c(10, 10, 10, 20, 20, 20, 30, 30, 30, 30)
  col.two <- c("a", "a", "a", "a", "a", "a", "a", "a", "a", "a")
  col.three <- c(1, 1, 0, 1, 1, 1, 2, 2, 2, 2)
  
  multi.window.data <- data.frame(window.name.column, col.two, col.three)
  
  result <- GetHomogeneousWindows(multi.window.data, "window.name.column", c("col.two", "col.three"))
  
  expect_equal(length(result), 2)
  

})


test_that("GetSubsetOfWindows works", {
  
  window.name.column <- c(10, 10, 10, 20, 20, 20, 30, 30, 30, 30)
  col.two <- c("a", "a", "a", "a", "a", "a", "a", "a", "a", "a")
  col.three <- c(1, 1, 0, 1, 1, 1, 2, 2, 2, 2)
  
  multi.window.data <- data.frame(window.name.column, col.two, col.three)
  
  list.of.homogeneous.windows <- GetHomogeneousWindows(multi.window.data,
                                                       "window.name.column", c("col.two", "col.three"))
  
  subset.list.of.homogeneous.windows <- GetSubsetOfWindows(list.of.homogeneous.windows,
                                                           "col.three", "2")
  
  expect_equal(length(subset.list.of.homogeneous.windows), 1)
  
  expect_equal(unique(subset.list.of.homogeneous.windows[[1]]$col.three), 2)
  
  
})


test_that("CountWindows works", {
  
  window.name.column <- c(10, 10, 10, 20, 20, 20, 30, 30, 30, 30, 40, 40, 50, 50, 50, 50)
  col.two <- c("a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "a", "a", "a", "a")
  col.three <- c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 3, 3, 3, 3)
  
  multi.window.data <- data.frame(window.name.column, col.two, col.three)
  
  list.of.homogeneous.windows <- GetHomogeneousWindows(multi.window.data,
                                                       "window.name.column", c("col.two", "col.three"))
  
  actual_matrix <- CountWindows(list.of.homogeneous.windows, "col.two", "col.three",
                         c("a", "b"), c("1", "2", "3"))
  
  expected_matrix <- matrix(c(2, 1, 1, 0, 1, 0), nrow = 2, dimnames = list(c("a","b"), c("1","2","3")))
  
  expect_equal(actual_matrix, expected_matrix)
  
})


test_that("GetSubsetOfWindowsTwoLevels works", {
  
  #Example using a dataframe with 5 homogeneous windows.
  
  #Windows are homogeneous if looking at col.two and col.three values.
  window.name.column <- c(10, 10, 10, 20, 20, 20, 30, 30, 30, 30, 40, 40, 50, 50, 50, 50)
  col.two <- c("a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "a", "a", "a", "a")
  col.three <- c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 3, 3, 3, 3)
  
  multi.window.data <- data.frame(window.name.column, col.two, col.three)
  
  list.of.homogeneous.windows <- GetHomogeneousWindows(multi.window.data,
                                                       "window.name.column", c("col.two", "col.three"))
  
  result <- GetSubsetOfWindowsTwoLevels(list.of.homogeneous.windows, "col.two", "col.three",
                                        c("a"), c("1", "2"))
  
  #Should contain windows 10, 20, 30 because col.two is "a" and col.three can be "1" or "2"
  
  expect_equal(length(result), 3)
  
})




