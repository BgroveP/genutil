test_that("Matrix input is not numeric", {
  expect_error(
    create_diagonal_matrix(
      LETTERS[1:10]
    )
  )
})

test_that("Matrix input is empty", {
  expect_error(
    create_diagonal_matrix(
      c()
    )
  )
})
