test_that("Matrix dimension is zero", {
    expect_error(
        create_empty_matrix(
            0
        )
    )
})


test_that("Matrix dimension is not integer", {
    expect_error(
        create_empty_matrix(
            1.1
        )
    )
})

test_that("Matrix dimension is not integer", {
    expect_error(
        create_empty_matrix(
            1,
            1.000012
        )
    )
})
