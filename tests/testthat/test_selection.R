library(rstudioapi)
library(raddins)
context('Test selection functionalities')

test_that('Extend document selection correctly', {
    expect_equal(extend_range(document_range(c(1, 1), c(1, 2)), 1),
                 list(document_range(c(2, 1), c(2, 2))))
    expect_equal(extend_range(document_range(c(1, 1), c(1, 2)), -1),
                 list(document_range(c(0, 1), c(0, 2))))
    expect_equal(extend_range(document_range(c(1, 3), c(1, 5)), 1),
                 list(document_range(c(2, 3), c(2, 5))))
    expect_equal(extend_range(document_range(c(1, 3), c(1, 5)), -1),
                 list(document_range(c(0, 3), c(0, 5))))
    expect_equal(extend_range(document_range(c(1, 3), c(1, 5)), 2),
                 list(document_range(c(2, 3), c(2, 5)),
                      document_range(c(3, 3), c(3, 5))))
})

range_top_two_lines = list(
    document_range(c(1, 1), c(1, 2)),
    document_range(c(2, 1), c(2, 2))
)

range_middle_two_lines = list(
    document_range(c(3, 1), c(3, 2)),
    document_range(c(4, 1), c(4, 2))
)

test_that('Shrink document selection correctly', {
    expect_equal(remove_range(range_top_two_lines, -1),
                 list(document_range(c(2, 1), c(2, 2))))
    expect_equal(remove_range(range_top_two_lines, 1),
                 list(document_range(c(1, 1), c(1, 2))))
    expect_equal(remove_range(range_middle_two_lines, -1),
                 list(document_range(c(4, 1), c(4, 2))))
    expect_equal(remove_range(range_middle_two_lines, 1),
                 list(document_range(c(3, 1), c(3, 2))))
})
