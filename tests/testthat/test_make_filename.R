context('make_filename')

test_that('filename pattern is correct',
    expect_equal(make_filename(2013), "accident_2013.csv.bz2")
)
