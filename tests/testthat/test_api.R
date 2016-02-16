orderb <- get_orderbook("TESTEX", "FOOBAR")
test_that("test venue is available", {
    expect_equal(httr::status_code(get_orderbook("TESTEX", "FOOBAR")), 200)
})
test_that("get_quote returns a response",
          expect_is(get_quote("TESTEX", "FOOBAR"), "response"))

test_that("get_orderbook returns a response", {
    expect_is(get_orderbook("TESTEX", "FOOBAR"), "response")
})

test_that("get_tickers returns a response", {
    expect_is(get_tickers("TESTEX"), "response")
})
