orderb <- get_orderbook("TESTEX", "FOOBAR")
test_that("test venue is available", {
    expect_equal(httr::status_code(get_orderbook("TESTEX", "FOOBAR")), 200)})
ordbuy <- create_order(account = "EXB123456",
                                         venue = "TESTEX",
                                         stock = "FOOBAR",
                                         price = 2000,
                                         qty = 200,
                                         direction = "buy",
                       ordertype = "limit")

test_that("create order returns a list",
          expect_is(ordbuy, "list" ))





test_that("get_quote returns a response",
          expect_is(get_quote("TESTEX", "FOOBAR"), "response"))

test_that("get_orderbook returns a response", {
    expect_is(get_orderbook("TESTEX", "FOOBAR"), "response")
})

test_that("get_tickers returns a response", {
    expect_is(get_tickers("TESTEX"), "response")
})

placebuy.p <- parse_response(placebuy)





