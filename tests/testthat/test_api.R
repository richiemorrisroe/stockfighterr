orderb <- get_orderbook("TESTEX", "FOOBAR")
test_that("test venue is available", {
    expect_equal(status_code(get_orderbook("TESTEX", "FOOBAR")), 200)})
ordbuy <- create_order(account = "EXB123456",
                                         venue = "TESTEX",
                                         stock = "FOOBAR",
                                         price = 2000,
                                         qty = 200,
                                         direction = "buy",
                                         ordertype = "limit")
test_that("create order returns a list",
          expect_is(ordbuy, "list" ))
apikey <- get_api_key("~/Dropbox/Code/Starfighter/apikey.txt")
test_that("get_api_key returns a character vector", {
    expect_is(apikey, "character")
    })
test_that("api key is length one", {
    expect_equal(length(apikey), 1)
})
placebuy <- place_order("TESTEX", "FOOBAR",
                        body = ordbuy, apikey=apikey)
placebuyp <- parse_response(placebuy)
test_that("parse_response returns a list from an order object",
          expect_is(parse_response(placebuy), "list"))
test_that("a placed order returns a totalFilled numeric greater or equal to 0", {expect_gte(placebuyp$totalFilled, 0)
          })
test_that("place_order returns a response", {
    expect_is(placebuy, "response")
})
test_that("get_quote returns a response",
          expect_is(get_quote("TESTEX", "FOOBAR"), "response"))
## response_test <- function(call) {
##     cl <- match.fun(call)
##     res <- eval(call)
##     ## test_that(paste(call, " returns a response", sep=""), {
##     expect_is(res, "response")
##     ## })
## }
test_that("get_orderbook returns a response", {
    expect_is(get_orderbook("TESTEX", "FOOBAR"), "response")
})
test_that("get_tickers returns a response", {
    expect_is(get_tickers("TESTEX"), "response")
})
placebuy.p <- parse_response(placebuy)
test_that("get_order_status returns a response", {
          expect_is(
              get_order_status(
                  placebuy.p$id,
                  "TESTEX",
                  "FOOBAR"), "response")})
test_that("cancel order returns a response", {
    expect_is(cancel_order(placebuy.p$id,
                           "TESTEX",
                           "FOOBAR"), "response")})
test_that("get_all_orders returns a response", {
    expect_is(get_all_orders("TESTEX", "EXB123456", apikey), "response")})
test_that("ioc orders are always closed after response", {
    ord <- create_order("EXB123456",
                        "TESTEX",
                        "FOOBAR",
                        1000,
                        100000,
                        direction = "buy",
                        ordertype="ioc")
    placed <- place_order("TESTEX", "FOOBAR", body=ord, apikey=apikey)
    placedp <- parse_response(placed)
    expect_equal(placedp$open, FALSE) })
