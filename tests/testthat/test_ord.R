ordbuy <- create_order(account = "EXB123456",
                                         venue = "TESTEX",
                                         stock = "FOOBAR",
                                         price = 2000,
                                         qty = 200,
                                         direction = "buy",
                       ordertype = "limit")

test_that("create order returns a list",
          expect_is(ordbuy, "list" ))
test_that("place_order returns a response", {
    skip_on_cran("This test requires an API key")
placebuy <- place_order("TESTEX", "FOOBAR",
                        body = ordbuy, apikey=apikey)
    expect_is(placebuy, "response")})


test_that("get_all_orders returns a response", {
    skip_on_cran("This test requires an API key")
    expect_is(get_all_orders("TESTEX", "EXB123456", apikey), "response")})


test_that("get_order_status returns a response", {
    skip_on_cran("This test requires an API key")
          expect_is(
              get_order_status(
                  placebuy.p$id,
                  "TESTEX",
                  "FOOBAR"), "response")})


test_that("cancel order returns a response", {
    skip_on_cran("This test requires an API key")
    expect_is(cancel_order(placebuy.p$id,
                           "TESTEX",
                           "FOOBAR"), "response")})


test_that("ioc orders are always closed after response", {
    skip_on_cran("This test requires an API key")
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


test_that("parse_response returns a list from an order object", {
    skip_on_cran("This test requires an API key")
          expect_is(parse_response(placebuy), "list")})
skip_on_cran("This test requires an API key") 
test_that(
    skip_on_cran("This test requires an API key")
    "a placed order returns a totalFilled numeric greater than 0", {
        expect_gte(placebuyp$totalFilled, 0)
    })

test_that("place_order returns a response", {
    skip_on_cran("This test requires an API key")
    expect_is(placebuy, "response")
})

apikey <- get_api_key("~/Dropbox/Code/Starfighter/apikey.txt")
test_that("get_api_key returns a character vector", {
    skip_on_cran("This test requires an API key")
    expect_is(apikey, "character")
})
skip_on_cran("This test requires an API key")
test_that("api key is length one", {
    skip_on_cran("This test requires an API key")
    expect_equal(length(apikey), 1)
})
