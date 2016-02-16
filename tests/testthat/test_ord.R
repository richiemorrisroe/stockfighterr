skip_on_cran("This test requires an API key")
test_that("get_all_orders returns a response", {
    expect_is(get_all_orders("TESTEX", "EXB123456", apikey), "response")})
skip_on_cran("This test requires an API key")
test_that("get_order_status returns a response", {
          expect_is(
              get_order_status(
                  placebuy.p$id,
                  "TESTEX",
                  "FOOBAR"), "response")})
skip_on_cran("This test requires an API key")
test_that("cancel order returns a response", {
    expect_is(cancel_order(placebuy.p$id,
                           "TESTEX",
                           "FOOBAR"), "response")})
skip_on_cran("This test requires an API key")
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
skip_on_cran("This test requires an API key")
placebuy <- place_order("TESTEX", "FOOBAR",
                        body = ordbuy, apikey=apikey)
skip_on_cran("This test requires an API key")
test_that("parse_response returns a list from an order object",
          expect_is(parse_response(placebuy), "list"))
skip_on_cran("This test requires an API key")
test_that("a placed order returns a totalFilled numeric greater or equal to 0", {expect_gte(placebuyp$totalFilled, 0)
})
skip_on_cran("This test requires an API key")
test_that("place_order returns a response", {
    expect_is(placebuy, "response")
})
skip_on_cran("This test requires an API key")
apikey <- get_api_key("~/Dropbox/Code/Starfighter/apikey.txt")
test_that("get_api_key returns a character vector", {
    expect_is(apikey, "character")
})
skip_on_cran("This test requires an API key")
test_that("api key is length one", {
    expect_equal(length(apikey), 1)
})
