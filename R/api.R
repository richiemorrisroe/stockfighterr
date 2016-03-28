base_url <- "https://api.stockfighter.io/ob/api/"
options(fractional.seconds=7)
##' Return a quote for a given stock from a given venue
##' Note that the quote is valid for a time in the past, and may not reflect current state of the market. Makes a HTTP request to the relevant endpoint. Making a change to ensure that magit knows where my files are
##' @title get_quote
##' @param venue where the stock is being traded
##' @param stock which stock to get quote for
##' @param ... further arguments passed to httr::GET
##' @return a named list with components
##' @author Richie Morrisroe
##' @export
get_quote <- function(venue, stock, ...) {
    url <- paste(base_url,  "venues/", venue, "/stocks/", stock, "/quote", sep="")
    res <- httr::GET(url=url, ...)
    if(check_status(res)) {
        return(res)
    }
    else {
        error(res)
    }
}
##' Get the state of the orderbook at a particular venue and for a particular stock
##' This function returns a more granular picture than get_quote, with two dataframes containing price and qty available for both bid (buy) and ask (sell)
##' @title get_orderbook
##' @param venue a particular venue
##' @param stock a particular stock
##' @return an object of class orderbook
##' @author Richie Morrisroe
##' @export
get_orderbook <- function(venue, stock) {
    url <- paste(base_url, "venues/", venue, "/stocks/", stock, sep="")
    res <- httr::GET(url=url)
        if(check_status(res)) {
            return(res)
        }
        else {
            return(NULL)
        }

}
##' Create a named list to be used in a POST request for trading a particular stock
##' Really just provides a shim around creating a list for conversion to JSON
##' @title create_order
##' @param account trading account for this level
##' @param venue venue for the level
##' @param stock the stock to trade
##' @param price price in US cents, must be integer
##' @param qty number of shares to trade
##' @param direction buy or sell
##' @param ordertype one of market, limit, fill-or-kill or immediate-or-cancel
##' @return a named list with components as per parameters to function
##' @author Richie Morrisroe
##' @export
create_order <- function(account, venue, stock, price, qty, direction, ordertype="limit"){
    res <- list(account=account, venue=venue, stock=stock,
                price=price, qty=qty, direction=direction, orderType=ordertype)
}
##' Place an order to buy or sell stock
##'
##' 
##' @title place_order
##' @param venue the venue on which to place the order
##' @param stock the ticker which to order
##' @param body the result of a call to create_order
##' @param apikey authentication
##' @return a response object
##' @author Richie Morrisroe
##' @export
place_order <- function(venue, stock, body, apikey) {
    url <- paste(base_url, "/venues/", venue, "/stocks/", stock, "/orders", sep="")
    res <- httr::POST(url=url,
                      body=body,
                      encode="json", httr::add_headers(
                                         "X-Starfighter-Authorization"=apikey))
            if(check_status(res)) {
            return(res)
        }
        else {
            return(NULL)
        }
    
}
##' Parse JSON formatted text responses from the API
##'
##' essentially just calls content() and then jsonlite::fromJSON
##' @title parse_response
##' @param response the results of an API call
##' @return a named list with the components of the response
##' @author Richie Morrisroe
##' @export
parse_response <- function (response) {
    content <- httr::content(response, as="text")
    parsed <- jsonlite::fromJSON(content)

}
##' Return number of stocks traded at a venue
##'
##' See above
##' @title get_tickers
##' @param venue the venue to request stocks for
##' @return a HTTP response containing the requested tickers
##' @author Richie Morrisroe
##' @export
get_tickers <- function(venue) {
    url <- paste(base_url, "venues/", venue, "/stocks/", sep="")
    res <- httr::GET(url)
    if(check_status(res)) {
        return(res)
    }
    else {
        return(NULL)
    }
}
##' get the status of an order
##'
##' Only useful for limit/market as ioc or fok return immediately
##' @title get_order_status
##' @param id the id returned from a previous order
##' @param venue the venue
##' @param stock the stock which was in the previous order
##' @return a HTTP response indicating the status of this order
##' @author Richie Morrisroe
##' @export
get_order_status <- function(id, venue, stock) {
    url <- paste(base_url, "venues/", venue, "/stocks/", stock, "/orders/", id, sep="")
    print(url)
    res <- httr::GET(url)
    res
}
##' Cancel an outstanding order
##'
##' See above
##' @title cancel_order
##' @param id the order id
##' @param venue the venue
##' @param stock the stock
##' @param apikey an authorisation token
##' @return a HTTP response indicating the results of the call
##' @author Richie Morrisroe
##' @export
cancel_order <- function(id, venue, stock, apikey) {
    url <- paste(base_url, "venues/", venue, "/stocks/", stock, "/orders/", id, sep="")
    res <- httr::DELETE(url, httr::add_headers("X-Starfighter-Authorization"=apikey))
    res
}
##' Get the status of all orders related to a trading account
##' 
##' @title get_all_orders
##' @param venue trading venue
##' @param account current account
##' @param apikey your APIKey
##' @return a HTTP response with data 
##' @author Richie Morrisroe
##' @export
get_all_orders <- function(venue, account, apikey) {
    url <- paste(base_url, "venues/", venue, "/accounts/", account, "/orders/", sep="")
    res <- httr::GET(url,
                     httr::add_headers(
                         "X-Starfighter-Authorization"=apikey))
    res
}
##' Read the API key in from a file, assumes only the key is present in this file
##'
##' Use this to get your auth key into R. 
##' @title get_api_key
##' @param path location of the file containing the API key (absolute)
##' @return the apikey as a character string
##' @author Richie Morrisroe Morrisroe
##' @export
get_api_key <- function(path) {
    apikey <- scan(path, what="character")
}
##' Undocumented get_venues endpoint
##'
##' Honestly not sure what this is useful for, but it seems cool. 
##' @title get_venues
##' @return either an error message or the response
##' @author richie
get_venues <- function () {
    url <- paste0(base_url, "venues")
    res <- httr::GET(url)
    if(check_status(res)) {
        return(res)
    }
    else {
        error(res)
    }
}
##' Check response code of a http request
##'
##' If status_code is not 200, return FALSE. Used internally by the package, made available in case it proves useful
##' @title check_status
##' @param response a response object
##' @return TRUE if successful, else FALSE
##' @author Richie Morrisroe
##' @export
check_status <- function(response) {
    if(httr::status_code(response) == 200) {
        return(TRUE)
    } else {
        message(response)
        return(FALSE)
    }
}
