# **Brokerage Simulation**

**NetID and names:** Ethan Hersch esh87, Jerry Chen jwc279, Aarav Khanna ak2246, Mohammad Islam mai54

**NOTE: This API limits only 5 calls per minute (for the free version). The system will not let you buy or sell too many stocks too soon.**

Install opam dependencies (should take a few minutes):
```
opam install ppx_deriving tls-lwt cohttp cohttp-async cohttp-lwt cohttp-lwt-unix cohttp-lwt-jsoo cohttp-top lwt_ppx yojson ounit2 ANSITerminal calendar
```

## To use operate and try out the UI of our brokerage
```
dune build
make play
```

## Fine tuned stock viewing / options testing environment (not part of main UI/brokerage interface)
Specialized tesing environment to fine-tune options and view stock parameters. To view stock prices based on their ticker names. Toggle boolean flag in operate/main.ml to test fine tuned greek parameters e.g. volatility, time to expiration for options pricings. 
```
dune build
make view_stock
```

## To view documentation as HTML file

```
make doc
make opendoc (then open index.html)
```

## How to enter commands

Capitalization matters for the commands. For example, "-HELP" is invalid while "-help" is valid.
Capitalization does NOT matter for stock ticker symboles. For example "-view AAPL" and "-view aapl" both work.
You can deposit integer and float values. For example, "-deposit 1000" and "-deposit 3.5" both work.
You can buy whole stocks and fractional stocks. For example, "-buy aapl 1" and "-buy .5" both work.


## Information about some commands

"-bal" is your cash and stock balance combined

"-cash" is your cash balance

"-equity" is the amount of money you have invested in stocks

"-cashflow" gives a cashflow-history log. The history of deposits and withdrawals

"-portfolio" shows what stocks are in your portfolio (along with the amount and averge stock price)

"-dep [amt]" deposit amt into your account

"-withdraw [amt]" withdraw amt of cash

"-view [ticker]" view the stock with symbol ticker

"-view_option [ticker]" view the option with symbol ticker. Look at -options_ticker_help to see how to properly format an options ticker.

"-options_ticker_help" get some help understanding options

"-buy [ticker] [number of shares]" buy a certain number of shares of ticker

"-sell [ticker] [number of shares]" sell a certain number of shares of ticker

"-history" see your transaction history. What you recently bought and sold

"-watchlist" look out for the value of these stocks

"-watchlist add [ticker]" add stock to your watchlist

"-watchlist remove [ticker]" remove stock from your watchlist

"-help" get some help

"-quit" exit


**Final project for CS3110 Spring 2023**#
