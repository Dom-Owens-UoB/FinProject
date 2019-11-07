# FinProject
## COMPASS SM1/SC1 financial data project

https://finance.yahoo.com/
Yahoo Finance is one possible source of historical financial time series data.
We will mostly focus on the stock price history of large US companies, in the S&P
500 index. The current constituents of the S&P 500 index can be found here (https://www.barchart.com/stocks/indices/sp/sp500),
among many other sources. It may be cumbsersome to manually download stock
prices of all 500 companies, so we may have to find better data source.
One possible project is porfolio optimisation, i.e. to select the best asset
distribution subject to certain constraints so that the expected return is
maximized while financial risk is minimized. It is important to first construct
the covariance matrix for the rates of return on the assets in the portfolio. The
optimisation problem itself may be constrained by many factors, e.g. whether
short selling is allowed. Transaction cost is another consideration that will
impact on the frequency of adjustment to the portfolio.
