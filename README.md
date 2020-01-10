# FinProject: COMPASS SM1/SC1 financial data project

## Project Brief 


[Yahoo Finance](https://finance.yahoo.com/) is one possible source of historical financial time series data.
We will mostly focus on the stock price history of large US companies, in the S&P
500 index. The current constituents of the S&P 500 index can be found [here](https://www.barchart.com/stocks/indices/sp/sp500),
among many other sources. It may be cumbsersome to manually download stock
prices of all 500 companies, so we may have to find better data source.


One possible project is **porfolio optimisation**, i.e. to select the best asset
distribution subject to certain constraints so that the expected return is
maximized while financial risk is minimized. It is important to first construct
the covariance matrix for the rates of return on the assets in the portfolio. The
optimisation problem itself may be constrained by many factors, e.g. whether
short selling is allowed. Transaction cost is another consideration that will
impact on the frequency of adjustment to the portfolio.



## Layout
- [**Package**](https://github.com/Dom-Owens-UoB/FinProject/tree/master/pkg): access with `install_github("Dom-Owens-UoB/FinProject", subdir="pkg")`. Features [Principal Components Analysis in S4](https://github.com/Dom-Owens-UoB/FinProject/blob/master/pkg/R/myPCA.R); [Support Vector Machine for Classification](https://github.com/Dom-Owens-UoB/FinProject/blob/master/pkg/R/SupportVectorMachine.R);

- [**Notes on Factor Analysis**](https://github.com/Dom-Owens-UoB/FinProject/blob/master/Documents/FinProject.pdf)

- [**Modelling Oil Prices**](https://github.com/Dom-Owens-UoB/FinProject/blob/master/Documents/FinProject_Data_Analysis.pdf)

- [**Portfolio Selection**]()

- [**Gaussian Process Regression**]()


## Plan

- Factor analysis notes

  SM1 Content:
- VAR: method explanation, application, results
- Gaussian Process classification: method explanation, application, results
- forecast, cost comparison

  SC1 Content:
- Package: data, functions, testing
- Visualisation (time series, forecasts...)
- Debugging
