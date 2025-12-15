**Time Series Analysis Project - Wind Speeds at
Valentia Island Weather Station**

Using R’s *TSA* package, I’ve fitted a seasonal ARIMA model to around ten years' worth of monthly average wind speeds, recorded at the Valentia Island weather station, sourced online from Met Éireann.

I trimmed the original dataset to 85% of its entries, with the goal of fitting a model to the truncated data, 'forecasting' the rest of the data before analysing residuals.
I then ran some quick exploratory tools on the data to identify characteristics like seasonality and trend, before working out the most appropriate and most concise ARIMA models to use.

The final predicted values were notably accurate, with normally distributed residual values.

