# EfficientFrontiers
This code is based on some code found online about how to create an efficient frontier in R. The code was originally from the blog Economist at Large. This code was edited for a work assignment. The edits were made in order to fit the efficient frontier to the specific data requirments that we have. 

The code was edited in order for each asset to have a min and a max. In addition one of the assets didn't have data for the same time period as the other assets, so the code was edited in order to input assets with missing data. Additionaly the code was edited so that it is possible to enter the expected return for each asset. 

I have simulated the type of returns that this code was used on through the file 'Returns.csv' that is also located in this folder. 

This code will produce the allocation for each asset in an efficient portfolio, given a min and max for each asset as well as an expected return for each asset. The code also produces a chart that shows the optimal portfolio with the return and standard deviation along the efficient frontier. 

The only thing the user needs to do in order to run this code, is to input their work directory in the appropriate place. Additionally the user may need to download some packages. After that the user needs to save the Returns.csv file to their work directory. 
