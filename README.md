# BATZREALTY

Project Members: Zechariah Cheung, Ted Chan, Brianne Ichiyama, and Addison Sengvilay

Project Description:
We used the Zillow API and several other csv data provided by Zillow. Since there is so much we can do with these housing data, we narrowed our analysis down to answer 5 main questions:
1. Does the distance from the neighborhood to downtown Seattle affect the home price?
2. What is the fastest growing city, in terms of home value?
3. Is there a best time of the year to sell a home to maximize chance of sale and sales price?
4. What are the most important factors that influence a home value?
5. How does rent for the different apartment types differ in the different states?

R Code Files Description:
assemble_data.R: Since Zillow doesn't have a list of detailed property information available, we had to manually extract the information using sample property listings, then get the information for its comparables. This r code basically assembles the csv dataset used for analysis.
factor_analysis.R: This r code conducts statistical analysis on the zillow data set. We performed a linear regression model to examine the statistically significant factors in the model. Then used a KNN and random forest model to classify home values.
monthly_count.R: This r code constructs a table of avg. monthly home sales count in Seattle from 2009 - 2017. The table is called in the server.R to construct the bar plot.
monthly_price.R: This r code constructs a table of avg. monthly home sales prices in Seattle from 2009 - 2017. The table is called in the server.R to construct the bar plot.


Data sources and explanations:
https://www.zillow.com/howto/api/APIOverview.htm
https://www.zillow.com/research/data/

![alt text](https://www.zillow.com/widgets/GetVersionedResource.htm?path=/static/logos/Zillowlogo_200x50.gif)
