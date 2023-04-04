# Analyses of Metaculus data

This repo contains the code used to generate the results for a series of blog posts using Metaculus Data. 

## Wisdom of the Crowd vs. the Best of the Best of the Best

[Link to the blogpost](https://forum.effectivealtruism.org/posts/akn2BFhhM9CzwpLEA/wisdom-of-the-crowd-vs-the-best-of-the-best-of-the-best). 

Relevant folder: `forecaster-subsets`


## More is Probably More - Forecasting Accuracy and Number of Forecasters on Metaculus

[Link to the blogpost](https://forum.effectivealtruism.org/posts/more-is-probably-more-forecasting-accuracy-and-number-of). 

Relevant folder: `aggregation-more-users`

The folder `output/data/150` contains simulation results for all questions that have more than 150 unique users. 
The folder `output/data/charles` contains corresponding simulation results, but after applying the following filtering: Predictions were restricted to the those made within the first 25 percent of the question lifetime. In addition, questions were removed if the time between the first and the last prediction in the remaining dataset was more than 1 year. 

The folder `output/data/charles-without-replacement` contains results for the same dataset, but samples were drawn without replacement. Results seem broadly unchanged. 


## Predictive Performance on Metaculus vs. Manifold Markets

Relevant folder: `comparison-manifold`

The python script `get-manifold.py` downlaods the data from Manifold. The R script `comparison-manifold-metaculus.R` merges the data and analyses results. 
