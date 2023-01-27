# Analyses of Metaculus data

This repo contains the code used to generate the results shown in this [blogpost](https://forum.effectivealtruism.org/posts/more-is-probably-more-forecasting-accuracy-and-number-of). 

The code is in the folder `R`. 

The folder `output/data/150` contains simulation results for all questions that have more than 150 unique users. 
The folder `output/data/charles` contains corresponding simulation results, but after applying the following filtering: Predictions were restricted to the those made within the first 25 percent of the question lifetime. In addition, questions were removed if the time between the first and the last prediction in the remaining dataset was more than 1 year. 

The folder `output/data/charles-without-replacement` contains results for the same dataset, but samples were drawn without replacement. Results seem broadly unchanged. 