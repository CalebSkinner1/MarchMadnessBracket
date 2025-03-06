## MarchMadnessBracket GitHub Page

Each year, a 10 member committee selects and seeds 68 universities to compete in the college basketball postseason tournament.
Using seven years of data, I employ ensemble boosting trees to predict the committee's decisions.
The model is highly accurate in predicting the 68 teams and moderately accurate in ranking the teams.
A university's Strength of Record is the best predictor for their selection and ranking.
In further analysis, I find that the model's selections perform slightly better than the committee's selections in the tournament, but more work is needed to validate these results.

## Data
full_team.csv includes several summary statistics and team-level measurements for each college basketball team from 2017 to 2024. I use this data set for my predictions of the committee's rankings.
The predictors can generally be divided into two categories: metrics designed to predict future success and summary statistics gauged to measure the team's resume.
The committee intentionally developed the NCAA Evaluation Tool (NET) in 2019 to measure the school's resume and replace RPI as the primary resume tool.
All summary statistics and predictive tools are ranks.

full_tourney.csv focuses on the tournament results for each year. It shows the results for each game and the important traits (above) of the two teams participating. I use this data set in the second
stage of the report.

## Project Tidy.R
This R Script tidys the data and combines it into the aforementioned csv files.

## First Stage Boosting.R

This file uses boosting trees in two steps. First, classification ensemble boosting trees predict the teams that make the tournament and, second, regression ensemble boosting trees predict the seed of each team.
For each of the seven years, I use data from the other six years to train the model, and predict the 68 team lineup with the trained model.
Each year, only 68 teams make the tournament, so I take the 68 teams most predicted by the classifcation trees. Next, I train a regression boosted tree on the teams that made the tournament
from the other six years. I apply this model to the 68 teams that the classification model selected. After grouping the predicted seeds into bins of four, I have the model's expected seed.

## Second Stage Boosting.R

This file uses boosting trees to predict the winners of each of the tournament games.

## Live Predictions.R
I added this file in 2025 to actively predict the tournament teams as the season progresses. It works by updating Project Tidy.R and running
the files in First Stage Boosting.R. I bootstrap the training data 500 times to produce more realistic probabilities of inclusion
and final seed estimates.

## Final Report

This report is the three-page report I submitted in my Data Science II class. The code is located in the qmd file.

The final report also contains the second stage of the analysis. In this section, I compare the performance of the teams that the committee selected and the teams that the model would have selected.
I do this in two ways. First, I measure *accuracy* as the proportion of tournament games that the higher seed wins.
Second, I measure *error* as the seed differential if the lower seed wins. Error is zero if the higher seed wins. Overall, the model's rankings have a smaller error and higher accuracy, but the difference is neglible.
Of course, this analysis suffers from selection bias (although it is difficult to see if it favors the committee or model's selection), because we don't observe games played from the model's rankings.
In the future, a more robust method will be needed to determine the statistical significance of this result.

## References
1. [Kaggle March Madness](https://www.kaggle.com/competitions/march-machine-learning-mania-2024)
2. [Strength of Record Data](ESPN.com)
3. [KPI](https://faktorsports.com)

