# Basketball-Analysis

The goal of this project is to redefine player roles of basketball players using clustering - similarly as Alagappan (2012), a paper which is one of the first quantative analysis attempting to characterize the playing roles of basketball players.

We employed a kmeans algorithm while investigating multiple ways to make the clustering output more insightful: Firstly, we explored developing a more extensive set of statistical variables that measure player performance in various skills of the game. As an example, we look in depth at introducing a score from -1 to 1, indicating how well a player performs under certain pressured situations. Secondly, we make use of a visualization tool to understand the clusters better, namely Multi-Dimensional Scaling, an algorithm aimed at presenting similarities between players over multiple dimensions. Thirdly, we attempt to build a variable selection algorithm. Over the course of the analysis, we encounter approximately 45 different variables, and the variable selection algorithm aims to select those variables that have explanatory power whilst removing variables with insignificant noise, or collinearity with other variables, therefore increasing the risk of overfitting in the dataset. 

The analysis, its results and detailed explanations can be found in **BasketballAnalysis.html** 
