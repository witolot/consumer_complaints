# From Alternative Data to Investment Alpha: A Textual Analysis Case Study

This is a case study in verifying whether an alternative data set can generate statistically significant alpha in publicly traded stocks. Specifically, we use the publicly available Consumer Financial Protection Bureau's (CFPB) [Complaints Database](https://www.consumerfinance.gov/data-research/consumer-complaints/). We tie the data to a universe of 25 publicly traded US consumer facing financial institutions. We extract features from the data and derive variables using textual analysis of the consumer complaint narratives. 

The derived variable is year-on-year cosine similarity of the consumer complaint narratives. The main idea is that if the complaints a company receives change year on year, this means something. In this case, it turns out that companies with the largest change (i.e. changers) in their complaints similarity outperform non-changers. Here are the resulting portfolios for each quintile. Inspecting the differences visually, it's clearly visible that Portfolio Q1 performed worst and Q5 second best. The 130/30 long-short portfolio is best.

<img src="https://github.com/witolot/consumer_complaints/blob/master/images/portfolio_comp.png" width="600"/>

While the charts for the different quintile portfolios are suggestive, their alphas in the Fama-French three factor model are unfortunately not statistically significant. Some ideas are provided for increasing statistical significance.

The full write up with the code walk-through is available <a href="http://witolot.github.io">here</a>.
