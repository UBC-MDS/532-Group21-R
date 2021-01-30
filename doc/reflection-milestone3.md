## Functionality

Our group accomplished the same in using R as we did in Python, therefore this explanation will be brief and we refer you to the milestone 2 reflection for more detailed functionality.  The main 'statistic of interest' (1), 'region' (2), 'subregion' (3) and 'income group' (4) can be chosen which controls all 3 plots. The 'Year' slider (6) controls the line plot, while the maximum year chosen also changes the year shown on the map and box plot. The 'Show me' filter to display either to top 5 or bottom five countries on the bar and line plots. You can zoom in on both the bar and line plot, but they do not impact each other. Lastly, you can hover over all three plots for a tooltip popup of the country name and statistic of interest.

The 'population size' slider does not currently have functionality to influence the plots. 

Improving upon last week, we were able to identify missing countries from the map and ensured more strings matched when merging identifier codes to have a more informative map plot

## Experience using R
Overall, this week was slightly more challenging to implement using R compared to Python. It was slightly more difficult to customize plots using ggplot and plotly. The tidyverse syntax makes it a bit more challenging to use variables names that have quotations (character vectors). We were not able to correctly size the map plot at the top of the page to extend horizontally and so it's difficult to visually inspect the map right now. Furthermore, the tooltips in all three plots are difficult to customize and therefore show too many decimal points and 'reordered' as the name in some cases.

Fortunately we were able to use pretty much the sample template of our Python code from last milestone and this made it easier to design the dashboard in R. Last milestone, we opted to create functions which filtered the data independently of Altair, and this made it easier to transfer to R; instead of having to translate between Altair and GGplot, it was easier to translate via base Python and base R (tidyverse) as we kept the plotting code as simple as possible.
