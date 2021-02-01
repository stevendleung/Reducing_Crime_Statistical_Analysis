Reducing Crime- A Statistical Analysis
================ 

In this study we analyze the causal elements on crime in the dataset below and determine recommendations based on a statistical regression analysis.

    ├── README.md          <- The top-level README for developers using this project.
    ├── data
    │   ├── interim        <- Intermediate data that has been transformed.
    │   ├── processed      <- The final, canonical data sets for modeling.
    │   └── raw            <- The original, immutable data dump.
    │
    ├── notebooks          <-  Notebooks. These are exploratory. 
    │
    ├── references         <- Data dictionaries, manuals, and all other explanatory materials.
    │
    ├── reports            <- Generated analysis as HTML, PDF, LaTeX, etc.
    │   └── figures        <- Generated graphics and figures to be used in reporting
    ├── src                <- Source code for use in this project.
    │   │
    │   ├── data           <- Scripts to download or generate data
    │   │   └── make_dataset.R
    │   │
    │   ├── features       <- Scripts to turn raw data into features for modeling
    │   │   └── build_features.R


The data is provided in a file, `crime_v2.csv`. It was first used in a
study by Cornwell and Trumball, researchers from the University of
Georgia and West Virginia University (C. Cornwell and W. Trumball
(1994), “Estimating the Economic Model of Crime with Panel Data,” Review
of Economics and Statistics 76, 360-366.)


``` 
  variable  | label
------------|--------------------------------
1    county |               county identifier
2      year |                            1987
3    crmrte |     crimes committed per person
4    prbarr |         'probability' of arrest
5   prbconv |     'probability' of conviction
6   prbpris | 'probability' of prison sentence
7    avgsen |             avg. sentence, days
8     polpc |               police per capita
9   density |             people per sq. mile
10    taxpc |          tax revenue per capita
11     west |           =1 if in western N.C.
12  central |           =1 if in central N.C.
13    urban |                   =1 if in SMSA
14 pctmin80 |            perc. minority, 1980
15     wcon |       weekly wage, construction
16     wtuc |    wkly wge, trns, util, commun
17     wtrd | wkly wge, whlesle, retail trade
18     wfir |    wkly wge, fin, ins, real est
19     wser |      wkly wge, service industry
20     wmfg |         wkly wge, manufacturing
21     wfed |         wkly wge, fed employees
22     wsta |       wkly wge, state employees
23     wloc |        wkly wge, local gov emps
24      mix | offense mix: face-to-face/other
25  pctymle |              percent young male
```

