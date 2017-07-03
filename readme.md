---
title: 'Introduction to Bayesian data analysis'
author:
- Mark Andrews
...

A presentation given at the PsyPAG meeting entitled *Alternative approaches to
inferential statistics: Likelihoods and Bayesian analysis in Social Science
research*, [University of Kent], July 8, 2017.

## Demos

1. [Why most published research findings are false](https://lawsofthought.shinyapps.io/false_discovery).
2. [How p-hacking can make almost anything significant](https://lawsofthought.shinyapps.io/p_hacking).
3. [How optional stopping can inflate Type I error rates](https://lawsofthought.shinyapps.io/optional_stopping).
4. [How lower powered studies can inflate the value of true effects](https://lawsofthought.shinyapps.io/power_failure).

## Source code

The demos above are all written in R and Shiny. The source code can be found at
<https://github.com/lawsofthought/replication-crisis-demos>. All code is
released according a free and open-source licence, see `Licence.txt` for more
info.

[University of Kent]: <https://www.kent.ac.uk/>

*Note*: To use shinyapps.io, I had to install package PKI and this lead to errors when I tried to install the usual way. This was fixed with

```
install.packages('PKI',,'http://www.rforge.net/')
```


