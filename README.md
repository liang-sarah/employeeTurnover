# employeeTurnover
*Collaborators: David Li, Aryanka Thaker, Sarah Liang*

The completed presentation document for this group project is seen [here](https://liang-sarah.github.io/employeeTurnover/employeeTurnoverAnalysis.pdf).

<br />

#### R packages used:
<table border = "0">
  <tr>
    <td>dplyr</td> <td>tidyverse</td> <td>ggplot2</td> <td>survival</td>
    <td>survminer</td> <td>janitor</td> <td>formatR</td> <td>knitr</td>
  </tr>
</table>


<br />

## Introduction
The goal of this project is to predict the employee turnover time using the survival analysis method. In this
dataset, we have 16 columns, with `stag` as the months that a employee quit and event as the censoring status.
Besides interest in the impact of independent variables like `age` and `industry` on our survival function, we
are more interested in the peculiar variables like `extraversion`, `independ`, `selfcontrol`, `novator`, and
anxiety which can be indicative towards the employee’s personality traits. We are wondering: is there an
efficient way to manipulate the personality variables such that we are able to determine if there is a particular
combination of personality traits with significant effect on employee turnover? From our project, it would
be interesting to find out the type of employee most likely to stay with employers.

## Reference
Data obtained from Kaggle: Employee Turnover, uploaded by DAVIN WIJAYA URL: https://www.kaggle.
com/datasets/davinwijaya/employee-turnover

Step Function R document:
Hastie, T. J. and Pregibon, D. (1992) Generalized linear models. Chapter 6 of Statistical Models in S eds J.
M. Chambers and T. J. Hastie, Wadsworth & Brooks/Cole.

Venables, W. N. and Ripley, B. D. (2002) Modern Applied Statistics with S. New York: Springer (4th ed).

Survival Analysis Functions:
Therneau, Terry. A package for Survival analysis in R

Carter, Andrew. Lecture slides for Fall 2022 at UC Santa Barbara
