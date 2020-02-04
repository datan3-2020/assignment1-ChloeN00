Statistical assignment 1
================
Chloe Neal, 680011126
29/01/2020

## Open data (10 points)

In this assignment you will work with the individual level data from
wave 8 of the Understanding Society survey. First, you need to open the
data set. Please complete the code
    below.

``` r
library(tidyverse)
```

    ## ── Attaching packages ────────────────────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.2.1     ✓ purrr   0.3.3
    ## ✓ tibble  2.1.3     ✓ dplyr   0.8.3
    ## ✓ tidyr   1.0.0     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.4.0

    ## ── Conflicts ───────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
Data <- read_tsv("/Users/chloeneal/University Year 2/Data Analysis 3/Assignment/test-assignment-ChloeN00/UKDA-6614-tab/tab/ukhls_w8/h_indresp.tab")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double()
    ## )

    ## See spec(...) for full column specifications.

Now you have got your data frame stored as Data.

## Select variables (10 points)

The data for Wave 8 of the Understanding Society were collected in
2016-18. Among other things, people were asked the following question:
“Should the United Kingdom remain a member of the European Union or
leave the European Union?” In this assignment, we will explore how
answers to this question depend on sex and age.

First, you need to select the variables for the analysis. You want to
keep the following variables: cross-wave individual identifier (*pidp*),
support for the UK remaining or leaving the EU (*h\_eumem*), sex
(*h\_sex\_dv*), age (*h\_age\_dv*), and sample origin (*h\_memorig*).

Complete the code below to select those variables from the data frame
and save the result.

``` r
Data <- Data %>%
        select(pidp, h_eumem, h_sex_dv, h_age_dv, h_memorig)

Data
```

    ## # A tibble: 39,293 x 5
    ##       pidp h_eumem h_sex_dv h_age_dv h_memorig
    ##      <dbl>   <dbl>    <dbl>    <dbl>     <dbl>
    ##  1   22445       1        2       31         3
    ##  2   29925       1        2       39         3
    ##  3   76165       1        2       33         3
    ##  4  223725      -7        1       41         3
    ##  5  280165       2        2       37         3
    ##  6  333205       1        2       26         3
    ##  7  665045       1        1       34         3
    ##  8  813285      -7        1       46         3
    ##  9 1587125       2        2       50         3
    ## 10 1697285       1        1       43         3
    ## # … with 39,283 more rows

## Filter observations (10 points)

To make nationally representative estimates from the Understanding
Society data we would need to use weight coefficients. There are many
different types of weight coefficients that can be used depending on the
question and the level of analysis (see the User Guide, pp. 65-71). We
will not do this in this assignment. However, what we want to do is to
keep data from the original Understanding Society sample only (ukhls gb
2009-10), dropping data for Northern Ireland, the BHPS cohort members
and ethnic minority boost samples. This will make data closer to be
representative for Great Britain. You need to choose the observations
where *h\_memorig* has the value of 1.

``` r
Data <- Data %>%
        filter(h_memorig == 1)
table(Data$h_memorig)
```

    ## 
    ##     1 
    ## 22957

## Recode data (20 points)

Let us tabulate the variables for EU support, sex, and age.

``` r
table(Data$h_eumem)
```

    ## 
    ##    -9    -8    -7    -2    -1     1     2 
    ##    33   482   879   354   753 11118  9338

``` r
table(Data$h_sex_dv)
```

    ## 
    ##     0     1     2 
    ##     1 10470 12486

``` r
table(Data$h_age_dv)
```

    ## 
    ##  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35 
    ## 284 309 290 291 278 295 268 326 287 257 243 234 229 249 274 278 278 293 314 332 
    ##  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55 
    ## 351 332 321 336 320 327 368 404 372 386 435 465 425 447 406 420 427 414 432 422 
    ##  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75 
    ## 408 413 416 434 369 398 358 399 354 412 345 358 412 434 431 334 326 293 275 251 
    ##  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95 
    ## 219 231 211 205 181 162 138 117 117 108  89  78  77  48  41  27  15  18  15   7 
    ##  96  97  98  99 101 102 
    ##   6   2   3   1   1   1

You will see that all these variables are numeric. You can learn what
the numeric codes mean by checking the codebook here:
<https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/datafile/indresp/wave/8>
.

We want to do the following:

1)  Recode the variable for EU support as binary (1 for Remain, 0 for
    Leave), coding all types of missing values (including refusals and
    “don’t know”) as NA.
2)  Recode sex into a character vector with the values “male” or
    “female”.
3)  Recode age into a variable with the following categories: 16 to 25,
    26 to 40, 41 to 55, 56 to 70, over 70.

In all cases, we want to create new variables.

``` r
Data <- Data %>%
        mutate(EU = ifelse(h_eumem ==1, 1, 
                           ifelse(h_eumem == 2, 0, NA))) %>%
  mutate(Sex = ifelse(h_sex_dv == 1, "male",
                            ifelse(h_sex_dv == 2, "female", NA))) %>%
  mutate(agegr = case_when(
    between(h_age_dv, 16, 25) ~ "16 to 25",
    between(h_age_dv, 26, 40) ~ "26 to 40", 
    between(h_age_dv, 41,55) ~ "41 to 55", 
    between(h_age_dv, 56,70) ~ "56 to 70", 
    h_age_dv > 70 ~ "Over 70"
  ))

table(Data$EU)
```

    ## 
    ##     0     1 
    ##  9338 11118

``` r
table(Data$Sex)
```

    ## 
    ## female   male 
    ##  12486  10470

``` r
table(Data$agegr)
```

    ## 
    ## 16 to 25 26 to 40 41 to 55 56 to 70  Over 70 
    ##     2885     4384     6150     5941     3597

## Summarise data (20 points)

Let us **dplyr** to calculate how many people in the sample supported
Remain and Leave, both as absolute numbers and percentages.

``` r
Data %>%
  count(EU) %>%
  mutate(perc = n / sum(n) *100)
```

    ## # A tibble: 3 x 3
    ##      EU     n  perc
    ##   <dbl> <int> <dbl>
    ## 1     0  9338  40.7
    ## 2     1 11118  48.4
    ## 3    NA  2501  10.9

``` r
#1 is to remain, 0 is to leave.
```

Write a couple of sentences with the interpretation of this result. How
this compares with the result of the 2016 referendum? Why?

In the 2016 referendum,51.9% of the UK voted leave while 48.1% voted to
remain (BBC, 2016). The data for wave 8 of the understanding Society
shows that 11118 people felt we should remain in the EU, which accounts
for 48.4% (1 d.p). While 9338 participants felt we should leave the EU
which makes up 40.% (1 d.p) of the sample. 2501 did not answer which
equates for 10.89% (2 d.p) of the sample. The results depict different
opinions on whether to leave or remain in the EU.

## Summarise data by sex and age (30 points)

Now let us look at the support for Leave and Remain by sex and age. Use
your newly created variables.

``` r
Data %>%
  na.omit() %>%
  count(EU, Sex, agegr) %>%
  mutate(perc = n / sum(n) *100)
```

    ## # A tibble: 20 x 5
    ##       EU Sex    agegr        n  perc
    ##    <dbl> <chr>  <chr>    <int> <dbl>
    ##  1     0 female 16 to 25   371  1.81
    ##  2     0 female 26 to 40   817  3.99
    ##  3     0 female 41 to 55  1313  6.42
    ##  4     0 female 56 to 70  1448  7.08
    ##  5     0 female Over 70    910  4.45
    ##  6     0 male   16 to 25   392  1.92
    ##  7     0 male   26 to 40   691  3.38
    ##  8     0 male   41 to 55  1196  5.85
    ##  9     0 male   56 to 70  1289  6.30
    ## 10     0 male   Over 70    911  4.45
    ## 11     1 female 16 to 25   985  4.82
    ## 12     1 female 26 to 40  1455  7.11
    ## 13     1 female 41 to 55  1733  8.47
    ## 14     1 female 56 to 70  1476  7.22
    ## 15     1 female Over 70    722  3.53
    ## 16     1 male   16 to 25   763  3.73
    ## 17     1 male   26 to 40   985  4.82
    ## 18     1 male   41 to 55  1280  6.26
    ## 19     1 male   56 to 70  1170  5.72
    ## 20     1 male   Over 70    548  2.68

Write a couple of sentences interpreting your results. Out of the
females who felt we should leave the EU, the biggest proportion came
from 56-70 year olds with 1448 participants. This is the same with males
who felt we should leave; the biggest proportion came from 56-70 year
olds with 1289 participants. The smallest proportion of votes for both
sexes came from 16-25 year olds. Out of females and males who felt we
should vote remain, the biggest age group was 41-55 with 1733 (Females)
and 1280 (Males). The smallest age category for those who wanted to
remain was over 70’s.
