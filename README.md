
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Effects of human-animal interactions on affect and cognition

This repository provides the reproducible research materials for our
project that investigates the characteristics of dogs, owners, and their
interaction that predict dog training success. This includes the
following:

-   Data
-   R script for data analysis
-   R Markdown file for the manuscript
-   R Markdown file for supplementary materials

# Citation

If you use any of these materials, please cite:

Thayer, E.R. & Stevens, J.R. (2021). Effects of human-animal
interactions on affect and cognition. *PsyArXiv*. doi:
10.31234/osf.io/7v5nq

# Summary

Two experiments were conducted with 73 and 84 participants from the
University of Nebraska-Lincoln Department of Psychology undergraduate
participant pool between September-November 2018 and November 2018-April
2019. Each experiment generated two data files: one for the primary
affective, cognitive, and pet-related measures for each participant and
one with the survey item responses for calculating Cronbach’s alpha. For
each of these data files, both experiments are included and labeled. For
the primary analysis data file, each row represents all of a single
participant’s responses. For the survey item data file, each row
represents a participant’s responses to a particular survey.

# License

All materials presented here are released under the Creative Commons
Attribution 4.0 International Public License (CC BY 4.0). You are free
to:

-   Share — copy and redistribute the material in any medium or format
-   Adapt — remix, transform, and build upon the material for any
    purpose, even commercially. Under the following terms:
-   Attribution — You must give appropriate credit, provide a link to
    the license, and indicate if changes were made. You may do so in any
    reasonable manner, but not in any way that suggests the licensor
    endorses you or your use.

No additional restrictions — You may not apply legal terms or
technological measures that legally restrict others from doing anything
the license permits.

# Files

## Data files

`thayer_stevens_2021_data1.csv` (primary affective and cognitive data
set)

-   experiment - experiment number (1 or 2)
-   date - date participant completed experiment
-   participant - participant number
-   condition - experimental condition (hai = human-animal interaction,
    control)
-   age\_num - participant age
-   gender - participant gender
-   race - participant race/ethnicity
-   parent\_income - participant’s parental income
-   pas - Pet Attitude Scale mean score
-   pets\_now - response to whether participant currently has pet (1 =
    yes, 0 = now)
-   pets\_child - response to whether participant had pet as child (1 =
    yes, 0 = now)
-   dog\_discomfort\_revkey - response to Gee et al.’s discomfort toward
    dog (reverse coded)
-   dog\_ambivalence\_revkey - response to Gee et al.’s ambivalence
    toward dogreverse coded)
-   dog\_desire\_to\_interact - response to Gee et al.’s desire to
    interact with dog
-   dog\_comfort - response to Gee et al.’s comfort toward dogreverse
    coded)
-   duration\_interaction - duration of interaction with dog (in
    seconds)
-   panas\_pre\_neg - pre-condition PANAS score for negative affect
-   panas\_pre\_pos - pre-condition PANAS score for positive affect
-   panas\_post\_neg - post-condition PANAS score for negative affect
-   panas\_post\_pos - post-condition PANAS score for positive affect
-   panas\_pos\_diff - pre-post difference for PANAS score for positive
    affect
-   panas\_neg\_diff - pre-post difference for PANAS score for negative
    affect
-   vas\_anxiety\_pre - pre-condition visual analog scale for anxiety
-   vas\_anxiety\_post - post-condition visual analog scale for anxiety
-   vas\_stress\_pre - pre-condition visual analog scale for stress
-   vas\_stress\_post - post-condition visual analog scale for stress
-   stai\_trait - trait score of State-Trait Anxiety Index
-   stai\_state - state score of State-Trait Anxiety Index
-   drm\_accuracy - accuracy score for Deese-Roedinger-McDermott
    long-term memory task
-   drm\_d\_prime - d’ score for Deese-Roedinger-McDermott long-term
    memory task
-   ncpc\_pre\_diff - pre-condition difference between second and first
    trial of Necker Cube Pattern Control Test
-   ncpc\_post\_diff - post-condition difference between second and
    first trial of Necker Cube Pattern Control Test
-   ncpc\_diff - pre-post difference for difference between second and
    first trial of Necker Cube Pattern Control Test
-   bds\_index\_pre - pre-condition backwards digit span index
-   bds\_index\_post - post-condition backwards digit span index
-   bds\_index\_diff - pre-post difference for backwards digit span
    index
-   nback\_d\_prime\_pre - pre-condition d’ for n-back task
-   nback\_d\_prime\_post - post-condition d’ for n-back task
-   nback\_d\_prime\_diff - pre-post difference for d’ for n-back task

`thayer_stevens_2021_data2.csv` (item-specific data for calculating
reliability)

-   item\_1-item\_20 - individual items (surveys differ on number of
    items, so NAs represent no items)
-   survey - name of survey

## R code

`thayer_stevens_2021_rcode.R` - code for running computations and
generating figures

## R Markdown documents

`thayer_stevens_2021.Rmd` - R Markdown document with R code embedded for
main manuscript `thayer_stevens_2021_SM.Rmd` - R Markdown document with
R code embedded for supplementary materials

## Installation

To reproduce these results, first clone or unzip the Git repository into
a folder. Then, ensure that subfolders named `data/`, `docs/`,
`figures/`, and `R/` are in the folder. The best way to work with this
material is to open the `haicognition2021.Rproj` file in RStudio. Next,
open `thayer_stevens_2021_rcode.R` and ensure that all packages
mentioned at the top of the script are installed. Once all packages are
installed, run the script in R using
`source("thayer_stevens_2021_rcode.R")`.

Once the script runs without errors, you can compile the R Markdown
document `thayer_stevens_2021.Rmd.` Open this file in RStudio and ensure
that you have packages [{knitr}](https://yihui.org/knitr/) and
[{rmarkdown}](https://github.com/rstudio/rmarkdown) installed. Once
installed, use {knitr} to render the document (control-shift-k). Use the
same process to render `thayer_stevens_2021_SM.Rmd`.
