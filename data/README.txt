ReadMe file
Created on 2020-07-10 by Jeffrey R. Stevens (jeffrey.r.stevens@gmail.com)
Finalized on 2021-02-18

**********************************************************
If you use the data, please cite the following:
Thayer, E.R. & Stevens, J.R. (2021). Effects of human-animal interactions on affect and cognition. PsyArXiv. doi: 10.31234/osf.io/7v5nq
**********************************************************

Summary: Two experiments were conducted with 73 and 84 participants from the University of Nebraska-Lincoln Department of Psychology undergraduate participant pool between September-November 2018 and November 2018-April 2019. Each experiment generated two data files: one for the primary affective, cognitive, and pet-related measures for each participant and one with the survey item responses for calculating Cronbach's alpha. For each of these data files, both experiments are included and labeled. For the primary analysis data file, each row represents all of a single participant's responses. For the survey item data file, each row represents a participant's responses to a particular survey.

License:
All materials presented here are released under the Creative Commons Attribution 4.0 International Public License (CC BY 4.0). You are free to:
    Share — copy and redistribute the material in any medium or format
    Adapt — remix, transform, and build upon the material for any purpose, even commercially.
Under the following terms:
    Attribution — You must give appropriate credit, provide a link to the license, and indicate if changes were made. You may do so in any reasonable manner, but not in any way that suggests the licensor endorses you or your use.
    No additional restrictions — You may not apply legal terms or technological measures that legally restrict others from doing anything the license permits.

Data files:
thayer_stevens_2021_data1.csv (primary affective and cognitive data set)
 experiment - experiment number (1 or 2)
 date - date participant completed experiment
 participant - participant number
 condition - experimental condition (hai = human-animal interaction, control)
 age_num - participant age
 gender - participant gender
 race - participant race/ethnicity
 parent_income - participant's parental income
 pas - Pet Attitude Scale mean score
 pets_now - response to whether participant currently has pet (1 = yes, 0 = now)
 pets_child - response to whether participant had pet as child (1 = yes, 0 = now)
 dog_discomfort_revkey - response to Gee et al.'s discomfort toward dog (reverse coded)
 dog_ambivalence_revkey - response to Gee et al.'s ambivalence toward dogreverse coded)
 dog_desire_to_interact - response to Gee et al.'s desire to interact with dog
 dog_comfort - response to Gee et al.'s comfort toward dogreverse coded)
 duration_interaction - duration of interaction with dog (in seconds)
 panas_pre_neg - pre-condition PANAS score for negative affect
 panas_pre_pos - pre-condition PANAS score for positive affect
 panas_post_neg - post-condition PANAS score for negative affect
 panas_post_pos - post-condition PANAS score for positive affect
 panas_pos_diff - pre-post difference for PANAS score for positive affect
 panas_neg_diff - pre-post difference for PANAS score for negative affect
 vas_anxiety_pre - pre-condition visual analog scale for anxiety
 vas_anxiety_post - post-condition visual analog scale for anxiety
 vas_stress_pre - pre-condition visual analog scale for stress
 vas_stress_post - post-condition visual analog scale for stress
 stai_trait - trait score of State-Trait Anxiety Index
 stai_state - state score of State-Trait Anxiety Index
 drm_accuracy - accuracy score for Deese-Roedinger-McDermott long-term memory task
 drm_d_prime - d' score for Deese-Roedinger-McDermott long-term memory task
 ncpc_pre_diff - pre-condition difference between second and first trial of Necker Cube Pattern Control Test
 ncpc_post_diff - post-condition difference between second and first trial of Necker Cube Pattern Control Test 
 ncpc_diff - pre-post difference for difference between second and first trial of Necker Cube Pattern Control Test
 bds_index_pre - pre-condition backwards digit span index
 bds_index_post - post-condition backwards digit span index
 bds_index_diff - pre-post difference for backwards digit span index
 nback_d_prime_pre - pre-condition d' for n-back task
 nback_d_prime_post - post-condition d' for n-back task
 nback_d_prime_diff - pre-post difference for d' for n-back task

thayer_stevens_2021_data2.csv (item-specifc data for calculating Cronbach's alpha)
 item_1-item_20 - individual items (surveys differ on number of items, so NAs represent no items)
 survey - name of survey

R code:
 thayer_stevens_2021_rcode.R - code for running computations and generating figures

R Markdown documents:
 thayer_stevens_2021.Rmd - R Markdown document with R code embedded for main manuscript
 thayer_stevens_2021_SM.Rmd - R Markdown document with R code embedded for supplementary materials

Instructions to reproduce results:
 To reproduce these results, first clone or unzip the Git repository into a folder. Then, ensure that subfolders named "data", "docs", "figures", and "R" are in the folder. The best way to work with this material is to open the haicognition2021.Rproj file in RStudio. Next, open thayer_stevens_2021_rcode.R and ensure that all packages mentioned at the top of the script are installed.  Once all packages are installed, run the script in R using "source("thayer_stevens_2021_rcode.R")".

 Once the script runs without errors, you can compile the R Markdown document thayer_stevens_2021.Rmd.  Open this file in RStudio and ensure that you have packages {knitr} and {rmarkdown} installed.  Once installed, use {knitr} to render the document (control-shift-k).  Use the same process to render thayer_stevens_2021_SM.Rmd.
 
