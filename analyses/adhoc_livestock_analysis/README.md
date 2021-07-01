### Adhoc Livestock Analysis
This RMarkdown document in this repository contains the R codes employed in executing the adhoc livestock analysis task. The task <br>
involves creating sleeping and exposure variables for each provided COST ACD households within the study area. The Code is based on instructions outlined in
the [sleeping_variable](https://drive.google.com/file/d/1IovDGDuUdnb4muwjiM8lcUz79PrjPqcw/view?usp=sharing) and [exposure_variable](https://drive.google.com/file/d/1bprl_kso5TREE4Lbmw1JlDvIamuXwu8G/view?usp=sharing) pdf files.<br>

#### Variables to Create

Sleeping Variables<br>
• sleep_livestock: all animals.<br>
• sleep_pigs: only pigs.<br>
• sleep_cattle: only cow.<br>

Exposure Variables<br>
• exp_acd_livestock: all animals using COST acd data.<br>
• exp_acd_pigs: only pigs using COST acd data.<br>
• exp_acd_goats: only goats using COST acd data.<br>
• exp_boh_livestock: all animals using BOHEMIA data.<br>
• exp_boh_pigs: only pigs using BOHEMIA data.<br>
• exp_boh_cattle: only cattle using BOHEMIA data.<br>

### Input Data
[acd_mopeia.dta](https://drive.google.com/file/d/1TDurkNF7RQft_7BYT-OSybVbR7U246s6/view?usp=sharing) :  This contains data on all cost acd  households. This includes
                the number of livestock (all livestocks, cattle, goats, sheep, chicken and horse) owned.<br>
[minicensus_data.RData](https://drive.google.com/file/d/1O14wl2NRq-ffRaHYhvinRfiHm0T2_O_6/view?usp=sharing) : This contains data on all Bohemia  households including type,
                number and location of livestock owned(cattle , pig and both) and other useful features on Bohemia households.<br>
[COST_ACD_Children_Ages_and_Gender._EE.16.04.2019.csv](https://drive.google.com/file/d/1vXGrh-P2jZ9Ml2xQUJ6g4mZLhXQhI2dW/view?usp=sharing) : Data on all cost acd households including location (longitude and latitude). <br>

Follow the code in the RMarkdown document to reproduce results.
