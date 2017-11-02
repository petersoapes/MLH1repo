MLH1 data report
================
April Peterson
August 29, 2017

#### Research Discription

Measure nMLH1 foci per meiotic cell to estimate recombination rate for diverse strains of house mice (rodents). Comparisons the differences in recombination rates across sexes and genetic background to inform models of how meiotic recombination rates evolve. (insert small picture of meixyte)

#### Discription of Report

-   Display plots and figures of distributions of MLH1 counts to assess intial overall patterns.

-   *Report basic statistics of results and to help keep track of the number of images for each category and mouse*

#### Discription of quantification process

Aquired microscope cell images are quantified in batches after being anonimzed. The number of MLH1 foci, number of "achiasmate" and asynasped bivalents are quantified. A quality score is given; 1 to 5 (best to worst).

``` r
#order the data frame
MLH1_data <- with(MLH1_data, MLH1_data[order(sex, strain),])
#set the order of another column, based on another variable (so that when)

MLH1_data <- MLH1_data %>%
  arrange(strain, sex, mouse) %>%
  mutate(Original.Name = factor(Original.Name)) #another category that you want the order to match
# sort your dataframe, by the focal categories
MLH1_data$mouse <- factor(MLH1_data$mouse, levels=unique(MLH1_data$mouse))

#Add fake CAST female data
last_row <-data.frame(Batch = c("100", "100"), X= c("",""), Original.Name=c("12dec18_20dec20_CAST_f1_sp1_12_1_rev.tif", "12dec18_20dec20_CAST_f1_sp1_12_2_rev.tif"), Random.Name = c("1234567.tif","123456789.tif"), quality=c(1,1),nMLH1.foci =c(25,22), XY.paired =c("no","no"), REDO.crop =c("no","no"),n =c(20,20), achiasmate=c(0,0),asynased=c(0,0),notes=c("",""),category = c("CAST female","CAST female"), strain= c("CAST","CAST"), sex=c("female","female"),adj_nMLH1.foci=c(25,22),mouse=c("20dec20_CAST_f1","20dec20_CAST_f1") )

MLH1_data <- rbind(MLH1_data, last_row)

#count the non-quality measures,  #remove non qualit
#length(MLH1_data[ !(is.na(MLH1_data$quality) | MLH1_data$quality==""), ] )  #15 rows with out quality scores, remove.
MLH1_data <- MLH1_data[ !(is.na(MLH1_data$quality) | MLH1_data$quality==""), ]

MLH1_by_F_strain <- MLH1_data[MLH1_data$sex == "female", ]
MLH1_by_M_strain <- MLH1_data[MLH1_data$sex == "male", ]
```

#### Mice table

Table of the number of mice used and MLH1 stats. (made with kable).

| strain | sex    |  Nmice|  Ncells| mean\_co | var    |     sd|     se| dataset |
|:-------|:-------|------:|-------:|:---------|:-------|------:|------:|:--------|
| CAST   | male   |      2|      51| 11.176   | 11.748 |  3.428|  0.480| AP      |
| G      | female |      7|     230| 18.639   | 17.935 |  4.235|  0.279| AP      |
| G      | male   |      8|     179| 13.341   | 7.012  |  2.648|  0.198| AP      |
| HMI    | male   |      1|       8| 15.125   | 28.125 |  5.303|  1.875| AP      |
| LEWES  | female |      4|      80| 15.713   | 25.043 |  5.004|  0.559| AP      |
| LEWES  | male   |      5|     166| 14.602   | 12.350 |  3.514|  0.273| AP      |
| MSM    | female |      4|      90| 17.011   | 19.764 |  4.446|  0.469| AP      |
| MSM    | male   |      6|     148| 19.615   | 17.245 |  4.153|  0.341| AP      |
| other  | male   |      1|      25| 14.160   | 11.057 |  3.325|  0.665| AP      |
| PWD    | female |     13|     257| 15.747   | 14.221 |  3.771|  0.235| AP      |
| PWD    | male   |      7|     162| 18.858   | 9.166  |  3.028|  0.238| AP      |
| SPRET  | male   |      1|      31| 14.774   | 5.647  |  2.376|  0.427| AP      |
| WSB    | female |      7|     134| 13.791   | 11.339 |  3.367|  0.291| AP      |
| WSB    | male   |      5|      96| 13.052   | 8.787  |  2.964|  0.303| AP      |

#### Initial Patterns from MLH1 distributions

<img src="rmd_report_files/figure-markdown_github-ascii_identifiers/first boxplots-1.png" style="display: block; margin: auto;" />

After taking the data from 2 highest cell quality, some but not all mouse means converge.

<img src="rmd_report_files/figure-markdown_github-ascii_identifiers/histogram-1.png" style="display: block; margin: auto;" />

#### Comparison of MLH1 distributions

![](rmd_report_files/figure-markdown_github-ascii_identifiers/show%20boxplots-1.png)![](rmd_report_files/figure-markdown_github-ascii_identifiers/show%20boxplots-2.png)

After taking the data from 2 highest cell quality, some but not all mouse means converge. For some mice (mostly female) the majority of cells are excluded. This is especially true for PWD and WSB females.

#### Tests for quality and nMLH1 foci number

> *How do the distributions change across different quality scores?*

> *How do the distributions change across mice with a good number of cells?*

##### Effects of quality

Human quantification seems to be biased towards rating cells with more MLH1 foci as higher quality. Unbiased cell quality assignment, would not show a positive correlation with quality and nMLH1. (CAST female data is fake)

![caption](rmd_report_files/figure-markdown_github-ascii_identifiers/scatter%20plots%20of%20nMLH1%20by%20score-1.png)

I ploted the mean of each quality bin with a red dot. From the pattern of the red dots, there is definately a negative relationship with quality and nMLH1 foci across the data. This is most pronounced in MSM males and least pronounced in G males. The CAST female data is dummy data.

#### Assessing the Distributions by mouse

latice plot of scatter plots for jitter plots of cell oberservations by quality. The category mean is in black and the mouse specific mean is in red.

Making all of these scatter plots, allows us to look at the whole distributions of the data for each mouse. The distance of the red line from the black could be a indicator of slides or mice with slide specific technical noise.

PWD females and a very large range. This is driven by data from 17apr15\_PWD\_f4. These data should be looked at more closely.

In some mice there seems to be bimodel cell populations, those of clearly high quality and those of lower quality. This may be due to the screening of images before quantification, and allowing more bad cells data remain in the data set, instead of deleting them.

#### Power Calculations

Main questions to address:

> *How many cells per mouse should be sampled?*

> *How many mice per strain should be sampled?*

Outline discription of the loop for comparing sample size effects

1.  take ~100 independant samples of total images across different sample sizes (10%, 25%, 50%, 75%, 90%)
2.  then run t.tests across those samples, and report the p value
3.  make a table of the p values for these sample permutations
4.  plot the p values or mean p values across sample sizes

Currently this section is under construction.
