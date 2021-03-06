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

#### Mice table

Table of the number of mice used and MLH1 stats. (made with kable).

MLH1\_data &lt;- MLH1\_data %&gt;% arrange(category) %&gt;% mutate(Original.Name = factor(Original.Name)) \#another category that you want the order to match \#order the data frame MLH1\_data &lt;- with(MLH1\_data, MLH1\_data\[order(category),\])

#### Initial Patterns from MLH1 distributions

<img src="rmd_report_files/figure-markdown_github-ascii_identifiers/first boxplots-1.png" style="display: block; margin: auto;" /><img src="rmd_report_files/figure-markdown_github-ascii_identifiers/first boxplots-2.png" style="display: block; margin: auto;" />

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

    ## Warning: Removed 7 rows containing missing values (geom_point).

    ## Warning: Removed 5 rows containing missing values (geom_point).

![caption](rmd_report_files/figure-markdown_github-ascii_identifiers/scatter%20plots%20of%20nMLH1%20by%20score-1.png)

I ploted the mean of each quality bin with a red dot. From the pattern of the red dots, there is definately a negative relationship with quality and nMLH1 foci across the data. This is most pronounced in MSM males and least pronounced in G males. The CAST female data is dummy data.

#### Assessing the Distributions by mouse

latice plot of scatter plots for jitter plots of cell oberservations by quality. The category mean is in black and the mouse specific mean is in red.

![](rmd_report_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-4-1.png)![](rmd_report_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-4-2.png)![](rmd_report_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-4-3.png)![](rmd_report_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-4-4.png)![](rmd_report_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-4-5.png)![](rmd_report_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-4-6.png)![](rmd_report_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-4-7.png)![](rmd_report_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-4-8.png)![](rmd_report_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-4-9.png)![](rmd_report_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-4-10.png)![](rmd_report_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-4-11.png)![](rmd_report_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-4-12.png)![](rmd_report_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-4-13.png)![](rmd_report_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-4-14.png)![](rmd_report_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-4-15.png)

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
