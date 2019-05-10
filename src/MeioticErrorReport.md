MeioticErrorReport
================
April Peterson
March 1, 2018

Meiotic Errors Report
---------------------

### background on 'defects'

Example images, what do I mean by asynapsis and achiasmate?

tried adding example images, but rmd not coorporating.

![Cell with asynasped chrms.](asyn_example.png) asynapsis, SC comes apart, SC not fully synapsed. (prevents MLH1 foci)

![0CO chrm example.](achi_example.png)

0CO class (not technically achiasmate, but that's what we've been calling it) more prevelant in oocytes because checkpoints are weaker... (source)

##### Biological relevance

Different asynaspsis levels could indicate that there are differences in the speed and timing of the meiotic cycle. Synapsis marks the pachtyenma stage, there is asynapsis before and affer. I believe that The asynapsis I note ... is affects a subset of the chromosomes in a cell. (chrms are out of sync with each other), sometimes I observed all chrms are fully synasped and a single homolog pair is unsynapsed. (BD mentioned she observed a high number of X homolog asynapsis I'm not sure how she knew they were Xs)

0CO chrms are not nesscearilly achiasmate tetrads, a better measure would be taken from when chiasmata appear across the spindel. I think the biological relevance to these measures are either the CO maturation effect or balance/bias of MLH1 CO pathway compared to MLH1-independant pathways.

##### Predictions:

-   Females will have more 0COs and asynasped chrms per cell
-   Within females, higher CO strains, (G), will have fewer 'defects'

#### Data

I will calculate the average number of 0CO and asynasped chrm per cell (since the number of cells in the MLH1 data set is variable.) I'll do this at the strain and mouse level.

From going back and looking at the files and counting in the table, it seems that the number of cells for which 0CO and asynasped wasn't counted is minimal. These missing values will be easy to go back a fix, but in the meantime, I these will do for assessing general patterns.

(make sure to check where the datasetup, removes any of these rows)

or other ways to visualize these boxplots not good since they need distributions. the average mesure per cell is a single measure- point, (with CI). If I calculate the mouse average proportions, those can then form distributions.

![](MeioticErrorReport_files/figure-markdown_github-ascii_identifiers/plots1%20show-1.png)![](MeioticErrorReport_files/figure-markdown_github-ascii_identifiers/plots1%20show-2.png)

In all strains, females have more 'defects' than males. (expect for the CAST female data, which is probaby fake). Spicilegus females have the highest 0COs and asynapsed chrms. Males seem to have a similar low level of average 0COs (PWD and SPRET have slightly more)

Females have more 0CO and asynasped chrms than males. The number of asynased varies by strain, while the proportion of 0CO is pretty consistant across strains.

The mouse average distributions of defect proportions --- females have higher values and are more variable across mice.

    ## Warning: Removed 2 rows containing missing values (geom_point).

![](MeioticErrorReport_files/figure-markdown_github-ascii_identifiers/plots2%20show-1.png)

    ## Warning: Removed 2 rows containing missing values (geom_point).

![](MeioticErrorReport_files/figure-markdown_github-ascii_identifiers/plots2%20show-2.png)

### Further Questions

-   do defects affect MLH1 measures (compare mouse averages for defective and all normal cells)?

-   what is the relationship between cell quality scores and defects?
