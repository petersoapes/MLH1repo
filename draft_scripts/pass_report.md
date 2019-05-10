Sub-Sampling Effect
================
April Peterson
June 4, 2018

#### Past certeria for mouse level

BD: Approximately 20 cells were scored per animal. We were unable to obtain a sufficient number of high quality images for 39 of the 315 F2 animals.

RW: The average individual was represented by counts from &gt;20 spermatocytes; we omitted individuals with counts from &lt;5 spermatocytes.

AP: &gt;20 cells with 3 highest quality (1,2,3).

Guide / report document for showing which mice have passed - sufficient cells samples for showing good estimate of MLH1 count. Compare re-sampling simulations for 'Hard Pass / Strict Threshold' data to on the edge data, so assess if there is a large effect on mouse-MLH1 mean for cell sample size.

Maybe female and males will have different effects.

### Passing Mouse list

Mice with slides with over 25 cells of good quality: 30 Mice with fewer than 20 cells, but have high proportion of good quality cells (&gt;60%): 39.

What is the distribution of total cell counts for these? For the boarderline mice, total is around ~20 cells, ~15 cells above 4 quality.

Run sub-sampling simulations for data of all these mice ... This requires adjusting the sample sizes, over half the mice have 15 or more.

The chunks with the code for the sub-sampling simulations are skipped.

``` r
#![text](./outside_gitrepo/sampling_effect/BL_Sample_sizes_p_4dec17_HMI_m1.png)

#setwd("./outside_gitrepo/sampling_effect")
```

![text](C:/Users/alpeterson7/Documents/MLH1repo/outside_gitrepo/sampling_effect/BL_Sample_sizes_p_4dec17_HMI_m1.png)

From comparing the sub-sampling plots from High quality (HQ) and Boarder line (BL),

7 mice out of 29 boardline mice, looked like there was a sampling effect. I will prioritie re-imaging those, and start re-imaging the rest with fewer than 20 cells.
