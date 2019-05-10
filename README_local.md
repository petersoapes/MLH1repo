### TODO:

- making strain specific reports, link the excel files (csv files) to these rmd pages
- add the passing report / make passing function
- make sure the full makefile works
- learn to make a pdf version of these reports
- make a table of contents


### subdirs
- data: all anon_batch csv files,
  goal: put the strain specific files in this..?


### Relevant Files
- Main_report.md; Displays basic stats of MLH1 data by mouse. Used to asses distributions of MLH1 count and cell quality score.
- MeioticErrorReport.md; plots for stats of 'error' meterics from data, (0CO and asynapsed chrms).

- PolymorphismDivergenceSimulations.md* (Coming soon); display results which run simulations to calculate within and between subspecies variation. 
- SubSample.md* (coming soon); analysis for how mouse and genotype mean changes with subsampling, (test effect of uneven sample sizes).
- ResultsOutline.md*; outline and draft figures for Result section of manuscript.





### trouble shooting

- duplicate data in MLH1_data:

  1. delete header.csv, big .. anon.csv files (remake them fresh)
  2. ran make MakeFile, (nothing to be done...) --- but it works when, "make data/big.csv"

initial - 3054
slimed down - 2781
  


### Image Preprocessing notes

Run join.py script with Python 3.3.5. Skimage, numpy, and tiffle should be installed. There is some imcompatibility with other versions of Python and tiffle (or skimage). 
