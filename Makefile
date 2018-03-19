#Make File for MLH1 repo file
#run report file
Main_Report.md : MLH1_data_setup.RData rmd_report.rmd
	Rscript Main_Report.rmd

#all
all : adj_HetC.png maleMLH1_plot.png femaleMLH1_plot.png PnD_poold_sim.png

#PnD figs
PnD_poold_sim.png : fresh_PnD.R
	Rscript fresh_PnD.R

#MLH1 strain plots
maleMLH1_plot2se.png femaleMLH12se_plot.png : src/MLH1_strain_means.R data/AnonData.csv
	Rscript src/MLH1_strain_means.R src/HetC_plot.R

#HetC Figure
adj_HetC.png : src/HetC_plot.R data/AnonData.csv
	Rscript src/HetC_plot.R

#setup Rdata file
MLH1_data_setup.RData : data/AnonData.csv setup_data.R
	Rscript setup_data.R

#add header
data/AnonData.csv : data/header.csv data/bigdata.csv data/anon_ba*.csv
	cat data/header.csv data/bigdata.csv > data/AnonData.csv

#merge all anon csv files into 1
data/bigdata.csv : data/anon_b*.csv
	tail -q -n +2 data/anon_b*.csv >> data/bigdata.csv

#make header file
data/header.csv : data/anon_b*.csv
	head -1 `ls data/anon_b*.csv -S | head -1` > data/header.csv
