#Make File for MLH1 repo file


#setup Rdata file
MLH1_data_setup.RData : data/AnonData.csv setup_data.R
	Rscript setup_data.R

#add header
data/AnonData.csv : data/anonheader.csv data/bigdata.csv
	cat data/anonheader.csv data/bigdata.csv > data/AnonData.csv

#merge all anon csv files into 1
data/bigdata.csv : data/*.csv
	tail -q -n +2 data/*.csv >> data/bigdata.csv

#make header file
data/anonheader.csv : data/*.csv
	head -1 `ls data/*.csv -S | head -1` >> data/anonheader.csv
