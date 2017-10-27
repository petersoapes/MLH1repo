#Make File for MLH1 repo file

#add header
AnonData.csv : anonheader.csv bigdata.csv
	cat anonheader.csv bigdata.csv > AnonData.csv

#merge all anon csv files into 1
bigdata.csv : data/*.csv
	tail -q -n +2 *.csv >> bigdata.csv

#make header file
anonheader.csv : data/*.csv
	head -1 `ls *.csv -S | head -1` >> anonheader.csv
