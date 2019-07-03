# CBDA

<a href="http://socr.umich.edu/HTML5/CBDA/"><img align="middle" src="https://raw.githubusercontent.com/SOCR/CBDA/master/Images/CBDA_wordcloud.png"></a>

**Compressive Big Data Analytics (CBDA)**
**Synthetic Datasets**

Large synthetic datasets can be downloaded using the pipeline workflow *Download-Datasets.pipe* in this directory.
If you already have a client version installed on your laptop and a guest account, you can download the pipe file, make the appropriate changes (directories and file names). and run it. The dataset that you selected on the ["remote location on Cranium"](/ifs/loni/ccb/collabs/2016/CBDA_SL_2016/2018/Simeone/Datasets/CBDA2-datasets) will be compressed first and then saved in the local directory you specified.
Otherwise the ["LONI webapp"](http://pipeline.loni.usc.edu/webapp) can be used  with a guest account. Similar edits should be made to the pie file before loading on the LONI webapp.

Any dataset used in th CBDA #2 manuscript can be generated from scratch using the R script *SyntheticData_Generator.R* in this directory.