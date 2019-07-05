# CBDA

<a href="http://socr.umich.edu/HTML5/CBDA/"><img align="middle" src="https://raw.githubusercontent.com/SOCR/CBDA/master/Images/CBDA_wordcloud.png"></a>

**Compressive Big Data Analytics (CBDA)**
**Synthetic Datasets**

Large synthetic datasets can be downloaded using the pipeline workflow [Download-Datasets.pipe](https://github.com/SOCR/CBDA/blob/master/Data/Synthetic_Datasets/Large/Download-Datasets.pipe)

If you already have a client version installed on your laptop and a guest account, you can download the pipe file and make the appropriate changes (directories and file name on the remote location on Cranium, the server at USC where the LONI pipeline resides). The selected dataset will be compressed first and then saved in the local directory you specified.

Otherwise the ["LONI webapp"](http://pipeline.loni.usc.edu/webapp) can be used  with a guest account. Similar edits should be made to the pie file before loading on the LONI webapp.

Any dataset used in th CBDA #2 manuscript can be generated from scratch using the R script [SyntheticData_Generator.R](https://github.com/SOCR/CBDA/blob/master/Data/Synthetic_Datasets/Large/SyntheticData_Generator.R) in this directory. Small size synthetic datasets are available [here](https://github.com/SOCR/CBDA/tree/master/Data/Synthetic_Datasets/Small).

The UK BioBank dataset is not publicly accessible, unless you have IRB approval. A modified publicly available version of it can be downloaded [here](https://github.com/SOCR/CBDA/blob/master/Data/UKBB).