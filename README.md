---
title: '**Compressive Big Data Analytics**'
subtitle: 'A complete guide to the Compressive Big Data Analytics [**CBDA**]'
author: 'Simeone Marino<em>&#42;, Nina Zhou<em>&#42;, Yi Zhao and Ivo D. Dinov          <em>&#42;equal contribution'
#author: "Simeone Marino(*), Nina Zhou(*), Yi Zhao and Ivo D. Dinov. (*)=Equal contribution"
date: "`r format(Sys.time(), '%b %d %Y')`"
tags: [Big Data, SOCR, MIDAS, Compressive Sensing, Predictive Analytics]
output:
  html_document:
    ##theme: cerulean
    theme: spacelab
    highlight: tango
    includes:
      before_body: SOCR_CBDA_header.html
      after_body: SOCR_CBDA_footer.html
    toc: true
    number_sections: false
    toc_depth: 4
    toc_float:
      collapsed: false
      smooth_scroll: true
---

###1 How to navigate the CBDA site{.tabset .tabset-fade .tabset-dropdown}
This is a dynamic webpage that allows interactive navigation via ToC (top-left) and tabs (horizontal and vertical hide/expand segments). By clicking on each blue hyperlinks in the TOC, the reader can navigate between different sections. By clicking on the tabs the reader can navigate within the many parts of each section.
**The video below illustrates the main features of the CBDA protocol.**


```{r Figure, echo=FALSE, eval=FALSE, out.width = '100%'}
knitr::include_graphics("C:/Users/simeonem/Documents/CBDA-package/CBDA-RMD-SOCR/CBDA-flowchart-v4.png")
#htmltools::tags$iframe(title = "My DataSifter Images Slideshow", src = "slideshow_file.html",    width="1000",    height="500")

```

<video width="800" height="600" poster="CBDA-flowchart-v4.png" controls>
  <source src="C:/Users/simeonem/Documents/DataSifterProject/DEMO/DataSifter_Animation.mp4" type="video/mp4" >
</video>

--------------------------------------------------------------------------------------------------------------

###2 Motivation{.tabset .tabset-fade .tabset-dropdown}
The theoretical foundations of Big Data Science are not fully developed, yet. This research proposes a new scalable framework for Big Data representation, high-throughput analytics (variable selection and noise reduction), and model-free inference. Specifically, we explore the core principles of distribution-free and model-agnostic methods for scientific inference based on Big Data sets. We are also working on elucidating the mathematical framework that will eventually enable the study of the ergodic properties and the asymptotics of the specific statistical inference approaches utilized within the CBDA technique.

--------------------------------------------------------------------------------------------------------------

####2.1 Background
This document illustrates the first release of the CBDA protocol as described in the published manuscript ["Controlled Feature Selection and Compressive Big Data Analytics: Applications to Big Biomedical and Health Studies"](https://doi.org/10.1371/journal.pone.0202674).


```{r PlosOne, echo=FALSE, eval=TRUE, out.width = '100%'}
knitr::include_graphics("C:/Users/simeonem/Documents/CBDA-package/CBDA-RMD-SOCR/PlosHeader.png")
#htmltools::tags$iframe(title = "My DataSifter Images Slideshow", src = "slideshow_file.html",    width="1000",    height="500")
```


The proposed Compressive Big Data analytics (CBDA) provides a general foundation for effective representation, efficient processing, and model-free inference for complex heterogeneous data archives. Specifically, CBDA allows us to eliminate noise, forecast trends, compute probabilities, estimate likelihoods, and classify large, incomplete, and heterogeneous data from multiple sources. 
We developed a novel method and a protocol that iteratively generates random (sub)samples from a big and complex dataset. This subsampling with replacement is conducted on the feature and case levels and results in samples that are not necessarily consistent or congruent across iterations. The approach relies on an ensemble predictor where established model-based or model-free inference techniques are iteratively applied to preprocessed and harmonized samples. 
In a nutshell, repeating the subsampling and prediction steps many times, yields derived likelihoods, probabilities, or parameter estimates, which can be used to assess the algorithm reliability and accuracy of findings via bootstrapping methods, or to extract important features via controlled variable selection. CBDA provides a scalable foundation for addressing some of the challenges associated with handling complex, incongruent, incomplete and multi-source data and analytics challenges.

The CBDA protocol has been developed in the [R environment](https://www.r-project.org). Since a large number of smaller training sets are needed for the convergence of the protocol, we created a workflow that runs on the [LONI pipeline environment](http://pipeline.loni.usc.edu), a free platform for high performance computing that allows the simultaneous submission of hundreds of independent instances/jobs of the CBDA protocol. The methods, software and protocols developed here are openly shared on our [GitHub repository](https://github.com/SOCR/CBDA). All software, workflows, and datasets are publicly accessible. 

####2.2 CBDA R Package Installation
The version 1.0.0 of the CBDA package can be downloaded and installed with the following command:
```{r Installation of the CBDA package from CRAN, eval = FALSE}
install.packages("CBDA",repos = 'https://cran.r-project.org/')
```

The documentation and vignettes can be found on  [CRAN](https://cran.r-project.org/web/packages/CBDA/index.html). 
The source and binary files for updates can be found on our [Github repository](https://github.com/SOCR/CBDA). 
```{r Installation of the CBDA package, eval = FALSE, echo=FALSE}
The [binary](https://github.com/SOCR/CBDA/releases/download/1.0.0/CBDA_1.0.0.zip) and the  [source](https://github.com/SOCR/CBDA/releases/download/1.0.0/CBDA_1.0.0.tar.gz) files for the CBDA R package can also be downloaded from our [Github repository](https://github.com/SOCR/CBDA/releases/tag/1.0.0) and install it via the following commands.
# Installation from the Windows binary (recommended for Windows systems)
install.packages("/filepath/CBDA_1.0.0.zip", repos = NULL, type = "win.binary") 
# Installation from the source (recommended for Macs and Linux systems)
install.packages("/filepath/CBDA_1.0.0.tar.gz", repos = NULL, type = "source")
```


The necessary packages to run the CBDA algortihm are installed automatically at installation. However, they can also be installed/attached by launching the *CBDA_initialization()* function (see example in the R chunk below).  If the parameter *install* is set to *TRUE* (by default it's set to FALSE), then the *CBDA_initialization()* function installs (if needed) and attaches all the necessary packages to run the CBDA package v1.0.0. This function can be run before any production run or test. The list of packages can pe personalized to comprise extra packages needed for an expanded SL.library or for other needs by the user. The output shows a table (see Figure below) where for each package a TRUE or FALSE is displayed. Thus the necessary steps can be pursued in case some package has a FALSE. 

**N.B.: to eliminate a warning in Windows due to the "doMC" package not available (it was intended for Mac), install the "doMC" with the following command "install.packages("doMC", repos="http://R-Forge.R-project.org")"**

![ipaktable](https://user-images.githubusercontent.com/18661302/36685272-d55b23c0-1af0-11e8-9479-528ef2dfacf6.JPG){width=90%}

###3 Acknowledgments
We thank all the co-authors of the CBDA manuscript as well as everyone that contributed and is still contributing to the CBDA project in many ways.

More details can be found in the manuscript [Simeone Marino, Jiachen Xu, Yi Zhao, Nina Zhou, Yiwang Zhou and Ivo D. Dinov. (2018) "Controlled Feature Selection and Compressive Big Data Analytics: Applications to Big Biomedical and Health Studies"](https://doi.org/10.1371/journal.pone.0202674).



<!--html_preserve-->

<div>
<footer>
  <center>
<a href="http://www.socr.umich.edu/">SOCR Resource</a>
Visitor number <img src="http://counter.digits.net/?counter=SOCR"
align="middle" border="0" height="20" hspace="4" vspace="2" width="60">
<script type="text/javascript">
var d = new Date();
document.write(" | " + d.getFullYear() + " | ");
</script>

<a href="http://socr.umich.edu/img/SOCR_Email.png"><img alt="SOCR Email"

title="SOCR Email" src="http://socr.umich.edu/img/SOCR_Email.png"

style="border: 0px solid ;"></a>
</center>
</footer>

<!-- Start of StatCounter Code -->
<script type="text/javascript">
var sc_project=5714596;
var sc_invisible=1;
var sc_partition=71;
var sc_click_stat=1;
var sc_security="038e9ac4";
</script>

<script type="text/javascript" src="https://www.statcounter.com/counter/counter.js"></script>

<!-- End of StatCounter Code -->

<!-- GoogleAnalytics -->

<script src="https://www.google-analytics.com/urchin.js" type="text/javascript"> </script>
<script type="text/javascript"> _uacct = "UA-676559-1"; urchinTracker(); </script>

<!-- End of GoogleAnalytics Code -->

</div>

<!--/html_preserve-->
