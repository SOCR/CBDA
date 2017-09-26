# CBDA
Compressive Big Data Analytics (CBDA)  

The theoretical foundations of Big Data Science are not fully developed, yet. 
The CBDA project investigates a new Big Data theory for high-throughput analytics and model-free Inference. 
Specifically, we explore the core principles of distribution-free and model-agnostic methods for scientific inference 
based on Big Data sets. Compressive Big Data analytics (CBDA) represents an idea that iteratively generates random (sub)samples
from the Big Data collection, uses established techniques to develop model-based or non-parametric inference, 
repeats the (re)sampling and inference steps many times, and finally uses bootstrapping techniques to quantify probabilities, 
estimate likelihoods, or assess accuracy of findings. The CBDA approach may provide a scalable solution avoiding 
some of the Big Data management and analytics challenges. CBDA sampling is conducted on the data-element level, 
not on the case level, and the sampled values are not necessarily consistent across all data elements 
(e.g., high-throughput random sampling from cases and variables within cases). An alternative approach is to use 
Bayesian methods to investigate the theoretical properties (e.g., asymptotics, as sample sizes increase to infinity, 
but the data has sparse conditions) of model-free inference entirely based on the complete dataset without any parametric 
or model-limited restrictions.

This project investigates the parallels between the (established) compressive sensing (CS) for signal representation, 
reconstruction, recovery and data denoising, and the (new) field of big data analytics and inference. Ultimately, 
the project will develop the foundational principles of scientific inference based on compressive big data analytics. 
We investigate various methods for efficient data-aggregation, compressive-analytics, scientific inference, 
interactive services for data interrogation, including high-dimensional data visualization, and integration of research 
and education. Specific applications include neuroimaging-genetics studies of Alzheimer’s disease, predictive modeling of 
cancer treatment outcomes, and high-throughput data analytics using graphical pipeline workflows.

A pre-release of the CBDA protocol is available [here](https://github.com/SOCR/CBDA/releases/tag/v0.1-alpha). A manuscript entitled "Controlled Feature Selection and Compressive Big Data Analytics: Applications to Big Biomedical and Health Studies" is currently under review.

The CBDA protocol has been developed in the [R environment](https://www.r-project.org). Since a large number of smaller training sets are needed for the convergence of the protocol, we created a workflow that runs on the [LONI pipeline environment](http://pipeline.loni.usc.edu), a free platform for high performance computing that allows the simultaneous submission of hundreds of independent instances/jobs of the CBDA protocol. The methods, software and protocols developed here are openly shared on our [GitHub repository](https://github.com/SOCR/CBDA). All software, workflows, and datasets are publicly accessible. The CBDA protocol steps are illustrated in **Figure 1**. 
![figure1](https://user-images.githubusercontent.com/18661302/30587406-0c2edf2c-9d01-11e7-8cef-45f3595ade65.png).
See the [CBDA pre-release](https://github.com/SOCR/CBDA/releases/tag/v0.1-alpha) for details.

The source code to run the CBDA protocol is at [source1.zip](https://github.com/SOCR/CBDA/archive/v0.1-alpha.zip) or at [source2.zip](https://github.com/SOCR/CBDA/archive/v0.1-alpha.tar.gz).

## References
* [PMID:26998309](https://www.ncbi.nlm.nih.gov/pubmed/26998309)
* [PMID:26918190](https://www.ncbi.nlm.nih.gov/pubmed/26918190)
