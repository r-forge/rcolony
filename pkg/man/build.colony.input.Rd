\name{build.colony.input}
\alias{build.colony.input}

\title{A wizard-like function to help build Colony2 input files.}

\description{
This function prompts the user to provide information about their system in order to correctly build and format a Colony2 input file.
}

\usage{
build.colony.input(wd=getwd(),name="Colony2.DAT")

}

\arguments{
  \item{wd}{The directory where the generated file will be placed. The default is the current working directory.}
  \item{name}{The desired filename for the Colony input file (the default is Colony2.DAT).}
 }

\details{

This wizard will guide you through the process of creating an input file to be executed by Colony2.

You are first requested to specify the \textit{male and female mating system}. Here in our specific context, male monogamous signifies that two offspring in the OFS sample must be fathered by 2 different males if they have separate mothers. In other words, male monogamous specifies that no paternal halfsibs exist in the OFS sample. Note that the mating system herein is defined with regard to the samples being analyzed, not to the population or species from where the samples are taken. For example, consider a population in which males mate singly with females in a breeding season but mate with different females in different breeding seasons. An OFS sample with individuals taken from multiple breeding seasons may contain offspring from different mothers but from a single male (i.e. paternal halfsibs). Therefore, for the purpose of the Colony analysis, the male mating system should still be set as polygamous. The female mating system is similarly defined. Note also that when both males and females are defined as polygamous and the markers have genotyping errors, the computation can become very slow simply because all offspring in the OFS can be related in the pedigree and must be considered together in computing the likelihood of a configuration.

\textit{Species ploidy:} Colony can be used for both diploid species and haplodiploid species. In both cases, the offspring are always assumed to be diploid (for haploid offspring, please use a previous version of Colony). In the haplodiploid case, males and females are assumed to be haploid and diploid respectively (for species with diploid males and haploid females, you just need to swap the two sexes).

\textit{Length of run:} Longer runs consider more configurations in the searching process and thus are more likely to find the maximum likelihood configuration, but take more time to do so. In most cases, a medium run is a good compromise.

\textit{Update allele frequency:} Allele frequencies are required in calculating the likelihood of a configuration. These frequencies can be provided by the user (see below) or are calculated by Colony using the genotypes in OFS, CMS (optional) and CFS (optional). In the latter case, you can ask Colony to update allele frequency estimates by taking into account of the inferred sibship and parentage relationships during the process of searching for the maximum likelihood configuration. However, updating allele frequencies could increase computational time substantially, and may not improve relationship inference much if the genetic structure of your sample is not strong (i.e. family sizes small and evenly distributed, most candidates are not assigned parentage). I suggest not updating allele frequencies except when family sizes (unknown) are large relative to sample size.

\textit{Number of runs:} For the same dataset and parameters of a project, multiple runs can be conducted so that the best configuration with the maximum likelihood is more likely to be found and the uncertainties of the estimates (see below) are more reliable. However, it is very time costly to do multiple runs. Furthermore, in typical situations a single run suffices.

\textit{Random number seed:} Colony takes a simulated annealing algorithm to search for the ML configuration. It is a Monte Carlo method similar to MCMC, with a fine control of re-configuration acceptance rate though “temperature”. Starting from the initial configuration in which all individuals are unrelated except for those individuals with known relationships, a random change is made to part of the configuration to generate a new configuration. The likelihoods of the new and old configurations are then calculated and compared to determine whether the new one is accepted or rejected. If the new likelihood is larger than the old one, then the new configuration is accepted. Otherwise, an acceptance rate is calculated using the current temperature, the new and old likelihood values, and is compared with a random number drawn from a uniform distribution in the range of [0,1]. If the random number value is smaller than the acceptance rate, the new configuration is still accepted although it is inferior to the old one. This is intended to avoid the algorithm getting stuck on a local maximum in the likelihood surface. Therefore, the random number seed partially determines the searching path. With exactly the same data and parameter values, different runs using different random number seeds may give slightly different final best configuration and likelihood values. Such a case occurs occasionally when there is not enough information in the maker data to infer the genetic structure, the actual genetic structure of the sample is extremely weak, or the sample size is very large (i.e. thousands of individuals). For example, when the number of markers is small, and/or the markers are not informative (few alleles with uneven frequency distribution), and/or most families are extremely small (e.g. one offspring per sibship), it is difficult to have replicate runs (using different random number seeds) converge to the same best configuration. One can do multiple runs for the same dataset by using different random number seeds to check/confirm the reliability of the analysis results. In the case replicate runs yield different results, the good news is that relationships reliably inferred are usually reconstructed consistently among runs, while dubious relationships are inferred inconsistently among the runs. One just needs to focus on those reliable, consistent relationships and ignore (abandon) those unreliable, inconsistent relationships in downstream analyses.
 
\textit{Number of threads:} The current version of Colony allows parallel computation using multiple cores/CPUs with shared memory in a single computer. With slight modification, it can also use multiple CPUs with distributed memory in different computers. The parallelization is realized by using MPI, Message  Passing Interface. In brief, parallization is realized at the calculation of likelihood over loci. Each thread calculates the sub-sum of the log-likelihood of a subset of the loci. Once all threads have compeleted their share of computation, the sub-sums are summed and returned as the total log-likelihood. The number of threads specifies how many CPUs/cores of your computer you want to use in the computation. Ideally it should always be defined an integer not larger than the total number of CPUs/Cores of your computer or the number of loci, whichever is smaller. Too many threads actually slows down the computation because of the inter-CPU communication cost. This parallization algorithm is implemented in the current version of Colony.

Another parallization algorithm is that each thread generates a new configuration and calculates its log-likelihood. Then all the threads poll to see whether any new configuration is accepted. If the number of accepted new configurations is zero, then all threads go on. Otherwise, the accepted new configuration of the largest likelihood is broadcased to all threads. The number of threads specifies how many CPUs/cores of your computer you want to use in the computation. Ideally it should be equal to the total number of CPUs/Cores of your computer. When the number of threads is larger than this optimum, the computation becomes slower because of the intercommunication cost among the threads. This parallization algorithm is not included in the current version of Colony.

If your computer has a single CPU/Core, specify a single thread for the best performance.


If 2 or more threads are specified, you need your user credentials to launch Colony for parallel computation. The first time you start the run, you will be asked for the your user account, password and domain. These 3 pieces of information are the same as those you give when you logon the computer. 

\textit{Note to the project.} (not yet implemented in this R version) You can put anything in the text box, such as when you set up the project, notes to the dataset, etc.

\textit{Sibship size prior.} You can choose to use or not use a prior distribution for the paternal and maternal sibship sizes of the offspring. Select No if you have no idea about the average sibship size, or you simply do not want to use a prior. Select Yes if you have a rough estimate of the average paternal and maternal sibship sizes and want to use them in the inference. If you select Yes, you are required to provide the average paternal ($np$) and maternal ($nm$) sibship sizes. Using paternal sibship prior as an example, the prior probability is calculated using Ewen’s sampling formula as follows. Suppose paternal sibship size distribution is $m={m1, m2,..., mn}$, where $mi (i=1, ..., n)$ is the number of paternal sibships each consisting of exactly $i$ offspring. The total number of offspring is EQUATION, and the average number of non-empty paternal sibships (= the number of contributing fathers) is $k =$ EQUATION, where $\alpha$ is a concentration parameter that determines the degree to which individuals are allocated to the same father. We can substitute k by $n/np$ and solve numerically for $\alpha$. Given $\alpha$, the prior probability of $m={m1, m2, ..., mn}$ is .

Note that whenever the male or female mating system parameters have changed, the sibship prior is reset automatically to the default value. Therefore, if you decide to use the sibship prior, you should input the prior parameters \textit{after} setting the mating system parameters.




2)Markers tab
	Number of loci
	Load a file
	Allele frequency (known/unknown)
	
	Checks the loaded file for number of loci, and that the frequencies are numbers rather than letters.

3) Offspring Genotype tab
	Load file, define number of offspring

	Checks the number of indivs (rows) and the number of loci (cols/2)

3) Male genotype data
	Load file
	Prob of dad in male candidates
	
	Checks the number of indivs (rows) and the number of loci (cols/2)

4) Female genotype data
	Load file
	Prob of mum in female candidates
	
		Checks the number of indivs (rows) and the number of loci (cols/2)

	
5) Known Paternal sibs
	Load file with 2 columns - 1) FatherID, 2) OffspringID

6) Known Maternal sibs
	Load file with 2 columns - 1) MotherID, 2) OffspringID

6) Excluded paternity
Load file containing data in 2 columns OffspringID and MaleID
Or a file with n rows. The first element of the row should be offspringID, followed by IDs of candidate males that are excluded from parentage.

8) Excluded maternity
Load file containin	g data in 2 columns OffspringID and FemaleID

9) Excluded Paternal Sibs

In some cases we know that an offspring cannot ahare the same father with one or more other offspring in the sample.
FOrmat for this is A file with n rows.  The first element of the row should be offspringID, followed by IDs of other sibs that are excluded from sibship.


10) Excluded Maternal Sibs
  
  
  
}

\value{
A text file is produced. This file can be used by Colony2 as an input file.
}

\references{ Wang, J. (2004) Sibship reconstruction from genetic data with typing errors.  Genetics 166: 1963-1979. }
\author{Owen R. Jones}
\note{}
\seealso{\code{\link{run.colony}}}
\examples{

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
