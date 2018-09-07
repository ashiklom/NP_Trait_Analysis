# Editor

> Thank you for the revised manuscript on leaf economic spectrum within and among plant functional traits. I have received two reviews of your paper and the reviewers and I appreciate the work you have conducted. As you will see reviewer 1 raises several concerns, while reviewer 2 expresses satisfaction with the revision.
>
> Improvements in this revision include the simplified designation of functional groups, the discussion of sample size issues, and further discussion of biological implications of the results. However, I concur with the concerns raised by reviewer 1. I am not able to accept the paper in its current form, but am willing to invite a further revision.
>
> In a revised manuscript, I would need you to address the following. Some of this could be put into supplemental material, but it needs to be available to readers, especially those who may use the results in the context of DGVMs.
>
> 1) Data imputation. Reviewer 2 is very concerned about imputed data, and I raised this in my prior decision letter.

We believe that most of the concerns with imputation in this manuscript stem from a misunderstanding of our methods.
The issues with data imputation discussed in these reviews primarily affect _single_ imputation,
where data are imputed (by whatever method) in one step, and then subsequent analyses are performed on the imputed data.
However, our approach uses iterative _multiple_ imputation, where the algorithm alternates between drawing parameters and imputing values based on those parameters until it converges on a stable solution.

To address this confusion, we have
(1) revised the relevant text in the methods (lines 246, 253-258. Note that all lines refer to the file with track changes enabled.) and
(2) added a supplementary literate programming document describing in detail our multiple imputation approach and how it is used to estimate trait means and covariances with uncertainty (Method S1).

> I would like to see the following addressed:
>
> a) a table in supplemental material that provides the sample size for each trait, in each functional group, and the number of imputed data points for each trait in each functional group, as well as across all groups (the global data).
>
> b) a table listing all the pairwise correlations (again in supp material) that provides the data presented in the figures: covariance, correlation, sample size of available paired empirical data, sample size after imputation that was used in your analysis.

We have included the corresponding tables as Supplementary Tables S3 and S4, respectively, and added text describing these figures to the methods (lines 267, 307-311).

> c) as requested by reviewer 2, please report the correlations and/or covariance for the various analyses based only on available pairwise data and based on imputed data. A scatterplot would be interesting, across all relationships (correlations better than covariance so that the units are comparable). We need to know how much data imputation influences your results.

Given that multiple imputation is a fundamental aspect of our method and the large extent of missingness (especially pairwise) in the data, we believe that an analysis on un-imputed data would not be worthwhile.
However, we agree with the broader point that the relationship between sampling and correlation strength is worth more detailed investigation.
We have expanded our methods (lines 307-311) and results (lines 397-413) text accordingly.

> d) finally, unless this has been done in previous papers (in which case you could provide citation and discussion), could you do a jackknife where you leave each data point out, rerun the imputation routine and estimate that value, and then report a scatterplot of observed vs. imputed data for each point, and the RMSE? If this was done on normalized data results could be compared across all variables.

Similarly to the previous comment, we believe this analysis does not make sense in the context of our methods.
Again, we are not generating a single imputation for each point, but rather re-imputing the data at each step of our sampling algorithm conditioned on the current values of the mean vector(s) and covariance matrix (or matrices).
Although it is technically possible to calculate summary statistics on these imputations and explore their patterns, we do not believe this provides much additional value beyond that already provided by the mean vectors and covariance matrices estimated using these values.

> 2) You have discussed verbally the influence of sample size on the correlations and significance values. In my prior decision letter I was raising a different issue - the well known behavior that when you sample a narrower range of data (not just a smaller sample), bivariate relationships will weaken.
>
> Please provide (again could be in supplemental material) a scatterplot of a) sample size vs. correlation values across all pairwise relationships...

We have included the corresponding figure as Supplementary Figure S1.

> ...and b) data range vs. correlation values. B is a bit trickier since you have two ranges, one for each data point. If the data were normalized you could report the average range for the two variables, vs. the correlation. These plots will look like the current Fig. 2 but apply to the question of the correlations, not just the data estimates.

We have included such a figure as Supplementary Figure S2.
For completeness, rather than showing the average range of the two variables, we use facets to show the relationship between correlation and data range for each value.

> You report that correlations are weaker within functional groups than in the global data set. But you have not taken steps, as I suggested, to determine if they are weaker purely due to sampling and the narrower range of data, or if there is something biological beyond those sampling issues. Unless you can show that the reduced correlations go above and beyond the sampling effects, it is not clear that the weaker correlations within PFTs are biologically meaningful.
>
> I know these analyses are intensive and the recommended tables may be rather large, but it should all be relatively straightforward in a fully scripted analysis. And please revise the text as appropriate to describe the outcomes of these analyses, including text appendices in supplemental material if needed.

All of these supplemental analyses show that although the strength of our correlation estimates is not entirely independent of sample size and data range,
there is substantial, PFT- and trait-specific variability in patterns of trait correlation that cannot be explained by either of these two variables.
We have revised the text in the results (lines 397-413) to highlight these analyses.

> 3) Please provide the estimated values of the parameters for each functional group in a table, so that they are easily accessible for the DGVM modeling groups.

We have included this information in Tables S1 and S2.

> It would also be desirable to provide scripts for the analysis so other groups could repeat your work, using different PFT designations or updated datasets.

Our implementation of the sampling algorithm used in this manuscript is available on GitHub in a standalone R package (github.com/ashiklom/mvtraits).
This package is designed to handle any data (as long as it is in matrix form), and includes a literate programming vignette demonstrating its application to R's built in "Iris" dataset.
In addition, all of the materials (including code) required to reproduce the analysis for this manuscript is available through the Open Science Framework (https://osf.io/w8y73).
This is stated in the "Methods" section of the manuscript (lines 312-317).

> 4) L283 Does TRY archive the data set requested for this project? How would a user request from TRY and get the same data that you used?

We have confirmed with TRY administrator Jens Kattge that TRY data requests are archived,
and we have expanded this text in the methods accordingly (lines 312-317).
In addition, we have indicated that the data can be provided directly from the lead author (Alexey Shiklomanov) on reasonable request.
Unfortunately, because a sizable fraction of the data used are listed as "restricted" in TRY, re-publishing them in an open repository would be a violation of TRY's data use policy.

> 5) Fig. 4. How did you assign the length of the lines for these correlations. It seems that the global lines should span the full data, but they are contained within the broader cluster of lines for the various PFTs.

The lengths of these lines are determined by the eigenvalues of the estimated variance-covariance matrix subset to the traits of interest (line 286-299).
The reason the global lines do not cover the full extent of the data is that the black and colored lines represent two different models:
The black lines are a multivariate model fit to all of the data at once,
while the colored lines are the results of the hierarchical fit.
The much larger sample size when the data are pooled in the multivariate fit makes the covariance estimate less sensitive to extreme values,
so when a particular PFT has relatively few observations clustered far from the global mean,
the global line may not extend out to that PFT's mean value.

# Reviewer 1

> This ms is focused on estimating trait values by functional group.  This paper finds that current parameters have some problems and that parameteriziation can be improved by estimating the parameters multivariately. This is an important problem for parameterizing the newest versions of earth systems models but it is unclear to me of how much interest this will be to readers of NP.
>
> This ms also has a biological question of whether the covariance structure of traits is the same within and between functional groups. It finds that the structure is similar, although most of the strong covariance is between functional groups and much weaker within functional groups. While novel in the depth at which this question is answered, it is not surprising and matches less careful results elsewhere. It is also not set up as the primary question or result.

The scale-dependence of trait covariance patterns is in the title of our manuscript, is the primary focus of the first and third bullets of the manuscript summary, and is covered by roughly half of our results (including 2 figures) and discussion text.
Therefore, while we acknowledge that we have also devoted substantial space to trait estimates and uncertainty,

> Overall the ms is well-written and executed. The methods use Bayesian hierarchical models which is necessary in this case because a hiearchical model with covariances can not be fit with simpler methods. The authors are to be commended for using conjugate priors instead of just throwing MCMC at this.
>
> 1) My biggest concern is that missing values are imputed. While I understand the issues of how small the sample size would be if species with missing values were ommitted. But I feel like imputing values is equally problematic. The authors make no effort to justify or verify that missing value imputation is acceptable. I would want to see this addressed and also see an analysis with no missing value imputation (however small the sample size) to confirm robustness to this.

Please see responses to similar comments from the Editor above.

> Smaller comments:
> 2) Too many results are not presented in results but in the discussion. Present all results (especially those set up in the methods) in the results section.

All of our results figures are initially presented, and their essential details summarized, in the results section.
We feel that highlighting specific correlations makes more sense in the context of their ecophysiological significance, which is better suited to the discussion.
Therefore, we have left the current structure of the text as is, though we are open to reconsidering the organization of our results and discussion in future drafts if necessary.

> 3) Lines 293-319 - Use a table and one paragraph summarizing biologically interesting/surprising results. Don't need to list each result in prose.

If all of these were similarly structured comparisons (e.g. PFT with highest and lowest trait value), we would agree that a table would be a better way to represent this.
However, our comparisons here are substantially varied that summarizing these results to a table would involve effectively the same text but within a table (which may actually take up more space than the current paragraph form). 
Therefore, we have decided to leave the text as is for this draft, though we would be willing to reconsider revising this section in future drafts if necessary.

> 4) Line 336-337 - I dont' understand why it is not obvious, even axiomatic, that uncertainty would decrease with sample size

In a univariate example, when comparing data that have comparable variances, decrease in uncertainty with increasing sample size is axiomatic.
However, the different plant functional types here have not only unequal sample sizes, but also different data ranges, so it is possible to have different ranges of uncertainty for the same sample size (and indeed, this is the case, as evidenced by the significant scatter around the lines in Figure 3). 
Furthermore, in our multivariate and hierarchical models, uncertainty is also strongly dependent on the strength of covariance between traits, which further confounds the relationship of uncertainty to sample size (as we show).
Therefore, although the decline in uncertainty with increasing sample size is not completely surprising, we believe it is worth our brief but explicit mention.

> 5) Line 343-345 - I expect this is due to use of imputation and the method used. Please address.

Yes, this additional constraint from trait covariance is a result of our multiple imputation method.
However, as discussed above (see responses to Editor comments), we believe that multiple imputation overcomes many of the limitations of single imputation, and is an appropriate technique for the goals of this manuscript.
Notably, the iterative nature of multiple imputation is precisely the reason we are able to provide uncertainty estimates on the covariance matrix.

> 6) Line 405 - just FYI this is the first thing I thought was really biologically interesting and deserving of discussion in the discusison. The results and discussion could be greatly shortened prior to here.

> 7) Line 426 - another important result that should be highlighted by eliminating text around it.

We respectfully disagree with both of these points.
The first discussion paragraph provides our general answer to the question "Do leaf economic relationships hold within PFTs?", which is the primary focus of this manuscript.
The second discussion paragraph begins our set of caveats to that answer, and is important for setting up our point about modeling needleleaf tree productivity.
Therefore, we have chosen to keep this text in its current form, but, again, are willing to consider revision if necessary.

# Reviewer 2

> This is an impressive paper that seems to have done a thorough job of addressing previous reviewers concerns.  It is addressing an important question, uses a rigorous approach, and is remarkably clearly written.  The findings are will provide important guidance to how traits are incorporated into global ecosystem models and fill an important gap in the scales at which trait-trait and trait-environment relationships have been investigated.  I only have a few minor concerns/suggestions.
>
> The authors fail to state how they selected which species to select from the TRY database for their analysis.  I suspect is was all species for which data on at least one of the traits was provided, but further details on the selection process should be provided.

We selected species for which at least one trait measurement was available,
_and_ for which we were able to obtain sufficient information to assign a plant functional type with reasonable confidence.
We have added text clarifying this to the Methods section (line 167).

> Also in the paragraph on line 230, the authors should explicitly state the scale of the imputation --- in other words, what percentage of the matrix was populated with observed vs imputed data.

See responses to related comments from the Editor.

> I found the discussion on the lack of relationship of SLA and temperature (lines 446-448) to be rather weak.  The design of this study is not appropriate for addressing such questions as you have binned a tremendous amount of variability in temperature into three bins.

We agree that this study is not well-equipped to assess trait-climate relationships, and we acknowledge this limitation at both the beginning and end of this paragraph.
However, we believe that placing our work in the context of trait biogeography is useful both as a test of the validity of our results and to highlight the limitations of broad plant functional types for capturing this variability.

> Finally the sentence starting on line 135 appears to missing a word.

We have revised this accordingly.
