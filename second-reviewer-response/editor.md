# Editor

> Thank you for the revised manuscript on leaf economic spectrum within and among plant functional traits. I have received two reviews of your paper and the reviewers and I appreciate the work you have conducted. As you will see reviewer 1 raises several concerns, while reviewer 2 expresses satisfaction with the revision.

> Improvements in this revision include the simplified designation of functional groups, the discussion of sample size issues, and further discussion of biological implications of the results. However, I concur with the concerns raised by reviewer 1. I am not able to accept the paper in its current form, but am willing to invite a further revision.

> In a revised manuscript, I would need you to address the following. Some of this could be put into supplemental material, but it needs to be available to readers, especially those who may use the results in the context of DGVMs.

> 1) Data imputation. Reviewer 2 is very concerned about imputed data, and I raised this in my prior decision letter.

We believe that most of the concerns with imputation in this manuscript stem from a misunderstanding of our methods.
The issues with data imputation discussed in these reviews primarily affect _single_ imputation,
where data are imputed (by whatever method) in one step, and then subsequent analyses are performed on the imputed data.
However, our approach uses iterative _multiple_ imputation, where the algorithm alternates between drawing parameters and imputing values based on those parameters until it converges on a stable solution.

To address this confusion, we have
(TODO) revised the relevant text in the methods and
(TODO) added a supplementary "vignette" describing in detail our multiple imputation approach and how it is used to estimate trait means and covariances with uncertainty.

> I would like to see the following addressed:
> a) a table in supplemental material that provides the sample size for each trait, in each functional group, and the number of imputed data points for each trait in each functional group, as well as across all groups (the global data).

(TODO) We have included the corresponding table as Supplementary Table X. 

> b) a table listing all the pairwise correlations (again in supp material) that provides the data presented in the figures: covariance, correlation, sample size of available paired empirical data, sample size after imputation that was used in your analysis.

(TODO) We have included the corresponding table as Supplementary Table X.

> c) as requested by reviewer 2, please report the correlations and/or covariance for the various analyses based only on available pairwise data and based on imputed data. A scatterplot would be interesting, across all relationships (correlations better than covariance so that the units are comparable). We need to know how much data imputation influences your results.

(TODO)

> d) finally, unless this has been done in previous papers (in which case you could provide citation and discussion), could you do a jackknife where you leave each data point out, rerun the imputation routine and estimate that value, and then report a scatterplot of observed vs. imputed data for each point, and the RMSE? If this was done on normalized data results could be compared across all variables.

(TODO)

> 2) You have discussed verbally the influence of sample size on the correlations and significance values. In my prior decision letter I was raising a different issue - the well known behavior that when you sample a narrower range of data (not just a smaller sample), bivariate relationships will weaken.

> Please provide (again could be in supplemental material) a scatterplot of a) sample size vs. correlation values across all pairwise relationships...

(TODO) We have included the corresponding figure as Supplementary Figure X.

> ...and b) data range vs. correlation values. B is a bit trickier since you have two ranges, one for each data point. If the data were normalized you could report the average range for the two variables, vs. the correlation. These plots will look like the current Fig. 2 but apply to the question of the correlations, not just the data estimates. You report that correlations are weaker within functional groups than in the global data set. But you have not taken steps, as I suggested, to determine if they are weaker purely due to sampling and the narrower range of data, or if there is something biological beyond those sampling issues. Unless you can show that the reduced correlations go above and beyond the sampling effects, it is not clear that the weaker correlations within PFTs are biologically meaningful.

(TODO)

> I know these analyses are intensive and the recommended tables may be rather large, but it should all be relatively straightforward in a fully scripted analysis. And please revise the text as appropriate to describe the outcomes of these analyses, including text appendices in supplemental material if needed.

(TODO)

> 3) Please provide the estimated values of the parameters for each functional group in a table, so that they are easily accessible for the DGVM modeling groups. It would also be desirable to provide scripts for the analysis so other groups could repeat your work, using different PFT designations or updated datasets.

(TODO)
> 4) L283 Does TRY archive the data set requested for this project? How would a user request from TRY and get the same data that you used?

(TODO)

> 5) Fig. 4. How did you assign the length of the lines for these correlations. It seems that the global lines should span the full data, but they are contained within the broader cluster of lines for the various PFTs.

(TODO)

# Reviewer 1

> This ms is focused on estimating trait values by functional group.  This paper finds that current parameters have some problems and that parameteriziation can be improved by estimating the parameters multivariately. This is an important problem for parameterizing the newest versions of earth systems models but it is unclear to me of how much interest this will be to readers of NP.
>
> This ms also has a biological question of whether the covariance structure of traits is the same within and between functional groups. It finds that the structure is similar, although most of the strong covariance is between functional groups and much weaker within functional groups. While novel in the depth at which this question is answered, it is not surprising and matches less careful results elsewhere. It is also not set up as the primary question or result.
>
> Overall the ms is well-written and executed. The methods use Bayesian hierarchical models which is necessary in this case because a hiearchical model with covariances can not be fit with simpler methods. The authors are to be commended for using conjugate priors instead of just throwing MCMC at this.
>
> 1) My biggest concern is that missing values are imputed. While I understand the issues of how small the sample size would be if species with missing values were ommitted. But I feel like imputing values is equally problematic. The authors make no effort to justify or verify that missing value imputation is acceptable. I would want to see this addressed and also see an analysis with no missing value imputation (however small the sample size) to confirm robustness to this.
>
> Smaller comments:
> 2) Too many results are not presented in results but in the discussion. Present all results (especially those set up in the methods) in the results section.

(TODO)

> 3) Lines 293-319 - Use a table and one paragraph summarizing biologically interesting/surprising results. Don't need to list each result in prose.

(TODO)

> 4) Line 336-337 - I dont' understand why it is not obvious, even axiomatic, that uncertainty would decrease with sample size

(TODO)

> 5) Line 343-345 - I expect this is due to use of imputation and the method used. Please address.

(TODO)

> 6) LIne 405 - just FYI this is the first thing I thought was really biologically interesting and deserving of discussion in the discusison. The results and discussion could be greatly shortened prior to hear.

(TODO)

> 7) LIne 426 - another important result that should be highlighted by eliminating text around it.

(TODO)

# Reviewer 2

> This is an impressive paper that seems to have done a thorough job of addressing previous reviewers concerns.  It is addressing an important question, uses a rigorous approach, and is remarkably clearly written.  The findings are will provide important guidance to how traits are incorporated into global ecosystem models and fill an important gap in the scales at which trait-trait and trait-environment relationships have been investigated.  I only have a few minor concerns/suggestions.
>
> The authors fail to state how they selected which species to select from the TRY database for their analysis.  I suspect is was all species for which data on at least one of the traits was provided, but further details on the selection process should be provided.

(TODO) We selected species for which at least one trait measurement was available,
_and_ for which we were able to obtain sufficient information to assign a plant functional type with reasonable confidence.
We have added text clarifying this to the Methods section (Line XXX).

> Also in the paragraph on line 230, the authors should explicitly state the scale of the imputation --- in other words, what percentage of the matrix was populated with observed vs imputed data.

(TODO) We have added this information to the supplementary material, and revised the indicated text accordingly (Line XXX).

> I found the discussion on the lack of relationship of SLA and temperature (lines 446-448) to be rather weak.  The design of this study is not appropriate for addressing such questions as you have binned a tremendous amount of variability in temperature into three bins.

(TODO)

> Finally the sentence starting on line 135 appears to missing a word.

(TODO)

# References {-}