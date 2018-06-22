# Discussion

## Scale dependence of the leaf economic spectrum

The canonical leaf economic spectrum is defined by a negative correlation of between SLA and leaf lifespan, and a positive correlation of SLA with $N_{mass}$, $P_{mass}$, and photosynthesis and respiration rates [@wright_worldwide_2004].
The first objective of this paper was to investigate the extent to which these relationships hold within and across PFTs.
Our results indicate that the leaf economic spectrum generally still holds within PFTs, at least at the functional and phylogenetic scale of the current generation of Earth System Models.
Within PFTs, correlations between SLA, $N_{mass}$, and $P_{mass}$ were consistently positive, and correlations of these traits with leaf lifespan were generally negative (though, for many PFTs, correlations were not significantly different from zero).

While the direction of relationships we observed was largely consistent across different PFTs, the strength of these correlations was more variable.
(TODO Revise)
Taken together with the finding that evergreen conifers are relatively unique in their consistently proportional allocation of N to cell walls and Rubisco [@onoda_physiological_2017], our results support the idea that needleleaved species primarily adapt to environmental changes through structural changes (i.e. increasing or decreasing SLA) rather than through modulating foliar biochemistry [@robakowski_2004_needle].

We found that correlations between leaf nutrient concentrations and traits related to photosynthetic metabolism---which are often used in ecosystem modeling to parameterize photosynthesis [@clm45_note; @rogers_roadmap_2017]---were highly variable between PFTs.
Although trait correlations are not necessarily indicative of allocation strategies, this result generally supports the findings of @ghimire_2017_Nallocation that N allocation strategies to photosynthesis vary widely by PFT.
In particular, the correlations of $N_{mass}$ with traits related to photosynthesis and respiration ($R_{d,mass}$, $V_{c,max,mass}$, and $J_{max,mass}$) were weaker for tropical evergreen broadleaved trees than for most other PFTs, but the correlation of these traits with $P_{mass}$ was comparable or even slightly higher.
This suggests that the productivity of tropical species is generally P-limited [@reich_global_2004; @ghimire_2017_Nallocation], that N allocation strategies are more variable under N-poor than N-rich conditions [@ghimire_2017_Nallocation], or more generally that photosynthetic parameters are affected more strongly by environmental covariates than leaf nitrogen contents [@Ali_2015].
Meanwhile, our result that the $N_{area}$ - $V_{c,max,area}$ correlation is generally weaker for needleleaved than broadleaved species was also found by @kattge_2009_vcmax, and supports the lower allocation of N to photosynthesis found by @ghimire_2017_Nallocation.
In light of this fact and considering that boreal forests, largely populated by needleleaf species, have the largest influence on global climate of any biome [@Snyder_2004; @bonan_forests_2008], we suggest that parameterization of needleleaf tree productivity based on foliar nitrogen content in Earth System Models be treated with caution.

Correlations of all traits with leaf lifespan were weaker (often to the point of becoming insignificant) within most PFTs than globally.
This suggests that leaf economic relationships related to leaf lifespan are dominated by the fundamental differences between deciduous and evergreen PFTs, while the factors driving variability in leaf lifespan within PFTs are more complex and idiosyncratic [@Reich_2014; @wu_leaf_2016].
That being said, much of this within-PFT variability is known to be driven by variations in shade responses, and a key limitation of our study is that we have no information about the relative canopy positions at which traits were collected [@lusk_why_2008; @keenan_global_2016].

Across PFTs, the interaction between growth form and biome in PFT definitions (Table 1) confounds the interpretation of our results with respect to well established ecological and biogeographic patterns.
For example, @poorter_causes_2009 found variation in SLA by growth form was, from highest to lowest, grasses, deciduous woody plants, evergreen woody plants, and succulents, while the pattern by biome was, again from highest to lowest, grassland, tundra, tropical forest, temperate forest, woodland, shrubland, and desert.
We observed as expected that arctic grasses had lower mean SLA than temperate grasses, and that evergreen trees had lower SLA than their deciduous counterparts.
However, we observed by far the highest mean SLA values in temperate deciduous broadleaf trees, rather than in grass PFTs as expected.
Similarly to @onoda_2011_global, we found no consistent patterns in SLA with temperature:
Among broadleaved evergreen PFTs, temperate species had lower SLA than tropical, but among broadleaved deciduous PFTs, temperate species had higher SLA than both tropical and boreal species.
With respect to the expected decline in foliar N:P ratios with latitude [@reich_global_2004], we found that $N_{mass}$ was generally higher in PFTs associated with colder biomes compared to warmer ones, but observed biome-related differences in $P_{mass}$ only among deciduous broadleaved and needleleaved evergreen PFTs.
Contrary to @atkin_global_2015, our results for both $R_{d,mass}$ and $R_{d,area}$ failed to show a trend with respect to biome.
However, this comparison may not be entirely fair because our study design inherently averages over the extensive climatic variability within PFTs.

Finally, there has been some debate in the trait ecology community about the use of mass- or area-normalized traits in analyses of the leaf economic spectrum.
Two studies [@osnas_global_2013; @lloyd_les] independently concluded that leaf economic relationships among mass-based traits emerge inevitably out of variation in SLA and are therefore not ecologically meaningful.
Responses to these criticisms have suggested that both mass- and area-based normalization have merit, as mass-based traits have a natural interpretation in terms of investment costs and allocation while area-based traits are important due to the fundamentally area-based nature of energy and gas fluxes through leaf surfaces [@westoby_lloyd_response; @poorter_les_response].
Our study suggests the latter, that investigation of trait correlations on both a mass- and area-basis can yield biologically and ecologically meaningful conclusions.
For one, our discussion of differences in ecological strategies between broadleaved and needleaved species fundamentally depends on comparative analysis of mass- and area-normalized nutrient contents.
Meanwhile, our discussion of tropical tree productivity with respect to foliar nutrient contents is generally supported by both mass- and area-normalized traits (i.e. the correlation between $P$ and $V_{c,max}$ is stronger than the correlation of $N$ and $V_{c,max}$ for tropical species whether we use the mass- or area-normalized versions of these traits).


## Covariance as constraint

The second objective of this paper was to investigate the ability of trait covariance to provide additional information that could be used to reduce uncertainties in trait estimates.
In agreement with our expectations, our results show that accounting for trait covariance constrained the uncertainty around PFT-level trait means, particularly for trait-PFT combinations with low sample sizes (Fig.\ 2 and 3).
Moreover, accounting for trait covariance occasionally also changed the position of trait mean estimates, even for PFT-trait combinations with relatively large sample sizes (e.g. $N_{mass}$ for temperate broadleaved deciduous trees, Fig.\ 2).
This result echoes @diaz_global_2016 in demonstrating the importance of studying the multivariate trait space rather than individual traits. 
Such shifts suggest that the sampling of these traits in the TRY database is not representative, which is evident from looking at the relative sample sizes of different traits (Fig.\ 1; see also Kattge et al. -@kattge_try_2011). 
These shifts also indicate that parameter estimates based on univariate trait data [e.g. @lebauer_facilitating_2013; @dietze_quantitative_2014; @butler_2017_mapping] may not only be overestimating uncertainty, but may also be systematically biased.
Although our results clearly show that many time- and labor-intensive traits, such as $R_{d}$, $V_{c,max}$, and $J_{max}$, still lack the observations to estimate covariance with other traits for certain PFTs,
our results also show that the effective sample size of all traits is enhanced when covariance is taken into account.
For example, field and remote sensing studies that estimate only certain traits like SLA and $N_{mass}$ but not others (such as $P_{mass}$ and $R_{d,mass}$) can leverage trait correlations to provide indirect constraint [@serbin_spectroscopic_2014; @singh_imaging_2015; @musavi_imprint_2015; @lepine_examining_2016].
We therefore suggest that future observational campaigns consider trait covariance when deciding which traits to measure.

The additional benefit of hierarchical multivariate modeling in our study was limited, largely due to the relatively low number of points used to estimate that across-PFT covariance structure.
Therefore, for parameterizing the current generation of ecosystem models using well-sampled traits, we suggest that simple multivariate models fit independently to each PFT are sufficient and that the additional conceptual challenges and computational overhead of hierarchical modeling are not required.
However, for modeling work that requires larger numbers of PFTs [@boulangeat_improving_2012], and especially for models that are parameterized on the basis of individual species [e.g. Linkages, @linkages], the benefits of hierarchical modeling may accumulate [@clark_why_2005; @dietze_capturing_2008; @cressie_accounting_2009; @webb_structured_2010].

More generally, we foresee tremendous potential of multivariate and hierarchical modeling approaches to elucidating the role of functional traits in organismal and ecosystem function. 
Besides the many important foliar traits that we did not include (e.g.\ pigment contents, leaf hydraulic traits), our approach could readily be applied to other plant traits whose relationship to the leaf economic spectrum is less clear. 
One example is hydraulic traits:
While stem and leaf hydraulic traits are correlated [@bartlett_hydraulic], a potential scaling between hydraulic and leaf economic traits is poorly understood [@reich_world-wide_2014; @li_leaf_2015]. 
Similarly, reexamining the relationships defining wood [@chave_woodeconomics; @baraloto_decoupled_2010; @fortunel_leaf_2012] and root [@kramer-walter_root_2016; @valverde-barrantes_root_2016] economic spectra, as well as their relationship to the foliar traits, would provide useful information on possible scale-dependence of plant growth and allocation strategies.
We emphasize that the relative difficulty of measuring hydraulic and other non-foliar traits [e.g @jansen_hydraulic_traits] further increases the value of any technique that can fully leverage the information they provide. 
More generally, multivariate and hierarchical modeling has the potential to reveal functional trade-offs that are mutually confounding at different scales, thereby enhancing our understanding of processes driving functional diversity.

## Conclusions

The tremendous functional diversity of plants continues to be a major challenge for functional ecology and ecosystem modeling.
Functional diversity research fundamentally depends on dimensionality reduction through a search for meaningful pattern that can be exploited to take reasonable guesses at average behavior. 
The trait trade-offs comprising the leaf economic spectrum are one such pattern. 
In this paper, we reaffirm the existence of the leaf economic spectrum both globally and, with some caveats, within plant functional types typically used in the current generation of Earth System Models.
We also highlight how the strength of leaf economic relationships can be influenced by biotic and abiotic factors specific to certain PFTs.
Finally, we show how patterns of trait covariance like the leaf economic spectrum can be leveraged to inform trait estimates, particularly at small sample sizes.

<div custom-style="Heading, no number">Acknowledgements</div>

This project was supported by NASA grant NNX14AH65G and NSF grants 1261582 and 1458021, as well as the TRY initiative on plant traits (http://www.try-db.org).
The TRY initiative and database is hosted, developed and maintained by J. Kattge and G. Boenisch (Max Planck Institute for Biogeochemistry, Jena, Germany).
TRY is currently supported by DIVERSITAS/Future Earth and the German Centre for Integrative Biodiversity Research (iDiv) Halle-Jena-Leipzig.
The authors would also like to thank Christine Rollinson, Istem Fer, and Colin Averill for their valuable feedback on early drafts of this manuscript.

<div custom-style="Heading, no number">Author contributions</div>

ANS wrote the manuscript and implemented the analysis.
ANS and EMC designed the analysis and figures.
MCD conceived the original idea for the manuscript, guided its development, and provided financial support.
MB, SJ, KK, ÜN, and NAS provided extensive feedback on multiple drafts of the manuscript, and contributed data.
All other authors contributed data.
