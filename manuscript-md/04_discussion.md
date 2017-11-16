# Discussion

## Variation in trait means among PFTs

A key application of this study is to provide data-driven parameter estimates for earth system models.
To this end, we compared our mean parameter estimates (Fig. \@ref(fig:meancompare), Table 3) with corresponding default parameters in CLM 4.5 [@clm45_note].
Our mean estimates of SLA agreed with CLM's defaults [@clm45_note, Table 8.1] only for tropical broadleaved evergreen trees, and for all other PFTs, our estimates are significantly lower.
We could not directly compare our estimates of $N_{mass}$ to CLM because CLM reports the C to N ratio instead, but we can comment on the relative magnitudes and trends in these respective traits.
PFTs in CLM, by default, take on one of three C:N ratios: all grasses and deciduous PFTs have the lowest C:N (i.e. highest $N_{mass}$), followed by a higher value for all evergreen PFTs except boreal needleleaf evergreen, which has the highest value (i.e. lowest $N_{mass}$) [@clm45_note].
Our results agree that most evergreen PFTs, particularly needleleaved evergreen species, have lower $N_{mass}$ than deciduous PFTs.
However, in our results, broadleaved tropical evergreen species had $N_{mass}$ that was more similar to most deciduous PFTs and significantly higher than other evergreen PFTs.
Meanwhile, grasses generally had lower $N_{mass}$ than deciduous trees, and C4 grasses in particular (which comprise many of CLM's agricultural PFTs) had $N_{mass}$ that was nearly as low as the evergreen species.
Our mean estimates of $V_{c,max_area}$ (among those that were reasonably constrained by data) were lower than those of CLM for all PFTs except needleleaf evergreen trees and grasses, for which our estimates overlapped CLM's parameters.
@kattge_2009_vcmax also found that $V_{c,max,area}$ was overestimated by Earth System models, but their estimates of $V_{c,max,area}$ and $N_{area}$ are generally slightly higher than ours.
This discrepancy may be at least partially due to our use of $\log_{10}$ for all trait values, which weights low values more strongly than high values.

The interaction between growth form and biome in discrete PFT definitions confounds the interpretation of our results with respect to well established ecological and biogeographic patterns.
For example, @poorter_causes_2009 found variation in SLA by growth form was, from highest to lowest, grasses, deciduous woody plants, evergreen woody plants, and succulents, while the pattern by biome was, again from highest to lowest, grassland, tundra, tropical forest, temperate forest, woodland, shrubland, and desert.
We observed as expected that arctic grasses had lower mean SLA than temperate grasses, and that evergreen trees had higher specific leaf area than their deciduous counterparts.
However, we observed by far the highest mean specific leaf area values in deciduous broadleaf trees, rather than in grass PFTs as expected.
Similarly to @onoda_2011_global, we found no consistent patterns in SLA with temperature:
Among broadleaved evergreen PFTs, temperate species had lower SLA than tropical, and among broadleaved deciduous PFTs, temperate species had higher SLA than both tropical and boreal species.
With respect to the expected decline in foliar N:P ratios with latitude [@reich_global_2004], we found that $N_{mass}$ was generally higher in PFTs associated with colder biomes compared to warmer ones, but observed biome-related differences in $P_{mass}$ only among deciduous broadleaved and needleleaved evergreen PFT.
Our results for both $R_{d,mass}$ and $R_{d,area}$ failed to show a trend with respect to biome, contrary to the results of @atkin_global_2015 (though this may be partially because our results are for respiration at ambient temperature, whereas Atkin et al. found that the climatic gradient was much stronger when respiration rates were standardized to a common temperature).

The addition of the multivariate constraint reproduced more ecologically realistic results than the univariate model, especially for data-limited traits.
For example, @Ali_2015 found that $V_{c,max,area}$ was higher and leaf nitrogen content (on an area basis) was lower in temperate compared to tropical biomes.
We observed the same pattern in our mean estimates of $V_{c,max,area}$ and $N_{area}$ when using the multivariate and hierarchical models, but not when using the univariate model (Fig. \@ref(fig:meancompare)).

## Trait correlations and leaf economics

The canonical leaf economic spectrum is defined by a negative correlation of between SLA and leaf lifespan, and a positive correlation of SLA with $N_{mass}$, $P_{mass}$, and photosynthesis and respiration rates [@wright_worldwide_2004].
We were able to reproduce the directionality of these relationships for most PFTs in our hierarchical model.
Relationships of SLA with $N_{mass}$ and $P_{mass}$ were consistently positive, and relationships of SLA with leaf lifespan were generally negative (though, for some PFTs, correlations were not significantly different from zero).
Although we did not include photosynthesis or daytime respiration rates in our analysis, $V_{c,max,mass}$ and $J_{max,mass}$ as proxies of the former and $R_{d,mass}$ of the latter generally exhibited the expected positive correlations with SLA, though many correlations were not significant due to low sample sizes for some PFTs.

While the direction of relationships we observed was largely consistent across different PFTs, the strength of these correlations was more variable.
For example, the correlations of SLA with $N_{mass}$ and $P_{mass}$ were significantly weaker in needleleaved PFTs than in broadleaved PFTs.
On the other hand, relationships of SLA and $N_{area}$ were consistently negative for all PFTs, and temperate needleleaved species in particular had the strongest SLA-$N_{area}$ and SLA-$P_{area}$ correlations of any PFT.
Taken together with the finding that evergreen conifers are relatively unique in their consitently proportional allocation of N to cell walls and Rubisco [@onoda_physiological_2017], our results support the idea that needleleaved species primarily adapt to environmental changes through structural changes (i.e. increasing or decreasing SLA) rather than through modulating foliar biochemistry [@robakowski_2004_needle].
Similarly, the correlations of $N_{mass}$ with traits directly related to function ($R_{d,mass}$, $V_{c,max,mass}$, and $J_{max,mass}$) was weaker for tropical evergreen broadleaved trees than for most other PFTs, but the correlation of these traits with $P_{mass}$ was comparable or even slightly higher.
This suggests that the productivity of tropical species is generally P-limited [@reich_global_2004; @ghimire_2017_Nallocation], that N allocation strategies are more variable under N-poor than N-rich conditions [@ghimire_2017_Nallocation], or more generally that photosynthetic parameters are affected more strongly by environmental covariates than leaf nitrogen contents [@Ali_2015].
We also found that the strength of the correlations of $N$ with photosynthetic parameters $V_{c,max}$ and $J_{max}$--often used in ecosystem modeling to parameterize photosynthesis [@clm45_note; @rogers_roadmap_2017]--varied by plant functional type, whether the traits were normalized by mass or area.
Although trait correlations are not necessarily indicative of allocation strategies, this result generally supports the findings of @ghimire_2017_Nallocation that N allocation strategies to photosynthesis vary widely by plant functional type.
Our result that the $N_{area}$ - $V_{c,max,area}$ correlation is generally weaker for needleleaved than broadleaved species was also found by @kattge_2009_vcmax, and supports the lower allocation of N to photosynthesis found by @ghimire_2017_Nallocation.
In light of this fact and considering that boreal forests, largely populated by needleleaf species, have the largest influence on global climate of any biome [@Snyder_2004; @bonan_forests_2008], we suggest that parameterization of needleleaf tree productivity based on foliar nitrogen content in Earth System Models be treated with caution.
Meanwhile, the correlations of all traits with leaf lifespan was significantly weaker (often to the point of becoming insignificant) within most PFTs than globally.
This suggests that leaf economic relationships related to leaf lifespan are dominated by the fundamental differences between deciduous and evergreen PFTs, while the factors driving variability in leaf lifespan within PFTs are more complex and idiosyncratic [@Reich_2014; @wu_leaf_2016].
That being said, much of this within-PFT variability is driven by variations in shade responses, and a key limitation of our study is that we have no information about the relative canopy positions at which traits were collected [@lusk_why_2008; @keenan_global_2016].

There has been some debate in the trait ecology community about the use of mass- or area-normalized traits in the leaf economic spectrum.
Two independent studies [@osnas_global_2013; @lloyd_les] have suggested that leaf economic relationships among mass-based traits emerge inevitably out of variation in SLA and are therefore not ecologically meaningful.
Responses to these criticisms have suggested that both mass- and area-based normalization have merit, as mass-based traits have a natural interpretation in terms of investment costs and allocation while area-based traits are important due to the fundamentally area-based nature of energy and gas fluxes through leaf surfaces [@westoby_lloyd_response; @poorter_les_response].
Our study supports the idea that investigation of trait correlations on both a mass- and area-basis can yield biologically and ecologically meaningful conclusions.
For one, our discussion of differences in ecological strategies between broadleaved and needleaved species fundamentally depends on comparative analysis of mass- and area-normalized nutrient contents.
Meanwhile, our discussion of tropical tree productivity with respect to foliar nutrient contents is generally supported by both mass- and area-normalized traits (i.e. the correlation between $P$ and $V_{c,max}$ is stronger than the correlation of $N$ and $V_{c,max}$ for tropical species whether we use the mass- or area-normalized versions of these traits).


## Covariance as constraint

In agreement with our expectations, our results show that accounting for covariance between traits constrained the uncertainty around PFT-level trait means, particularly for trait-PFT combinations with low sample sizes.
Moreover, accounting for trait covariance occasionally also changed the position of trait mean estimates, even for PFT-trait combinations with relatively large sample sizes (e.g. $N_{mass}$ for temperate broadleaved deciduous trees, Fig. \@ref(fig:meancompare)).
This result echoes @diaz_global_2016 in demonstrating the importance of studying the multivariate trait space rather than individual traits. 
Such shifts suggest that the sampling of these traits in the TRY database is not representative, which is evident from looking at the relative sample sizes of different traits (Fig. \@ref(fig:samplesize); see also Kattge et al. -@kattge_try_2011). 
These shifts also indicate that parameter estimates based on univariate trait data [e.g. @lebauer_facilitating_2013; @dietze_quantitative_2014] may not only be overestimating uncertainty, but may also be systematically biased.
Although our results clearly show that many time- and labor-intensive traits such as $R_{d}$, $V_{c,max}$, and $J_{max}$ for certain PFTs still lack the observations to estimate covariance with other traits,
our results also show that the effective sample size of all traits is significantly enhanced when covariance is taken into account, and we therefore suggest that future observational campaigns consider trait covariance when deciding which traits to measure.
For example, field and remote sensing studies that estimate only certain traits like SLA and $N_{mass}$ but not others (such as $P_{mass}$ and $R_{d,mass}$) can leverage trait correlations to provide indirect constraint [@serbin_spectroscopic_2014; @singh_imaging_2015; @musavi_imprint_2015; @lepine_examining_2016].

The additional benefit of hierarchical multivariate modeling in our study was limited, largely due to the relatively low number of points used to estimate that across-PFT covariance structure.
Therefore, for parameterizing the current generation of ecosystem models, we suggest that simple multivariate models fit independently to each PFT are sufficient and that the additional conceptual challenges and computational overhead of hierarchical modeling are not required.
However, for modeling work that requires larger numbers of PFTs [@boulangeat_improving_2012], and especially for models that are parameterized on the basis of individual species [e.g. Linkages, @linkages], we believe that the benefits of hierarchical modeling may become increasingly important [@clark_why_2005; @dietze_capturing_2008; @cressie_accounting_2009; @webb_structured_2010].
