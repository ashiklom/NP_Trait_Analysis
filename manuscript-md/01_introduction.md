# Introduction

The diversity and dimensionality of the terrestrial biosphere is vast and complex, and therefore there has been a recurring debate in ecology about the utility of reductionist approaches for capturing this variability.
In particular, the use of functional groups with common characteristics has been widely applied in biodiversity studies [@naeem_disentangling_2003] and is essential to the structure of many ecosystem models [@lavorel_plant_1997; @wullschleger_plant_2014].
However, ecologists have long recognized the importance of individual variability and stochasticity in shaping ecosystems
[@gleason_individualistic_1926; @bolnick_why_2011; @rosindell_unified_2011; @clark_why_2016],
and the benefits of more finely-resolved representation of functional diversity for predictive ecology are supported by an increasing body of trait ecology literature
[@mayfield_diversity_2006; @mcmahon_improving_2011; @van_bodegom_going_2012; @reichstein_linking_2014; @violle_emergence_2014; @medlyn_using_2015; @moran_intraspecific_2016].

Plant functional traits can be used to link directly measurable features of individuals to their fitness within an ecosystem and, by extension, ecosystem performance as a whole [@violle_let_2007].
Recent syntheses of global trait databases have revealed that although the functional diversity among plant species is immense, this diversity is constrained by allometries and trade-offs between plant strategies [@wright_worldwide_2004; @kattge_try_2011; @kleyer_why_2015; @diaz_global_2016].
One axis of trait covariation currently receiving attention is the ‘leaf economic spectrum’, which defines a trade-off between plant investment in productive but short-lived leaves versus less productive but sturdy and long-lived leaves [@wright_worldwide_2004; @shipley_fundamental_2006; @reich_world-wide_2014; @diaz_global_2016]
Leaf economic traits are well-correlated with
individual plant productivity [@shipley_functional_2005; @niinemets_within-canopy_2016; @wu_convergence_2016],
litter decomposition rates [@bakker_leaf_2011; @hobbie_plant_2015],
community composition [@burns_patterns_2004; @cavender-bares_multiple_2004],
and ecosystem function [@diaz_plant_2004; @musavi_imprint_2015].
The relative position of plant species along the leaf economic spectrum has been shown to be influenced by climate and soil conditions
[@wright_worldwide_2004; @wright_modulation_2005; @cornwell_community_2009; @ordonez_global_2009; @wigley_leaf_2016].
As a result, relationships between leaf economic traits and climate have been incorporated into ecosystem models to allow for continuous variation in plant function and environmental responses
[@sakschewski_leaf_2015; @verheijen_inclusion_2015].

However, the use of among-trait and trait-environment correlations at the global scale, for both ecological inference and land surface modeling, has several important caveats.
First, observed correlations at the global scale do not always hold at smaller scales (such as sites, species, and individuals).
For example, some studies suggested consistent correlations across scales [@wright_worldwide_2004; @albert_multi-trait_2010; @asner_amazonian_2014]
whereas others showed no or even opposite correlations [@albert_intraspecific_2010; @messier_how_2010; @wright_does_2012; @feng_scale_2013; @grubb_relationships_2015; @wigley_leaf_2016; @messier_traitnetwork; @kichenin_2013_contrasting].
Many mechanisms have been suggested for deviation from global trait relationships at smaller scales.
Trade-offs between strategies may only be applicable when multiple competing strategies co-occur, and in cases where strong environmental filters allow only a narrow range of strategies, alternative processes can drive community assembly [@rosado_trait_dominance; @pierce_csr_cooccur; @grim_pierce_book].
Different selective pressures dominate at different scales, particularly within versus among species [@albert_intraspecific_2010; @messier_how_2010; @kichenin_2013_contrasting],
and the large heterogeneity in the variance structures of traits suggests that different traits have contrasting sensitivity to these different pressures [@messier_trait_2016].
Experimental evidence shows that species can alter different aspects of their leaf economy in a relatively uncoordinated fashion, even when the direction of univariate trait responses to environmental change is consistent [@wright_does_2012].
Meanwhile, across different plant functional types, resource allocation patterns, for instance of nutrients to photosynthesis versus structure and defense, differ substantially, suggesting different investment strategies and varying relationships among traits [@ghimire_2017_Nallocation].
Second, among-trait correlations at any scale do not provide causal evidence for functional trade-offs or even similarity in response to external stimuli [@messier_trait_2016].
Therefore, ascribing too much leverage to trait correlations can lead to an underestimation of plant functional diversity [@grubb_trade-offs_2015].
Third, plants maintain their fitness in a given environment through multiple independent strategies (corresponding to multiple mutually orthogonal axes of trait variability).
As a consequence, changes in key leaf economic traits such as nitrogen content and mass per unit area may not affect other aspects of plant function, such as
hydraulics [@li_leaf_2015],
overall plant carbon budget [@edwards_leaf_2014],
and dispersal [@westoby_plant_2002].
Finally, modeling ecosystem function based on trait correlations with abiotic factors is sampling from the hypothetical space of potential species and communities that could have evolved, rather than constraining models to forecast the actual vegetation we have today as the result of spatial separation and constraints on convergent evolution.
Among other problems, this approach fails to account for the timescales required for adaptation as well as actual limitations of the physiology of different species and community assembly.

An alternative approach is to preserve existing PFT classifications
[though potentially with finer taxonomic, functional, or spatial resolution, e.g. @boulangeat_improving_2012]
while using statistical analyses to account for uncertainty and variability in the aggregated trait values.
For example, the Predictive Ecosystem Analyzer (PEcAn, pecanproject.org), an ecosystem model-data informatics system, parameterizes PFTs using trait probability distributions from a Bayesian meta-analysis of plant trait data across many studies
[@dietze_improving_2013; @lebauer_facilitating_2013].
This approach explicitly separates the processes driving PFT-level differentiation from processes that drive finer-scale functional variability,
and is useful for guiding future data collection and model refinement [@dietze_quantitative_2014].
However, a univariate meta-analysis, like the one currently in PEcAn, is limited by its failure to account for trait correlations, therefore neglecting useful knowledge about relationships across PFTs and between traits.
At the other extreme, existing regional and global scale analyses [e.g. @van_bodegom_going_2012; @sakschewski_leaf_2015] ignore variability within PFTs, often resulting in macroecological, evolutionary, and competitive trade-offs across PFTs being used to drive both acclimation and instantaneous responses within PFTs.

The above discussion culminates in the following question: 
Can the constraint on average functional characteristics across PFTs provided by the leaf economic spectrum be reconciled with trait relationships within each PFT? 
The answer to this question has relevant implications for both functional ecology and ecosystem modelling.
Here we hypothesize that global leaf economic relationships represent a biome-level environmental filter that regulates variability among PFTs more than trait correlations within PFTs.
To evaluate this hypothesis, we develop a hierarchical multivariate Bayesian model that explicitly accounts for across- and within-PFT variability in trait correlations.
We then fit this model to a global database of foliar traits to estimate mean trait values and variance-covariance matrices for PFTs as defined in a major earth system model [Community Land Model, CLM, @clm45_note].
To our knowledge, this study is unique in its treatment of within- and among-PFT variability simultaneously and with a global dataset.
We evaluate the ability of this model to reduce uncertainties in trait estimates and reproduce observed patterns of global trait variation compared to non-hierarchical multivariate and univariate models.
Finally, we assess the generality and scale dependence of trait trade-offs by comparing covariance estimates globally and within each PFT.
