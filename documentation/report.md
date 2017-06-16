# A mixed flatfish fisheries application in FLasher

Finlay Scott, Jan Jaap Poos, Iago Mosqueira

## Introduction

Many demersal fisheries catch mixed fisheries, where a multitude of species contribute to the output of the fishery (Poos et al. 2010). In such mixed fisheries, different fleets fishing in the same area may have different "target species", but the species that they catch may overlap. Setting single species quota in such fisheries systems may result in high-grading and overquota discarding, where parts of the catch are thrown overboard in order to be able to fish when quotas of one or more stocks are exhausted.

The management of these mixed fisheries is an area of concern: how can these fisheries be sustainably managed, reducing the incentives for discarding on the one hand, while safeguarding the future reproduction of the stocks in these mixed fisheries. Simulations based on Management Strategy Evaluation (Kell et al. 2007) can be an important tool to address these questions. Such tools rely on algortihms that can project the dynamics of populations under different management regimes. 

Within the FLR framework, FLasher has been developed to be a mixed fisheries projection model. Starting from a population with known properties, targets and constraints can be set to fisheries inputs (e.g. fishing effort) and output (e.g. catches) in different time periods, and for different fleets. The North Sea demersal mixed fisheries can potentially be used as a case study to test the application of FLasher. To this end, the life-history and fisheries characteristics needed to collated, and a simple FLasher based MSE needed to be parameterized based on these characteristics.

## Goal
To parameterize a MSE using FLasher based on flatfish stocks in the demersal fisheries of the North Sea 

## Methods 
### Data
The basis of the MSE is a set of stocks that are defined by their life-history characteristics. There are a number if flstfish stocks in the North Sea that are of interest (Gillis *et al.* 2008): plaice (*Pleuronectes platessa*) and sole (*Solea solea*) are the most valuable species in terms of overal vaue of the catch. In addition, turbot (*Scophthalmus maximus*) and brill (*Scophthalmus rhombus*) are two valuable species for which the overall catch is lower, but the value is high. The life-history characteristics of these species is available in literature, and was collated. These life-history characteristics incuded:

#### Growth

The asymptotic average length Linf and body growth rate coefficient K (year<sup>-1</sup>) in the von Bertalanffy growth equation used in the simulations are available in literature, and listed below. Because these species exhibit sexual dimorphism in growth, the data is given for males and females separately.


| Species         |*L*<sub>inf<sub> female|*L*<sub>inf<sub> male| *K* female| *K* male|	Source|
| ---             | ---                   | ---                 |  ---  | ---   | ---|
| European plaice | 48	                  | 32.6                | 0.232	| 0.393 | Van Walraven *et al.* (2010)|
| Sole	          | 39.63	          | 30.98	        | 0.342	| 0.347	| De Veen (1976)|
| Turbot          | 66.7	          | 44.5	        | 0.32	| 0.44	| Van der Hammen *et al.* (2013)|
| Brill	          | 58.0                  | 43.3	        | 0.38	| 0.48	| Van der Hammen *et al.* (2013)|
  
#### The length-weight relationship
- The length-weight relationship is defined by *a* and *b* paramaters in a nonlinear power function (length = *a* W <sup>*b*</sup>). These parameters are available in Bedford *et al.* (1986).
	
|Species	| *a*     | *b*   | Source |
|---            |  ---    | ---   | ---  |
|European plaice| 0.00890 | 3.053 | Bedford et al. (1986)|
|Sole	        | 0.00762 | 3.068 | Bedford et al. (1986)|
|Turbot	        | 0.01508 | 3.090 | Bedford et al. (1986)|
|Brill	        | 0.02492 | 2.857 | Bedford et al. (1986)|

#### Maturity

| Species         | *L*50% (cm) female | *L*50% (cm) male                    | A50% (year) used in assessment |	Ato95% | Source | 
| ---             |	---            | ---                                 | ---                            | ---    | ---    |
| European plaice | 31	               | Males probably smaller than females |	2.5                           | 1.5    |  Grift et al. (2003)|
| Sole	          |                                                          |  2                             | 1      |   | 
| Turbot	  | 34.2	       | 17.9                                |  3                             | 1      | Van der Hammen et al. (2013) |
| Brill           | 31.3	       | 18.4                                |                                |        | Van der Hammen et al. (2013) |




#### Catch and effort time  series

The four flatfish species that in this study are caught by vessels using different gears. Time-series of annual fishing effort and landings for these fleets are available through the STECF website.  

### Conditioning

### Hindcasting

## Results
					
					
					


## References
- Bedford, B. C., Woolner, L.E., Jones, B. W. (1986) Length-weight relationships for commercial fish species and conversion factors for various presentations. Fisheries research data report No. 10., MAFF Directorate for fisheries research. 41 pp.
- De Veen J. (1976) On changes in some biological parameters in the North Sea sole (Solea solea L.). ICES Journal of Marine Science 37, 60-90.  
- Gillis, D. M., Rijnsdorp, A.D., and Poos, J. J. 2008. Behavioral inferences from the statistical distribution of commercial catch: patterns of targeting
in the landings of the Dutch beam trawler fleet. Canadian Journal of Fisheries and Aquatic Sciences, 65: 27–37.
- Grift, R.E., Rijnsdorp, A.D., Barot, S., Heino, M., Dieckmann, U. (2003) Fisheries-induced trends in reaction norms for maturation in North Sea plaice. Marine Ecology Progress Series 257, 247-257.
- Kell, L. T., Mosqueira, I., Grosjean, P., Fromentin, J-M., Garcia, D., Hillary, R., Jardim, E., Mardle, S., Pastoors, M. A., Poos, J. J., Scott, F., and
Scott, R. D. 2007. FLR: an open-source framework for the evaluation and development of management strategies. – ICES Journal of
Marine Science, 64, 640–646.
- Poos, J. J., Bogaards, J. A., Quirijns, F. J., Gillis, D. M., and Rijnsdorp, A. D. 2010. Individual quotas, fishing effort allocation, and over-quota
discarding in mixed fisheries. ICES Journal of Marine Science 67, 323–333.
- van der Hammen T., Poos, J.J., van Overzee H.M.J., Heessen H.J.L., Magnusson A., Rijnsdorp, A.D. (2013) Population ecology of turbot and brill: What can we learn from two rare flatfish species? Journal of Sea Research 84, 96–108
- van Walraven, L., Mollet, F.M., van Damme, C.J.G., Rijnsdorp, A.D. (2010) Fisheries-induced evolution in growth, maturation and reproductive investment of the sexually dimorphic North Sea plaice (Pleuronectes platessa L.). Journal of Sea Research 64, 85–93.

