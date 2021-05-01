
# Prioritize-species-restoration

Prioritizing plant species for seed or planting mixes in terretrial ecological restoration. Here, we use the prioritizr package to efficiently find the smallest number of plant species that met the target based-objectives in restoration treatment species mixes.  We used the ‘minimum-set’ problem formulation which is commonly applied to spatially-explicit decision making that cost-efficiently meet targets for conservation features (e.g. habitats, species ranges, or ecological processes), and apply it to our non-spatial problem.

Below we detail the Datasets and Code Provided to replicate this method.

### Data
All that is really needed for this analysis is a plant species list of appropriate species from the regional species pool, appropriate for the target habitat for use is a restoration species mix, and a list of associated plant traits, or attributes associated with those plant species, to form a plant species, trait matrix. The next step is to get the data into the right format for analysis. There are multiple options for this data formatting, and here we take the approach to data formatting that can also be used in MARXANThe data for each objective is provided in this format. Methods for formatting data for these  applications is well-documented for each sotfware platform mentioned here, and adapting these formats to this application is detailed in the supplementary information of this paper. 

**Table_S1.csv** These data are the total plant species-attribute matrix used for this analysis.


### R Scripts
This method can be performed in R, using the [prioritizr](https://prioritizr.net/) package, *AND* can also be performed in [MARXAN](https://marxansolutions.org/). In the associated paper we describe the prioritizr method, and we present the associated code here. R Script file names- which are listed below, are numbered and listed in the order they should be used.

**puvssp_wrangle.csv**

**problems_solutions.R**

**Figures.R**

