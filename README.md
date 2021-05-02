
# Prioritize-Species-Restoration

Prioritizing plant species for seed or planting mixes in terretrial ecological restoration. This method can be performed in R, using the [prioritizr](https://prioritizr.net/) package, *AND* can also be performed in [MARXAN](https://marxansolutions.org/). Here, we use the prioritizr package to efficiently find the smallest number of plant species that met the target based-objectives in restoration treatment species mixes.  We used the ‘minimum-set’ problem formulation which is commonly applied to spatially-explicit decision making that cost-efficiently meet targets for conservation features (e.g. habitats, species ranges, or ecological processes), and apply it to our non-spatial problem.

Below we detail the Datasets and Code Provided to replicate this method.

### Data
All that is really needed for this analysis is a plant species list of appropriate species from the regional species pool, appropriate for the target habitat for use in a restoration species mix, and a list of associated plant traits, or attributes associated with those plant species, to form a plant species-trait matrix. The next step is to get the data into the right format for analysis. There are multiple options for this data formatting, and here we take the approach to data formatting that can also be used in MARXAN. The data unique to  each objective is provided in this format. Methods for formatting data for these  applications is well-documented for each sotfware platform mentioned here, and adapting these formats to this non-spatial application is detailed in the supplementary information of this paper. 

**Table_S1.csv** These data are the total plant species-attribute matrix used for this analysis.

#### Folders
Each folder contains the following data for each objective. Each folder is named intuitively after each objective.

**pu.csv**  This is a list of planning units and planning unit id. In this case, planning units are plant species. The contains the planning unit name (s), planning unit id (id), the cost of each planning unit, the status (in prioritizr/MARXAN) documentation), and the total number of conservation features/atributes of each plant species (totalcf).

**puvsp_w.csv** This is  a 'plant species-trait/attribute' matrix. Coonverted to long format in *1_puvsp_wrangle.R*  for use in *2_problems_solutions.R*.

**puvsp.csv** This is the 'plant species-trait/attribute' matrix in long format. 

**spec.csv** This is a list of 'attributes'conservation features' (terminology from prioritizr and MARXAN documentations). In this case conservation features are plant traits/atrributes. This contains the conservation feature/attribute name (name), the conservation feature id (id), the target (target- in prioritizr and MARXAN documentation), broad category assigned to each conservation feature attribute (category), and the objective.



### R Scripts
This method can be performed in R, using the [prioritizr](https://prioritizr.net/) package, *AND* can also be performed in [MARXAN](https://marxansolutions.org/). In the associated paper we describe the prioritizr method, and we present the associated code here. R Script file names- which are listed below, are numbered and listed in the order they should be used.

**1_puvssp_wrangle.csv** This wrangled data is already provided for you, but this wrangle script is provided to demonstrate how to get data into required formats. This just converts *puvsp_w.csv* into *puvsp.csv* 

**2_problems_solutions.R** This script takes data for each objective, creates prioritizr problems, and solves to create solutions for this paper. data is the wrangled into formats needed for analysis and Figures.

**3_Figures.R** This script produces figures for the associated paper.

