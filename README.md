
# Code for large scale forecasting the potential distribution of Heracleum Sosnowskyi on the territory of Russia under climate change.

__Source code for paper Large scale forecasting the potential distribution of Heracleum Sosnowskyi on the territory of Russia under the climate change__

We propose a machine learning approach based on the Random Forest model for forecasting the potential distribution of Heracleum Sosnowskyi. This  research  aims  to  establish  the  possible  habitat suitability of HS in current and future climate conditions across the territory of European part of Russia.

Map demonstrate points with Heracleum Sosnowskyi that were obtained from open sources

![Occurence points](/plots/occurence_points_HS.png)

## Installation

Clone this repository

Install R packages

* biomod
* spThin
* biomod2
* ggplot2
* gridExtra
* raster
* rasterVis
* maptools

## Data 

### Occurrence points of Heracleum Sosnowskyi

The CSV file contains the coordinates of the location of the Heracleum Sosnowskyi and the parameters (soil variables, bioclim data) used to train the Random Forest model

CSV file: [Occurrence points](/input_data/dataset4.csv)

### Climatic variables

Climatic variables were collected from the [Worldclim](https://worldclim.org/) project 

### Soil data

Soil data were downloaded from the [SoilGrids](https://www.isric.org/explore/soilgrids/faq-soilgrids) database 

## Source Code


Source code of paper to conduct Random Forect model training, reproduce results and plots contatins in `src.R` file - [Code](src.R)

Trained model stored in `models` folder  - [Model](/models/final_final_model.rds)

Code to forecast future dictribution of Heracleum Sosnowskyi under different climate scenarios - [Code](future_climate_distribution.R)

### Plots

ROC-AUC, MDG and MDA plots created with python.

To reproduce plots install python packages

* matplotlib
* numpy 
* seaborn 
* sklearn
* pandas 
* ipython
* jupyter

Open `ROC-AUC plots.ipynb` file with Jupyter-notebook 

## License

Distributed under the CC0 1.0 license. See ``LICENSE`` for more information.
