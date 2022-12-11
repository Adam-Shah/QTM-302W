# International Trends in Abortion Policy and Mortality
author: Adam Shah and Melanie Yu


## Short Description  
The discussion of abortion policy has been a topic of contentious debate for decades; many argue that access to safe abortion is a human right which offers reproductive freedom, while others posits against abortion on the basis of morality. The primary research question of this project is to determine a possible effect of more restrictions in abortion legislation on the deaths attributed to these abortions. We believe that observing various countries with different levels of abortion restrictions may illustrate how the overturning of Roe v. Wade could affect those in states whose legislations have changed. 


## Table of contents
* [General info](#general-info)
  * [Methods and Analyses](#methods-and-analyses)
    * [To do list](#to-do-list)
      * [Contact](#contact)

## General info

We utilized the mortality database from the WHO and selected five countries with the best data usability to represent various degrees of policy strictness to observe mortality trends due to unsafe abortions. The countries which were selected are: the Philippines for the “Prohibited Altogether” policy category, Brazil was selected for “To Save the Women’s Life” policy category, United States for the “To Preserve Health” policy category, Japan for the “Socioeconomic Grounds” policy category, and The Netherlands for the “On Request” policy category. To visualize trends in mortality, we use ggplot and plotly to summarize the dataset.

The primary dependent variable for the analyses was described as “percentage of cause-specific deaths out of total deaths in our data set, which was selected to maximize comparison ability between countries from different regions in the world with significantly different population sizes. The analyses performed were to investigate the trends in this value over different years and age groups in the countries in question and to determine the relationship between this abortion outcome and the policy restrictiveness score. Once this restrictiveness score was determined for each country in the data set, each country name was recorded to this value and the countries were assigned their respective score. 

It was found that stricter policies correlates to higher rates of mortality related to abortion, where, as policies decrease in restrictiveness, mortality rates also decrease, with mortality rates fall close to zero when policies are least restrictive.

## Methods and Analysis

Data for abortion death rates and percentage of total deaths by country and year were taken from the World Health Organization Mortality Database. The database is maintained by the WHO Division of Data, Analytics and Delivery for Impact (DDI) and contains data from over 120 countries and areas. The WHO mortality database requests data from all countries annually since 1950 and includes information on sex, age, year, and cause if death. Cause of death is recorded according to the International Classification of Diseases (ICD). Some difficulties arose ot all countries report their data effectively to the WHO, particularly sub-Saharan and Asian countries due to systems that might have not been institutionalized and, in some cases, shortage in trained personnel who can report cause of death also contribute to the incompleteness of the data. When data is reported to the WHO, mortality information can still be missing, incomplete, and/or incorrect. This dataset is adjusted with WHO’s Global Health Estimates (GHE) to accommodate missing and incorrect data. 

The mortality due to unsafe abortion dataset consisted of the number of deaths within each age group across all countries with its corresponding years. To select countries of interest, the usability of the country’s data was paired with their abortion policy as of 2021. The WHO assessed the usability of each country’s data from 2008-2019. Countries with the most complete and usable data were selected. Then, the restrictiveness of abortion polices of each country were divided into five groups based on the classification developed by the Center for Reproductive Rights: “Prohibited Altogether,” “To Save the Women’s Life,” “To Preserve Health,” “Socioeconomic Grounds,” and “On Request". One country from each category was selected to attempt an even distribution of policy restrictiveness and geographic location. The variable “restrictiveness score” in our analyses was derived from the order of this categorization, with a score of 1 being the most restrictive (“Prohibited Altogether”) and 5 being the least restrictive (“On Request”). 

The Philippines (100.0% completeness, 83.7% usability) were selected form the “Prohibited Altogether” category; Brazil (100.0% completeness, 84.3% usability) were selected from “To Save the Women’s Life”; United States (100.0% completeness, 87.9% usability) from “To Preserve Health”; Japan (100.0% completeness, 79.0% usability) from “Socioeconomic Grounds”; and The Netherlands (100.0% completeness, 84.0% usability) from “On Request”. The Netherlands was selected from the “On Request” category since their law does not limit pre-viability abortion. Only data from the years 2008 - 2019 were analyzed due to the assessment of data usability in those years. 

The data was filtered to include ages 5 to 74 and all sexes, and the data for the desired countries in the analysis was selected. There were some gaps in years in the data. For example, the US began in 1962, while many countries’ began much later. After narrowing the dataset down to 2008-2019, there were much less missing values. There were occasional gaps in the Philippines, but it was not enough to impact the quality of the data. Age trends were analyzed by bins of [<20], [20-29], [30-39], [40-49], [50-59], and [60+]. Most analyses were performed using data later than the year 2007 to minimize missing data. From there, the percentage of total deaths due to abortions was graphed against year.

In order to determine the nature of the correlation between restrictiveness and abortion-related deaths, a kind of linear regression was run on the categorical variable of restrictiveness score. Although this variable is categorical and the numerical spacing between restrictiveness score values is arbitrary, the linear regression was still performed to obtain statistics describing the significance of the relationship between these values. 

## Instructions
1. Clone this repo [tutorial](https://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository).
2. Raw Data can be downloaded [here] (https://platform.who.int/mortality/themes/theme-details/topics/indicator-groups/indicator-group-details/MDB/abortion). When downloading from the WHO, export dataset and select full data.
3. Data processing/visualization scripts are being kept [here](Repo folder containing data processing scripts/notebooks)
4. Policy categories of each country can be found [here](https://reproductiverights.org/maps/abortion-laws-by-state/)

## To-do list

Next steps would be to utilize other datasets to derive more detailed explorations of the topic, since the data from the WHO does not include much information about possible covariates. Also, expanding the analysis to more countries would yield insights as to if the relationships we found in fact held true as a significant correlation across the globe. However, due to a lack of data standardization across countries, this would present a challenge. Likely, datasets from countries of interest would have to be normalized and cleaned.


## Status
Project is: _in progress_

## Contact
Created by [Adam Shah](adam.shah@emory.edu)
Created by [Melanie Yu](melanie.feng.yu@emory.edu)
