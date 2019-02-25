---
title: 'cde - R package to retrieve data from the Environment Agency Catchment Data Explorer site'
tags:
  - R
  - water quality
  - Water Framework Directive
authors:
  - name: Robert A Briers
    orcid: 0000-0003-0341-1203
    affiliation: 1
affiliations:
 - name: School of Applied Sciences, Edinburgh Napier University
   index: 1
date: 25 Feburary 2019
bibliography: paper.bib

# Summary

The [WFD](http://ec.europa.eu/environment/water/water-framework/index_en.html) set EU-wide standards for how the quality of surface- and ground-waters across Europe is assessed and classified (EC 2000). Assessment of quality is based on a range of elements that vary depending on the type of water being assessed. The elements cover biological, chemical and hydromorphological/quantitative components with a hierarchical structure. These are combined to give an overall classification of waterbodies into five classes(High, Good, Moderate, Poor and Bad) for surface waters and two clases (Good or Poor) for groundwaters. The overall aim of the WFD is that all European surface and ground waters will reach at least Good status by 2027, although progress towards this target has been relatively slow in some areas (EEA ref).

In the UK the Environment Agency (EA) is the competent body responsible for monitoring and assessment of water quality within England. The EA have made the reporting data relating to the rquirements of the WFD available via the Catchment Data Explorer (CDE) website (link). The cde package for R provides functions that allow users to download data from the CDE website directly into R.

The package contains functions to allow users to search for waterbodies, catchments or River Basin Districts that match given search strings. Having identified the relevant sites, fours different types of data can be downloaded.

Firstly status classification data, either for overall water body classification or at a range of more detailed levels (biological, chemical or priority substances for example), can be downloaded for specified year ranges and for specific waterbody types (such as lakes). Summary plots of percentage of water bodies in each status class can also be produced, again for a range of levels, years and waterbody types.

For catchments or River Basin Districts where there are waterbodies that have not achieved Good status, the package provides the functionality to download a summary of the Reasons for Not Achieving Good (RNAG) data. This gives a range of information regarding the relevant pressures identified as contributing to the current failure, classified according to a standard hierarchy given on the CDE website.

Where less than Good status has been achieved, objectives have been set to indicate what status is aimed for in the longer term, along with the measures that have been put in place or are proposed to achieve these objectives. Separate functions allow both aspects to be downloaded for specified waterbodies, catchments or River Basin Districts. Finally the package also provides a function to download a summary of associated protected areas, again at a range of levels from individual waterbodies to whole River Basin Districts.

The cde package facilitates the querying and download of the data available on the CDE site, allowing efficient collation of data for the analysis of trends or patterns in water quality and pressures on waterbodies across England.