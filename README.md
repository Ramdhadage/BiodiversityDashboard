
<!-- README.md is generated from README.Rmd. Please edit that file -->

# BiodiversityDashboard

<!-- badges: start -->
<!-- badges: end -->
<!-- TABLE OF CONTENTS -->
<details>
<summary>
Table of Contents
</summary>
<ol>
<li>
<a href="#about-the-project">About The Project</a>
<ul>
<li>
<a href="#built-with">Built With</a>
</li>
</ul>
</li>
<li>
<a href="#installation">Installation</a>
</li>
<li>
<a href="#roadmap">Roadmap</a>
</li>
</ol>
</details>

## About The Project

The goal of Biodiversity Dashboard is to build a Shiny app that
visualizes observed species on the map.

## Installation

Please fallow following instruction to install Biodiversity Dashboard:

1.  Clone the repository:

``` r
git clone https://github.com/Ramdhadage/BiodiversityDashboard.git
```

2.  Install renv package and restore all package

``` r
install.package('renv')
library(renv)
renv::restore()
```

3.  run the App

``` r
runApp()
```

<!-- ROADMAP -->

## Roadmap

-   [x] Users should be able to search for species using their
    scientificName and vernacularName. The application should display
    its observations on the map after the search box returns matching
    names. A radio button for species Name by Vernacular Name or
    Scientific Name has been added. When you select Scientific Name from
    the menu, the Vernacular Name is rendered and vice versa. Users
    should be able to search for species using their scientificName and
    vernacularName. The application should display its observations on
    the map after the search box returns matching names. A radio button
    for species Name by Vernacular Name or Scientific Name has been
    added. When you select Scientific Name from the menu, the Vernacular
    Name is presented and vice versa.

-   [x] Default view when no species is selected yet should make sense
    to the user. It shouldn’t be just an empty map and plot. Please
    decide what you will display.Added default values of species to map.

-   [x] Users should be able to view a visualization of a timeline when
    selected species were observed. Added monthly occurrence plot.

-   [x] Deploy the app to shinyapps.io. Deployed on you can see it
    [here](https://ti5syn-ramdhadage.shinyapps.io/BiodiversityDashboard/)

-   [x] Decompose independent functionalities into shinyModules. Added
    shiny modules mod_loadData, mod_timelineVisualization and
    mod_viewMap

-   [x] Add unit tests for the most important functions and cover edge
    cases. Added unit tests for important functions as well as server
    and ui components.

-   [x] Use CSS and Sass to style your dashboard. Make the dashboard
    look better than standard Shiny. Used
    [fresh](https://dreamrs.github.io/fresh/index.html) which use sass.

-   [x] Make sure your app initializes fast and the search field is
    fast. Use as many optimization techniques as possible.Used shiny
    cashing for application performance optimization. In addition,
    [memoise](https://github.com/r-lib/memoise) was utilised to memorise
    R functions. To read data quickly, I used the fread function from
    the data.table package. Data in sysdata.rda was updated to be stored
    as internal data so that app will launched quickly in the package.
    Instead of choose select-input, server-side-selectize input was
    used. For visualization , Javascript libraries such as Echarts4r,
    and leaflet used.  The library shinyWidgets used for input widgets.

-   [x] Use JavaScript to create non-trivial visualization. Javascript
    was used to display popups and labels on the map, as well as the
    monthly occurrence plot.

-   [ ] Make a separate module for dynamic ui for species selection
    based on scientific names and vernacular names.

-   [ ] Convert selected species modification code to function from Load
    Data module.

-   [ ] Add more test cases to unit test few more functions.

-   [ ] Add images in popups and label using javascript.

    <p align="right">

    (<a href="#top">back to top</a>)

    </p>

\`
