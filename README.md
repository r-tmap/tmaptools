tmaptools: tools for thematic maps in R
===

[![Build Status](https://travis-ci.org/mtennekes/tmaptools.png?branch=master)](https://travis-ci.org/mtennekes/tmaptools)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/mtennekes/tmaptools?branch=master&svg=true)](https://ci.appveyor.com/project/mtennekes/tmaptools)<!---[![Coverage Status](https://img.shields.io/codecov/c/github/mtennekes/tmaptools/master.svg)](https://codecov.io/github/mtennekes/tmaptools?branch=master)--->
[![License](https://img.shields.io/badge/License-GPL%20v3-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-3.0.html) 
[![CRAN](http://www.r-pkg.org/badges/version/tmap)](https://cran.r-project.org/package=tmaptools) 
[![Downloads](http://cranlogs.r-pkg.org/badges/tmaptools?color=brightgreen)](http://www.r-pkg.org/pkg/tmaptools)

This package offers a set of handy tool functions for reading and processing spatial data. The aim of these functions is to supply the workflow to create thematic maps, e.g. read shape files, set map projections, append data, calculate areas and distances, and query OpenStreetMap. The visualization of thematic maps can be done with the [tmap](https://github.com/mtennekes/tmap) package.

Installation
------------

`tmaptools` is available on CRAN. The development version can be installed as follows:

```r
library(devtools)
install_github("mtennekes/tmaptools")
```

The `tmaptools` packages relies on the R packages `rgdal` and `rgeos`, which depend on the external libraries `gdal`, `proj.4` and `geos`. On Windows, these are embedded in `rgdal` and `rgeos`. On Linux (Ubuntu), these libraries can be installed as follows:

```bash
sudo apt-get install libgdal-dev
sudo apt-get install libproj-dev
sudo apt-get install libgeos-dev
```

See source pages for [gdal](http://trac.osgeo.org/gdal), [proj](http://trac.osgeo.org/proj), and [geos](http://trac.osgeo.org/geos). For Mac OS users, see http://www.kyngchaos.com.

Also, `tmaptools` relies indirectly on the R package `V8`. For Linux (Ubuntu), the `v8` library needs to be installed for this:

```bash
sudo apt-get install libv8-dev
```

Java is required for obtaining OpenStreetMap bitmaps (with the function`read_osm`). It can be installed in Linux (Ubuntu) with:

```bash
sudo apt-get install openjdk-9-jre
sudo R CMD javareconf
```


Development
------------

Odd numbered versions will be development versions and even numbered versions stable CRAN releases (as with data.table).

The latest development version can be installed using `devtools`.

```r
library(devtools)
install_github("mtennekes/tmaptools")
```
