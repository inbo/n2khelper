[![License](http://img.shields.io/badge/license-GPL--3-blue.svg?style=flat)](http://www.gnu.org/licenses/gpl-3.0.html)

**Branch** | **Build status** | **Test coverage**
---------- | ---------------- | -----------------
master | [![wercker status](https://app.wercker.com/status/a0db92c2c346d3ce044568c17879fb51/m/master "wercker status")](https://app.wercker.com/project/bykey/a0db92c2c346d3ce044568c17879fb51) | [![codecov](https://codecov.io/gh/inbo/n2khelper/branch/master/graph/badge.svg)](https://codecov.io/gh/inbo/n2khelper)
develop | [![wercker status](https://app.wercker.com/status/a0db92c2c346d3ce044568c17879fb51/m/develop "wercker status")](https://app.wercker.com/project/bykey/a0db92c2c346d3ce044568c17879fb51) | [![codecov](https://codecov.io/gh/inbo/n2khelper/branch/develop/graph/badge.svg)](https://codecov.io/gh/inbo/n2khelper)

# The n2khelper package

The `n2khelper` package constains auxiliary functions for the analysis and reporting of the Natura 2000 Monitoring.

It currently holds functions for importing the raw data, creating analysis dataset and running the analysis for the Common Breeding Bird Survey and the Wintering Bird Survey in Flanders.

The package can be installed in R with the `devtools` package. Use `devtools::install_git("INBO-Natura2000/n2khelper")` to get the latest stable version. `devtools::install_git("INBO-Natura2000/n2khelper", ref = "develop")` will give the latest development version.

We also provide a [repository](https://hub.docker.com/r/inbonatura2000/n2khelper/) with a [docker](https://www.docker.com/) container holding the package and a stable version of all it dependencies. `docker pull inbonatura2000/n2khelper` will give the latest stable version. `docker pull inbonatura2000/n2khelper:0.1` gives stable version `0.1`. `docker pull inbonatura2000/n2khelper:dev-0.1` gives develop version `0.1`.
