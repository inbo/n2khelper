**Branch** | **Build status** | **Test coverage**
---------- | ---------------- | -----------------
master | [![wercker status](https://app.wercker.com/status/eb9d3d71876d5cb515b6631b2eb90ce9/m/master "wercker status")](https://app.wercker.com/project/bykey/eb9d3d71876d5cb515b6631b2eb90ce9) | [![codecov.io](https://codecov.io/github/INBO-Natura2000/n2khelper/coverage.svg?branch=master)](https://codecov.io/github/INBO-Natura2000/n2khelper?branch=master)
develop | [![wercker status](https://app.wercker.com/status/eb9d3d71876d5cb515b6631b2eb90ce9/m/develop "wercker status")](https://app.wercker.com/project/bykey/eb9d3d71876d5cb515b6631b2eb90ce9) | [![codecov.io](https://codecov.io/github/INBO-Natura2000/n2khelper/coverage.svg?branch=master)](https://codecov.io/github/INBO-Natura2000/n2khelper?branch=develop)

# The n2khelper package

The `n2khelper` package constains auxiliary functions for the analysis and reporting of the Natura 2000 Monitoring.

It currently holds functions for importing the raw data, creating analysis dataset and running the analysis for the Common Breeding Bird Survey and the Wintering Bird Survey in Flanders.

The package can be installed in R with the `devtools` package. Use `devtools::install_git("INBO-Natura2000/n2khelper")` to get the latest stable version. `devtools::install_git("INBO-Natura2000/n2khelper", ref = "develop")` will give the latest development version.

We also provide a (https://hub.docker.com/r/inbonatura2000/n2khelper/)[repository] with a (https://www.docker.com/)[docker] container holding the package and a stable version of all it dependencies. `docker pull inbonatura2000/n2khelper` will give the latest stable version. `docker pull inbonatura2000/n2khelper:0.1` gives stable version `0.1`. `docker pull inbonatura2000/n2khelper:dev-0.1` gives develop version `0.1`.
