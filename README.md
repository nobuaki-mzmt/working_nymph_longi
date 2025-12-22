# README
## Article Information
This repository provides access to the data and source code used for the manuscript    
**Hidden labor of the reproductive caste in a highly structured termite society**  
**Nobuaki Mizumoto, Clement Het Kaliang, Taisuke Kanao**  
**Paper DOI:** [TBA](XXX)

This study describes the brood transportation by nymphs of a marching termite, _Longipeditermes longipes_, while the rare nest relocation event. The ~8 hours of videos of nest relocation was analyzed using a event logging software, [BORIS](https://www.boris.unito.it/), to measure the time development of traffic flow, and a deep-learning posture tracking software, [SLEAP](https://sleap.ai), to investigate the movement patterns of nymphs with and without brood transportation. 
This repository includes data and the Python/R scripts.  
Additional data available elsewhere includes, 
- The models for SLEAP are available at TBA.

## Table of Contents
This repository includes tracking data, R codes to analyze it, and Python code for video analysis.  
* [README](./README.md)
* [code](./code)
  * [data_prep.py](./code/data_prep.py): Python script to process and clean data from SLEAP tracking outputs.
  * [analysis.R](./code/analysis.R): R script for conducting statistical analysis and generating figures.
* [data_raw](./data_raw) - folder containing raw data
* [data_fmt](./data_fmt) - folder containing data converted from raw data
* [output](./output) - folder containing outputs

## Setup & Dependencies
The scripts of this project is written in R and Python, tested on Windows 11 (64-bit). Following is the environments.

### R Session Info
R version R version 4.4.1 (2024-06-14)  
```r
packages <- c(car="3.1-3", carData="3.0-5", lme4="1.1-36", Matrix="1.7-0", ggridges="0.5.6", RColorBrewer="1.1-3", patchwork="1.3.0", ggplot2="3.5.1", dplyr="1.1.4", tibble="3.2.1", tidyr="1.3.1", stringr="1.5.1", data.table="1.17.0")
for (pkg in names(packages)) remotes::install_version(pkg, version = packages[pkg])
```
### Python Environment
Python 3.11.4
```bash
pip install \
  h5py==3.13.0 \
  numpy==1.25.0 \
  pandas==2.2.3 \
  scipy==1.15.2 \
  feather-format==0.4.1 \
  pillow==11.2.0
```

## Citation
TBA

## License
This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Contact
Nobuaki Mizumoto: nzm0095@auburn.edu
