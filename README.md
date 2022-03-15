# LepTraits v1.0

This repository contains the most recently updated trait dataset for LepTraits: A globally comprehensive dataset of butterfly and moth traits. The pre-print for the original publication of this dataset can be found here. A snapshot of v1.0 is available via DataDryad by the following DOI: .

When using this dataset in your research, please cite the original manuscript as follows:

Shirey, V., Larsen, E., Doherty, A., Kim, C.A., Al-Sulaiman, F.T., Hinolan, J.D., Itliong, M.G.A., Naive, M.A.K., Ku, M., Belitz, M., Jeschke, G., Barve, V., Lamas, G., Kawahara, A.Y., Guralnick, R., Pierce, N.E., Lohman, D.J., and Ries, L. 2022. "LepTraits 1.0: A globally comprehensive dataset of butterfly and moth traits." Pre-print available. 

The repository follows a simple directory structure in order to provide data to end-users. The directories and their contents are as follows:
```
LepTraits
│   README.md         # This document.
│   Preprint.pdf      # Pre-print of the data paper.
└───consensus
│   │   consensus.csv # Species-level consensus traits.
│ 
│   
└───misc
|   │   figures.r     # R code used to generate figures for the data paper.
|   └───miscData      # .csv files used to create figures for the manuscript
│       | book_data.csv
|       | name_data.csv
|       | record_data.csv
|
└───records
|   │   records.csv   # Record-level traits.
|   |   habitat_recordKey.csv # A key for the codes utilized in habitat affinity scoring.


```

We envision this project evolving over time. Please check back often for smaller updates and notifications.
 
