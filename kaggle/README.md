# Kaggle acquisition and notebook workflow

Dataset: [Nutritional values for common foods and products](https://www.kaggle.com/datasets/trolukovich/nutritional-values-for-common-foods-and-products),
by trolukovich. The Kaggle API reports version 1 and CC0: Public Domain.
Original provenance is not sufficiently described by the uploader to equate every
record with a verified current USDA record.

From the RStudio project root:

```r
source("scripts/acquire_kaggle.R")
source("scripts/analyze_foods.R")
```

The scripts use the public Kaggle dataset API, pin the retrieved version, and
record the source, license, row counts and checksums. Raw downloads are ignored by
Git. Generated reports are local build products. Review the committed manifest
before updating the dataset.

For an R Kaggle notebook, attach this dataset and use the notebook-ready
`food_eda.R` script. Hosted execution/upload has not been performed.
