# soma-pipeline
Pipeline for SOMA data wrangling, visualisation, preprocessing, and analysis.


## Prerequisites & Data Security

1. **Local Data Storage:** The `data/` folder is already included in this repository template.
2. **File Setup:** Place your raw JSON database exports directly inside the pre-existing `data/` folder.
   * *Data Security Note: The contents of the `data/` directory are strictly blocked via `.gitignore` to prevent unauthorized uploads of Protected Health Information (PHI/GDPR). A hidden `.gitkeep` file ensures the empty directory structure is safely tracked by Git without committing your local data.*
3. **Configuration:** Open `config.yml` and verify that the filenames match your local file exports exactly.

---

## Execution Order

Follow these steps in sequence to initialize the project environment and run the pipeline.

### Step 1: Initialize the Reproducible Environment
Before running the code, you must restore the exact package versions used to build this pipeline. Open your R console in this project directory and run:

```R
source("install.R")
```

### Step 2: Execute the Pipeline
```R
source("main.R")
```
