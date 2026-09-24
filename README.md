# Data-&-Code-for-Pintail-Dynamic-Subsidies-Paper
This repository hosts analysis code and supporting materials for the forthcoming paper pintail duck management paper:
**Regional conservation actions propagate system‑wide benefits in a migratory species network**.
- **Authors:** B.J. Mattsson, J. Windt, M. Ibañez, C. Gottfried, D. Semmens, W. E. Thogmartin, C. Sample, J. Dubovsky, J. J. Derbridge, J. Diffendorfer, and L. Lopez-Hoffman
- **Primary data archive:** [https://drive.boku.ac.at/d/cf40da8d7f4c468bb5b0/?dl=1](url)
- **Repository purpose:** Transparent, reproducible code and minimal data necessary to regenerate figures, tables, and key results referenced in the manuscript.


---

## 1. Project overview

This project explores how regional conservation interventions (e.g., harvest moratoriums, habitat restoration) can have **network‑level effects** across the migratory annual cycle of northern pintail. Analyses emphasize scenario comparison, equilibrium vs. transition dynamics, and cross‑regional propagation of benefits.

---

## 2. Repository structure
```
├─ code/                # Analysis scripts (R), functions, helpers
├─ results/             # Generated outputs (figures, tables, model objects)
├─ docs/                # Manuscript snippets, figure captions, notes
├─ code.json            # metadata JSON
├─ LICENSE              # License (e.g., CC BY 4.0)
└─ README.md
```

The structure can also be viewed visually at this link: https://docs.google.com/presentation/d/1UjhgXnqCptuxABpWU7--vbBmPAYiVsYy9PZ6UnObFN0/edit?usp=sharing

## 3. Getting started

Full data are provided via the external archive link above. This keeps the repo lightweight and reproducible while meeting FAIR/archival expectations.

### Prerequisites
- R ≥ 4.3 and/or Python ≥ 3.10
- Suggested R packages: `tidyverse`, `targets`/`tarchetypes`, `sf`, `readr`, `here`
- Suggested Python packages: `pandas`, `numpy`, `matplotlib`, `pyproj`

> Pin exact versions in `renv.lock` (R) or `requirements.txt` (Python) to enhance reproducibility. 【2-bef018】

### Setup
```bash
# clone the repository
git clone https://github.com/Cooper-13/Data-Storage-for-Pintail-Dynamic-Subsidies-Paper.git
cd Data-Storage-for-Pintail-Dynamic-Subsidies-Paper

# (optional) set up reproducible env
R -e "install.packages('renv'); renv::init()"
# or
python -m venv .venv && source .venv/bin/activate && pip install -r requirements.txt

## 4. Data access
Primary data archive: https://drive.boku.ac.at/d/cf40da8d7f4c468bb5b0/?dl=1
After downloading, place large files in data/inputs/ as indicated by config/paths.yml.

## 5. How to run
# R workflow (targets)
R -e "targets::tar_make()"

## 6. License
MIT license (see license file for more)

## 7. Contributing
Please open issues or pull requests with proposed changes. Use the provided templates and follow the coding style guidelines (docs/style.md).

## 8. Acknowledgments
We thank all collaborators and institutions involved in data provision, scenario design, and manuscript review.

## 9. Contact
For questions about code or analysis, open a GitHub issue or contact the maintainers listed in code.json.
