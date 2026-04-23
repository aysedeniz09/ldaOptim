# ldaOptim 0.1.0

## Initial Release

### Features

* **Alpha Optimization (Stage 1)**
  - `lda_find_alpha()`: Cross-validation for alpha parameter optimization
- `plot_alpha_crossval()`: Boxplot visualization of cross-validation results
- `plot_alpha_smooth()`: Smooth trend lines for perplexity across k values
- `plot_alpha_second_derivative()`: Second derivative analysis for identifying optimal k
- `suggest_alpha_elbow()`: Numeric elbow detection on the perplexity vs k curve (Kneedle / maximum-distance-from-chord method) to complement visual inspection

* **Topic Number Optimization (Stage 2)**
  - `lda_find_topics()`: Four metrics (Griffiths2004, CaoJuan2009, Arun2010, Deveaud2014) for topic number selection
- `plot_topics_metrics()`: Two-panel visualization of all four metrics
- `suggest_topics_elbow()`: Numeric elbow detection applied uniformly across all four metrics, with per-metric chord distance as a sharpness diagnostic

* **Model Fitting (Stage 3)**
  - `lda_run_models()`: Parallel fitting of final LDA models at chosen k values

* **Results Export (Stage 4)**
  - `export_lda_results()`: One-step export of all results to Excel
- `get_top_words()`: Extract top words by beta probability
- `get_frex_words()`: Extract FREX (frequency-exclusivity) words
- `get_top_docs()`: Extract top documents by gamma probability

### Improvements over existing packages

* Modern replacement for deprecated `ldatuning` package
* Systematic workflow from DTM to final results
* Parallel processing support throughout
* Flexible column naming for easy integration with existing workflows
* Publication-ready visualizations with Times New Roman fonts
* Optional numeric elbow-detection helpers as a complement to visual inspection
* Excel export functionality for easy sharing with collaborators
* Comprehensive preprocessing example code in documentation

### Documentation

* Complete function documentation with roxygen2
* README with quick start guide and full workflow examples
* MIT License

### Authors

* Ayse Deniz Lokmanoglu (Boston University)
* Dror Walter
* Yotam Ophir

### Citation

Lokmanoglu, A.D., Walter, D., & Ophir, Y. (2026). ldaOptim: Systematic Parameter Optimization for LDA Topic Modeling. R package version 0.1.0. https://github.com/aysedeniz09/ldaOptim

### References

Murzintcev, N. (2020). ldatuning: Tuning of the Latent Dirichlet Allocation Models Parameters. R package version 1.0.2. https://CRAN.R-project.org/package=ldatuning (Archived)

Jacobi, C., van Atteveldt, W., & Welbers, K. (2016). Quantitative analysis of large amounts of journalistic texts using topic modelling. *Digital Journalism*, 4(1), 89-106. https://doi.org/10.1080/21670811.2015.1093271

Satopaa, V., Albrecht, J., Irwin, D., & Raghavan, B. (2011). Finding a "Kneedle" in a Haystack: Detecting Knee Points in System Behavior. *31st International Conference on Distributed Computing Systems Workshops*, 166-171. https://doi.org/10.1109/ICDCSW.2011.20
