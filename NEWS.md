# live 1.5.3

* Minor fix to `euclidean_kernel` function.
* Default kernel in `fit_explanation` is now `gaussian_kernel`.
* Order of arguments changed in `add_predictions` and `data` arguments defaults to `NULL`.
* Variables are standardized after predictions are added, before explanation model is fitted in `fit_explanation` function.

# live 1.5.2

* Print functions for results of sample_locally, add_predictions and fit_explanation.

# live 1.5.1

* New, LIME-like method of sampling as an option in `sample_locally`.

# live 1.5.0

* Observations in simulated dataset can now be weighted according to their distance from the explained instance. The distance is defined by `kernel` argument to `fit_explanation` function.
* Some variables can be excluded from sampling. This is controled via `fixed_variables` argument to `sample_locally` function.
* Documentation was improved.
* Object returned by `sample_locally`, `add_predictions` and `fit_explanation` functions now carry more information (mainly explained instance) so some function calls were simplified (`plot_explanation`).

# live 1.4.2

* Fixed bug in variable selection.

# live 1.4.1

* Variable selection is now better suited to work with factor/character variables.

# live 1.4.0

* Variable selection is now based on LASSO as implemented in glmnet package.
* Updated documentation and vignette.

# live 1.3.3

* `add_predictions` also returns black box model object (`model` element).


# live 1.3.2

* Hyperparameters can be also passed to `add_predictions` function.

# live 1.3.1

* `fit_explanation` is now more flexible, can take a list of hyperparameters for a chosen model.

# live 1.3.0

* For classification problems waterfall plots can be drawn on probability or logit scale.

# live 1.2.0

* Now using forestmodel package for better factor handling.

# live 1.1.2

* Date variables will now be hold constant while performing local exploration.
* Improved performance.

# live 1.1.1

* `add_predictions` improved to handle more learners (for example ranger).

# live 1.1.0

* Added a `NEWS.md` file to track changes to the package.
* `sample\_locally` uses data.table for faster local exploration.

# live 1.0.0

* Cheatsheet added.
* First package release.
