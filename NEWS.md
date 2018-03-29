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
