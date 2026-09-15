Package purpose

Build an R package that extends multiclass ROC AUC analysis by providing
Hand Till multiclass AUC support
bootstrap confidence intervals
pairwise class diagnostics
ROC specific data validation
visualisation and uncertainty reporting


Initial input

truth

A vector or factor containing the actual class labels

Example

A B C A B C


predictions

A matrix or data frame containing prediction scores or probabilities

Example

        A      B      C
obs1   0.8    0.1    0.1
obs2   0.2    0.7    0.1
obs3   0.1    0.2    0.7


Initial rules

At least 3 classes

Number of truth values must match number of prediction rows

Prediction columns must correspond to classes

Input must not contain invalid values

Missing values must be detected and handled or reported clearly


Initial workflow

truth + predictions
        ↓
input validation
        ↓
Hand Till multiclass AUC
        ↓
later bootstrap CI
        ↓
later pairwise diagnostics
        ↓
later visualisation


Initial result structure

auc

classes

warnings

diagnostic information

Later this can also include

confidence interval

bootstrap results

pairwise AUC and CI results


Week 1 package structure

R

tests/testthat

vignettes

README

DESCRIPTION

NAMESPACE

benchmark or analysis area


Week 1 goal

Create the first working package baseline

Input can be validated

Hand Till multiclass AUC can be calculated

Result can be checked against pROC

Tests can run

Package can build and load

Documentation structure exists