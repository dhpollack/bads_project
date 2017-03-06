#!/usr/bin/env python
import matplotlib
matplotlib.use('Agg')
from badsproject import BADS

#####################################
#
#  Variable Selection
#
#####################################
params_rf = {
    ######### Set Seed #########
    "rs": 90049,
    "save_model": False,

    ######### Feature Selection #########
    "manual_features_to_remove": ["x_order_date_num", "x_account_creation_date_num", 
                                  "x_deliverydate_estimated_num", "x_deliverydate_actual_num"],
    "feature_correlation_removal": False,
    "feature_correlation_threshold": 0.7,
    "automatic_feature_selection": False,
    "automatic_feature_threshold": 0.005,

    ######### Oversampling #########
    # non-standard package: http://contrib.scikit-learn.org/imbalanced-learn/index.html
    "oversample_method": "none",

    ######### Cross-Valdiation #########
    "do_cv": False, # this takes a long time
    "cv_num_folds": 8,
    "cv_validation_frac": 0.15,
    "cv_rs_iters": 10,

    ######### Model Selection #########
    "model_to_use": "rf", # "rf" or "gbc" or "linear"
    "automatic_feature_selection_params": {'n_estimators': 250, 'verbose': 0, 'n_jobs': 3},
    "clf_default_params": {'min_samples_split': 2, 'n_estimators': 250, 
                           'min_samples_leaf': 9, 'criterion': 'gini', 
                           'verbose': 0, 'oob_score': True, 'n_jobs': 3},
    "cv_param_grid": {'n_estimators':[100, 250, 500], 
                      'min_samples_split':[2, 4, 8], 
                      'min_samples_leaf': [1, 3, 9, 15], 
                      'n_jobs': [3]}

}

a = BADS()
a.set_model("rf")
a.__dict__.update(params_rf)
# Load and split training  and testing data and create cross validation sets from training data
a.create_datasets(use_woe = False)
# Oversample if desired
a.oversample()
# Run algorithm-based feature selection
a.automagic_feature_selection()
# Print size of training sets
print(a.X_train.shape, a.X_test.shape, a.X_train_cv.shape, a.X_valid_cv.shape)
# Run the models
a.run_model("output/8.csv")

