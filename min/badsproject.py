##################### MIT LICENSE #########################################
#
# Copyright 2017 
#   David Pollack <david.pollack@cms.hu-berlin.de>
#   
#   Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
#   
#   The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
# LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
# OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
# WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
# 
##########################################################################

"""Functions to run machine learning data processing and predictions

This library creates convenience functions to run models for the Business Analytics 
and Data Science class at Humboldt Universitaet in Winter Semester 2016/17.


Examples
--------
Create a BADS object

>>> a = BADS()
>>> a.create_datasets(use_woe = True)
# Oversample if desired
>>> a.oversample()
# Run algorithm-based feature selection
>>> a.automagic_feature_selection()
# Print size of training sets
>>> print(a.X_train.shape, a.X_test.shape, a.X_train_cv.shape, a.X_valid_cv.shape)
# Run the models
>>> a.run_model()

"""


import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.metrics import confusion_matrix, make_scorer, roc_auc_score
from sklearn.model_selection import RandomizedSearchCV, GridSearchCV, train_test_split
from sklearn.externals import joblib


class BADS(object):
    def __init__(self):
        # Data
        self.X_train = None
        self.y_train = None
        self.X_train_cv = None
        self.X_valid_cv = None
        self.y_train_cv = None
        self.y_valid_cv = None
        self.column_names = None

        self.X_test = None
        self.yhat = None
        
        self.thresholds = None

        # Classifiers
        self.clf = None
        self.clf_cv = None
        
        # Cost matrix
        self.cm = np.array([[3., 0.], [-10., 0.]])
        
        # variables to be set
        self.rs = 90049
        self.save_model = False

        ######### Feature Selection #########
        self.manual_features_to_remove = ["x_order_date_num", 
                                          "x_account_creation_date_num", "x_deliverydate_estimated_num", 
                                          "x_deliverydate_actual_num"]
        self.feature_correlation_removal = False
        self.feature_correlation_threshold = 0.7
        self.automatic_feature_selection = False
        self.automatic_feature_threshold = 0.005

        ######### Oversampling #########
        # non-standard package: http://contrib.scikit-learn.org/imbalanced-learn/index.html
        self.oversample_method = "none"

        ######### Cross-Valdiation #########
        self.do_cv = False # this takes a long time
        self.cv_num_folds = 4
        self.cv_validation_frac = 0.15
        self.cv_rs_iters = 20
        self.cost_func = self.bads_costs # bads_costs, roc_auc_score
        self.score_func = self.bads_scorer # bads_scorer, roc_auc_score
        self.set_model("rf") # "rf" or "gbc" or "linear"

    def set_model(self, model_to_use = None):
        """Set the model to use from a pre-set list.

        One could set these variables manually but for ease of use, we have created 
        a list of predefined models to ease of use.

        Parameters
        ----------
        model_to_use: a string of the model to be used.
            If None, use the objects models.

        """
        if model_to_use == None:
            model_to_use = self.model_to_use
        ######### Model Selection #########
        if model_to_use == "rf":
            # Random Forest Classifier
            from sklearn.ensemble import RandomForestClassifier
            self.clf = RandomForestClassifier(random_state=self.rs)
            self.automatic_feature_selection_params = {'n_estimators': 250, 'verbose': 0, 'n_jobs': 3}
            self.clf_default_params = {'min_samples_split': 2, 'n_estimators': 250, 
                                       'min_samples_leaf': 9, 'criterion': 'gini', 
                                       'verbose': 0, 'oob_score': True, 'n_jobs': 3}
            self.cv_param_grid = {'n_estimators':[100, 250, 500], 
                                  'min_samples_split':[2, 4, 8], 
                                  'min_samples_leaf': [1, 3, 9], 
                                  'n_jobs': [3]}
        elif model_to_use == "gbc":
            # Gradient Boosting Classifier
            from sklearn.ensemble import GradientBoostingClassifier
            self.clf = GradientBoostingClassifier(random_state=self.rs)
            self.automatic_feature_selection_params = {'n_estimators': 50, 'verbose': 1}
            self.clf_default_params = {'learning_rate': 0.1, 'max_depth': 3, 
                                       'n_estimators': 100, 'verbose': 1}
            self.cv_param_grid = {'n_estimators':[50, 100, 250, 500], 
                                  'learning_rate':[0.05, 0.1, .25], 
                                  'max_depth': [3, 5, 9]}
        elif model_to_use == "linear":
            # Logistic Regression Classifier
            from sklearn import linear_model
            self.clf = linear_model.LogisticRegression()
            self.clf_default_params = {'penalty': 'l1'}
            self.cv_param_grid = {'penalty':['l1', 'l2'], 'C': 2 ** np.linspace(-3, 5, 17), 'n_jobs': [3]}
        else:
            print("Please Set The Model")

    def simple_oversample_idx(self, y):
        """Simple oversample to equalize the two groups.

        Parameters
        ----------
        y: an array of the true target variable values.

        """

        y_idx_0 = np.where(y == 0)[0]
        y_idx_1 = np.random.choice(np.where(y == 1)[0], size=y_idx_0.shape[0], replace=True)
        ret_cust_idx = []
        ret_cust_idx.extend(y[y_idx_0])
        ret_cust_idx.extend(y[y_idx_1])
        return(ret_cust_idx)

    def bads_costs(self, y_t, yhat):
        """Return the profit per customer.

        This function calculates the profit per customer based on the matrix given 
        to us in the assignment.

        Parameters
        ----------
        y_t: an array of true target variable values.

        yhat: an array of binary predictions from our model.

        """
        N = yhat.shape[0]
        C = confusion_matrix(y_t, yhat)
        return(np.multiply(C, self.cm).sum() / N)

    def bads_scorer(self, y_t, yhat_prob):
        """Return the maximum profit per customer

        This function does a simple line search using the assignment cost/profit 
        function.  For each threshold, we create a vector of binary predictions 
        and then we calculate the profit per customer using these binary predictions.  
        The maximum value is returned. 
        
        Note: All threshold levels at which this maximum occurred are saved to 
              the object.

        Parameters
        ----------
        y_t: an array of true target variable values.

        yhat_prob: an array of probability predictions from our model.

        """
        thresholds = np.linspace(0.01, 0.99)
        costs = [self.bads_costs(y_t, yhat_prob[:,1] > threshold) for threshold in thresholds]
        self.thresholds.append(thresholds[np.argmax(costs)])
        return(np.max(costs))

    def find_corr_features(self, df, threshold = 0.7):
        """Return list of column names.

        This function calculates the simple correlation matrix between features 
        and based on a given threshold (default: abs(0.7)) removes the feature 
        that comes later on in the feature list.  This will prioritize original 
        features over features we have created. 

        Parameters
        ----------
        df: a pandas dataframe (either the train or test set)

        threshold: a scalar value above whose absolute value features will be 
                   considered "highly correllated".

        """
        cols = df.columns.values.tolist()
        corr_mat = df.corr()
        corr_items = np.where(np.abs(np.triu(corr_mat, k=1)) > threshold)
        cols_removed = []
        for corr_item in list(set([cols[max(item)] for item in zip(*corr_items)])):
            cols_removed.append(corr_item)
            cols.remove(corr_item)
        print("Removing Columns:", ", ".join(cols_removed))
        return(cols)

    def loadDataset(self, df, date_to_int = True, use_woe = True):
        """Return pandas dataframe.

        The purpose of this function is to put our dataframe into a form that is 
        as close as possible to our R dataframe that we've used in our data 
        processing steps.
        
        Additionally, we have calculated dates, added an "is_weekday" dummy variable 
        and chosen to use or not use the Weight of Evidence variables.

        Parameters
        ----------
        df: a pandas dataframe (either the train or test set)

        date_to_int: a logical value to decide whether to convert dates to integers 
                     based on the epoch date of January 1st, 2013.

        use_woe: a logical value whether to use Weight of Evidence converted 
                 variables or use the original variables as k-1 dummies.

        """

        # remove NA
        df.fillna(-99, inplace = True)
        # Convert Dates
        df.order_date = pd.to_datetime(df.order_date, format='%Y-%m-%d')
        df.account_creation_date = pd.to_datetime(df.account_creation_date, format='%Y-%m-%d')
        df.deliverydate_estimated = pd.to_datetime(df.deliverydate_estimated, format='%Y-%m-%d')
        df.deliverydate_actual = pd.to_datetime(df.deliverydate_actual, format='%Y-%m-%d')
        # Create weekday dummy for order_date
        df['x_order_date_is_weekday'] = df.order_date.dt.dayofweek < 5
        if date_to_int:
            epoch_date = pd.Timestamp("2013-01-01")
            df.order_date = (df.order_date - epoch_date).astype('timedelta64[D]').astype(int)
            df.account_creation_date = (df.account_creation_date - epoch_date).astype('timedelta64[D]').astype(int)
            df.deliverydate_estimated = (df.deliverydate_estimated - epoch_date).astype('timedelta64[D]').astype(int)
            df.deliverydate_actual = (df.deliverydate_actual - epoch_date).astype('timedelta64[D]').astype(int)
        # Convert Categories (factors in R lingo)
        cols_to_categorize = ["model", "form_of_address", "email_domain", 
                              "postcode_invoice", "postcode_delivery", 
                              "payment", "advertising_code", "x_order_date_yearweek"]
        # Categorize _bin columns
        cols = df.columns
        cols_to_categorize.extend(cols[cols.str.contains("_bin")].values.tolist())
        for col_to_cat in cols_to_categorize:
            #print(col_to_cat)
            if(col_to_cat in df.columns.values):
                df[col_to_cat] = df[col_to_cat].astype('category')
            
        return(df)

    def create_datasets(self, use_woe = False, 
                        fp_train = "output/train_cleaned_woe.csv", 
                        fp_test = "output/test_cleaned_woe.csv"):
        """Load datasets.

        This is a convenience function that loads both the training and testing 
        datasets and implements any feature selection that we've decided to use.  
        Additionally, we impose the column structure of the train set on the test 
        set.  Implicitly, this adds and removes appropriate "factor levels" and 
        gives any added factor level a default of 0.

        Parameters
        ----------
        use_woe: a logical value whether to use Weight of Evidence converted 
                 variables or use the original variables as k-1 dummies.

        fp_train: a string of the train set CSV file

        fp_test: a string of the test set CSV file

        """

        train = pd.read_csv(fp_train, sep=";", decimal=',', index_col="ID")
        train = self.loadDataset(train)
        # Create Feature List
        features_to_use = train.columns.values.tolist()
        features_to_use.remove("return_customer")
        cols_woe_removal = [col for col in features_to_use if "x_woe_" in col]
        if use_woe:
            cols_woe_removal = [col.replace("x_woe_", "") for col in cols_woe_removal]
        self.manual_features_to_remove.extend(cols_woe_removal)

        for ftr in self.manual_features_to_remove:
            if ftr in features_to_use:
                features_to_use.remove(ftr)
            elif "x_"+ftr in features_to_use:
                features_to_use.remove("x_"+ftr)
        # remove dates if not converted to ints
        for date_feature, v in train.dtypes.items():
            if v == "datetime64[ns]": 
                features_to_use.remove(date_feature)
        train = train[features_to_use + ["return_customer"]]
        # Visualize Correlation before splitting out dummy variables
        if self.feature_correlation_removal: 
            sns.heatmap(train.drop("return_customer", 1).corr())
            plt.show()
        # Split out dummy variables
        train = pd.get_dummies(train)
        # feature Correlation Removal
        if self.feature_correlation_removal:
            print("Removing correlated features...")
            noncorr_cols = self.find_corr_features(train.drop("return_customer", 1), 
                                                   self.feature_correlation_threshold)
            train = train[noncorr_cols + ["return_customer"]]
        # set train datasets
        self.X_train, self.y_train = train.drop("return_customer", 1).values, train["return_customer"].values
        self.column_names = train.columns

        test = pd.read_csv(fp_test, sep=";", decimal=',', index_col="ID")
        test = self.loadDataset(test)
        test = pd.get_dummies(test)
        # The following line gives the test set the same columns as the training set. 
        # This simultaneously adds columns to the test set and sets the values in those columns to 0 and 
        # drops any columns in the test set that did not exist in the training set.
        print("Imposing train column structure on test...")
        test = test.reindex(columns = self.column_names, fill_value=0)
        test.drop("return_customer", 1, inplace=True)
        # set test dataset
        self.X_test = test.values
        self.X_train_cv, self.X_valid_cv, self.y_train_cv, self.y_valid_cv = train_test_split(self.X_train, 
                                                                                              self.y_train, 
                                                                                              test_size = self.cv_validation_frac,
                                                                                              stratify = self.y_train,
                                                                                              random_state = self.rs)


    def oversample(self):
        """Oversample datasets.

        Simple: This just normalizes the number of data points to make the two 
                classes equal sizes.  Samples are duplicated at random with 
                replacement.
                
        SMOTE: SMOTE oversampling on the minority class to an equal weight as the 
               majority class.
        
        SMOTE+Tomek: This option oversamples the minority class and then removes 
                     data points which are determined to be Tomek links.
        """
        if self.oversample_method == "simple":
            # oversampling with replacement of the minority group to equalize the size of the minority and 
            # majority group
            print("Simple oversampling...")
            # Create the Hyper-Parameter Cross-Validation train and test sets
            ret_cust_idx_cv = self.simple_oversample_idx(self.y_train_cv)
            self.X_train_cv, self.y_train_cv = self.X_train_cv[ret_cust_idx_cv,:], self.y_train_cv[ret_cust_idx_cv]
            # Create the full train and test sets
            ret_cust_idx = simple_oversample_idx(self.y_train)
            self.X_train, self.y_train = self.X_train[ret_cust_idx,:], self.y_train[ret_cust_idx]
        elif self.oversample_method == "SMOTE":
            # https://www.jair.org/media/953/live-953-2037-jair.pdf
            from imblearn.over_sampling import SMOTE

            print("SMOTE oversampling...")
            sm = SMOTE(kind='regular', random_state = self.rs)
            # Create the Hyper-Parameter Cross-Validation train and test sets
            self.X_train_cv, self.y_train_cv = sm.fit_sample(self.X_train_cv, self.y_train_cv)
            # Create the full train and test sets
            self.X_train, self.y_train = sm.fit_sample(self.X_train, self.y_train)
        elif self.oversample_method == "SMOTETomek":
            from imblearn.combine import SMOTETomek

            print("SMOTE + Tomek Links oversampling...")
            sm = SMOTETomek(random_state = rs)
            # Create the Hyper-Parameter Cross-Validation train and test sets
            self.X_train_cv, self.y_train_cv = sm.fit_sample(self.X_train_cv, self.y_train_cv)
            # Create the full train and test sets
            self.X_train, self.y_train = sm.fit_sample(self.X_train, self.y_train)
        else:
            print("No oversampling...")

    def automagic_feature_selection(self):
        """Prune data sets based on algorithmic feature selection.

        We use a particular threshold to keep certain columns based on the 
        "feature importances" of tree-based classifiers (i.e. random forest or 
        gradient boosted trees)
        
        """
        if self.automatic_feature_selection:
            print("Starting automatic feature selection...")
            # this takes about 10 minutes to run
            self.clf.set_params(**self.automatic_feature_selection_params)
            self.clf.fit(self.X_train, self.y_train)
            important_features = np.where(self.clf.feature_importances_ > self.automatic_feature_threshold)[0].tolist()
            important_features_labels = self.column_names[important_features]
            print("High Importance Features:", ", ".join(important_features_labels.tolist()))
            np.savetxt("output/optimal_features.csv", important_features_labels.values, fmt="%s", delimiter=";")

            self.X_train, self.X_test = self.X_train[:,important_features], self.X_test[:,important_features]
            self.X_train_cv, self.X_valid_cv = self.X_train_cv[:,important_features], self.X_valid_cv[:,important_features]

        else:
            print("No automatic feature selection...")

    def run_model(self, fp_output = "output/test_return_customer.csv"):
        """Do hyperparameter search, if desired, and then make prediction on test set.

        We do our hyper parameter search and make our prediction on the test set.  
        At this time, we print out diagnostics and results throughout the process.  
        
        """
        self.thresholds = []
        if self.do_cv:
            # this can take a LONG time
            print("Searching for best parameters with CV search...")
            self.clf_cv = RandomizedSearchCV(self.clf, self.cv_param_grid, 
                                             scoring = make_scorer(self.score_func, needs_proba=True), 
                                             cv = self.cv_num_folds, 
                                             n_iter = self.cv_rs_iters, 
                                             random_state = self.rs, verbose = 1)
            self.clf_cv.fit(self.X_train_cv, self.y_train_cv)
            #clf_rf_cv.cv_results_
            joblib.dump(self.clf_cv.cv_results_, 'output/clf_rf_cv.results.pkl')
            print("Cross Valdiation Report:")
            print("Best Params:", self.clf_cv.best_params_)
            print("Best Score:", self.clf_cv.best_score_)
            # Plot Expected ROI per Customer
            plt.errorbar(range(self.cv_rs_iters), 
                         self.clf_cv.cv_results_["mean_test_score"], 
                         yerr = self.clf_cv.cv_results_["std_test_score"], 
                         fmt="o")
            plt.title("Errorbar Plot of Hyper Parameter Search")
            plt.ylabel("Average ROI")
            plt.xlabel("Iteration (See Table Below)")
            plt.margins(0.03)
            plt.show()
            
            print(pd.DataFrame(list(self.clf_cv.cv_results_['params'])))

            # Train and Validate a random forest classifier with the best parameters
            yhat_valid_prob = self.clf_cv.predict_proba(self.X_valid_cv)

            params_star = self.clf_cv.best_params_
            self.clf.set_params(**params_star)
        else:
            self.clf.set_params(**self.clf_default_params)
            self.clf.fit(self.X_train_cv, self.y_train_cv)
            yhat_valid_prob = self.clf.predict_proba(self.X_valid_cv)

        print("Validation Summary:")
        print("Calculate Optimal Threshold")
        thresholds = np.linspace(0.01, 0.99, 197)
        costs = [self.bads_costs(self.y_valid_cv, yhat_valid_prob[:,1] > threshold) for threshold in thresholds]
        threshold_star = thresholds[np.argmax(costs)]
        # Plot
        plt.plot(thresholds, costs)
        plt.title("Threshold Search")
        plt.ylabel("Average ROI")
        plt.xlabel("Threshold of return_customer = 1")
        plt.show()
        print("Threshold:", threshold_star)
        yhat_valid = yhat_valid_prob[:,1] > threshold_star
        print("Average ROI:", self.cost_func(self.y_valid_cv, yhat_valid))
        print("ROC Score:", roc_auc_score(self.y_valid_cv, yhat_valid_prob[:,1]))
        print("Validation Return Customers: {} of {} ({}%)".format(np.sum(yhat_valid), 
                                                                   len(yhat_valid), 
                                                                   np.round(100*np.sum(yhat_valid)/len(yhat_valid),2) ))
        print(confusion_matrix(self.y_valid_cv, yhat_valid))
        # Train model with all data and use on the Test set
        self.clf.fit(self.X_train, self.y_train)
        yhat_test_proba = self.clf.predict_proba(self.X_test)
        yhat_test = yhat_test_proba[:,1] > threshold_star
        preds = pd.DataFrame(np.c_[np.arange(51885,51885+yhat_test.shape[0]), yhat_test.astype(int)], columns = ["ID", "return_customer"])
        preds.to_csv(fp_output, index=False)
        preds_probs = pd.DataFrame(np.c_[np.arange(51885,51885+yhat_test.shape[0]), yhat_test_proba[:,1]], columns = ["ID", "return_customer"])
        preds_probs.to_csv(fp_output.split(".")[0]+"_probs.csv", index=False)
        print("Testing Return Customers: {} of {} ({}%)".format(np.sum(yhat_test), 
                                                                len(yhat_test), 
                                                                np.round(100*np.sum(yhat_test)/len(yhat_test),2) ))
        self.yhat = yhat_test
        if self.save_model:
            joblib.dump(self.clf, 'output/model_final.pkl')
            #clf_rf = joblib.load('output/model_final.pkl')

    def pca_analysis(self, X, y, num_PC = 5, recalc_PC = True):
        """Visualize data with predictions with Principal Component Analysis.

        Due to the high dimentionality of our data, we found it hard to conceptualize 
        without reducing the dimensionality.  We do a principal component analysis 
        and create a scatter matrix of the results.  Importantly, we allow the PCA 
        to be fitted with the training data and then applied on the testing data, 
        so we will see the two data sets rotated in the same manner.
        
        Parameters
        ----------
        X: numpy array of train or test data

        y: numpy vector of true target values or model predictions

        num_PC: number of principal components to use

        recalc_PC: a logical value
            used to decide whether to recalculate the principal components including 
            the rotation vectors.  If false, one will rotate the data-based on the 
            rotations of the previously calculated principal components.

        """
        # PCA Analysis of Results
        from sklearn.decomposition import PCA
        from sklearn.preprocessing import scale
        train_scaled = scale(X)

        if recalc_PC:
            self.pca = PCA(n_components=num_PC)
            self.pca.fit(train_scaled)
        else:
            print("Using previous eigenvectors to rotate data...")
        train_rotated = self.pca.transform(train_scaled)
        df_train = pd.DataFrame(train_rotated)
        df_train["colors"] = ["returning" if y_i else "non-returning" for y_i in y]
        sns.pairplot(df_train, hue = "colors", diag_kind="kde", vars=range(num_PC))
        plt.show()

