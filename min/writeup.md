## BADS Coding Readme

### Acornyms

* rf: Random Forest  
* gbm: Gradient Boosting or Gradient Boosted Trees  
* logit: Logistic Regression  

### Data Sets
  
* test/test set/testing set: This is the set we made our final predictions on.  Originally the file, "assignment_BADS_WS1617_class.csv". (X_test)  
* train/train set/training set: This is the entire training set.  Originally the file, "assignment_BADS_WS1617_known.csv". (X_train, y_train)
* cross-validation train set: These are the sets that I did all cross-validation on. 85% of the training set. (X_train_cv, y_train_cv)  
* cross-validation validation set: After the hyper-parameter search, I used this set to validate the model with the best parameters 15% of the training set. (X_valid_cv, y_valid_cv).  Note, this is a step further than we did in class.  This way is a slightly more complete method because one do an extra step to make sure your hyper parameter search doesn't overfit, but both are perfectly acceptable ways of doing it.  

### Libraries Used

1) [Numpy](http://www.numpy.org/): The standard python library for scientific computing.  
2) [Pandas](http://pandas.pydata.org/): A data-structure built on top of Numpy.  It has a lot of convenience functions and gives similar functionality to R's dataframes.  
3) [scikit-learn](http://scikit-learn.org/stable/): The most popular machine learning library for Python.  Similar to caret.  
4) [imbalanced learn](http://contrib.scikit-learn.org/imbalanced-learn/): This is an contributed library to scikit learn.  So it doesn't come with scikit-learn  

### Method

Here's what the code did step by step:  

1) Load data from CSV file, add is_weekday variable, convert dates to integers, and code the variables that were 'factors' in R as 'categories' in pandas.  I also impose the structure of the train set onto the test set.  This *creates* columns in TEST and were in TRAIN and *removes* columns in TEST that are *not* in the TRAIN.  This ensures the final model will work on the test set.  
2) Choose an algorithm.  Either [Random Forest](http://scikit-learn.org/stable/modules/generated/sklearn.ensemble.RandomForestClassifier.html), [Gradient Boosting](http://scikit-learn.org/stable/modules/generated/sklearn.ensemble.GradientBoostingClassifier.html), or [Logistic Regression](http://scikit-learn.org/stable/modules/generated/sklearn.linear_model.LogisticRegression.html).  I'll put the parameters used below.  
3) Do feature selection based on given options.  First, I decide whether or not to use the WOE or non-WOE variables.  Then, I manually deleted a few columns related to the dates.  Next, I allowed the option to drop columns based on the correlation to other columns, when two columns are correlated, I would drop the column that comes later in our dataframe.  This leads to keeping more original variables and removing more of the features we added.  Lastly, for the tree algorithms (rf and gbm), I would run the algorithm with near default settings and then keep features based on a threshold in the 'feature_importances_' variable.  Note that the number formats are the same, so I don't have to change the code for the different algorithms, but the calculation of this 'importance' is very different.  This is because random forest and gradient boosting are fundamentally different algorithms.  
4) Do oversampling if desired.  The three methods of oversampling are simple oversampling (random sample of minority class with replacement to equalize classes), SMOTE, and [SMOTE with removal of Tomek links](https://github.com/scikit-learn-contrib/imbalanced-learn#id31).  
5) Do a random hyper-parameter search using a k-Fold cross-validated search.  I used K of 5 and a random search number of 10 different parameter combinations.  After I found the best parameters, I did a final check on an out of sample set (X_valid_cv) to make sure the results were not overfitting.  Also importantly in the case of oversampling, this out of sample validation set was NOT oversampled.  Thus the composition should be the same as the final test set.  
6) Train a model with ALL of the training data (X_train == X_train_cv + X_valid_cv)
7) Use model from (5) to make a prediction on the test set.  
8) Combine the results from our two best models (RF and SVM)  

### Parameters used

There are three possible models to be trained.  First would be the close to default model for feature selection, the next is the model that will search for the best hyper-parameters.  The final is a model to be used without a hyper-parameter search.  This is what I used when I already found the best parameters but wanted to re-run the model.  

For the tree methods, I do the feature importance threshold for feature selection.  For logit, the algorithm uses either L1 ('LASSO') or L2 ('ridge regression').  In particular, LASSO will set coefficients to zero which is a form of automated feature selection.  

The 'n_jobs' parameter is a parallelization parameter.  It's probably something that you should mention in the paper.  

```python
# Random Forest Classifier
self.automatic_feature_selection_params = {'n_estimators': 250, 'verbose': 0, 'n_jobs': 3}
self.clf_default_params = {'min_samples_split': 2, 'n_estimators': 250, 
                           'min_samples_leaf': 9, 'criterion': 'gini', 
                           'verbose': 0, 'oob_score': True, 'n_jobs': 3}
self.cv_param_grid = {'n_estimators':[100, 250, 500], 
                      'min_samples_split':[2, 4, 8], 
                      'min_samples_leaf': [1, 3, 9], 
                      'n_jobs': [3]}
# Gradient Boosting Classifier
from sklearn.ensemble import GradientBoostingClassifier
self.clf = GradientBoostingClassifier(random_state=self.rs)
self.automatic_feature_selection_params = {'n_estimators': 50, 'verbose': 1}
self.clf_default_params = {'learning_rate': 0.1, 'max_depth': 3, 
                           'n_estimators': 100, 'verbose': 1}
self.cv_param_grid = {'n_estimators':[50, 100, 250, 500], 
                      'learning_rate':[0.05, 0.1, .25], 
                      'max_depth': [3, 5, 9]}
# Logistic Regression Classifier
from sklearn import linear_model
self.clf = linear_model.LogisticRegression()
self.clf_default_params = {'penalty': 'l1'}
self.cv_param_grid = {'penalty':['l1', 'l2'], 
                      'C': 2 ** np.linspace(-3, 5, 17), 
                      'n_jobs': [3]}

```
