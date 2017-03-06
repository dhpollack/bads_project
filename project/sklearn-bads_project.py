
# coding: utf-8

# In[1]:

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.ensemble import RandomForestClassifier, GradientBoostingClassifier
from sklearn.metrics import confusion_matrix, make_scorer, roc_auc_score
from sklearn.model_selection import RandomizedSearchCV, GridSearchCV, train_test_split
from sklearn.externals import joblib
# non-standard package: http://contrib.scikit-learn.org/imbalanced-learn/index.html
# https://www.jair.org/media/953/live-953-2037-jair.pdf
from imblearn.over_sampling import SMOTE
from imblearn.combine import SMOTETomek

get_ipython().magic('matplotlib inline')


# In[2]:

def loadDataset(df):
    # remove NA
    df.fillna(-99999, inplace = True)
    # Convert Dates
    df.order_date = pd.to_datetime(df.order_date, format='%Y-%m-%d')
    df.account_creation_date = pd.to_datetime(df.account_creation_date, format='%Y-%m-%d')
    df.deliverydate_estimated = pd.to_datetime(df.deliverydate_estimated, format='%Y-%m-%d')
    df.deliverydate_actual = pd.to_datetime(df.deliverydate_actual, format='%Y-%m-%d')
    # Convert Categories
    df.form_of_address = df.form_of_address.astype('category')
    df.email_domain = df.email_domain.astype('category')
    df.postcode_invoice = df.postcode_invoice.astype('category')
    df.postcode_delivery = df.postcode_delivery.astype('category')
    df.payment = df.payment.astype('category')
    df.advertising_code = df.advertising_code.astype('category')
    df.x_advertising_code_bin = df.x_advertising_code_bin.astype('category')
    df.x_order_date_yearweek = df.x_order_date_yearweek.astype('category')
    return(df)

def bads_costs(y_t, yhat, m = np.array([[3., 0.], [-10., 0.]])):
    N = yhat.shape[0]
    C = confusion_matrix(y_t, yhat)
    return(np.multiply(C, m).sum() / N)

def bads_scorer(y_t, yhat_prob, m = np.array([[3., 0.], [-10., 0.]])):
    thresholds = np.linspace(0.01, 0.99)
    costs = [bads_costs(y_t, yhat_prob[:,1] > threshold, m) for threshold in thresholds]
    return(np.max(costs))

def find_corr_features(df, threshold = 0.7):
    cols = df.columns.values.tolist()
    corr_items = np.where(np.abs(np.triu(df.corr(), k=1)) > threshold)
    for corr_item in list(set([cols[max(item)] for item in zip(*corr_items)])):
        cols.remove(corr_item)
    return(cols)


# In[3]:

#####################################
#
#  Variable Selection
#
#####################################
######### Set Seed #########
rs = 90049
save_model = False

######### Feature Selection #########
#manual_features_to_remove = ["item_count", "x_order_date_yearweek", "x_remitted", "x_remitted_all", "x_canceled", "x_tot_canceled_remitted"]
manual_features_to_remove = ["item_count"]
feature_correlation_removal = True
feature_correlation_threshold = 0.7
automatic_feature_selection = True
automatic_feature_threshold = 0.005

######### Oversampling #########
oversample = "SMOTE"

######### Cross-Valdiation #########
do_cv = True # this takes a long time
cv_num_folds = 4
cv_validation_frac = 0.15
cost_func = bads_costs # bads_costs, roc_auc_score
score_func = bads_scorer # bads_scorer, roc_auc_score

######### Model Selection #########
# Random Forest Classifier
clf = RandomForestClassifier(random_state=rs)
automatic_feature_selection_params = {'n_estimators': 250, 'n_jobs': 3, 'verbose': 0}
clf_default_params = {'min_samples_split': 4, 'n_estimators': 500, 'min_samples_leaf': 9, 'verbose': 0}
cv_param_grid = {'n_estimators':[100, 250, 500], 'min_samples_split':[2, 4, 8], 'min_samples_leaf': [1, 3, 9]}

# Gradient Boosting Classifier
#clf = GradientBoostingClassifier(random_state=rs)
#automatic_feature_selection_params = {'n_estimators': 50, 'verbose': 1}
#clf_default_params = {'learning_rate': 0.1, 'max_depth': 5, 'n_estimators': 500, 'verbose': 1}
#cv_param_grid = {'n_estimators':[100, 250, 500], 'learning_rate':[0.25, 0.1, 0.25], 'max_depth': [3, 5, 9]}



# In[4]:

# Load and split training  and testing data
train = pd.read_csv("output/train_cleaned.csv", sep=";", index_col="ID")
train = loadDataset(train)
# Create Feature List
features_to_use = train.columns.values.tolist()
features_to_use.remove("return_customer")
for ftr in manual_features_to_remove:
    features_to_use.remove(ftr)
for date_feature, v in train.dtypes.items():
    if v == "datetime64[ns]": features_to_use.remove(date_feature)
train = train[features_to_use + ["return_customer"]]
train = pd.get_dummies(train)
# feature Correlation Removal
if feature_correlation_removal:
    print("Removing correlated features...")
    noncorr_cols = find_corr_features(train.drop("return_customer", 1), feature_correlation_threshold)
    train = train[noncorr_cols + ["return_customer"]]
X_train, y_train = train.drop("return_customer", 1), train["return_customer"]

test = pd.read_csv("output/test_cleaned.csv", sep=";", index_col="ID")
test = loadDataset(test)
test = pd.get_dummies(test)
test = test.reindex(columns = train.columns, fill_value=0)
test.drop("return_customer", 1, inplace=True)
X_test = test.values

# Oversample
if oversample == "simple":
    print("Simple oversampling...")
    ret_cust_idx = []
    ret_cust_idx.extend(y_train.loc[y_train == 0].index)
    ret_cust_idx.extend(y_train.loc[y_train == 1]
                .sample(frac=(y_train.loc[y_train == 0].count() / y_train.loc[y_train == 1].count()),replace=True)
                .index)
    X_train, y_train = X_train.loc[ret_cust_idx,:].reset_index(drop=True).values, y_train[ret_cust_idx].reset_index(drop=True).values
elif oversample == "SMOTE":
    print("SMOTE oversampling...")
    sm = SMOTE(kind='regular', random_state = rs)
    X_train, y_train = sm.fit_sample(X_train, y_train)
elif oversample == "SMOTETomek":
    print("SMOTE + Tomek Links oversampling...")
    sm = SMOTETomek(random_state = rs)
    X_train, y_train = sm.fit_sample(X_train, y_train)
else:
    print("No oversampling...")
    X_train, y_train = X_train.values, y_train.values

# Automatic Feature Selection
if automatic_feature_selection:
    print("Starting automatic feature selection...")
    # this takes about 10 minutes to run
    clf.set_params(**automatic_feature_selection_params)
    clf.fit(X_train, y_train)
    important_features = np.where(clf.feature_importances_ > automatic_feature_threshold)[0].tolist()
    important_features_labels = train.drop("return_customer", 1).columns[important_features]
    print(important_features_labels)
    np.savetxt("output/optimal_features.csv", important_features_labels.values, fmt="%s", delimiter=";")

    X_train, X_test = X_train[:,important_features], X_test[:,important_features]
        
print(X_train.shape, X_test.shape)


# In[5]:

# Run the model
# Create Validation Splits
X_train_cv, X_valid_cv, y_train_cv, y_valid_cv = train_test_split(X_train, y_train, 
                                                                  test_size = cv_validation_frac, 
                                                                  random_state = rs)

# Cross-Validation
if do_cv:
    # this can take a LONG time
    clf_cv = RandomizedSearchCV(clf, cv_param_grid, 
                             scoring = make_scorer(score_func, needs_proba=True), 
                             cv = cv_num_folds,
                             random_state = rs, verbose = 1)
    clf_cv.fit(X_train_cv, y_train_cv)
    #clf_rf_cv.cv_results_
    joblib.dump(clf_cv.cv_results_, 'output/clf_rf_cv.results.pkl')
    print("Cross Valdiation Report:")
    print(clf_cv.best_params_)

    # Train and Validate a random forest classifier with the best parameters
    yhat_valid_prob = clf_cv.predict_proba(X_valid_cv)
    
    params_star = clf_cv.best_params_
    clf.set_params(**params_star)
else:
    clf.set_params(**clf_default_params)
    clf.fit(X_train_cv, y_train_cv)
    yhat_valid_prob = clf.predict_proba(X_valid_cv)
    
print("Validation Summary:")
print("Calculate Optimal Threshold")
thresholds = np.linspace(0.01, 0.99)
costs = [bads_costs(y_valid_cv, yhat_valid_prob[:,1] > threshold) for threshold in thresholds]
threshold_star = thresholds[np.argmax(costs)]
# Plot
plt.plot(thresholds, costs)
plt.show()
print("Threshold:", threshold_star)
yhat_valid = yhat_valid_prob[:,1] > threshold_star
print(cost_func(y_valid_cv, yhat_valid))
print("Validation: {} of {}".format(np.sum(yhat_valid), len(yhat_valid)))
print(confusion_matrix(y_valid_cv, yhat_valid))
# Train model with all data and use on the Test set
clf.fit(X_train, y_train)
yhat_test_proba = clf.predict_proba(X_test)
yhat_test = yhat_test_proba[:,1] > threshold_star
np.savetxt("output/test_return_customer.csv", yhat_test.astype(int), fmt='%i', delimiter=";")
print("Testing: {} of {}".format(np.sum(yhat_test), len(yhat_test)))

if save_model:
    joblib.dump(clf, 'output/model_final.pkl')
    #clf_rf = joblib.load('output/model_final.pkl')


# In[6]:

# PCA Analysis of Results

from sklearn.decomposition import PCA
from sklearn.preprocessing import scale
import seaborn as sns

num_PC = 5

train_scaled = scale(X_train)
pca = PCA(n_components=num_PC)
pca.fit(train_scaled)
train_rotated = pca.transform(train_scaled)
df_train = pd.DataFrame(train_rotated)
df_train["colors"] = y_train
sns.pairplot(df_train, hue = "colors", diag_kind="kde", vars=range(num_PC))
plt.show()
test_scaled = scale(X_test)
test_rotated = pca.transform(test_scaled)
df_test = pd.DataFrame(test_rotated)
df_test["colors"] = yhat_test
sns.pairplot(df_test, hue = "colors", diag_kind="kde", vars=range(num_PC))
plt.show()


# ## Tests Below This Point

# In[ ]:

# Single Random Forest

cost_func = roc_auc_score # bads_costs roc_auc_score

clf_rf = RandomForestClassifier(n_estimators=4000)

clf_rf.fit(X_train_cv, y_train_cv)
yhat_valid = clf_rf.predict(X_valid_cv)
print("Validation Summary:")
print(cost_func(y_valid_cv, yhat_valid))
print("Validation: {} of {}".format(np.sum(yhat_valid), len(yhat_valid)))
print(confusion_matrix(y_valid_cv, yhat_valid))

clf_rf.fit(X_train, y_train)
print("Test Results")
yhat = clf_rf.predict(X_test)
print("{} of {}".format(np.sum(yhat), len(yhat)))


# In[ ]:

# Gradient Boost Classifier (testing)

cost_func = roc_auc_score # bads_costs roc_auc_score

clf_gbc = GradientBoostingClassifier(learning_rate=0.05, n_estimators=500, random_state=1234, verbose=1)
clf_gbc.fit(X_train_cv, y_train_cv)
yhat_valid = clf_gbc.predict(X_valid_cv)
print("Validation Summary:")
print(cost_func(y_valid_cv, yhat_valid))
print("Validation: {} of {}".format(np.sum(yhat_valid), len(yhat_valid)))
print(confusion_matrix(y_valid_cv, yhat_valid))
print(X_train.columns[np.where(clf_gbc.feature_importances_ > 0.005)])


# In[ ]:

# Sample Confusion Matrix

a = np.array([1, 0, 0, 1, 1, 1, 0, 0, 0, 0])
b = np.array([True, False, False, False, False, False, True, True, True, True])
confusion_matrix(a,b), bads_costs(a,b)


# In[ ]:




# In[ ]:



