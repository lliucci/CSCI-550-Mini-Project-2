data = readr::read_csv("data_cleaned.csv")

# Simple Linear Regression -----------------------------
lm_slr = lm(`Sale Price` ~ Age, data = data)
summary(lm_slr)

# Multiple Linear Regression ---------------------------
lm_mlr = lm(`Sale Price` ~ Age + Baths * Bedrooms, data = data)
summary(lm_mlr)

# PCA --------------------------------------------------
numeric_data = data %>% select_if(is.numeric)
PCs = prcomp(numeric_data)

PCs$sdev^2/sum(PCs$sdev^2)

# Non-Linear Models ------------------------------------
lm_nonlinear = lm(`Sale Price` ~ poly(Age, 3), data = data)
summary(lm_nonlinear)

# Comparing Models -------------------------------------
AIC(lm_slr, lm_mlr, lm_nonlinear)

# Cross-Validation -------------------------------------

# define number of folds
k = 5

# select k-1/k ids for training
train_ids = sample(1:nrow(data), size = round((nrow(data)*(k-1))/k,0), replace = FALSE)

# make training data
train_data = data[train_ids,]
test_data = data[-train_ids,]

nrow(test_data) + nrow(train_data) == nrow(data)

lm_cv = lm(`Sale Price` ~ Age, data = train_data)
lm_cv_preds = predict(lm_cv, newdata = test_data)

Metrics::mse(predicted = cv_preds, actual = test_data$`Sale Price`)
