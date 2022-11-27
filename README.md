# Risk-Analysis-Credit


## Background

Lending Club is a P2P online lending platform which provides an online environment for matching borrowers seeking loans and lenders looking to make an investment. With lower operational costs than traditional lenders (banks), such online lending platforms leverage technology, data and analytics to bring quicker and more convenient financing for individual and small business borrowers from investors looking for attractive investment yields.

## Objective
•	Analyze, explore and process the data, develop models to predict which loans are at risk of default. 

•	Suggest investment strategies to the client to help them invest in P2P loans with high returns and low risk

## Data
Data is available at Loan ID level containing loan and borrower characteristics
Rows: 100K
Variables: 145

## Target Variable
Since we’ll be looking at two types of approaches – predicting loan default as a classification problem and predicting returns as a regression problem, well have the following two target variables:

1.	The target variable is loan_status (classification problem).
 
![image](https://user-images.githubusercontent.com/58203363/204113931-d95d00c6-eb42-45f6-a1cb-1e7761b6bff2.png)



2.	The target variable is actualReturn (calculated attribute) for regression problem.

![image](https://user-images.githubusercontent.com/58203363/204113936-62f7b9f0-dcb1-4537-b4f6-2df309f146eb.png)

 
## Exploratory Data Analysis/Feature Selection

Explored the data on loans to develop an understanding of loan grades and subgrades and how they may relate to default and returns performance, loan purpose and any relation to performance, analyses of returns from loans.

•	Evaluate proportion of defaults
•	Analyze how default rate, interest rate, loan amounts vary with grade and sub-grade
•	Calculate annual returns and analyze performance with grade and sub grade
•	Examine how number of loans, loan amounts and defaults vary with loan purpose
•	Analyze borrower characteristics like employment-length, annual-income, home ownership and how they relate to loan status
•	Outlier Analysis and handling missing values (data cleaning)

## Feature Engineering and selection
-	Generate derived attributes which may be useful for predicting default
-	Consider the potential for data leakage and eliminate variables which may not be available when applying the model
-	Perform univariate analyses to determine which variables will be individually useful for predicting the dependent variable (loan_status)

## Predictive Modeling

### Approach 1 

The following classification models have been developed to predict default (where loan_status is charged off). Experimentation with different parameters has been done to improve performance. 

•	Decision Tree

•	Random Forest

•	Generalized Boosted Modeling (GBM)

•	GLM 

Evaluation metrics: Accuracy, Sensitivity, Specificity, AUROC

Best model is GBM with highest specificity

### Approach 2

The following regression models have been developed to predict actual annual returns

•	GLM (Regression)

•	Random Forest Regression

Evaluation metrics: MSE

Best model –Random Forest with lowest MSE

### Approach 3

The best way to find what loans to invest in would be to find out the prediction which loan would have the maximum return as well as least probability to default. Therefore, a hybrid approach can be  developed to combine approach 1 and 2 to predict

•	Loans with low default likelihood, and good returns  

•	High returns loans, and those with low default likelihood

## Conclusion

The overall goal is to minimize loss due to charged off loans and invest in safe loans with higher interest rates. Diversification can achieve this goal by limiting exposure to a single risk level, while aiming for a range of annual returns. After developing and evaluating models to predict loan status and annual returns, we can conclude that investors can follow a hybrid approach for investing in loans by diversifying their investment portfolio. This can be done by combining the following approaches

1.	Maximizing returns by predicting loans that are likely to be paid off that fall under lower grades (higher returns with high risk);

2.	Minimizing risk to balance any losses by investing in higher grade loans (lower returns with low risk)
