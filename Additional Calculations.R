#______________________________Financial Risk for Loan Approval__Additional Calculations_______________

#AnuallIncome vs RiskScore 
ggplot(loan, aes(x = AnnualIncome, y = RiskScore)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(title = "Income vs Risk Score",
       x = "Annual Income",
       y = "Risk Score")
#The plot shows a negative relationship between Annual Income and Risk Score.
#This means that customers with higher income tend to have lower risk scores, so they are seen as less risky borrowers.
#The downward trend line confirms this pattern, suggesting that income is an important factor in reducing credit risk.

summary(loan$AnnualIncome)

loan$IncomeGroup <- cut(
  loan$AnnualIncome,
  breaks = c(-Inf, 25000, 50000, 100000, 200000, Inf),
  labels = c("Very Low", "Low", "Medium", "High", "Very High"),
  include.lowest = TRUE
)
#shows the limits of the intervals, we divided them into 5 because there is a large spread
#in further analysis, we can use the segmented/changepoint function

table(loan$IncomeGroup)

ggplot(loan, aes(x = AnnualIncome, y = RiskScore)) +
  geom_point(alpha = 0.4, color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 1) +
  facet_wrap(~ IncomeGroup, ncol = 3, scales = "free_x") + 
  theme_minimal() +
  labs(
    title = "Risk Score vs Annual Income by Income Group",
    x = "Annual Income",
    y = "Risk Score"
  )
#calculation of the sum of squares error (SSE) (the lower the SSE, the better the fit)
sse_by_group <- loan %>%
  group_by(IncomeGroup) %>%
  group_map(~ {
    model <- lm(RiskScore ~ AnnualIncome, data = .x)
    tibble(SSE = sum(residuals(model)^2))
  }) %>%
  bind_rows(.id = "IncomeGroup")

sse_by_group

ggplot(sse_by_group, aes(x = IncomeGroup, y = SSE, fill = IncomeGroup)) +
  geom_col() +
  theme_minimal() +
  labs(
    title = "SSE for each Income Group",
    x = "Income Group",
    y = "Sum of Squared Errors (SSE)"
  ) +
  theme(legend.position = "none")

#DebtToIncomeRatio vs AnnualIncome
ggplot(loan, aes(x = DebtToIncomeRatio, y = AnnualIncome)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(title = "DebtToIncomeRatio vs AnnualIncome",
       x = "DebtToIncomeRatio",
       y = "AnnualIncome")

#AnnualIncome vs DebtToIncomeRatio 
ggplot(loan, aes(x = AnnualIncome, y = DebtToIncomeRatio)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(title = "Annual Income vs Debt To Income Ratio",
       x = "Annual Income",
       y = "DebtTo Income Ratio")


#AnnualIncome vs DebtToIncomeRatio (only for those richer than the threshold)
subset_high_income <- subset(loan, AnnualIncome > 130587)

ggplot(subset_high_income, aes(x = AnnualIncome, y = DebtToIncomeRatio)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(title = "Annual Income vs Debt To Income Ratio",
       x = "Annual Income",
       y = "DebtTo Income Ratio") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.title.position = "plot")

#We have a variable DebtToIncomeRatio, which shows how much debt someone has in relation to how much they earn.
subset_vhigh <- subset(loan, IncomeGroup == "Very High")
#we calculate correlations between variables
cor(subset_vhigh$RiskScore, subset_vhigh$DebtToIncomeRatio, use = "complete.obs")

ggplot(subset_vhigh, aes(x = DebtToIncomeRatio, y = RiskScore)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1) +
  theme_minimal() +
  labs(
    title = "Very High Income: Relationship between Debt and Risk Score",
    x = "Debt-to-Income Ratio",
    y = "Risk Score"
  )
#people who earn a lot are considered more risky in terms of creditworthiness
#LoanAmount vs RiskScore
ggplot(loan, aes(x = LoanAmount, y = RiskScore)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(title = "Loan Amount vs RiskScore",
       x = "Loan Amount",
       y = "RiskScore")
#The plot shows a weak positive relationship between Loan Amount and Risk Score.
#This means that applicants requesting larger loans tend to have slightly higher risk scores.
#However, the points are widely scattered around the trend line, indicating that the relationship is weak and that loan amount alone is not a strong predictor of risk.

# CreditScore vs RiskScore
ggplot(loan, aes(x = CreditScore, y = RiskScore)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "blue") +
  theme_minimal() +
  labs(title = "Credit Score vs Risk Score",
       x = "Credit Score",
       y = "Risk Score")
#The plot shows a clear negative relationship between Credit Score and Risk Score.
#This means that customers with higher credit scores tend to have lower risk scores, indicating better financial stability and lower credit risk.
#The downward trend line confirms that Credit Score is an important predictor of risk level.

# DebtToIncomeRatio vs RiskScore
ggplot(loan, aes(x = DebtToIncomeRatio, y = RiskScore)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "blue") +
  theme_minimal() +
  labs(title = "Debt-to-Income Ratio vs Risk Score",
       x = "Debt-to-Income Ratio",
       y = "Risk Score")

#The plot shows a positive relationship between Debt-to-Income Ratio and Risk Score.
#This indicates that applicants with higher debt levels relative to their income tend to have higher risk scores.
#The upward trend suggests that excessive debt may increase the likelihood of financial instability or loan default.


#Additional Scatterplots 

#Income vs LoanAmount 
ggplot(loan, aes(x = AnnualIncome, y = LoanAmount)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(title = "Income vs Loan Amount",
       x = "Annual Income",
       y = "Loan Amount")
#The plot shows almost no visible relationship between Annual Income and Loan Amount.
#The blue trend line is nearly flat, which means that higher income does not necessarily lead to larger loan amounts.
#This suggests that the loan size may be influenced more by other factors, such as credit score or loan purpose, rather than income alone. 

#LoanAmount vs InterestRate
ggplot(loan, aes(x = LoanAmount, y = InterestRate)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(title = "Loan Amount vs Interest Rate",
       x = "Loan Amount",
       y = "Interest Rate")
#The plot shows a positive relationship between Loan Amount and Interest Rate.
#This means that larger loans tend to have higher interest rates.
#The upward trend line suggests that banks may charge more interest for bigger loan amounts, possibly to balance the higher financial risk involved.

#LoanAmount vs RiskScore
ggplot(loan, aes(x = LoanAmount, y = RiskScore)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(title = "Loan Amount vs RiskScore",
       x = "Loan Amount",
       y = "RiskScore")

#InterestRate vs RiskScore
ggplot(loan, aes(x = InterestRate, y = RiskScore)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(title = "InterestRate vs RiskScore",
       x = "InterestRate",
       y = "RiskScore")

#MonthlyIncome vs AnnualIncome !!!
ggplot(loan, aes(x = MonthlyIncome, y = AnnualIncome)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(title = "MonthlyIncome vs AnnualIncome",
       x = "MonthlyIncome",
       y = "AnnualIncome")
#The scatter plot reveals a strong linear relationship between MonthlyIncome and AnnualIncome, with data points closely aligned along the regression line.
#Given the high correlation, one of these variables could potentially be removed in predictive modeling to reduce redundancy.

#NetWorth vs TotalAssets !!!
ggplot(loan, aes(x = NetWorth, y = TotalAssets)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(title = "NetWorth vs TotalAssets",
       x = "NetWorth",
       y = "TotalAssets")
#Net worth and total assets show a strong positive linear relationship, with data points closely following the trend line. 
#This suggests that as assets increase, net worth rises proportionally. The correlation is stable and could allow one variable to be dropped in modeling.


#NetWorth vs AnnualIncome
ggplot(loan, aes(x = NetWorth, y = AnnualIncome)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(title = "NetWorth vs AnnualIncome",
       x = "NetWorth",
       y = "AnnualIncome")
#Most people with low net worth also have low income. As net worth increases, income varies more. The link between them is weak.

#TotalAssets vs AnnualIncome
ggplot(loan, aes(x = TotalAssets, y = AnnualIncome)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(title = "TotalAssets vs AnnualIncome",
       x = "TotalAssets",
       y = "AnnualIncome")
#The same as above



# Violin plots for Loan Approval

#Interest Rate vs Loan Approval
ggplot(loan, aes(x = as.factor(LoanApproved), y = InterestRate, fill = as.factor(LoanApproved))) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  theme_minimal() +
  labs(title = "Interest Rate vs Loan Approval",
       x = "Loan Approved (0 = No, 1 = Yes)",
       y = "Interest Rate",
       fill = "Loan Approved") +
  scale_fill_manual(values = c("#fdae61", "#2c7bb6"))


#Annual Income vs Loan Approval
ggplot(loan, aes(x = as.factor(LoanApproved), y = AnnualIncome, fill = as.factor(LoanApproved))) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  theme_minimal() +
  labs(title = "Annual Income vs Loan Approval",
       x = "Loan Approved (0 = No, 1 = Yes)",
       y = "Annual Income (USD)",
       fill = "Loan Approved") +
  scale_fill_manual(values = c("#fdae61", "#2c7bb6"))


#TotalAssets vs Loan Approval
ggplot(loan, aes(x = as.factor(LoanApproved), y = TotalAssets, fill = as.factor(LoanApproved))) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  theme_minimal() +
  labs(title = "TotalAssets vs Loan Approval",
       x = "Loan Approved (0 = No, 1 = Yes)",
       y = "TotalAssets",
       fill = "Loan Approved") +
  scale_fill_manual(values = c("#fdae61", "#2c7bb6"))


#CheckingAccountBalance vs Loan Approval
ggplot(loan, aes(x = as.factor(LoanApproved), y = CheckingAccountBalance, fill = as.factor(LoanApproved))) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  theme_minimal() +
  labs(title = "CheckingAccountBalance vs Loan Approval",
       x = "Loan Approved (0 = No, 1 = Yes)",
       y = "CheckingAccountBalance",
       fill = "Loan Approved") +
  scale_fill_manual(values = c("#fdae61", "#2c7bb6"))

#DebtToIncomeRatio vs Loan Approval
ggplot(loan, aes(x = as.factor(LoanApproved), y = DebtToIncomeRatio, fill = as.factor(LoanApproved))) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  theme_minimal() +
  labs(title = "DebtToIncomeRatio vs Loan Approval",
       x = "Loan Approved (0 = No, 1 = Yes)",
       y = "DebtToIncomeRatio",
       fill = "Loan Approved") +
  scale_fill_manual(values = c("#fdae61", "#2c7bb6"))


#Loan Amount vs Loan Approval
ggplot(loan, aes(x = as.factor(LoanApproved), y = LoanAmount, fill = as.factor(LoanApproved))) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  theme_minimal() +
  labs(title = "Loan Amount vs Loan Approval",
       x = "Loan Approved (0 = No, 1 = Yes)",
       y = "Loan Amount (USD)",
       fill = "Loan Approved") +
  scale_fill_manual(values = c("#fdae61", "#2c7bb6"))


#Credit Score vs Loan Approval
ggplot(loan, aes(x = as.factor(LoanApproved), y = CreditScore, fill = as.factor(LoanApproved))) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  theme_minimal() +
  labs(title = "Credit Score vs Loan Approval",
       x = "Loan Approved (0 = No, 1 = Yes)",
       y = "Credit Score",
       fill = "Loan Approved") +
  scale_fill_manual(values = c("#fdae61", "#2c7bb6"))


#Debt-to-Income Ratio vs Loan Approval
ggplot(loan, aes(x = as.factor(LoanApproved), y = DebtToIncomeRatio, fill = as.factor(LoanApproved))) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  theme_minimal() +
  labs(title = "Debt-to-Income Ratio vs Loan Approval",
       x = "Loan Approved (0 = No, 1 = Yes)",
       y = "Debt-to-Income Ratio",
       fill = "Loan Approved") +
  scale_fill_manual(values = c("#fdae61", "#2c7bb6"))



# -----------------------------------------------------


# Correlation 

# Select only numeric columns
num_vars_cor <- loan %>% select_if(is.numeric)
str(num_vars_cor) # check if all selected columns are numeric

cor_matrix <- round(cor(num_vars_cor),2)
cor_matrix

# Interpretation:
# value = 1  → variable correlated with itself
# values close to +1 → strong positive relationship (both variables increase together)
# values close to -1 → strong negative relationship (one increases while the other decreases)
# values near 0 → no linear relationship


#Full Correlation Matrix of Numeric Variables
ggcorrplot(cor_matrix,
           method = "square",       
           type = "full",           
           lab = FALSE,              
           lab_size = 2.5,
           colors = c("#d73027", "white", "#1a9850"),  # czerwony → biały → zielony
           title = "Full Correlation Matrix of Numeric Variables",
           ggtheme = theme_minimal(base_size = 12)) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8)
  )

library(reshape2)

# Convert matrix to long format
cor_long <- melt(cor_matrix)
cor_long <- cor_long[abs(cor_long$value) > 0.4 & cor_long$Var1 != cor_long$Var2, ]

# Plot of strong correlations only (>|0.4|)
ggplot(cor_long, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), size = 3) +
  scale_fill_gradient2(low = "#d73027", high = "#1a9850", mid = "white", midpoint = 0) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8)) +
  labs(title = "Strong Correlations Between Variables (>|0.4|)",
       fill = "Correlation")

# Plot of correlations only (>|0.2|)
cor_long <- melt(cor_matrix)
cor_long_2 <- cor_long[abs(cor_long$value) > 0.2 & cor_long$Var1 != cor_long$Var2, ]
ggplot(cor_long, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  #geom_text(aes(label = round(value, 2)), size = 3) +
  scale_fill_gradient2(low = "#d73027", high = "#1a9850", mid = "white", midpoint = 0) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8)) +
  labs(title = "Correlations Between Variables (>|0.2|)",
       fill = "Correlation")


#  Correlation of each variable with RiskScore
risk_corr <- sort(cor_matrix[, "RiskScore"], decreasing = TRUE)
risk_df <- data.frame(Variable = names(risk_corr), Correlation = risk_corr)

ggplot(risk_df, aes(x = reorder(Variable, Correlation), y = Correlation, fill = Correlation)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradient2(low = "#d73027", mid = "white", high = "#1a9850", midpoint = 0) +
  theme_minimal(base_size = 12) +
  labs(title = "Correlation of Variables with RiskScore",
       x = "Variable", y = "Correlation")

#  Correlation of each variable with LoanApproved 
loan_corr <- sort(cor_matrix[, "LoanApproved"], decreasing = TRUE)
loan_df <- data.frame(Variable = names(loan_corr), Correlation = loan_corr)

ggplot(loan_df, aes(x = reorder(Variable, Correlation), 
                    y = Correlation, fill = Correlation)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradient2(low = "#d73027", mid = "white", high = "#1a9850", midpoint = 0) +
  theme_minimal(base_size = 12) +
  labs(title = "Correlation of Variables with LoanApproved (0 = No, 1 = Yes)",
       x = "Variable",
       y = "Correlation") +
  theme(plot.title = element_text(hjust = 0.5))



#Split large correlation matrix into more readable parts

subset1 <- num_vars_cor[, 1:10]
subset2 <- num_vars_cor[, 11:20]
subset3 <- num_vars_cor[, 21:30]

ggcorrplot(round(cor(subset1), 2),
           lab = TRUE, type = "lower", title = "Correlations – subset 1")
ggcorrplot(round(cor(subset2), 2),
           lab = TRUE, type = "lower", title = "Correlations – subset 2")
ggcorrplot(round(cor(subset3), 2),
           lab = TRUE, type = "lower", title = "Correlations – subset 3")

#threshold - discernment; ROC analysis can be helpful in finding the threshold, if one exists

ggplot(loan, aes(x = RiskScore, y = LoanApproved)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "blue") +
  theme_minimal() +
  labs(
    x = "Risk Score",
    y = "LoanApproved")

max_approved <- loan %>%
  filter(LoanApproved == 1) %>%
  summarise(max_risk_approved = max(RiskScore, na.rm = TRUE))

min_rejected <- loan %>%
  filter(LoanApproved == 0) %>%
  summarise(min_risk_rejected = min(RiskScore, na.rm = TRUE))

max_approved
min_rejected


ggplot(loan, aes(x = as.factor(LoanApproved), y = RiskScore, fill = as.factor(LoanApproved))) +
  geom_boxplot(alpha = 0.6, width = 0.6, outlier.color = "black") +
  theme_minimal() +
  labs(
    title = "Risk Score vs Loan Approval",
    x = "Loan Approved (0 = No, 1 = Yes)",
    y = "Risk Score",
    fill = "Loan Approved"
  ) +
  scale_fill_manual(values = c("#fdae61", "#2c7bb6"))

loan %>%
  group_by(LoanApproved) %>%
  summarise(
    count = n(),
    mean_RiskScore = mean(RiskScore, na.rm = TRUE),
    median_RiskScore = median(RiskScore, na.rm = TRUE),
    sd_RiskScore = sd(RiskScore, na.rm = TRUE),
    min_RiskScore = min(RiskScore, na.rm = TRUE),
    max_RiskScore = max(RiskScore, na.rm = TRUE)
  ) %>%
  as.data.frame()

#Compare income levels between approved and rejected applicants

# Boxplot
ggplot(loan, aes(x = as.factor(LoanApproved), y = AnnualIncome, fill = as.factor(LoanApproved))) +
  geom_boxplot(alpha = 0.6, width = 0.6) +
  theme_minimal() +
  labs(
    title = "Annual Income vs Loan Approval",
    x = "Loan Approved (0 = No, 1 = Yes)",
    y = "Annual Income (USD)"
  ) +
  scale_fill_manual(values = c("#fdae61", "#2c7bb6"))

# Histogram: income distribution by loan approval
ggplot(loan, aes(x = AnnualIncome, fill = as.factor(LoanApproved))) +
  geom_histogram(bins = 30, alpha = 0.5, position = "identity") +
  theme_minimal() +
  labs(
    title = "Distribution of Annual Income by Loan Approval",
    x = "Annual Income",
    y = "Count",
    fill = "Loan Approved"
  )

#Check if high-income applicants can still be rejected (if they have a poor credit history)
ggplot(loan, aes(x = AnnualIncome, y = CreditScore, color = as.factor(LoanApproved))) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(
    title = "Credit Score vs Income by Loan Approval",
    x = "Annual Income",
    y = "Credit Score",
    color = "Loan Approved"
  )



###????
# Create income categories
loan <- loan %>%
  mutate(IncomeLevel = case_when(
    AnnualIncome < quantile(AnnualIncome, 0.33, na.rm = TRUE) ~ "Low",
    AnnualIncome < quantile(AnnualIncome, 0.66, na.rm = TRUE) ~ "Medium",
    TRUE ~ "High"
  ))

# Plot interaction effect
ggplot(loan, aes(x = NumberOfDependents, y = RiskScore, color = IncomeLevel)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(
    title = "Interaction: Income Level × Number of Dependents → Risk Score",
    x = "Number of Dependents",
    y = "Risk Score",
    color = "Income Level"
  )

#Is there any seasonality in when people were granted loans and when they were not?
library(scales)

loan %>%
  mutate(
    Year = year(ApplicationDate),
    DecadeGroup = case_when(
      Year >= 2018 & Year <= 2020 ~ "2018–2020",
      Year >= 2021 & Year <= 2030 ~ "2021–2030",
      Year >= 2031 & Year <= 2040 ~ "2031–2040",
      Year >= 2041 & Year <= 2050 ~ "2041–2050",
      Year >= 2051 & Year <= 2060 ~ "2051–2060",
      TRUE ~ "2061–2072"
    ),
    m_num  = month(ApplicationDate),                                 
    Month  = factor(m_num, levels = 1:12, labels = month.abb)         
  ) %>%
  group_by(DecadeGroup, Month) %>%
  summarise(ApprovalRate = mean(LoanApproved == 1, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = Month, y = ApprovalRate, group = 1)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~ DecadeGroup, ncol = 3) +
  scale_x_discrete(drop = FALSE) +                                    
  scale_y_continuous(labels = percent_format()) +
  theme_minimal() +
  labs(
    title = "Seasonality of lending – breakdown by decade",
    x = "Month",
    y = "Percentage of loans granted"
  )
#seasonality is moderately visible

approval_by_month <- loan %>%
  mutate(
    MonthNum = month(ApplicationDate),
    Month = factor(MonthNum, levels = 1:12, labels = month.abb)
  ) %>%
  group_by(Month) %>%
  summarise(
    AvgApprovalRate = mean(LoanApproved == 1, na.rm = TRUE)
  )

approval_by_month
####################################
#################################

#LoanAmount vs RiskScore
ggplot(loan, aes(x = LoanAmount, y = RiskScore)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(title = "Loan Amount vs RiskScore",
       x = "Loan Amount",
       y = "RiskScore")

#InterestRate vs RiskScore
 ggplot(loan, aes(x = InterestRate, y = RiskScore)) +
   geom_point(alpha = 0.6) +
   geom_smooth(method = "lm") +
   theme_minimal() +
   labs(title = "InterestRate vs RiskScore",
        x = "InterestRate",
        y = "RiskScore")

# Logistic Regression 

# Visualization of results:
# Preparation of predictive data
 newdata <- expand.grid(
   LoanAmount = seq(min(loan$LoanAmount), max(loan$LoanAmount), length.out = 100),
   EmploymentStatus = c("Employed", "Unemployed")
 )
 
# Probability prediction
 newdata$PredictedProb <- predict(model_full, newdata, type = "response")
 
 ggplot(newdata, aes(x = LoanAmount, y = PredictedProb, color = EmploymentStatus)) +
   geom_line(size = 1.2) +
   labs(title = "Predicted Probability of Loan Approval",
        x = "Loan Amount",
        y = "Probability",
        color = "Employment Status") +
   theme_minimal()