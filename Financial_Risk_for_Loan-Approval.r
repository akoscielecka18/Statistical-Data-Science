#______________________________Financial Risk for Loan Approval_________________

# Loading packages
library(tidyverse)
library(DataExplorer)
library(skimr) # more advanced than function summary()
library(dplyr)
library(ggcorrplot)
install.packages("segmented")
library(segmented)
library(tidyr)
library(scales) 
library(ggplot2)
library(gridExtra)
install.packages("psych")  
library(psych)
install.packages("ppcor")  
library(ppcor)


# Loading data
loan <- read.csv("C:/Users/akosc/OneDrive/Pulpit/Ania/studia/statistical data science/Loan.csv")
head(loan,5)

# Missing values
colSums(is.na(loan)) 

# Exploratory Data Analysis (EDA)
str(loan)
summary(loan)
 
# Numeric and categorical variables
num_vars <- loan %>% select(where(is.numeric)) %>% names()
cat_vars <- loan %>% select(where(is.character)) %>% names()
num_vars
cat_vars

# Character into factor
loan <- loan %>%
  mutate(across(c(EmploymentStatus, EducationLevel, MaritalStatus, HomeOwnershipStatus, LoanPurpose), as.factor))
str(loan)

# Histograms for continuous variables
loan %>%
  select(all_of(num_vars)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = "#2c7fb8", color = "white") +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Distribution of continuous variables",
    x = NULL,
    y = "Number of observations"
  ) +
  theme(
    strip.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5),
    axis.text.y = element_blank()
  )

# Boxplots for continuous variables
plots_per_page <- 6
num_var_groups <- split(num_vars, ceiling(seq_along(num_vars) / plots_per_page))

for (vars in num_var_groups) {
  p <- loan %>%
    select(all_of(vars)) %>%
    pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
    ggplot(aes(x = "", y = value)) +
    geom_boxplot(fill = "#2c7fb8", outlier.color = "black", outlier.shape = 1, alpha = 0.6) +
    facet_wrap(~ variable, scales = "free", ncol = 3) +
    theme_minimal(base_size = 12) +
    labs(
      title = "Boxplots for continuous variables",
      x = NULL,
      y = "Value"
    ) +
    theme(
      legend.position = "none",
      strip.text = element_text(face = "bold"),
      plot.title = element_text(hjust = 0.5)
    )
  print(p)
}

# Class balance check for binary outcome (for classification)
table(loan$LoanApproved) 
prop.table(table(loan$LoanApproved))

barplot(table(loan$LoanApproved),
        main = "Loan Approved",
        xlab = "Loan Approved",
        col = c("skyblue", "orange"))

# Class balance check for continuous variables
prop.table(table(loan$EmploymentStatus))
prop.table(table(loan$EducationLevel))
prop.table(table(loan$MaritalStatus))
prop.table(table(loan$HomeOwnershipStatus))
prop.table(table(loan$LoanPurpose))

#Chapter 3 
#Analysis taking into account the division by level of education (Interest Rate vs Risk Score)

table(loan$EducationLevel)
ggplot(loan, aes(x = EducationLevel, fill = EducationLevel)) +
  geom_bar() +
  labs(title = "Count of Education Levels",
       x = "Education Level",
       y = "Count") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "right"
  )
par(mar = c(6, 5, 4, 2) + 0.1)
op <- par(mar = c(7, 4, 4, 2) + 0.1)

bp <- boxplot(RiskScore ~ EducationLevel,
              data = loan,
              col = "BlueViolet",
              border = "black",
              main = "Risk Score by Education Level",
              xlab = "Education Level",
              ylab = "Risk Score",
              ylim = c(0, 90),
              xaxt = "n")  

# Angled labels 45°
text(x = seq_along(bp$names),
     y = par("usr")[3] - 0.03 * diff(par("usr")[3:4]),  
     labels = bp$names,
     srt = 45,             
     adj = 1,               
     xpd = NA,             
     cex = 0.9)             

par(op)

mediany_risk <- loan %>%
  mutate(RiskScore = as.numeric(RiskScore)) %>%
  group_by(EducationLevel) %>%
  summarise(Mediana_RiskScore = round(median(RiskScore, na.rm = TRUE), 5)) %>%
  arrange(EducationLevel)

mediany_risk

dane_plot <- loan %>%
  mutate(
    LoanApproved = case_when(
      LoanApproved %in% c(1, "1", TRUE, "TRUE", "Yes", "YES", "Approved", "APPROVED") ~ "Approved",
      LoanApproved %in% c(0, "0", FALSE, "FALSE", "No", "NO", "Not Approved", "DECLINED", "Rejected") ~ "Rejected",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(LoanApproved), !is.na(EducationLevel)) %>%
  count(EducationLevel, LoanApproved, name = "Liczba") %>%
  complete(EducationLevel, LoanApproved, fill = list(Liczba = 0))

ggplot(dane_plot, aes(x = EducationLevel, y = Liczba, fill = LoanApproved)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  labs(
    title = "Loan approval rate by education level",
    x = "Education level",
    y = "Number of Loans",
    fill = "Loan status"
  ) +
  scale_fill_manual(values = c("Approved" = "BlueViolet", "Rejected" = "Plum"))+
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

loan_box <- loan %>%
  mutate(
    EduGroup = case_when(
      EducationLevel %in% c("Bachelor", "Master", "Doctorate") ~ "Higher",
      EducationLevel %in% c("High School", "Associate") ~ "Lower",
      TRUE ~ NA_character_
    ),
    RiskScore = as.numeric(RiskScore)
  ) %>%
  filter(!is.na(EduGroup), !is.na(RiskScore)) %>%
  # set order: Lower, Higher
  mutate(EduGroup = factor(EduGroup, levels = c("Lower", "Higher")))

par(mar = c(5, 5, 4, 2) + 0.1)
boxplot(
  RiskScore ~ EduGroup,
  data = loan_box,
  col = adjustcolor("BlueViolet", alpha.f = 0.6),
  border = "gray30",
  main = "Risk Score vs Education level",
  xlab = "Education level",
  ylim = c(0, 90),
  ylab = "Risk Score",
  outline = TRUE
)

#Mean Risk Scores for education levels/overall
loan_means <- loan %>%
  mutate(
    EduGroup = case_when(
      EducationLevel %in% c("Bachelor", "Master", "Doctorate") ~ "Higher",
      EducationLevel %in% c("High School", "Associate") ~ "Lower",
      TRUE ~ NA_character_
    ),
    RiskScore = as.numeric(RiskScore)
  ) %>%
  filter(!is.na(EduGroup), !is.na(RiskScore))

# Calculation of averages
mu_lower  <- mean(loan_means$RiskScore[loan_means$EduGroup == "Lower"],  na.rm = TRUE)
mu_higher <- mean(loan_means$RiskScore[loan_means$EduGroup == "Higher"], na.rm = TRUE)
mu_total  <- mean(loan_means$RiskScore, na.rm = TRUE)
mu_lower
mu_higher
mu_total


#FULL MODEL - hypothesis testing 
loan_box <- loan %>%
  mutate(
    EduGroup = case_when(
      EducationLevel %in% c("Bachelor", "Master", "Doctorate") ~ "Higher",
      EducationLevel %in% c("High School", "Associate") ~ "Lower",
      TRUE ~ NA_character_
    ),
    RiskScore = as.numeric(RiskScore)
  ) %>%
  filter(!is.na(EduGroup), !is.na(RiskScore)) %>%
  mutate(EduGroup = factor(EduGroup, levels = c("Lower", "Higher")))

# Full model — two groups treated as separate categories
full_model <- lm(RiskScore ~ EduGroup, data = loan_box)
summary(full_model)

# REDUCED MODEL: one common mean (no group effect)
reduced_model <- lm(RiskScore ~ 1, data = loan_box)
summary(reduced_model)

#F-statistic model

m0 <- lm(RiskScore ~ 1,        data = loan_box)   # Model 0 (pooled)
m1 <- lm(RiskScore ~ EduGroup, data = loan_box)   # Model 1 (separate)

# Components for F-statistics
SSE0 <- sum(residuals(m0)^2)           
SSE1 <- sum(residuals(m1)^2)           
df0  <- df.residual(m0)
df1  <- df.residual(m1)

num_df <- df0 - df1                    
den_df <- df1                           

F_obs  <- ((SSE0 - SSE1) / num_df) / (SSE1 / den_df)
F_crit <- qf(0.95, df1 = num_df, df2 = den_df)    # α = 0.05
p_val  <- pf(F_obs, df1 = num_df, df2 = den_df, lower.tail = FALSE)
F_obs
F_crit


#t-test
x0 <- loan_box %>% filter(EduGroup == "Lower")  %>% pull(RiskScore)
x1 <- loan_box %>% filter(EduGroup == "Higher") %>% pull(RiskScore)

n0 <- length(x0); n1 <- length(x1)
xbar0 <- mean(x0); xbar1 <- mean(x1)
var0  <- var(x0);  var1  <- var(x1)

# Pooled variance s^2 = w1*Var1 + w0*Var0
w1 <- (n1 - 1) / (n0 + n1 - 2)
w0 <- (n0 - 1) / (n0 + n1 - 2)
s2 <- w1 * var1 + w0 * var0

# t-statistic (two-tailed, equal variances)
t_obs <- (xbar1 - xbar0) / sqrt(s2 * (1/n1 + 1/n0))
df    <- n0 + n1 - 2
t_left  <- qt(0.025, df = df)   
t_right <- qt(0.975, df = df)   
p_val   <- 2 * pt(abs(t_obs), df = df, lower.tail = FALSE)
t_obs
t_left
t_right


# Data preparation: 2 groups Employment
loan_emp_box <- loan %>%
  mutate(
    EmpGroup = case_when(
      EmploymentStatus %in% c("Employed", "Self-Employed") ~ "Employed",
      EmploymentStatus == "Unemployed"                     ~ "Unemployed",
      TRUE ~ NA_character_
    ),
    RiskScore = as.numeric(RiskScore)
  ) %>%
  filter(!is.na(EmpGroup), !is.na(RiskScore)) %>%
  mutate(EmpGroup = factor(EmpGroup, levels = c("Employed", "Unemployed")))

# Boxplot: RiskScore ~ EmpGroup 
par(mar = c(5, 5, 4, 2) + 0.1)
boxplot(
  RiskScore ~ EmpGroup,
  data   = loan_emp_box,
  col    = adjustcolor("BlueViolet", alpha.f = 0.6),
  border = "gray30",
  main   = "Risk Score vs Employment status",
  xlab   = "Employment status",
  ylab   = "Risk Score",
  ylim   = c(0, 90),
  outline = TRUE
)

loan_emp_means <- loan %>%
  mutate(
    EmpGroup = case_when(
      EmploymentStatus %in% c("Employed", "Self-Employed") ~ "Employed",
      EmploymentStatus == "Unemployed"                     ~ "Unemployed",
      TRUE ~ NA_character_
    ),
    RiskScore = as.numeric(RiskScore)
  ) %>%
  filter(!is.na(EmpGroup), !is.na(RiskScore))


mu_employed   <- mean(loan_emp_means$RiskScore[loan_emp_means$EmpGroup == "Employed"],
                      na.rm = TRUE)
mu_unemployed <- mean(loan_emp_means$RiskScore[loan_emp_means$EmpGroup == "Unemployed"],
                      na.rm = TRUE)

#Overall mean (both groups together)
mu_total_emp  <- mean(loan_emp_means$RiskScore, na.rm = TRUE)

mu_employed
mu_unemployed
mu_total_emp

#Risk Score vs Anuall Income (education level)

loan_plot <- loan %>%
  mutate(
    EduGroup = case_when(
      EducationLevel %in% c("Bachelor", "Master", "Doctorate") ~ "Higher",
      EducationLevel %in% c("High School", "Associate") ~ "Lower",
      TRUE ~ NA_character_
    ),
    RiskScore = as.numeric(RiskScore),
    AnnualIncome = as.numeric(AnnualIncome)
  ) %>%
  filter(!is.na(EduGroup), !is.na(RiskScore), !is.na(AnnualIncome))

# Scatter plot
ggplot(loan_plot, aes(x = AnnualIncome, y = RiskScore, color = EduGroup)) +
  geom_point(alpha = 0.6, size = 2) +
  labs(
    title = "Risk Score vs Annual Income (Education Level)",
    x = "Annual Income",
    y = "Risk Score",
    color = "Education Level"
  ) +
  scale_color_manual(values = c("Lower" = "BlueViolet", "Higher" = "Plum")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right"
  )
  
#Select the best tau 
#Fit a simple linear model
base_model <- lm(RiskScore ~ AnnualIncome, data = loan)
summary(base_model) 

#Fit the model with one breakpoint (1 breakpoint)
seg_model <- segmented(base_model, seg.Z = ~ AnnualIncome, npsi = 1)
summary(seg_model)$psi

#the results 
tau_hat <- as.numeric(summary(seg_model)$psi[,"Est."])
loan$hinge <- pmax(0, loan$AnnualIncome - tau_hat)

lm_fixed <- lm(RiskScore ~ AnnualIncome + hinge, data = loan)
summary(lm_fixed)

#piecewise regression model 
x_seq <- seq(min(loan$AnnualIncome, na.rm=TRUE),
             max(loan$AnnualIncome, na.rm=TRUE), length.out = 300)

df_lines <- data.frame(
  AnnualIncome = x_seq,
  linear   = predict(base_model, newdata = data.frame(AnnualIncome = x_seq)),
  piecewise= predict(seg_model,  newdata = data.frame(AnnualIncome = x_seq))
)
df_long <- pivot_longer(df_lines, cols = c(linear, piecewise),
                        names_to = "Model", values_to = "Pred")

ggplot(loan, aes(AnnualIncome, RiskScore)) +
  geom_point(color = "grey40") +
  geom_line(data = df_long,
            aes(y = Pred, color = Model, linetype = Model),
            linewidth = 1) +
  geom_vline(xintercept = tau_hat, color = "blue", linetype = "dotted") +
  scale_color_manual(values = c("piecewise" = "blue", "linear" = "red"),
                     labels = c("Piecewise Regression", "Linear Regression")) +
  scale_linetype_manual(values = c("piecewise" = "solid", "linear" = "dashed"),
                        labels = c("Piecewise Regression", "Linear Regression")) +
  labs(title = "Risk Score vs. Annual Income",
       x = "Annual Income",
       y = "Risk Score",
       color = "Model",
       linetype = "Model") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank())

ggplot(loan, aes(AnnualIncome, RiskScore)) +
  geom_point(color = "grey40") +
  geom_line(
    data = df_long,
    aes(y = Pred, color = Model, linetype = Model),
    linewidth = 1
  ) +
  geom_vline(xintercept = tau_hat, color = "blue", linetype = "dotted") +
  scale_color_manual(
    values = c(
      "piecewise" = "red",
      "linear"   = "blue"
    ),
    labels = c(
      "piecewise" = "Piecewise Regression",
      "linear"    = "Linear Regression"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "piecewise" = "dashed",
      "linear"   = "solid"
    ),
    labels = c(
      "piecewise" = "Piecewise Regression",
      "linear"    = "Linear Regression"
    )
  ) +
  labs(
    title   = "Risk Score vs. Annual Income",
    x       = "Annual Income",
    y       = "Risk Score",
    color   = "Model",
    linetype = "Model"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.title = element_blank()
  )


#calculation of SSE for both models 
SSE0 <- sum(residuals(base_model)^2)       
SSE1 <- sum(residuals(seg_model)^2)        
df0  <- df.residual(base_model)            
df1  <- df.residual(seg_model)             
F_stat <- ((SSE0 - SSE1) / (df0 - df1)) / (SSE1 / df1)
F_stat

F_crit <- qf(0.95, df0 - df1, df1)
F_crit

#summary with spline

#-----------------------------------------------------  
#RiskScore vs InterestRate
ggplot(loan, aes(x = RiskScore, y = InterestRate)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm") +
  theme_minimal()

loan$EducationCategory <- ifelse(loan$EducationLevel %in% c("Bachelor", "Master", "Doctorate"),
                                 "Higher", "Lower")

#Different trends
#InterestRate vs RiskScore with division on EducationLevel 
ggplot(loan, aes(x = RiskScore, y = InterestRate, color = EducationCategory)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(
    title = "Interest Rate vs Risk Score",
    color = "Education Level"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

#Only trend lines
ggplot(loan, aes(x = RiskScore, y = InterestRate, color = EducationCategory)) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
  theme_minimal() +
  labs(
    title = "Interest Rate vs Risk Score",
    color = "Education Level"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

#Common trend
ggplot(loan, aes(x = RiskScore, y = InterestRate)) +
  geom_point(alpha = 0.6, aes(color = EducationCategory)) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 1.2) +
  theme_minimal() +
  labs(
    title = "Interest Rate vs Risk Score (Common Trend)",
    color = "Education Level"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Calculations and statistics:
# Simplified model: common trend
model_common <- lm(InterestRate ~ RiskScore + EducationCategory, data = loan)

# Full model: separate trends (interaction)
model_full <- lm(InterestRate ~ RiskScore * EducationCategory, data = loan)

# Comparison of models using F-statistic:
anova(model_common, model_full)

# Determining the critical value (threshold):
df1 <- anova(model_common, model_full)$Df[2]
df2 <- anova(model_common, model_full)$Res.Df[2]
qf(0.95, df1, df2)

#Additional calculations to the above:
# Filter data for the group with lower education
lower_group <- loan[loan$EducationCategory == "Lower", ]

# Find the minimum and maximum RiskScore in this group.
min_risk <- min(lower_group$RiskScore, na.rm = TRUE)
max_risk <- max(lower_group$RiskScore, na.rm = TRUE)

# Calculate the average InterestRate for each group with these RiskScore values.
mean_lower_min <- mean(lower_group$InterestRate[lower_group$RiskScore == min_risk])
mean_higher_min <- mean(loan$InterestRate[loan$EducationCategory == "Higher" & loan$RiskScore == min_risk])
 
mean_lower_max <- mean(lower_group$InterestRate[lower_group$RiskScore == 78])    
mean_higher_max <- mean(loan$InterestRate[loan$EducationCategory == "Higher" & loan$RiskScore == 78])  

# Differences between groups
diff_min <- mean_lower_min - mean_higher_min
diff_max <- mean_lower_max - mean_higher_max

diff_min
diff_max

#Chapter 4
#EMPLOYMENT STATUS

table(loan$EmploymentStatus)

ggplot(loan, aes(x = EmploymentStatus, fill = EmploymentStatus)) +
  geom_bar() +
  labs(
    title = "Count of Employment Status",
    x = "Employment Status",
    y = "Count"
  ) +
  scale_fill_manual(
    values = c(
      "Employed" = "BlueViolet",
      "Self-Employed" = "Plum",
      "Unemployed" = "orchid"
    )
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "right"
  )

# Boxplot RiskScore ~ EmploymentStatus 

par(mar = c(6, 5, 4, 2) + 0.1)
op <- par(mar = c(7, 4, 4, 2) + 0.1)

bp_emp <- boxplot(
  RiskScore ~ EmploymentStatus,
  data  = loan,
  col   = "BlueViolet",
  border = "black",
  main  = "Risk Score by Employment Status",
  xlab  = "Employment Status",
  ylab  = "Risk Score",
  ylim  = c(0, 90),
  xaxt  = "n"   
)

text(
  x = seq_along(bp_emp$names),
  y = par("usr")[3] - 0.03 * diff(par("usr")[3:4]),
  labels = bp_emp$names,
  srt = 25,
  adj = 1,
  xpd = NA,
  cex = 0.9
)

par(op)

# RiskScore medians by EmploymentStatus

mediany_risk_emp <- loan %>%
  mutate(RiskScore = as.numeric(RiskScore)) %>%
  group_by(EmploymentStatus) %>%
  summarise(
    Mediana_RiskScore = round(median(RiskScore, na.rm = TRUE), 5)
  ) %>%
  arrange(EmploymentStatus)

mediany_risk_emp

# LoanApproved vs EmploymentStatus (Approved / Rejected)

dane_plot_emp <- loan %>%
  mutate(
    LoanApproved = case_when(
      LoanApproved %in% c(1, "1", TRUE, "TRUE", "Yes", "YES",
                          "Approved", "APPROVED") ~ "Approved",
      LoanApproved %in% c(0, "0", FALSE, "FALSE", "No", "NO",
                          "Not Approved", "DECLINED", "Rejected") ~ "Rejected",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(LoanApproved), !is.na(EmploymentStatus)) %>%
  count(EmploymentStatus, LoanApproved, name = "Liczba") %>%
  complete(EmploymentStatus, LoanApproved, fill = list(Liczba = 0))

ggplot(dane_plot_emp,
       aes(x = EmploymentStatus, y = Liczba, fill = LoanApproved)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  labs(
    title = "Loan approval rate by employment status",
    x = "Employment status",
    y = "Number of Loans",
    fill = "Loan status"
  ) +
  scale_fill_manual(values = c("Approved" = "BlueViolet",
                               "Rejected" = "Plum")) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Pie charts broken down by employment status 
dane_pie <- dane_plot_emp %>%
  group_by(EmploymentStatus) %>%
  mutate(
    Procent = Liczba / sum(Liczba),
    etykieta = percent(Procent, accuracy = 1)
  ) %>%
  ungroup()

ggplot(dane_pie,
       aes(x = "", y = Procent, fill = LoanApproved)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  facet_wrap(~ EmploymentStatus) +
  scale_fill_manual(values = c("Approved" = "BlueViolet",
                               "Rejected" = "Plum")) +
  geom_text(aes(label = etykieta),
            position = position_stack(vjust = 0.5),
            size = 4) +
  labs(
    fill = "Loan status"
  ) +
  theme_void(base_size = 14)

# Two groups Employment 
loan_emp_box <- loan %>%
  mutate(
    EmpGroup = case_when(
      EmploymentStatus %in% c("Employed", "Self-Employed") ~ "Employed",
      EmploymentStatus == "Unemployed"                     ~ "Unemployed",
      TRUE ~ NA_character_
    ),
    RiskScore = as.numeric(RiskScore)
  ) %>%
  filter(!is.na(EmpGroup), !is.na(RiskScore)) %>%
  mutate(EmpGroup = factor(EmpGroup, levels = c("Employed", "Unemployed")))

# Boxplot: RiskScore ~ EmpGroup 
par(mar = c(5, 5, 4, 2) + 0.1)
boxplot(
  RiskScore ~ EmpGroup,
  data   = loan_emp_box,
  col    = adjustcolor("BlueViolet", alpha.f = 0.6),
  border = "gray30",
  main   = "Risk Score vs Employment status",
  xlab   = "Employment status",
  ylab   = "Risk Score",
  ylim   = c(0, 90),
  outline = TRUE
)

loan_emp_means <- loan %>%
  mutate(
    EmpGroup = case_when(
      EmploymentStatus %in% c("Employed", "Self-Employed") ~ "Employed",
      EmploymentStatus == "Unemployed"                     ~ "Unemployed",
      TRUE ~ NA_character_
    ),
    RiskScore = as.numeric(RiskScore)
  ) %>%
  filter(!is.na(EmpGroup), !is.na(RiskScore))


mu_employed   <- mean(loan_emp_means$RiskScore[loan_emp_means$EmpGroup == "Employed"],
                      na.rm = TRUE)
mu_unemployed <- mean(loan_emp_means$RiskScore[loan_emp_means$EmpGroup == "Unemployed"],
                      na.rm = TRUE)

# Overall mean (both groups together)
mu_total_emp  <- mean(loan_emp_means$RiskScore, na.rm = TRUE)

mu_employed
mu_unemployed
mu_total_emp

loan_emp_box <- loan %>%
  mutate(
    EmpGroup = case_when(
      EmploymentStatus %in% c("Employed", "Self-Employed") ~ "Employed",
      EmploymentStatus == "Unemployed"                     ~ "Unemployed",
      TRUE ~ NA_character_
    ),
    RiskScore = as.numeric(RiskScore)
  ) %>%
  filter(!is.na(EmpGroup), !is.na(RiskScore)) %>%
  mutate(EmpGroup = factor(EmpGroup, levels = c("Employed", "Unemployed")))

# Full Model 
full_model_emp <- lm(RiskScore ~ EmpGroup, data = loan_emp_box)
summary(full_model_emp)

reduced_model <- lm(RiskScore ~ 1, data = loan_emp_box)
summary(reduced_model)

m0_emp <- lm(RiskScore ~ 1,        data = loan_emp_box)   # Model 0 (pooled mean)
m1_emp <- lm(RiskScore ~ EmpGroup, data = loan_emp_box)   # Model 1 (separate means for groups)

# F-statistics
SSE0_emp <- sum(residuals(m0_emp)^2)
SSE1_emp <- sum(residuals(m1_emp)^2)
df0_emp  <- df.residual(m0_emp)
df1_emp  <- df.residual(m1_emp)

num_df_emp <- df0_emp - df1_emp      
den_df_emp <- df1_emp                

F_obs_emp  <- ((SSE0_emp - SSE1_emp) / num_df_emp) / (SSE1_emp / den_df_emp)
F_crit_emp <- qf(0.95, df1 = num_df_emp, df2 = den_df_emp)    # α = 0.05
p_val_emp  <- pf(F_obs_emp, df1 = num_df_emp, df2 = den_df_emp, lower.tail = FALSE)

F_obs_emp
F_crit_emp
p_val_emp

set.seed(123)

# The observed difference in means
d_obs <- with(loan_emp_box,
              mean(RiskScore[EmpGroup == "Employed"]) -
                mean(RiskScore[EmpGroup == "Unemployed"]))

d_obs

# Permutations
B <- 10000
d_perm <- numeric(B)

for (b in 1:B) {
  perm_group <- sample(loan_emp_box$EmpGroup)  
  d_perm[b] <- with(loan_emp_box,
                    mean(RiskScore[perm_group == "Employed"]) -
                      mean(RiskScore[perm_group == "Unemployed"]))
}

# Two-tailed p-value
p_perm <- mean(abs(d_perm) >= abs(d_obs))
p_perm

# percentiles (critical limits)
L <- quantile(d_perm, 0.025)
U <- quantile(d_perm, 0.975)

L; U

hist(d_perm,
     breaks = 30,
     main = "Histogram of d_perm",
     xlab  = "d_perm",
     col   = "grey80",
     border = "white")

# quantile 2.5% i 97.5%
abline(v = L, col = "red", lty = 2, lwd = 2)
abline(v = U, col = "red", lty = 2, lwd = 2)
points(L, 0, col = "red", pch = 16, cex = 1.5)
points(U, 0, col = "red", pch = 16, cex = 1.5)

# observed difference in means
abline(v = d_obs, col = "blue", lty = 2, lwd = 2)
points(d_obs, 0, col = "blue", pch = 16, cex = 1.5)


# Chapter 5
# Logistic Regression for Employment Status

## Pie Charts
loan$EmploymentStatus <- ifelse(
  loan$EmploymentStatus %in% c("Employed", "Self-Employed"),
  "Employed",
  "Unemployed"
)

# Checking a new variable
table(loan$EmploymentStatus)


# Division into groups according to EmploymentStatus
loan_summary <- loan %>%
  group_by(EmploymentStatus, LoanApproved) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(EmploymentStatus) %>%
  mutate(perc = round(100 * count / sum(count), 1),
         label = paste0(perc, "%"))

approval_colors <- c("1" = "#BA55D3",  # Pass (teal)
                     "0" = "#FF69B4")  # Fail (dark green)

# Chart for Employed
plot_employed <- loan_summary %>%
  filter(EmploymentStatus == "Employed") %>%
  ggplot(aes(x = "", y = perc, fill = factor(LoanApproved))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  scale_fill_manual(values = approval_colors,
                    labels = c("Fail", "Pass"),
                    name = "LoanApproved") +
  theme_void() +
  labs(title = "Loan Approval for Employed") +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            size = 4)

# Chart for Unemployed
plot_unemployed <- loan_summary %>%
  filter(EmploymentStatus == "Unemployed") %>%
  ggplot(aes(x = "", y = perc, fill = factor(LoanApproved))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  scale_fill_manual(values = approval_colors,
                    labels = c("Fail", "Pass"),
                    name = "LoanApproved") +
  theme_void() +
  labs(title = "Loan Approval for Unemployed") +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            size = 4)

grid.arrange(plot_employed, plot_unemployed, ncol = 2)


# Hypothesis Testing: Loan Amount Effect by Employment Status

# Full Model
model_full <- glm(LoanApproved ~ LoanAmount * EmploymentStatus,
                  data = loan, family = binomial)

# Simplified model
model_reduced <- glm(LoanApproved ~ LoanAmount + EmploymentStatus,
                     data = loan, family = binomial)

# Likelihood Ratio Test
anova(model_reduced, model_full, test = "Chisq")
summary(model_full)

# Chapter 6
# MARITAL STATUS

custom_colors <- c("Divorced" = "#4B0082",         
                   "Married" = "#9370DB",    
                   "Single" = "#FF69B4",       
                   "Widowed" = "#BA55D3") 

ggplot(loan, aes(x = MaritalStatus, fill = MaritalStatus)) +
  geom_bar() +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  labs(title = "Count of Marital Status",
       x = "Marital Status",
       y = "Count") +
  theme(plot.title = element_text(hjust = 0.5))

# box plot
bp <- boxplot(RiskScore ~ MaritalStatus,
              data = loan,
              col = "BlueViolet",
              border = "black",
              main = "Risk Score by Marital Status",
              xlab = "Marital Status",
              ylab = "Risk Score",
              ylim = c(0, 90),
              xaxt = "n")  

text(x = seq_along(bp$names),
     y = par("usr")[3] - 0.03 * diff(par("usr")[3:4]),  
     labels = bp$names,
     srt = 45,             
     adj = 1,               
     xpd = NA,             
     cex = 0.9) 

loan$MaritalStatus <- ifelse(loan$MaritalStatus %in% c("Married"),
                             "Married", "Not married")


custom_colors <- c("Married" = "#4B0082",       
                   "Not married" = "#FF69B4")   

# Histogram 
hist_plot <- ggplot(loan, aes(x = MaritalStatus, fill = MaritalStatus)) +
  geom_bar() +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  labs(title = "Marital Status Distribution Histogram",
       x = "Marital Status",
       y = "Count") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none",   
        plot.margin = margin(5, 30, 5, 5))

# Data for pie chart
pie_data <- loan %>%
  count(MaritalStatus) %>%
  mutate(perc = round(100 * n / sum(n), 1),
         label = paste0(perc, "%"))   

# Pie chart (with legend)
pie_plot <- ggplot(pie_data, aes(x = "", y = perc, fill = MaritalStatus)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  scale_fill_manual(values = custom_colors) +
  theme_void() +
  labs(title = "Marital Status Distribution") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "right", 
        plot.margin = margin(10, 50, 5, 5)) +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            size = 4)

grid.arrange(hist_plot, pie_plot, ncol = 2)
 
# box plot
bp <- boxplot(RiskScore ~ MaritalStatus,
              data = loan,
              col = "BlueViolet",
              border = "black",
              main = "Risk Score by Marital Status",
              xlab = "Marital Status",
              ylab = "Risk Score",
              ylim = c(0, 90),
              xaxt = "n")  

text(x = seq_along(bp$names),
     y = par("usr")[3] - 0.03 * diff(par("usr")[3:4]),  
     labels = bp$names,
     srt = 45,             
     adj = 1,               
     xpd = NA,             
     cex = 0.9) 


# Calculating averages
mean_married <- mean(loan$RiskScore[loan$MaritalStatus == "Married"], na.rm = TRUE)
mean_not_married <- mean(loan$RiskScore[loan$MaritalStatus == "Not married"], na.rm = TRUE)
mean_overall <- mean(loan$RiskScore, na.rm = TRUE)
mean_married
mean_not_married
mean_overall

## Violin plot
#Risk Score vs Marital Status
ggplot(loan, aes(x = as.factor(MaritalStatus), y = RiskScore, fill = as.factor(MaritalStatus))) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  theme_minimal() +
  labs(title = "Risk Score vs Marital Status",
       x = "Marital Status",
       y = "Risk Score",
       fill = "Loan Approved") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("#4B0082", "#FF69B4"))


# Hypothesis testing (equality of means)
# FULL MODEL 
loan_box <- loan %>%
  mutate(
    EduGroup = case_when(
      EducationLevel %in% c("Bachelor", "Master", "Doctorate") ~ "Higher",
      EducationLevel %in% c("High School", "Associate") ~ "Lower",
      TRUE ~ NA_character_
    ),
    RiskScore = as.numeric(RiskScore)
  ) %>%
  filter(!is.na(EduGroup), !is.na(RiskScore)) %>%
  mutate(EduGroup = factor(EduGroup, levels = c("Lower", "Higher")))

# FULL MODEL
full_model <- lm(RiskScore ~ MaritalStatus, data = loan)
summary(full_model)

# REDUCED MODEL
reduced_model <- lm(RiskScore ~ 1, data = loan)
summary(reduced_model)

# Statistical testing
# F-statistic model
m0 <- lm(RiskScore ~ 1,        data = loan_box)   # Model 0 (pooled)
m1 <- lm(RiskScore ~ MaritalStatus, data = loan_box)   # Model 1 (separate)

# F-statistics
SSE0 <- sum(residuals(m0)^2)           
SSE1 <- sum(residuals(m1)^2)           
df0  <- df.residual(m0)
df1  <- df.residual(m1)

num_df <- df0 - df1                    
den_df <- df1                           

F_obs  <- ((SSE0 - SSE1) / num_df) / (SSE1 / den_df)
F_crit <- qf(0.95, df1 = num_df, df2 = den_df)    # α = 0.05
p_val  <- pf(F_obs, df1 = num_df, df2 = den_df, lower.tail = FALSE)
F_obs
F_crit

#t-test
x0 <- loan_box %>% filter(MaritalStatus == "Married")  %>% pull(RiskScore)
x1 <- loan_box %>% filter(MaritalStatus == "Not married") %>% pull(RiskScore)

n0 <- length(x0); n1 <- length(x1)
xbar0 <- mean(x0); xbar1 <- mean(x1)
var0  <- var(x0);  var1  <- var(x1)

# Pooled variance s^2 = w1*Var1 + w0*Var0
w1 <- (n1 - 1) / (n0 + n1 - 2)
w0 <- (n0 - 1) / (n0 + n1 - 2)
s2 <- w1 * var1 + w0 * var0

# T -statistic (two-tailed, equal variances)
t_obs <- (xbar1 - xbar0) / sqrt(s2 * (1/n1 + 1/n0))
df    <- n0 + n1 - 2
t_left  <- qt(0.025, df = df)   
t_right <- qt(0.975, df = df)   
p_val   <- 2 * pt(abs(t_obs), df = df, lower.tail = FALSE)
t_obs
t_left
t_right

#RiskScore vs AnnualIncome with MaritalStatus
#Different trends
#InterestRate vs RiskScore with division on EducationLevel 
ggplot(loan, aes(x = AnnualIncome, y = RiskScore, color = MaritalStatus)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(
    title = "Annaul Income vs Risk Score",
    color = "Marital Status"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Only trend lines
ggplot(loan, aes(x = AnnualIncome, y = RiskScore, color = MaritalStatus)) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
  theme_minimal() +
  labs(
    title = "Annual Income vs Risk Score",
    color = "Marital Status"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Common trend
ggplot(loan, aes(x = AnnualIncome, y = RiskScore)) +
  geom_point(alpha = 0.6, aes(color = MaritalStatus)) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 1.2) +
  theme_minimal() +
  labs(
    title = "Annual Income vs Risk Score (Common Trend)",
    color = "Marital Statsu"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

model_common <- lm(AnnualIncome ~ RiskScore + MaritalStatus, data = loan)

# Full Model
model_full <- lm(AnnualIncome ~ RiskScore * MaritalStatus, data = loan)

# Comparison of models using F-statistic:
anova(model_common, model_full)

# Treshold
df1 <- anova(model_common, model_full)$Df[2]
df2 <- anova(model_common, model_full)$Res.Df[2]
qf(0.95, df1, df2) 


# Chapter 7
# Logistic Regression Marital Status 

loan_summary <- loan %>%
  group_by(MaritalStatus, LoanApproved) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(MaritalStatus) %>%
  mutate(perc = round(100 * count / sum(count), 1),
         label = paste0(perc, "%"))

approval_colors <- c("1" = "#BA55D3",  # Pass (teal)
                     "0" = "#FF69B4")  # Fail (dark green)

# Chart for Married
plot_married <- loan_summary %>%
  filter(MaritalStatus == "Married") %>%
  ggplot(aes(x = "", y = perc, fill = factor(LoanApproved))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  scale_fill_manual(values = approval_colors,
                    labels = c("Fail", "Pass"),
                    name = "LoanApproved") +
  theme_void() +
  labs(title = "Loan Approval for Married") +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            size = 4)

# Chart for Not married
plot_not_married <- loan_summary %>%
  filter(MaritalStatus == "Not married") %>%
  ggplot(aes(x = "", y = perc, fill = factor(LoanApproved))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  scale_fill_manual(values = approval_colors,
                    labels = c("Fail", "Pass"),
                    name = "LoanApproved") +
  theme_void() +
  labs(title = "Loan Approval for Not Married") +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            size = 4)

grid.arrange(plot_married, plot_not_married, ncol = 2)


# Hypothesis Testing: Loan Annual Income by Marital Status
# Full Model
model_full <- glm(LoanApproved ~ AnnualIncome * MaritalStatus,
                  data = loan, family = binomial)

# Reduced Model
model_reduced <- glm(LoanApproved ~ AnnualIncome + MaritalStatus,
                     data = loan, family = binomial)

# Likelihood Ratio Test
anova(model_reduced, model_full, test = "Chisq")
summary(model_full)

# Chapter 8
# LOGISTIC REGRESSION FOR HOUSE OWNERSHIP 

loan$HomeOwnershipStatus <- ifelse(
  loan$HomeOwnershipStatus %in% c("Mortgage", "Own"),
  "Homeowner",
  "Non-homeowner"
)

table(loan$HomeOwnershipStatus)

loan_summary <- loan %>%
  group_by(HomeOwnershipStatus, LoanApproved) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(HomeOwnershipStatus) %>%
  mutate(perc = round(100 * count / sum(count), 1),
         label = paste0(perc, "%"))

approval_colors <- c("1" = "#BA55D3",  # Pass (teal)
                     "0" = "#FF69B4")  # Fail (dark green)

# Chart for Homeowner
plot_homeowner <- loan_summary %>%
  filter(HomeOwnershipStatus == "Homeowner") %>%
  ggplot(aes(x = "", y = perc, fill = factor(LoanApproved))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  scale_fill_manual(values = approval_colors,
                    labels = c("Fail", "Pass"),
                    name = "LoanApproved") +
  theme_void() +
  labs(title = "Loan Approval for Homeowner") +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            size = 4)

# Chart for Non-homeowners
plot_non_homeowner <- loan_summary %>%
  filter(HomeOwnershipStatus == "Non-homeowner") %>%
  ggplot(aes(x = "", y = perc, fill = factor(LoanApproved))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  scale_fill_manual(values = approval_colors,
                    labels = c("Fail", "Pass"),
                    name = "LoanApproved") +
  theme_void() +
  labs(title = "Loan Approval for Non-homeowner") +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            size = 4)

grid.arrange(plot_homeowner, plot_non_homeowner, ncol = 2)


# Hypothesis Testing: AnnualIncome Effect by HomeOwnershipStatus
# Full Model
model_full <- glm(LoanApproved ~ AnnualIncome * HomeOwnershipStatus,
                  data = loan, family = binomial)

# Reduced Model
model_reduced <- glm(LoanApproved ~ AnnualIncome + HomeOwnershipStatus,
                     data = loan, family = binomial)

# Likelihood Ratio Test
anova(model_reduced, model_full, test = "Chisq")
summary(model_full)

# Chapter 9
# RELATIONSHIPS BETWEEN PREDICTORS 

loan %>%
  mutate(
    EducationLevel = factor(
      EducationLevel,
      levels = c("High School", "Associate", "Bachelor", "Master", "Doctorate")
    )
  ) %>%
  ggplot(aes(x = EducationLevel, y = AnnualIncome)) +
  geom_boxplot(fill = "BlueViolet", alpha = 0.6, outlier.alpha = 0.3) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Annual Income by Education Level",
    x = "Education Level",
    y = "Annual Income"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

income_summary <- loan %>%
  mutate(
    EducationLevel = factor(
      EducationLevel,
      levels = c("High School", "Associate", "Bachelor", "Master", "Doctorate")
    )
  ) %>%
  group_by(EducationLevel) %>%
  summarise(
    n            = n(),
    mean_income  = mean(AnnualIncome, na.rm = TRUE),
    median_income = median(AnnualIncome, na.rm = TRUE),
    sd_income    = sd(AnnualIncome, na.rm = TRUE),
    q1           = quantile(AnnualIncome, 0.25, na.rm = TRUE),
    q3           = quantile(AnnualIncome, 0.75, na.rm = TRUE)
  )

income_summary

income_means <- loan %>%
  mutate(
    EducationLevel = factor(
      EducationLevel,
      levels = c("High School", "Associate", "Bachelor", "Master", "Doctorate")
    )
  ) %>%
  group_by(EducationLevel) %>%
  summarise(
    n    = n(),
    mean_income = mean(AnnualIncome, na.rm = TRUE),
    sd_income   = sd(AnnualIncome, na.rm = TRUE),
    se_income   = sd_income / sqrt(n),
    ci_lower    = mean_income - 1.96 * se_income,
    ci_upper    = mean_income + 1.96 * se_income
  )

ggplot(income_means,
       aes(x = EducationLevel, y = mean_income, group = 1)) +
  geom_line(color = "BlueViolet") +
  geom_point(size = 3, color = "BlueViolet") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.1) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Mean Annual Income by Education Level",
    x = "Education Level",
    y = "Mean Annual Income"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


num_vars <- loan %>%
  select(where(is.numeric)) %>%
  select(-RiskScore)  

corr_mat <- cor(num_vars, use = "pairwise.complete.obs")

ggcorrplot(corr_mat,
           lab = FALSE,
           type = "lower",
           outline.color = "white",
           show.legend = TRUE,
           ggtheme = theme_minimal())

corr_long <- corr_mat %>%
  as.data.frame() %>%
  mutate(var1 = rownames(.)) %>%
  pivot_longer(-var1, names_to = "var2", values_to = "corr") %>%
  filter(var1 < var2) %>%        
  arrange(desc(abs(corr)))

head(corr_long, 15)

# LOAN AMOUNT VS MONTHY LOAN PAYMENT
# Correlation
cor_lamt_mpay <- cor(
  loan$LoanAmount,
  loan$MonthlyLoanPayment,
  use = "pairwise.complete.obs"
)
cor_lamt_mpay

# Model liniowy
model_lamt_mpay <- lm(MonthlyLoanPayment ~ LoanAmount, data = loan)
summary(model_lamt_mpay)

loan %>%
  ggplot(aes(x = LoanAmount, y = MonthlyLoanPayment)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE, color = "BlueViolet") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Monthly Loan Payment vs Loan Amount",
    x = "Loan Amount",
    y = "Monthly Loan Payment"
  ) +
  theme_minimal()

loan %>%
  ggplot(aes(x = LoanAmount, y = MonthlyLoanPayment,
             color = LoanDuration)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  
  labs(
    title = "Loan Amount vs Monthly Payment by Loan Duration",
    x = "Loan Amount",
    y = "Monthly Loan Payment",
    color = "Loan Duration (months)"
  ) +
  theme_minimal()

loan_dur <- loan %>%
  mutate(
    Duration_cat = cut(
      LoanDuration,
      breaks = quantile(LoanDuration,
                        probs = c(0, 1/3, 2/3, 1),
                        na.rm = TRUE),
      include.lowest = TRUE,
      labels = c("short", "medium", "long")
    )
  )

model_cat <- lm(MonthlyLoanPayment ~ LoanAmount * Duration_cat, data = loan_dur)
summary(model_cat)

short <- loan_dur %>% filter(Duration_cat == "short")
long  <- loan_dur %>% filter(Duration_cat == "long")

r_short <- cor(short$LoanAmount, short$MonthlyLoanPayment,
               use = "pairwise.complete.obs")
r_long  <- cor(long$LoanAmount, long$MonthlyLoanPayment,
               use = "pairwise.complete.obs")

test_r <- r.test(n = nrow(short),
                 r12 = r_short,
                 r34 = r_long,
                 n2 = nrow(long))

test_r

#Monthly Loan Payment vs Loan Amount by Interest Rate
loan_ir <- loan %>%
  mutate(
    Interest_cat = cut(
      InterestRate,
      breaks = quantile(InterestRate,
                        probs = c(0, 1/3, 2/3, 1),
                        na.rm = TRUE),
      include.lowest = TRUE,
      labels = c("low", "medium", "high")
    )
  )


pcor_ir <- pcor.test(
  x = loan$MonthlyLoanPayment,
  y = loan$InterestRate,
  z = loan$LoanAmount,
  method = "pearson"
)

pcor_ir

#CreditScore vs BankruptcyHistory
loan_bh <- loan %>%
  filter(!is.na(BankruptcyHistory)) %>%
  mutate(
    BankruptcyHistory = factor(
      BankruptcyHistory,
      levels = c(0, 1),
      labels = c("No bankruptcy", "Bankruptcy")
    )
  )

bh_counts <- loan_bh %>%
  count(BankruptcyHistory)

bh_counts

loan_bh <- loan %>%
  filter(!is.na(BankruptcyHistory)) %>%        
  mutate(
    BankruptcyHistory = factor(
      BankruptcyHistory,
      levels = c(0, 1),                        
      labels = c("No bankruptcy", "Bankruptcy")
    )
  )

loan_bh %>%
  ggplot(aes(x = BankruptcyHistory, y = CreditScore)) +
  geom_boxplot(fill = "BlueViolet", alpha = 0.6, outlier.alpha = 0.3) +
  labs(
    title = "Credit Score by Bankruptcy History",
    x = "Bankruptcy History",
    y = "Credit Score"
  ) +
  theme_minimal()

mean_table <- loan_bh %>%
  group_by(BankruptcyHistory) %>%
  summarise(
    mean_cs = mean(CreditScore, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  bind_rows(
    loan_bh %>%
      summarise(
        BankruptcyHistory = "Overall",
        mean_cs = mean(CreditScore, na.rm = TRUE)
      )
  )

mean_table <- mean_table %>%
  mutate(mean_cs = round(mean_cs, 2))

mean_table

tt_cs_bh <- t.test(CreditScore ~ BankruptcyHistory,
                   data = loan_bh,
                   var.equal = FALSE)  # Welch
tt_cs_bh

# Chapter 10
# Linear regression model 
# Change binary variables to factor
loan <- loan %>% 
  mutate(
    across(where(is.character), as.factor),
    BankruptcyHistory      = as.factor(BankruptcyHistory),
    PreviousLoanDefaults   = as.factor(PreviousLoanDefaults),
    LoanApproved           = as.factor(LoanApproved)
  )

model_lm_full <- lm(
  RiskScore ~ . - ApplicationDate - LoanApproved,
  data = loan
)

summary(model_lm_full)

# Division into train/test on original data

set.seed(123)                 
n <- nrow(loan)

train_idx <- sample(seq_len(n), size = 0.8 * n)
train <- loan[train_idx, ]
test  <- loan[-train_idx, ]

# Full and null model on train 

full_lm_train <- lm(
  RiskScore ~ . - ApplicationDate - LoanApproved,
  data = train
)

null_lm_train <- lm(
  RiskScore ~ 1,
  data = train
)

#FORWARD SELECTION criterion BIC 

n_train <- nrow(train)

forward_lm_bic <- step(
  null_lm_train,
  scope     = list(lower = null_lm_train, upper = full_lm_train),
  direction = "forward",
  k         = log(n_train),   
  trace     = TRUE
)

summary(forward_lm_bic)

# AIC and BIC for the BIC model:
AIC(forward_lm_bic)
BIC(forward_lm_bic)

# BACKWARD SELECTION criterion BIC 

backward_lm_bic <- step(
  full_lm_train,
  direction = "backward",
  k         = log(n_train),   
  trace     = TRUE            
)

summary(backward_lm_bic)

# AIC and BIC for this model:
AIC(backward_lm_bic)
BIC(backward_lm_bic)

# STEPWISE SELECTION criterion BIC 

stepwise_lm_bic <- step(
  null_lm_train,
  scope     = list(lower = null_lm_train, upper = full_lm_train),
  direction = "both",       
  k         = log(n_train), 
  trace     = TRUE          
)

summary(stepwise_lm_bic)

# AIC and BIC for this model:
AIC(stepwise_lm_bic)
BIC(stepwise_lm_bic)

# errors on TEST SET
y_true <- test$RiskScore
y_pred <- predict(final_lm, newdata = test)

MSE  <- mean((y_true - y_pred)^2)
RMSE <- sqrt(MSE)

y_true_mape <- ifelse(y_true == 0, 1, y_true)
MAPE <- mean(abs((y_true - y_pred) / y_true_mape)) * 100

MSE
RMSE
MAPE

# FORWARD SELECTION – AIC 
forward_lm_aic <- step( 
  null_lm_train, 
  scope = list(lower = null_lm_train, upper = full_lm_train), 
  direction = "forward", 
  trace = TRUE  
)
  
summary(forward_lm_aic) 

AIC(forward_lm_aic) 
BIC(forward_lm_aic) 

# BACKWARD SELECTION – AIC 

backward_lm_aic <- step( 
  full_lm_train, 
  direction = "backward", 
  trace = TRUE 
  ) 

summary(backward_lm_aic) 
AIC(backward_lm_aic) 
BIC(backward_lm_aic) 

# STEPWISE (BOTH) – AIC 

stepwise_lm_aic <- step( 
  null_lm_train, 
  scope = list(lower = null_lm_train, upper = full_lm_train), 
  direction = "both", 
  trace = TRUE 
  ) 
summary(stepwise_lm_aic) 
AIC(stepwise_lm_aic) 
BIC(stepwise_lm_aic) 

AIC(forward_lm_aic, backward_lm_aic, stepwise_lm_aic) 
BIC(forward_lm_aic, backward_lm_aic, stepwise_lm_aic) 

# Final Model 
final_lm_aic <- forward_lm_aic 

# RiskScore predictions on the test set 
y_true <- test$RiskScore 
y_pred <- predict(final_lm_aic, newdata = test) 

#MSE 
MSE <- mean((y_true - y_pred)^2) 
#RMSE 
RMSE <- sqrt(MSE) 
#MAPE 
y_true_mape <- ifelse(y_true == 0, 1, y_true) 
MAPE <- mean(abs((y_true - y_pred) / y_true_mape)) * 100 

MSE; RMSE; MAPE

cooksd <- cooks.distance(model_lm_full)
head(cooksd)

n <- nrow(loan)
cutoff <- 4 / n   

plot(
  cooksd,
  type = "h",
  main = "Cook's distance for the linear model",
  xlab = "Observation number",
  ylab = "Cook's distance"
)
abline(h = cutoff, lty = 2, col = "red")

# COOK'S DISTANCE
top7_idx <- order(cooksd, decreasing = TRUE)[1:7]
top7_idx          

loan[top7_idx, ]

# Removing these observations from the data
loan_clean <- loan[-top7_idx, ]

nrow(loan)       
nrow(loan_clean)

# Revised model and new Cook's distance on cleaned data
model_lm_full_clean <- lm(
  RiskScore ~ . - ApplicationDate - LoanApproved,
  data = loan_clean
)

summary(model_lm_full_clean)

cooksd_clean <- cooks.distance(model_lm_full_clean)

n_clean <- nrow(loan_clean)
cutoff_clean <- 4 / n_clean

plot(
  cooksd_clean,
  type = "h",
  main = "Cook's distance",
  xlab = "Observation number",
  ylab = "Cook's distance"
)
abline(h = cutoff_clean, lty = 2, col = "red")

# Division into training and test sets (80% / 20%)

set.seed(123)  
n <- nrow(loan_clean)

train_idx <- sample(seq_len(n), size = 0.8 * n)

train <- loan_clean[train_idx, ]
test  <- loan_clean[-train_idx, ]

# Full model and Null model on the TRAIN SET
# Full model and zero model included in the TRAIN SET
full_lm_train <- lm(
  RiskScore ~ . - ApplicationDate - LoanApproved,
  data = train
)

null_lm_train <- lm(
  RiskScore ~ 1,
  data = train
)
