

# Load Packages

library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
require(ROI)
require(ROI.plugin.glpk)
require(ROI.plugin.quadprog)
library(fPortfolio)
#-----------------------------------------


data(edhec)
# Use the first 4 columns in edhec for a returns object
returns <- edhec
#colnames(returns) <- c("CA", "CTAG", "DS", "EM")
print(head(returns, 5))


# Get a character vector of the fund names
fund.names <- colnames(returns)

# Specify a portfolio object by passing a character vector for the  assets argument.
pspec <- portfolio.spec(assets=fund.names)

print.default(pspec)


# Add the full investment constraint that specifies the weights must sum to 1.
pspec <- add.constraint(portfolio=pspec, type="weight_sum", min_sum=1, max_sum=1)


# The full investment constraint can also be specified with type="full_investment"
pspec <- add.constraint(portfolio=pspec, type="full_investment")

# Another common constraint is that portfolio weights sum to 0.
# This can be specified any of the following ways
#pspec <- add.constraint(portfolio=pspec, type="weight_sum", min_sum=0, max_sum=0)

pspec <- add.constraint(portfolio=pspec, type="dollar_neutral")
pspec <- add.constraint(portfolio=pspec, type="active")


# BOX CONSTRAINT
# Add box constraints

pspec <- add.constraint(portfolio=pspec, type="box", min=0.05, max=0.4)
# Each asset in the portfolio is to be at least 5% of the portfolio and max 40% of the portfolio

# min and max can also be specified per asset
pspec <- add.constraint(portfolio=pspec, type="box", min=c(0.05, 0, 0.08, 0.1), max=c(0.4, 0.3, 0.7, 0.55))

# A special case of box constraints is long only where min=0 and max=1
# The default action is long only if min and max are not specified

pspec <- add.constraint(portfolio=pspec, type="box")

pspec <- add.constraint(portfolio=pspec, type="long_only")

# Group Constraint
pspec <- add.constraint(portfolio=pspec, type="group", 
                        groups=list(groupA=c(1, 2, 3), grouB=4), group_min=c(0.1, 0.15), group_max=c(0.85, 0.55))



# Factor Exposure Constraint
pspec <- add.constraint(portfolio=pspec, type="factor_exposure",
                         B=c(-0.08, 0.37, 0.79, 1.43),
                         lower=0.6, upper=0.9)




#**************************************************
# Specifying Constraints as Separate Objects
#**************************************************
# full investment constraint
weight_constr <- weight_sum_constraint(min_sum=1, max_sum=1)
# box constraint
box_constr <- box_constraint(assets=pspec$assets, min=0, max=1)
# group constraint
group_constr <- group_constraint(assets=pspec$assets,
                                     + groups=list(c(1, 2, 3),
                                                   + 4),
                                     + group_min=c(0.1, 0.15),
                                     + group_max=c(0.85, 0.55),
                                     + group_labels=c("GroupA", "GroupB"))
# position limit constraint
 poslimit_constr <- position_limit_constraint(assets=pspec$assets, max_pos=3)
# diversification constraint
 div_constr <- diversification_constraint(div_target=0.7)
# turnover constraint
 to_constr <- turnover_constraint(turnover_target=0.2)
#target return constraint
ret_constr <- return_constraint(return_target=0.007)

# factor exposure constraint
exp_constr <- factor_exposure_constraint(assets=pspec$assets,B=c(-0.08, 0.37, 0.79, 1.43),lower=0.6, upper=0.9)


my_constraints = list(box_constr, weight_constr)




#*************************************************
# Add objectives
#*************************************************

# Portfolio Risk Budget Objective

pspec <- add.objective(portfolio=pspec, type='risk', name='ETL', arguments=list(p=0.95))


pspec <- add.objective(portfolio=pspec, type='return', name='mean')



pspec <- add.objective(portfolio=pspec, type='risk', name='var')


opt_p <- optimize.portfolio(returns, portfolio=pspec, optimize_method="ROI", trace=TRUE)

opt_p_random = optimize.portfolio(returns, portfolio = pspec, constraints = NULL,
                   objectives = NULL, optimize_method = c("random"), search_size = 10000, trace = FALSE)
# Optimize Quadratic Utility

data(edhec)
R <- edhec[, 1:6]
colnames(R) <- c("CA", "CTAG", "DS", "EM", "EQMN", "ED")
funds <- colnames(R)
# Create an initial portfolio object with leverage and box constraints
init <- portfolio.spec(assets=funds)
init <- add.constraint(portfolio=init, type="leverage", min_sum=0.99, max_sum=1.01)
init <- add.constraint(portfolio=init, type="box", min=0.05, max=0.65)

qu <- add.objective(portfolio=init, type="return", name="mean")
qu <- add.objective(portfolio=qu, type="risk", name="var", risk_aversion=0.2)



library(ROI.plugin.quadprog)
opt_qu <- optimize.portfolio(R=R, portfolio=qu, optimize_method="ROI", trace=TRUE)
print(opt_qu)










