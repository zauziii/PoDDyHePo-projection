<!-- README.md is generated from README.Rmd. Please edit that file -->

# Introduction

The purpose of this package is to project prevalences of health
indicators in future. The method uses data on cross-sectional surveys
and national population forecasts to take into account the changing
population structure. This package contains the following functions:

1.  `PoDDyHePoFreqTable`: creates a descriptive table with sample size
    and amount of missingness in the data.
2.  `PoDDyHePoModelSelection`: selects models for projection step
    according to BIC.
3.  `PoDDyHePoPopulationDF`: uses population forecasts, in example from
    Statistics Finland, and well-formatted survey data to create a data
    frame ready for projection.
4.  `PoDDyHePoProjection`: projects individual-level values of the
    variables using multiple imputation.
5.  `PoDDyHePoPool`: calculates prevalences by pooling results from
    `PoDDyHePoProjection`.
6.  `PoDDyHePoPlot`: plots the prevalences against survey year.

# Getting stated

Before getting started, make sure that you have installed and attached
the package. Also, remember to load the following package: `splines`.

    # Install devtools if you do not have.
    install_github("zauziii/PoDDyHePo-projection")
    # After installation, please attach both PoDDyHePo-projection and splines.
    library(PoDDyHePoProjection)
    library(splines)

For all the examples, please load `testdata` (synthetic data built in
the package):

    data(testdata)

In the `testdata`, we have mandatory variables:

-   `year`: Survey year. Numeric. 1997, 2002, 2007, 2012 and 2017. It is
    not limited and could be any other survey years.

-   `sex`: Categorical. 1 for men and 2 for women. Currently, the
    function supports only 1 and 2 coding. Please ensure that the coding
    used for `sex` is correct.

-   `age`: Numeric. Ranges from 25 - 64, but not limited to this range.
    In your case, it could be any range that you are interested in. Main
    variables of interest:

-   `obe`: Obesity/Overweight/Normal. Categorical. 0 = Normal weight, 1
    = Overweight and 2 = Obesity.

-   `smo`: Smoking status. Binary. 0 = Non-smoker, 1 = Smoker.

-   `highbp`: Hypertension. Binary. 0 = Non-hypertensive, 1 =
    Hypertensive.

-   `dbt`: Diabetes. Binary. 0 = Non-diabetic, 1 = Diabetic.

-   `sed`: Sedentary lifestyle. Binary. 0 = Non-sedentary lifestyle, 1 =
    Sedentary lifestyle.

-   `highkol`: Elevated total cholesterol. Binary. 0 = Not high total
    cholesterol, 1 = High total cholesterol.

Other background variables:

-   `edulv`: Education level. Categorical. 1 = Low education level, 2 =
    Middle education level, 3 = High education level.
-   `marsta`: Marital Status. Binary. 0 = Unmarried/Separated or
    Divorced/Widowed, 1 = Married/Cohabiting/Registered relationship.
-   `area`: Categorical. 2 = North Karelia, 3 = North Savonia, 4 = Turku
    and Loimaa, 5 = Helsinki and Vantaa, 6 = Northern Ostrobothnia and
    Kainuu.

When formatting your dataset, the data types of year, sex and age should
be numeric, factor and numeric, respectively. Other binary/categorical
variables should be factor and continuous numeric.

## Guidelines for Using the package

**Step 1**: If data from past surveys include missing values, they are
imputed before selecting models for projection.

    imp <- mice(testdata, m = 5, maxit = 10, printFlag = F)

NB: `m` and `maxit` can be any number you want, but please consider the
computational power of your workstation. Usually, `m = 10` or `m = 20`
should be enough. And if `printFlag = T`, `mice` will print history on
console. Here, `printFlag = F` is used for silent computation. Data set
should be well-formatted like `testdata`, which means there are
mandatory variables: year, sex and age named exactly in this way, the
main variables of interest whose prevalences are to be estimated, and
possibly other background variables. Data from different years are
merged into the same data frame.

**Step 2**: Select models with `PoDDyHePoModelSelection`. The user can
specify if some variables are allowed to have non-linear effects, which
are modelled using natural splines. Some of the variables can also be
forced (fixed) to be included in the model. Here in this example, we
define knots and boundary knots in the spline function `ns`, and fix
variables year, sex, age and an interaction term: year:sex.

In the function
`PoDDyHePoModelSelection(imp, DV, NsVar = NULL, df = NULL, knots = NULL, b.knots = NULL, f.var = NULL)`,
the first argument `imp` is an object of class from in mice package (see
*Step 1*). `DV` is the abbreviation of dependent variable. Arguments
`NsVar`, `df`, `knots`, `b.knots` are for customizing `ns` function in
the model. `NsVar` specifies a variable with a natural spline, if it is
NULL, degrees of freedom(`df`), breakpoints that define the spline
(`knots`) and boundary points (`b.knots`) are ignored; if it is not
NULL, we can set df or knots and b.knots. Examples are given as follows
based on `testdata`. More details about `ns` function can be found via
<https://www.rdocumentation.org/packages/splines/versions/3.6.2/topics/ns>.

    smo <- PoDDyHePoModelSelection(imp, 
                                   NsVar = "year", 
                                   DV = "smo", 
                                   knots = 2002, 
                                   b.knots = c(1997, 2017), 
                                   f.var = c("year", "sex", "age", "year:sex"))

For other variables, if other arguments remain the same, what we need to
do is to change `DV`, like

    obe <- PoDDyHePoModelSelection(imp, 
                                   NsVar = "year", 
                                   DV = "obe", 
                                   knots = 2002, 
                                   b.knots = c(1997, 2017), 
                                   f.var = c("year", "sex", "age", "year:sex"))

This function returns a list with 4 elements. One for recording the
selection process, one for storing the mean BIC from each imputed data
set, one for saving the selected models, and the last one is for the
suggested model. Please notice that, the projections may be sensitive to
the imputation models used, so it is recommended to carry out
sensitivity analyses by trying alternative models.

**Step 3**: Use population forecasts for example from Statistics Finland
and your survey data (here is `testdata`) to create a data frame for
projection.

    testdata2040 <- PoDDyHePoPopulationDF(testdata,
                                          file = "~path/003_139f_2040_20211102-100631.csv", 
                                          size = 10000, 
                                          y2pred = c(2021, 2025, 2030, 2035, 2040))

For the function `PoDDyHePoPopulationDF`, the first argument is the
well-formatted data, in the example `testdata`. The second argument,
which is “file”, is a csv file downloaded from Statistics Finland (with
special requirements given below). The third is a sample size and the
fourth is the years to be projected. Above, 2021, 2025, 2030, 2035 and
2040, are the years to be projected, so
`y2pred = c(2021, 2025, 2030, 2035, 2040)`. If you want to try our
internal population forecasts, use
`file = system.file("extdata", "003_128v_2040_20210730-015759.csv", package = "csprojections")`.
That is,

    testdata2040 <- PoDDyHePoPopulationDF(testdata,
                                          file = system.file("extdata","003_128v_2040_20210730-015759.csv", package = "csprojections"), 
                                          size = 10000, 
                                          y2pred = c(2021, 2025, 2030, 2035, 2040))

In order to make this function work, the population forecast data has to
be in a specific form. In case of Finland, data can be downloaded in the
correct form by following the next steps:

1.  Visit
    <https://pxnet2.stat.fi/PXWeb/pxweb/en/StatFin/StatFin__vrm__vaenn/>,
    scroll down and find Population projection.
2.  Click 139f – Population projection 2021: Population according to age
    and sex by area, 2021-2040 \[Size: 5818 Kb\] \[Modified:
    9/30/2021\].
3.  Select Population 31 Dec (projection 2019) (Information) - WHOLE
    COUNTRY (Area) - Select the years to be projected (Year) -Select all
    (Sex) - Select all (Age), and click Show table. (In our example, it
    is 2021, 2025, 2030, 2035 and 2040).
4.  Click “pivot manual” and set Area, Age in the Rows box (Order: Area,
    Age) and set Year, Sex, Information in the Columns box (Order: Year,
    Sex, Information) and click “complete”.
5.  Click “Save the result as …” - select “Semicolon delimited with
    heading” and save.

If you use population forecasts outside Finland, please format it
correctly before putting it in the function. You can download the
population forecasts that we used via github to see how it looks like.
It is under extdata in the folder inst. Or by using the following
commands:

    # Create a dataframe ready for projection, skip = 2 tell the function to skip first two rows,
    # and by setting colClasses = c(Area="NULL"), we sikp the first column. 
    # Those are in the csv file when we downloaded it from Statistics Finland, but we do not need.
    population_forecasts <- utils::read.table(system.file("extdata", "003_128v_2040_20210730-015759.csv", package = "csprojections"), 
                                              sep = ";", 
                                              skip = 2, 
                                              header = T, 
                                              check.names = F,
                                              colClasses = c(Area="NULL"))

In the population forecasts, Age should be named exactly Age. Variable
names like 2020 Total Population 31 Dec (projection 2019), 2020 Males
Population 31 Dec (projection 2019) and 2020 Females Population 31 Dec
(projection 2019) can be simplified to 2020 Total, 2020 Males and 2020
Females. Data description is given below.

    # > str(population_forecasts)
    # 'data.frame': 102 obs. of  16 variables:
    #  $ Age                                             : Factor w/ 102 levels "0","1","10","100 -",..: 102 1 2 14 25 36 47 58 69 80 ...
    #  $ 2020 Total Population 31 Dec (projection 2019)  : int  5530922 45710 46123 48338 51539 54368 56932 59305 60339 61821 ...
    #  $ 2020 Males Population 31 Dec (projection 2019)  : int  2732995 23362 23569 25015 26321 27608 29244 30328 31004 31525 ...
    #  $ 2020 Females Population 31 Dec (projection 2019): int  2797927 22348 22554 23323 25218 26760 27688 28977 29335 30296 ...
    #  $ 2025 Total Population 31 Dec (projection 2019)  : int  5556546 44999 45557 46066 46522 46909 47251 47588 49744 52789 ...
    #  $ 2025 Males Population 31 Dec (projection 2019)  : int  2752594 22994 23286 23562 23774 23985 24165 24333 25721 26953 ...
    #  $ 2025 Females Population 31 Dec (projection 2019): int  2803952 22005 22271 22504 22748 22924 23086 23255 24023 25836 ...
    #  $ 2030 Total Population 31 Dec (projection 2019)  : int  5566685 44039 44540 45096 45592 46067 46564 47032 47451 47848 ...
    #  $ 2030 Males Population 31 Dec (projection 2019)  : int  2763297 22507 22766 23056 23313 23546 23817 24052 24255 24465 ...
    #  $ 2030 Females Population 31 Dec (projection 2019): int  2803388 21532 21774 22040 22279 22521 22747 22980 23196 23383 ...
    #  $ 2035 Total Population 31 Dec (projection 2019)  : int  5556472 43670 44077 44449 44810 45222 45611 46009 46490 46908 ...
    #  $ 2035 Males Population 31 Dec (projection 2019)  : int  2764088 22319 22531 22723 22906 23124 23326 23524 23774 23981 ...
    #  $ 2035 Females Population 31 Dec (projection 2019): int  2792384 21351 21546 21726 21904 22098 22285 22485 22716 22927 ...
    #  $ 2040 Total Population 31 Dec (projection 2019)  : int  5525528 43176 43729 44200 44604 44901 45239 45549 45829 46160 ...
    #  $ 2040 Males Population 31 Dec (projection 2019)  : int  2756428 22064 22353 22595 22809 22951 23144 23294 23422 23609 ...
    #  $ 2040 Females Population 31 Dec (projection 2019): int  2769100 21112 21376 21605 21795 21950 22095 22255 22407 22551 ...

So, when formatting your population forecasts, make sure you have Age
and Total, Males, Females for each predicted year. As long as you have
formatted your population forecasts which is similar to the population
forecasts we provide, you can put it in the function. And the function
will help you create a data frame ready for prediction.

**Step 4**: Project values for the future years.

    projection_with_knots <- PoDDyHePoProjection(testdata2040, 
                                                 m = 5, 
                                                 sep_col = "obe",
                                                 smo ~ ns(year, knots = c(2002), Boundary.knots = c(1997, 2017)) + 
                                                   sex + age + edulv + sed + marsta + obe + 
                                                   ns(year, knots = c(2002), Boundary.knots = c(1997, 2017)):sex + 
                                                   age:edulv + age:marsta + age:obe + sex:age,
                                                 obe$`Suggested Model`,
                                                 dbt$`Suggested Model`,
                                                 sed$`Suggested Model`,
                                                 highbp$`Suggested Model`,
                                                 highkol$`Suggested Model`,
                                                 edulv$`Suggested Model`,
                                                 marsta$`Suggested Model`,
                                                 area$`Suggested Model`)

In the `PoDDyHePoProjection` function, the first argument is the data
set from *Step 3*. The second argument is the number of multiple
imputations, by default this is 5, but much greater value is usually
needed in practice. The third argument is a categorical variable with
three or more levels, like in the testdata, `obe` has three levels: 0 =
Normal Weight, 1 = Overweight, 2 = Obesity. By specifying
`sep_col = "obe"`, the function calculates the proportions of each
category. The last part is for the formulas of the imputation models. In
*Step 2*, we found models for variables with missingness, and they have
been saved in the `Suggested Model` in the list given by
`PoDDyHePoModelSelection`. So, it can be used in a way like `obe`:
obe$\`Suggested Model\`, or specified our own model like for `smo` in
the code above. Every variable with missingness has to be given a
formula. When there is a variable with 3 or more levels like `obe` in
the example, it has to be specified in `sep_col`, as we use Wilson
Confidence Interval in the pooling step, which is for binomial
proportions.

**Step 5**: Pool the results from the projection step and plot the
prevalences. Given Current smoking and body weight

    # Current smoking
    smopool <- PoDDyHePoPool(projection_with_knots, "smo")
    PoDDyHePoPlot(smopool, year = 2017, "Current smoking")

    # Body weight
    obepool <- PoDDyHePoPool(projection_with_knots, "obe")
    PoDDyHePoPlot(obepool, year = 2017, "Body weight", sepvarlbl = c("Normal Weight", "Overweight", "Obesity"))

In the function `PoDDyHePoPool`, the first argument is the data with
projected values from *Step 4*, and the second argument is the main
variable whose prevalences we aim to estimate. In the function
`PoDDyHePoPlot`, the first argument is the result from `PoDDyHePoPool`;
the second argument is the maximum year in the observed data (In
`testdata`, it is 2017), the third argument is the title for the figure.
When there is a variable has three or more levels, an extra argument
`sepvarlbl` is available, which is to label the levels of this variable.
Otherwise, the labels for the newly created variables will be `obe_0`,
`obe_1` and `obe_2`.

The function PoDDyHePoPool returns the prevalences in numerical form. It
gives you a table with year, prevalences and prediction intervals.

Descriptive table with sample size and amount of missingness is also
available by using `PoDDyHePoFreqtable`.

There are two arguments in this function, the first one is the observed
data (`testdata`) while the second argument is a variable name of
interest. Examples are given below.

    PoDDyHePoFreqTable(testdata, colName = "smo")
    PoDDyHePoFreqTable(testdata, colName = "obe")

Notice: The function creates a table for only one variable. Does not
support several variables at one time.

# Acknowledgement

This R code has been developed in the framework of the Projections of
the burden of disease and disability in Finland - health policy
prospects (PoDDy-HePo) project which was funded by the Academy of
Finland no. 307907 (2017-2021).
