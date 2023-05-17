# Used Car Valuation Platform

_Hubert Wojewoda, Michał Grzyb_

## Project description

The **Used Car Valuation Platform** is a comprehensive tool designed to accurately estimate the
market value of used cars based on various attributes, such as power, fuel type, year of
production, mileage, and transmission type. The platform aims to assist both buyers and sellers
in making informed decisions in the used car market. It will leverage advanced R programming
techniques, machine learning algorithms, and an interactive Shiny dashboard to deliver real-
time pricing estimates to users.

To accomplish this, the following elements of the course will be applied:

• Writing advanced functions in R: The tool will include custom R functions for data
pre-processing, feature engineering, and model training. Defensive programming
techniques will be employed to ensure the functions are robust and handle potential
errors gracefully.

• Object-oriented programming: The project will utilize S3/S4/R6 systems for creating
custom classes, methods, and generic functions. These will help manage the complexity
of the project and promote code reusability.

• Use of C++ in R (Rcpp): Performance-critical parts of the code, such as complex
calculations or data manipulations, will be implemented in C++ and integrated with R
using the Rcpp package. This will enhance the tool's performance and efficiency.

• Shiny dashboard: An interactive analytical dashboard will be created using the Shiny
package, allowing users to input car details, view pricing estimates, and explore the
impact of various factors on the car's value.

Additional coding techniques:

• Tidyverse: The project will extensively use tidyverse packages, such as dplyr, ggplot2,
and tidyr, for data manipulation, visualization, and transformation.

• Machine learning: We will incorporate machine learning techniques, such as regression
algorithms and ensemble methods, to predict used car prices. The caret or tidy models
package will be used to streamline the model training and evaluation process.

The Used Car Valuation Platform will combine a variety of R programming techniques and
analytical strategies, creating a flexible, user-friendly, and valuable tool that addresses real-
world issues encountered in the used car market. By applying the knowledge gained from the
course, this project will showcase we can develop a practical solution that can be scaled and
adapted to various market scenarios, ultimately providing users with a comprehensive and
insightful tool for evaluating used car prices.

## Project steps

1. Data Preprocessing

   Clean the data to handle missing values, outliers, and inconsistent data entries.
   Use R functions and the tidyverse package for data cleaning and transformation.

2. Feature Engineering

   Identify and create relevant features that would influence a car's value.
   This might involve creating new variables or modifying existing ones.

3. Model Building

   Use machine learning techniques to predict used car prices.
   Use regression algorithms, ensemble methods, and possibly other machine learning models.
   Use caret or tidymodels package to streamline the model training and evaluation process.

4. Writing Custom Functions

   Develop custom R functions for data preprocessing, feature engineering, and model training.
   Use defensive programming techniques to ensure robustness.

5. Object-Oriented Programming

   Create custom classes, methods, and generic functions using S3/S4/R6 systems.
   This will help manage the complexity of the project and promote code reusability.

6. Performance Optimization

   Identify performance-critical parts of the code.
   Implement these parts in C++ and integrate them with R using the Rcpp package.

7. Shiny Dashboard Development

   Develop the interactive dashboard using the Shiny package.
   Include features for users to input car details, view pricing estimates, and
   explore factors impacting the car's value.

## Work split

Hubert Wojewoda:

- Data Preprocessing
- Performance Optimization
- Shiny Dashboard Development
- Object-Oriented Programming

Michał Grzyb:

- Feature Engineering
- Model Building
- Writing Custom Functions
- Object-Oriented Programming
