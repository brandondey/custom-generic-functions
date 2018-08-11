### Overview
**These aren't production functions. Use at your own risk.** 

Various custom functions are in ```Custom_Functions.rds```. See Function Directory below for details on what's included.

To use run:
```
load(file = "~/Functions/Custom_Functions.rds")
```

To edit, modify ```Custom_Functions.r``` and run 
```
save(
    new_function_name,
    old_function_name_1, 
    old_function_name_2,
    file = '~/Functions/Custom_Functions.rds'
    ) 
```

### Function Directory

```
rename_var(df, old_vars, new_vars)
```

```rename_var()```  renames old_vars in df to new_vars and outputs a dataframe in global environment named df_new
args
* df: dataframe
* old_vars: character vector of variable names to be renamed
* new_vars: character vector of variable names to be renamed
  
```
flattenCorrMatrix(cor_matrix, p_matrix)
```
Correlation coeficients from ```Hmsisc::rcorr``` come in a list and they're hard to work with. ```flattenCorrMatrix()``` Takes the list from ```rcorr``` and converts to a dataframe

* cor_matrix : matrix of the correlation coefficients
* p_matrix : matrix of the correlation p-values
* from [here](http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software).

```
prettify_logistic_reg(data, formula)
```
```prettify_logistic_reg()``` prints model stats: 
* accuracy
* true positive rate
* true negative rate 

for logistic regression with ```formula```

args:
* ```forumla```: formula string to be passed to ```stats::glm(data, formula = formula, family = "binomial")```: e.g.: ```"y ~ x + x1"```


```
make_me_md_table(var_names)
```
Making data dictionaries in a .md usually requries making a table with two columns. One for the variable name and another for its description. ```make_me_md_table()``` takes a vector of variable names and spits out a .md table with two columns.
args:
* ```var_names```: Vector of names(df)

```
make_me_md_table_files(path)
```
```make_me_md_table_files``` prints all objects files in ```path``` in a .md table format with two columns: File, Description. Helps make Readme.md's easier to make.

## ROC Curve Analysis
The following three functions were built to make ROC Curve analysis easier and more visual.
```
calculate_roc(df, cost_of_fp, cost_of_fn)
```
**About:**
``` calculate_roc()``` creates a data frame with variables that are needed for ROC curve analysis:  

args:
* df: dataframe
* cost_of_fp: integer to penalize false positves 
* cost_of_fn: integer to penalize false negatives 

This function shines because it automatically calculates the optimal prediction threshold for binary classification. The visualizations help see what observations are type I or II error too.

``` plot_prediction_type_distribution(df, threshold, show_plot = T) ```

**About:** 
* visualizes the Type I and II errors. 
* df must have vars named **prediction** and **actual**, where **prediction** is the probability score before it's rounded up or down to match the values of **actual**.


```plot_roc <- function(df, fpc = 1, fnc = 2)```

**About:**

```Plot_roc()``` plots 3 graphs arranged in one plot grid (via cowplot) to help illustrate how your model perfomed at binary classifcation.  It visualizes 

* cost 
* optimal threshold curve
* type I and II errors

**args:** 
* df: data frame with vars named: threshold, tpr, fpr, cost. Created from ``` calculate_roc()```
* fpc: cost to penalize false positives
* fnc: cost to penalize false negatives

![alt text](https://gitlab.directs.com/Data_Science_Projects/Customer_Insights/custom-functions/blob/master/ROC%20Plots%20(That%20rock).jpg)

*Modified from joyofdata @: https://github.com/joyofdata/joyofdata-articles/blob/master/roc-auc/plot_roc.R*

