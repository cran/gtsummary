# tbl_continuous(data)

    Code
      as.data.frame(tbl)
    Output
        **Characteristic** **Drug A**  \nN = 98 **Drug B**  \nN = 102
      1              Grade                 <NA>                  <NA>
      2                  I    0.96 (0.23, 1.71)     1.05 (0.28, 1.50)
      3                 II    0.66 (0.30, 1.24)     0.21 (0.09, 1.08)
      4                III    0.84 (0.16, 1.94)     0.58 (0.33, 1.63)
      5     Tumor Response                 <NA>                  <NA>
      6                  0    0.75 (0.22, 1.45)     0.42 (0.17, 1.15)
      7                  1    0.98 (0.31, 1.89)     0.89 (0.30, 1.37)

# tbl_continuous(variable) messaging

    Code
      as.data.frame(tbl_continuous(trial, variable = grade, include = trt))
    Message
      The following errors were returned during `tbl_continuous()`:
      x For variable `grade` (`trt = "Drug A"`) and "median" statistic: need numeric data
      x For variable `grade` (`trt = "Drug A"`) and "p25" and "p75" statistics: (unordered) factors are not allowed
      x For variable `grade` (`trt = "Drug B"`) and "median" statistic: need numeric data
      x For variable `grade` (`trt = "Drug B"`) and "p25" and "p75" statistics: (unordered) factors are not allowed
    Output
            **Characteristic** **N = 200**
      1 Chemotherapy Treatment        <NA>
      2                 Drug A NA (NA, NA)
      3                 Drug B NA (NA, NA)

# tbl_continuous(by)

    Code
      as.data.frame(tbl_continuous(trial, variable = age, include = grade, by = trt))
    Output
        **Characteristic** **Drug A**  \nN = 98 **Drug B**  \nN = 102
      1              Grade                 <NA>                  <NA>
      2                  I          46 (36, 60)           48 (42, 55)
      3                 II          45 (31, 55)           51 (42, 58)
      4                III          52 (42, 61)           45 (36, 52)

# tbl_continuous(by) messaging

    Code
      tbl_continuous(trial, variable = age, include = grade, by = c(stage, trt))
    Condition
      Error in `tbl_continuous()`:
      ! The `by` argument must be length 1 or empty.
      i Use `tbl_strata()` for more than one `by` variable.

# tbl_continuous(statistic)

    Code
      as.data.frame(tbl_continuous(trial, variable = age, include = c(trt, grade),
      statistic = list(trt = "{var}", grade = "{sd}")))
    Output
            **Characteristic** **N = 200**
      1 Chemotherapy Treatment        <NA>
      2                 Drug A         216
      3                 Drug B         196
      4                  Grade        <NA>
      5                      I          15
      6                     II          14
      7                    III          14

# tbl_continuous(statistic) messaging

    Code
      tbl_continuous(trial, variable = age, include = grade, statistic = ~letters)
    Condition
      Error in `tbl_continuous()`:
      ! Elements of the `statistic` argument must be a string with `glue` elements referring to functions.
      i For example `statistic = list(colname = '{mean} ({sd})')`, to report the mean and standard deviation.

---

    Code
      tbl_continuous(trial, variable = age, include = grade, statistic = ~"mean")
    Condition
      Error in `tbl_continuous()`:
      ! Elements of the `statistic` argument must be a string with `glue` elements referring to functions.
      i For example `statistic = list(colname = '{mean} ({sd})')`, to report the mean and standard deviation.

# tbl_continuous(label) messaging

    Code
      tbl_continuous(trial, variable = age, include = c(trt, grade), label = list(
        trt = mean))
    Condition
      Error in `tbl_continuous()`:
      ! Elements of the `label` argument must be strings.

# tbl_continuous(value)

    Code
      df <- as.data.frame(tbl_continuous(trial, variable = age, include = c(trt,
        grade), value = trt ~ "Drug B"))
      df
    Output
            **Characteristic** **N = 200**
      1 Chemotherapy Treatment 48 (39, 56)
      2                  Grade        <NA>
      3                      I 47 (37, 56)
      4                     II 49 (37, 57)
      5                    III 47 (38, 58)

---

    Code
      df <- as.data.frame(tbl_continuous(trial, variable = age, include = c(trt,
        grade), by = response, value = trt ~ "Drug B"))
    Message
      7 missing rows in the "response" column have been removed.
    Code
      df
    Output
            **Characteristic** **0**  \nN = 132 **1**  \nN = 61
      1 Chemotherapy Treatment      47 (37, 54)     49 (43, 59)
      2                  Grade             <NA>            <NA>
      3                      I      47 (36, 53)     48 (41, 63)
      4                     II      47 (34, 55)     49 (44, 58)
      5                    III      46 (38, 59)     50 (43, 58)

# tbl_continuous(value) messaging

    Code
      tbl_continuous(trial, variable = age, include = c(trt, grade), value = trt ~
        "XXXXXXXXXX")
    Condition
      Error in `tbl_continuous()`:
      ! There was an error in the `value` argument for variable "trt".
      The list value must be one of "Drug A" and "Drug B".

---

    Code
      tbl_continuous(trial, variable = age, include = c(trt, grade), value = trt ~
        letters)
    Condition
      Error in `tbl_continuous()`:
      ! Error in argument `value` for variable "trt".
      i Elements values must be a scalar.

