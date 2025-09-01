FS_JSON <- '
{
  "variables": {
    "input": {
      "mpg": {
        "mpg.1": 15.4686,
        "mpg.2": 17.1275,
        "mpg.3": 19.1549
      },
      "hp": {
        "hp.1": 95.2824,
        "hp.2": 137.4549,
        "hp.3": 228.4588
      },
      "wt": {
        "wt.1": 3.0621,
        "wt.2": 3.0927,
        "wt.3": 4.151
      }
    },
    "output": {
      "qsec": {
        "qsec.1": 16.6082,
        "qsec.2": 16.7729,
        "qsec.3": 20.7918
      }
    }
  },
  "rules": {
    "rule1": {
      "antecedents": {
        "wt": {
          "wt.1": 3.0621
        },
        "mpg": {
          "mpg.2": 17.1275
        }
      },
      "consequents": {
        "qsec": {
          "qsec.1": 16.6082
        }
      }
    },
    "rule2": {
      "antecedents": {
        "hp": {
          "hp.1": 95.2824
        }
      },
      "consequents": {
        "qsec": {
          "qsec.3": 20.7918
        }
      }
    }
  },
  "default_rules": {
    "qsec": "qsec.2"
  }
}
'
FS <- jsonlite::fromJSON(FS_JSON)

.fs_replace_positions <- 
test_that("fs_replace_positions", {
  pos <- compute_optimal_quantile_fuzzy_set_positions(mtcars, 3)

  fs <- fs_replace_positions(FS, pos)

  expect_equal(fs$variables$input$hp, pos$hp)
  expect_equal(fs$variables$output$qsec, pos$qsec)
})


.parse_fuzzyset <- 
test_that("parse_fuzzyset", {
  expect_identical(parse_fuzzyset("wt.1"), list(name = "wt", set = 1L))
  expect_identical(parse_fuzzyset("tricky.nasty.2"), list(name = "tricky.nasty", set = 2L))
})

.discard_rule_values <- 
test_that("discard_rule_values", {
  rule <- discard_rule_values(FS$rules[[1]])
  expect_identical(rule, 
    list(antecedents = list(wt = list(wt.1 = NA_real_), mpg = list(mpg.2 = NA_real_)), 
      consequents = list(qsec = list(qsec.1 = NA_real_))))
})


.new_rule <- 
test_that("new_rule", {
  rule <- new_rule(list(foo = "wt.1", bar = "mpg.2"), list("qsec.1"))
  expect_identical(rule, discard_rule_values(FS$rules[[1]]))

  rule <- new_rule(list(toto = "hp.1"), list(titi = "qsec.3"))
  expect_identical(rule, discard_rule_values(FS$rules[[2]]))
})


.new_default_rule <- 
test_that("new_default_rule", {
  rule <- new_default_rule(list(foo = "wt.1", bar = "mpg.2"))
  expect_identical(rule, list(wt = "wt.1", mpg = "mpg.2"))

  rule <- new_default_rule(list("wt.1"))
  expect_identical(rule, list(wt = "wt.1"))
})


.fs_set_rules <- 
test_that("fs_set_rules", {
  rules <- fs_get_rules(FS)
  rule <- new_rule(list("toto.3"), list("qsec.2"))

  rules <- c(rules, list(rule3 = rule))

  fs2 <- fs_set_rules(FS, rules)
  expect_identical(fs_get_rules(fs2), rules)

  ### fs_remove_rule
  fs3 <- fs_remove_rule(fs2, 3)
  expect_identical(fs3, FS)
  
  ### fs_add_rule
  fs4 <- fs_add_rule(fs3, rule)
  expect_identical(fs4, fs2)
})
