
expect_no_error({

  library(tidySEM)

  k <- 4

  make_script({
    data("empathy")
    df <- empathy[1:6]
    mx_growth_mixture(model = "i =~ 1*ec1 + 1*ec2 + 1*ec3 +1*ec4 +1*ec5 +1*ec6
                           s =~ 0*ec1 + 1*ec2 + 2*ec3 +3*ec4 +4*ec5 +5*ec6
                           ec1 ~~ vec1*ec1
                           ec2 ~~ vec2*ec2
                           ec3 ~~ vec3*ec3
                           ec4 ~~ vec4*ec4
                           ec5 ~~ vec5*ec5
                           ec6 ~~ vec6*ec6
                           i ~~ 0*i
                           s ~~ 0*s
                           i ~~ 0*s",
                      run = F, # For testing
                      classes = k,
                      data = df
    ) -> res

    res2 <- mxModel(res, name = glue("empathy-model-{k}"))

    invisible(mxSave(res2))

  }, name = glue('empathy-model-{k}'),
  path = tempdir()) -> m

  cat("Running script: ", m, "\n")

  ret <- run_script(script_path = m,
             wait = TRUE) # Wait is necessary for testing.

  cat("Script return code: ", ret, "\n")

  if(ret != 0) {
    cat(extract_log(script_path = m,
                type = 'message'), "\n")

  }

  summary(extract_variable(script_path = m,
                           variable_name = 'res2'))

  extract_log(script_path = m,
              type = 'output')

  remove_script(m)

})
