# rlang_error_to_html works

    Code
      rlang_error_to_html(e)
    Warning <simpleWarning>
      `x` contains unescaped HTML special character '<' at index [1] (see ?html_esc); you can use `warn=FALSE` to turn off these warnings.
    Output
      <pre><error/rlang_error><br>Error:<br>! test<br>---<br>Backtrace:<br>x</pre>

