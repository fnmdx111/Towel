bind Macro1 fun,
  (@ if>0 +, -)`
then bind UseMacro fun,
  (1 2 Macro1) "It's not compile time macro though, but it supports type"
               "checking."
then UseMacro.

