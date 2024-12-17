https://adventofcode.com/2024/day/7

## caveats:

1. use Long over Int. because 173208328696 will overflow Int.

```txt
Exception in thread "main" java.lang.NumberFormatException: For input string: 
"173208328696" at java.base/java.lang.NumberFormatException.forInputString(NumberFormatException.java:67) at 
java.base/java.lang.Integer.parseInt(Integer.java:668) 
at java.base/java.lang.Integer.parseInt(Integer.java:786) 
at scala.collection.StringOps$.toInt$extension(StringOps.scala:910) 
at day7.Reader$.read$$anonfun$1(RestoreOperators.scala:62) 
at scala.collection.immutable.List.map(List.scala:247) 
at day7.Reader$.read(RestoreOperators.scala:60) 
at day7.RestoreOperators$.main(RestoreOperators.scala:78) 
at day7.RestoreOperators.main(RestoreOperators.scala)
```

2. Be careful with the base of the foldLeft. We should use 0 for addition and 1 for multiplication.
