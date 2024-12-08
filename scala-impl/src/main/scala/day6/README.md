
https://adventofcode.com/2024/day/6


Got stuck at part 2. 

```scala
mutantMaps.map { mutantMap =>
      val guardCopy = guard.copy()
      val repr1 = mutantMap.toReprV1
      val result = guardCopy.move(mutantMap)
      val repr2 = mutantMap.toReprV1
      result
    }
```

The mutantMaps are not independent. The `guardCopy.move(mutantMap)` will tinker the mutantMap and somehow affect the other mutantMaps. 
I need to figure out make a deep copy of the mutantMap.
