Compile
-------

Use sbt (+0.13) to create a jar file:
```
sbt assembly
```

Test
----
Use sbt (+0.13) to run the tests:
```
sbt test
```

Run benchmarks
--------------

Only Java (+1.7) is needed.
For example:
```
java -cp target/scala-2.10/picc-assembly-1.0.jar \
     reopp.common.benchmarks.SCP14
```

Check the source code to see other benchmarks. Bash scripts (in the ```scripts``` folder) were used to perform several calls to the benchmarks.
