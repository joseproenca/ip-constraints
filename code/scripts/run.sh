#!/bin/bash

JAR=../out/artifacts/reopp.jar
OUT=out

mkdir -p $OUT

echo "# Schedules 10 - 70 (step 10)"                    > $OUT/Schedules.csv

for ((i=10;i<=70;i=i+10)); do
  printf "Choco, $i"                                    >> $OUT/Schedules.csv
  for run in {1..10}; do
    printf ", "                                         >> $OUT/Schedules.csv
    java -cp $JAR common.beh.benchmarks.ChoSchedules $i >> $OUT/Schedules.csv
  done
  echo ""                                               >> $OUT/Schedules.csv
done

for ((i=10;i<=70;i=i+10)); do
  printf "PAS, $i"                                      >> $OUT/Schedules.csv
  for run in {1..10}; do
    printf ", "                                         >> $OUT/Schedules.csv
    java -cp $JAR common.beh.benchmarks.GCSchedules $i  >> $OUT/Schedules.csv
  done
  echo ""                                               >> $OUT/Schedules.csv
done

for ((i=10;i<=70;i=i+10)); do
  printf "PAC, $i"                                      >> $OUT/Schedules.csv
  for run in {1..10}; do
    printf ", "                                         >> $OUT/Schedules.csv
    java -cp $JAR common.beh.benchmarks.GCSchedules $i c >> $OUT/Schedules.csv
  done
  echo ""                                               >> $OUT/Schedules.csv
done

for ((i=10;i<=70;i=i+10)); do
  printf "PAS-SAT, $i"                                  >> $OUT/Schedules.csv
  for run in {1..10}; do
    printf ", "                                         >> $OUT/Schedules.csv
    java -cp $JAR common.beh.benchmarks.GCSchedules $i n i >> $OUT/Schedules.csv
  done
  echo ""                                               >> $OUT/Schedules.csv
done

#####

echo "# Primes 10 - 150 (step 20)"                     > $OUT/Primes.csv       

for ((i=10;i<=150;i=i+20)); do
  printf "Choco, $i"                                    >> $OUT/Primes.csv
  for run in {1..10}; do
    printf ", "                                         >> $OUT/Primes.csv
    java -cp $JAR common.beh.benchmarks.ChoPrimes $i    >> $OUT/Primes.csv
  done
  echo ""                                               >> $OUT/Primes.csv
done

for ((i=10;i<=150;i=i+20)); do
  printf "PAS, $i"                                      >> $OUT/Primes.csv
  for run in {1..10}; do
    printf ", "                                         >> $OUT/Primes.csv
    java -cp $JAR common.beh.benchmarks.GCPrimes $i     >> $OUT/Primes.csv
  done
  echo ""                                               >> $OUT/Primes.csv
done

for ((i=10;i<=150;i=i+20)); do
  printf "PAC, $i"                                      >> $OUT/Primes.csv
  for run in {1..10}; do
    printf ", "                                         >> $OUT/Primes.csv
    java -cp $JAR common.beh.benchmarks.GCPrimes $i c   >> $OUT/Primes.csv
  done
  echo ""                                               >> $OUT/Primes.csv
done

for ((i=10;i<=150;i=i+20)); do
  printf "PAS-SAT, $i"                                  >> $OUT/Primes.csv
  for run in {1..10}; do
    printf ", "                                         >> $OUT/Primes.csv
    java -cp $JAR common.beh.benchmarks.GCPrimes $i n i >> $OUT/Primes.csv
  done
  echo ""                                               >> $OUT/Primes.csv
done


####

echo "# ParSpouts 10 - 150 (step 20)"                     > $OUT/ParSpouts.csv       

for ((i=10;i<=150;i=i+20)); do
  printf "Choco, $i"                                    >> $OUT/ParSpouts.csv
  for run in {1..1}; do
    printf ", "                                         >> $OUT/ParSpouts.csv
    java -cp $JAR common.beh.benchmarks.ChoParSpouts $i    >> $OUT/ParSpouts.csv
  done
  echo ""                                               >> $OUT/ParSpouts.csv
done

for ((i=10;i<=150;i=i+20)); do
  printf "PAS, $i"                                      >> $OUT/ParSpouts.csv
  for run in {1..1}; do
    printf ", "                                         >> $OUT/ParSpouts.csv
    java -cp $JAR common.beh.benchmarks.GCParSpouts $i     >> $OUT/ParSpouts.csv
  done
  echo ""                                               >> $OUT/ParSpouts.csv
done

for ((i=10;i<=150;i=i+20)); do
  printf "PAC, $i"                                      >> $OUT/ParSpouts.csv
  for run in {1..1}; do
    printf ", "                                         >> $OUT/ParSpouts.csv
    java -cp $JAR common.beh.benchmarks.GCParSpouts $i c   >> $OUT/ParSpouts.csv
  done
  echo ""                                               >> $OUT/ParSpouts.csv
done

for ((i=10;i<=150;i=i+20)); do
  printf "PAS-SAT, $i"                                  >> $OUT/ParSpouts.csv
  for run in {1..1}; do
    printf ", "                                         >> $OUT/ParSpouts.csv
    java -cp $JAR common.beh.benchmarks.GCParSpouts $i n i >> $OUT/ParSpouts.csv
  done
  echo ""                                               >> $OUT/ParSpouts.csv
done


####

echo "# Approval 0 - 10 (step 1)"                     > $OUT/Approval.csv       

for ((i=0;i<=10;i=i+1)); do
  printf "Choco, $i"                                    >> $OUT/Approval.csv
  for run in {1..10}; do
    printf ", "                                         >> $OUT/Approval.csv
    java -cp $JAR common.beh.benchmarks.ChoApproval $i    >> $OUT/Approval.csv
  done
  echo ""                                               >> $OUT/Approval.csv
done

for ((i=0;i<=10;i=i+1)); do
  printf "PAS, $i"                                      >> $OUT/Approval.csv
  for run in {1..10}; do
    printf ", "                                         >> $OUT/Approval.csv
    java -cp $JAR common.beh.benchmarks.GCApproval $i     >> $OUT/Approval.csv
  done
  echo ""                                               >> $OUT/Approval.csv
done

for ((i=0;i<=10;i=i+1)); do
  printf "PAC, $i"                                      >> $OUT/Approval.csv
  for run in {1..10}; do
    printf ", "                                         >> $OUT/Approval.csv
    java -cp $JAR common.beh.benchmarks.GCApproval $i c   >> $OUT/Approval.csv
  done
  echo ""                                               >> $OUT/Approval.csv
done

for ((i=0;i<=10;i=i+1)); do
  printf "PAS-SAT, $i"                                  >> $OUT/Approval.csv
  for run in {1..10}; do
    printf ", "                                         >> $OUT/Approval.csv
    java -cp $JAR common.beh.benchmarks.GCApproval $i n i >> $OUT/Approval.csv
  done
  echo ""                                               >> $OUT/Approval.csv
done

