#!/bin/bash

JAR=reopp_scp.jar
OUT=waldorf-1
RUNS=12

mkdir -p $OUT

function header {
  echo
}

function run {
  NAME=$1
  FROM=$2
  TO=$3
  STEP=$4
  MODE=$5
  HEADER=$6

  if [ -n "$6" ]
    then echo "# $NAME $FROM - $TO (step $STEP)"        > $OUT/$NAME.csv
  fi
  for ((i=$FROM;i<=$TO;i=i+$STEP)); do
    printf "$MODE, $i"                                 >> $OUT/$NAME.csv
    for run in $(eval echo {1..$RUNS}); do
      printf ", "                                      >> $OUT/$NAME.csv
      timeout 10 java -jar $JAR "$NAME" $i $MODE        >> $OUT/$NAME.csv
    done
    echo ""                                            >> $OUT/$NAME.csv
  done
}

function satsmt {
  run $1 $2 $3 $4 smt header
  run $1 $2 $3 $4 sat
}

function partial {
  run $1 $2 $3 $4 all header
  run $1 $2 $3 $4 partial1
  run $1 $2 $3 $4 partial2
  run $1 $2 $3 $4 partial4
}

satsmt sensors 1 501 50
satsmt transactions-spar  1 301 30
satsmt transactions-sseq1 1 301 30
satsmt transactions-sseqn 1 301 30
satsmt transactions-bpar  1 801 80
satsmt transactions-bseq1 1 801 80
satsmt transactions-bseqn 1 801 80
partial pairwise 1 81 8