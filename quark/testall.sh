#!/bin/bash
#script used for reg testing
COMPILER="./quarkc"

for TESTFILE in tests/*.qk;
do
    echo "  TESTING $TESTFILE"
    LEN=$((${#TESTFILE}-3))
    CPPOUTNAME="${TESTFILE:0:$LEN}.cpp"
    echo "$CPPOUTNAME"
    EXECUTABLENAME="${TESTFILE:0:$LEN}"
    OUTFILENAME="${TESTFILE:0:$LEN}.coutput"
    echo "$OUTFILENAME"
    TESTFILENAME="${TESTFILE:0:$LEN}.output"
    "$COMPILER" -s "$TESTFILE" -c "$CPPOUTNAME" -o "$EXECUTABLENAME"
    ./"$EXECUTABLENAME" > "$OUTFILENAME"
    if (diff "$OUTFILENAME" "$TESTFILENAME") 
    then
        echo "      ----- SUCESS -----"
    else
        echo "      ------- FAIL -----"
    fi
    rm "$OUTFILENAME" "$CPPOUTNAME" "$EXECUTABLENAME"
done