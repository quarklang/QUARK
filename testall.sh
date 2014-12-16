#!/bin/bash
COMPILER="quark/quarkc"

cd quark
make clean
make
cd ../

for TESTFILE in tests/*.qk;
do
    echo "  TESTING $TESTFILE"
    
    LEN=$((${#TESTFILE}-3))
    CPPOUTNAME="${TESTFILE:0:$LEN}.cpp"
    EXECUTABLENAME="${TESTFILE:0:$LEN}"
    OUTFILENAME="${TESTFILE:0:$LEN}.coutput"
    TESTFILENAME="${TESTFILE:0:$LEN}.output"

    ./"$COMPILER" -s "$TESTFILE" -c "$CPPOUTNAME" -o "$EXECUTABLENAME"
    ./"$EXECUTABLENAME" > "$OUTFILENAME"
    if (diff "$OUTFILENAME" "$TESTFILENAME") 
    then
        echo "      ----- SUCCESS -----"
    else
        echo "      ------- FAIL -----"
    fi
    rm "$OUTFILENAME" "$CPPOUTNAME" "$EXECUTABLENAME"
done