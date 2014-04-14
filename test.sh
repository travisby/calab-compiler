./clean
./compile
for expected in `ls assignment2_examples/`
do
    echo '-------------------------------';
    echo EXPECTING TO $expected;
    for file in `ls assignment2_examples/$expected`
    do
        echo "test: " $file
        result=`BOLT_FILE=./log_config ./compiler assignment2_examples/$expected/$file`
        warn=`echo $result | grep -i warn | wc -l`
        error=`echo $result | grep -i error | wc -l`
        echo "Warn Count: " $warn
        echo "Error Count: " $error
    done
done
