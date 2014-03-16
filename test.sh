for expected in `ls examples/`
do
    echo '-------------------------------';
    echo EXPECTING TO $expected;
    for file in `ls examples/$expected`
    do
        echo "test: " $file
        result=`BOLT_FILE=./log_config ./compiler examples/$expected/$file`
        warn=`echo $result | grep -i warn | wc -l`
        error=`echo $result | grep -i error | wc -l`
        echo "Warn Count: " $warn
        echo "Error Count: " $error
    done
done
