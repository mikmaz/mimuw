#!/bin/bash

if [ -z "$1" ] || [ -z "$2" ]
then
    echo "Not enough arguments."
    exit 1
elif [ ! -z "$3" ]
then
    echo "Too many arguments."
    exit 1
fi

program=$1
tests_dir=$2

passed=""
failed=""

echo -e '############ TESTING '"$program"' PROGRAM ############\n'
echo -e "------------ VALGRIND MEMORY TEST --------------\n"
for test in "$tests_dir"/*.in
do
    filename="${test%%.in}"

    valgrind --log-fd=3 ./"$program" < "$test" 1> "$filename"_tmp.out 2> "$filename"_tmp.err 3> "$filename"_val.out
    exit_code="$?"
    mem_summary=$( tail -n 1 $filename"_val.out" )
    
    echo 'Valgrind report for: '"$filename"
    echo -e "$mem_summary"'\n'

    # Check exit code.
    if [ "$exit_code" != "0" ]
    then
    	failed+="$filename"'.in [exit code '"$exit_code"']\n'
    else
      # Check stdout.
    	if diff "$filename".out "$filename"_tmp.out >/dev/null 2>&1
    	then
    		passed+="$filename"'.out\n'
    	else
    		failed+="$filename"'.out\n'
    	fi

    	# Check stderr.
    	if diff "$filename".err "$filename"_tmp.err >/dev/null 2>&1
    	then
    		passed+="$filename"'.err\n'
    	else
    		failed+="$filename"'.err\n'
    	fi
    fi

    # Remove temporary files.
    rm "$filename"_tmp.err
    rm "$filename"_tmp.out
    rm "$filename"_val.out
done

echo -e "------------ PASSED ------------"
if [ "$passed" = "" ]
then
	echo -e "All tests failed.\n"
else
	echo -e "$passed"
fi

echo -e "------------ FAILED ------------"
if [ "$failed" = "" ]
then
	echo -e "All tests passed.\n"
else
	echo -e "$failed"
fi
