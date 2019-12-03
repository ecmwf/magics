#!/bin/bash

# Auto-formatting script that checks whether the CWD and child directories have
# formatted source code.
find . -iname *.h -o -iname *.c -o -iname *.cpp -o -iname *.cc -o -iname *.hpp | while read path; do
    ORIG=$(cat $path)
    # The `--style=file` arguments while automatically find the .clang-format
    # config file. It doesn't take a file path but just the string `file`
    # If .clang-format is missing the script will exit.
    AUTO=$(clang-format --style=file --fallback-style=none $path)
    diff <(echo $ORIG) <(echo $AUTO) > /dev/null || echo "$path is not formatted, format with clang-format"
done
