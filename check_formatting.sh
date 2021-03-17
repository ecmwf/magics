#!/bin/bash

# Script to check whether a repo has been properly formatted with clang-format.

# The find `-regex` arg on Linux and Darwin works differently. Normally
# the Darwin path would not need to be invoked unless testing the script locally.
unameOut="$(uname -s)"
base_dir="."
case "${unameOut}" in
    Darwin*)    files=$(find -E $base_dir -regex '.*\.(cpp|hpp|cc|cxx|h|c)');;
    Linux*)     files=$(find $base_dir -regex '.*\.\(cpp\|hpp\|cc\|cxx\|h\|c\)')
esac

for file in $files; do
    if [[ $file =~ "/attributes/" ]]; then
        echo "Skipping autoformat check for file: $file"
	    continue
    fi

    if [[ $file =~ "drivers/minizip" ]]; then
        # Skip minizip because clang-format breaks this code to the point where it doesn't
        # compile.
        echo "Skipping autoformat check for file: $file"
    else
        ORIG=$(cat "$file")
        # The `--style=file` argument will automatically find the .clang-format and
        # it doesn't take a file path but just the string `file`.
        # If .clang-format is missing the script will exit.
        AUTO=$(clang-format --sort-includes --style=file --fallback-style=none "$file")
        diff <(echo "$ORIG") <(echo "$AUTO") > /dev/null || { echo "$file is not formatted"; exit 1; }
    fi
done

echo "Format check complete"
