#!/bin/bash

# Script to check whether a repo has been properly formatted with clang-format.

# The find `-regex` arg on Linux and Darwin works differently. Normally
# the Darwin path would not need to be invoked unless testing the script locally.
unameOut="$(uname -s)"
base_dir="."
case "${unameOut}" in
    Darwin*)    all_paths=$(find -E $base_dir -regex '.*\.(cpp|hpp|cc|cxx|h|c)');;
    Linux*)     all_paths=$(find $base_dir -regex '.*\.\(cpp\|hpp\|cc\|cxx\|h\|c\)')
esac

for path in $all_paths; do
    ORIG=$(cat "$path")
    # The `--style=file` argument will automatically find the .clang-format and
    # it doesn't take a file path but just the string `file`.
    # If .clang-format is missing the script will exit.
    AUTO=$(clang-format --style=file --fallback-style=none "$path")
    diff <(echo "$ORIG") <(echo "$AUTO") > /dev/null || { echo "$path is not formatted"; exit 1; }
done

echo "Format check complete"
