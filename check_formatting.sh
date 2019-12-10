#!/bin/bash

# Identify the platform/CI server on which this script is running
unameOut="$(uname -s)"
case "${unameOut}" in
    Linux*)     skipCheck=true;;
    Darwin*)    skipCheck=false;;
    *)          skipCheck=false
esac

# Unless testing locally, you will want to skip format checking on anything other
# thank linux
if  [[ "$skipCheck" = false ]] ; then
    echo "Skipping format check on $unameOut platform"
    exit 0
fi

# The find `-regex -E` args on Linux and Darwin work differently. Normally
# the Darwin path would not need to be invoked unless testing the script remotely
base_dir="."
case "${unameOut}" in
    Linux*)     all_paths=$(find $base_dir -iname *.cc -o -iname *.cpp -o -iname *.hpp -o -iname *.cxx -o -iname *.h);;
    Darwin*)    all_paths=$(find -E $base_dir -regex '.*\.(cc|cpp|hpp|cxx|h)')
esac

# Auto-formatting script that checks whether the CWD and child directories have
# formatted source code.
for path in $all_paths; do
    ORIG=$(cat "$path")
    # The `--style=file` argument will automatically find the .clang-format and
    # it doesn't take a file path but just the string `file`.
    # If .clang-format is missing the script will exit.
    # The `--style=llvm` will use the llvm standard style.
    AUTO=$(clang-format --style=llvm --fallback-style=none "$path")
    diff <(echo "$ORIG") <(echo "$AUTO") > /dev/null ||{ echo "$path is not formatted"; exit 1; }
done

echo "Format check complete"
exit 0
